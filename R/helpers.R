# Prepare tomstats inputs
#
# This function prepares all the necessary information prior to using the cpp
# to compute statistics in tomstats. It processes the 'reh' object, matches
# memory settings, converts start and stop indices, prepares main effects,
# fixed effects, interaction effects, covariate information, and scaling
# information.
#
# [param] effects A formula object specifying the effects to be included in
# the analysis.
# [param] reh The reh object containing the event history data.
# [param] attr_data (Optional) A data frame containing attribute data for the
# actors.
# [param] memory A character vector specifying the memory type, which can be
# one of "full", "window", "decay", or "interval".
# [param] memory_value (Optional) The value associated with the memory type.
# [param] start The starting index of the event history data to consider.
# [param] stop The ending index of the event history data to consider.
#
# [return] A list containing all the necessary information for input to the
# 'compute_stats_tie' function.
#
# [examples]
# inputs <- prepare_tomstats(
#   effects, reh, attr_data, memory, memory_value,
#   start, stop
# )
prepare_tomstats <- function(effects, reh, attr_data = NULL,
                             memory = c("full", "window", "decay", "interval"),
                             memory_value = NA, start = 1, stop = Inf) {
  # Check if reh is of class remify
  if (!("remify" %in% class(reh))) {
    stop("Expected a reh object of class remify")
  }

  # Check if the reh object is prepared with the correct model argument
  if (attr(reh, "model") != "tie") {
    stop("The reh object should be prepared with the model argument set to `tie' if tie_effects are computed")
  }

  # Extract relevant elements from the prepared remify::remify object
  edgelist.reh <- reh$edgelist
  dyads <- attr(reh, "dyad")
  actors <- attr(reh, "dictionary")$actors
  types <- attr(reh, "dictionary")$types

  # Convert actor IDs to cpp indexing
  actors$actorID <- actors$actorID - 1

  if (is.null(types)) {
    # Prepare a default types object
    types <- data.frame(typeName = 0, typeID = 0)
  } else {
    # Convert type IDs to cpp indexing
    types$typeID <- types$typeID - 1
  }

  # Prepare the event weights
  if (attr(reh, "weighted")) {
    weight <- edgelist.reh$weight
  } else {
    weight <- rep(1, nrow(edgelist.reh))
  }

  # Prepare the edgelist "evls"-style
  if(attr(reh, "ordinal")) {
    edgelist.reh$time <- 1:nrow(edgelist.reh)
  } else {
    edgelist.reh$time <- cumsum(reh$intereventTime)
  }
  edgelist.reh <- matrix(cbind(edgelist.reh$time, dyads, weight),
    ncol = 3, byrow = FALSE
  )
  edgelist.reh[, 2] <- edgelist.reh[, 2] - 1 # cpp indexing!

  # Prepare the full risk set
  prepR <- getRisksetMatrix(
    actorID = actors$actorID,
    typeID = types$typeID,
    N = nrow(actors),
    C = nrow(types),
    directed = attr(reh, "directed")
  )

  # Reduce risk set to "active" dyads only
  if (attr(reh, "riskset") == "active") {
    # Get dyadInfo
    dyad <- attr(reh, "dyad")
    dyadIDactive <- as.vector(attr(reh, "dyadIDactive"))
    dyadInfo <- data.frame(dyadIDactive, dyad)
    dyadInfo <- unique(dyadInfo[order(dyadIDactive), ])

    # Select "active" dyads only
    prepR <- prepR[dyadInfo$dyad, ]
    full_dyad_id <- prepR[, 4] # cpp indexing!
    prepR[, 4] <- seq(0, nrow(prepR) - 1, 1)
    edgelist.reh[, 2] <- match(dyads - 1, full_dyad_id) - 1 # cpp indexing!
  }

  # Match memory
  memory <- match.arg(memory)
  memory_value <- validate_memory(memory, memory_value)

  # Convert R start and stop indices to C++ (indexing starts at 0)
  if (start < 1) {
    stop("The 'start' value should be set to 1 or a larger number.")
  }
  if (stop < start) {
    stop("The 'stop' value cannot be smaller than the 'start' value.")
  }
  start <- start - 1
  if (stop == Inf) {
    stop <- nrow(edgelist.reh)
  }
  stop <- stop - 1

  # Prepare main effects
  check_formula(effects)
  form <- effects
  effects <- parse_formula(form, "rem", attr(reh, "ordinal"))
  effectNames <- sapply(effects, function(x) x$effect)

  # Check correct specification effects
  if(!attr(reh, "directed")) {
    if(!all(effectNames %in% tie_effects(directed = FALSE))) {
      stop("Attempting to request effects that are not (yet) defined for undirected events")
    }
  }

  if(attr(reh, "directed")) {
    if(!all(effectNames %in% tie_effects(directed = TRUE))) {
      stop("Attempting to request effects that are not (yet) defined for directed events")
    }
  }

  # Prepare fixed effects
  if (any(effectNames == "FEtype")) {
    C <- nrow(types)
    FEeffects <- lapply(2:C, function(c) {
      x <- list()
      x$effect <- "FEtype"
      x$scaling <- 1
      x$typeName <- types$typeName[c]
      x$typeID <- types$typeID[c]
      x
    })

    pos <- which(effectNames == "FEtype")
    effects <- append(effects[-pos], FEeffects, pos - 1)
  }

  # Prepare interaction effects
  effects_int <- parse_int(form, "rem", effects, attr(reh, "ordinal"))
  effectNames <- append(effectNames, rep("interact", length(effects_int)), length(effectNames))
  interactions <- list()
  interactions[which(effectNames == "interact")] <- effects_int

  # Prepare covariate information
  covar <- process_covariate(effects, attr_data, actors, edgelist.reh, reh, prepR)

  # Prepare scaling info (vector length p)
  scaling <- sapply(effects, function(x) {
	  ifelse("scaling" %in% names(x), x$scaling, "none")
  })
  scaling <- append(scaling, rep("none", length(effects_int)), length(scaling))

  # Prepare consider_type info
  consider_type <- sapply(effects, function(x) {
	  ifelse("consider_type" %in% names(x), x$consider_type, FALSE)
  })
  consider_type <- append(consider_type, rep(FALSE, length(effects_int)), length(consider_type))

  # Check correct scaling inertia statistic
  if (!attr(reh, "directed")) {
    if (any(sapply(effects, function(x) x$effect == "inertia"))) {
      idx <- which(sapply(effects, function(x) x$effect == "inertia"))
      if (any(scaling[idx] == "prop")) {
        stop("Proportional scaling of the inertia effect is not defined for undirected events")
      }
    }
  }

  # Output
  list(
    form = form,
    effects = effects,
    effectNames = effectNames,
    edgelist = edgelist.reh,
    actors = actors,
    types = types,
    prepR = prepR,
    memory = memory,
    memory_value = memory_value,
    scaling = scaling,
    consider_type = consider_type,
    covar = covar,
    interactions = interactions,
    start = start,
    stop = stop
  )
}

# Check if Formula Effects are Specified as Functions
#
# This function checks if the effects in a given formula are specified as
# functions, as expected. It throws an error if any effect is not specified as
# a function.
#
# [param] formula A formula object specifying the effects.
#
# [examples]
# formula1 <- ~ inertia()
# formula2 <- ~ inertia
# check_formula(formula1)  # No error
# check_formula(formula2)  # Throws an error
check_formula <- function(formula) {
  formula_str <- as.character(formula)
  effect_names <- gsub("~ ", "", formula_str) # Extract effect names
  effect_names <- setdiff(effect_names, "~") # Remove "~"
  effect_names <- strsplit(effect_names, "\\+\\s*")[[1]] # Split effect names by "+"
  effect_names <- gsub("\\s+", "", effect_names) # Remove spaces from effect names


  # Check if effects are specified as functions
  incorrect_effects <- effect_names[!grepl("\\(.*\\)", effect_names)]
  incorrect_effects <- incorrect_effects[!grepl("1", incorrect_effects)]

  if (length(incorrect_effects) > 0) {
    stop(paste(
      "The following effects are not specified as functions:",
      paste(incorrect_effects, collapse = ", ")
    ))
  }
}

# Validate memory settings
#
# This function validates the memory settings for a specific memory type. It
# checks the validity of the supplied 'memory_value' based on the selected
# 'memory' type.
#
# [param] memory The memory type, which can be one of "full", "window",
# "decay", or "interval".
# [param] memory_value The value associated with the memory type.
#
# [return] The validated memory value based on the memory type.
#
# [examples]
# validated_value <- validate_memory("window", 10)
validate_memory <- function(memory, memory_value) {
  if (memory == "full") {
    memory_value <- Inf
  }
  if (memory == "window") {
    stopifnot(
      "A 'memory_value' of numeric type should be supplied when memory is 'window'" =
        length(memory_value) == 1 && is.numeric(memory_value)
    )
  }
  if (memory == "decay") {
    stopifnot(
      "A 'memory_value' of numeric type should be supplied when memory is 'decay'" =
        length(memory_value) == 1 && is.numeric(memory_value)
    )
  }
  if (memory == "interval") {
    stopifnot(
      "Two 'memory_value' values of numeric type should be supplied when memory is 'interval'" =
        length(memory_value) == 2 && is.numeric(memory_value)
    )
    stopifnot(
      "The first 'memory_value' value should be lower than the second value" =
        memory_value[1] < memory_value[2]
    )
  }

  return(memory_value)
}

# Get all tie effects
#
# This function returns a vector of all available tie effects that can be used
# in tie-oriented models.
#
# [return] A character vector containing all tie effects.
#
# [examples]
# tie_effects <- all_tie_effects()
all_tie_effects <- function() {
  c(
    "baseline", 
    "FEtype",

    # Exogenous stats
    "send", 
    "receive",
    "tie",  
    "same", 
    "difference", 
    "average", 
    "minimum", 
    "maximum", 
    "event", 

    # Endogenous stats
    "inertia", 
    "reciprocity", 

    "indegreeSender", 
    "indegreeReceiver", 
    "outdegreeSender", 
    "outdegreeReceiver", 
    "totaldegreeSender", 
    "totaldegreeReceiver", 

    "totaldegreeDyad",
    "degreeMin", 
    "degreeMax", 
    "degreeDiff", 
    "ccp",  

    "otp", 
    "itp", 
    "osp", 
    "isp", 
    "sp", 

    "psABBA", 
    "psABBY", 
    "psABXA", 
    "psABXB", 
    "psABXY", 
    "psABAY", 
    "psABAB", 

    "rrankSend", 
    "rrankReceive", 
    
    "recencyContinue", 
    "recencySendSender", 
    "recencySendReceiver",
    "recencyReceiveSender", 
    "recencyReceiveReceiver",

    "userStat", 
    
    "interact" 
  )
}

# Add variable names to statistics
#
# This function adds variable names to the statistics object based on the
# specified effects and interactions. It modifies the dimnames of the
# statistics object to include the variable names for specific effects.
#
# [param] statistics The statistics object to which variable names will be
# added.
# [param] all_effects A vector containing all possible effects.
# [param] effectsN A numeric vector specifying the indices of the effects in
# 'all_effects' that are present in the statistics.
# [param] effects A list containing information about the effects, including
# the effect name and associated variable.
# [param] interactions A list containing information about the interactions,
# including the indices of the interacting effects.
#
# [return] The modified statistics object with variable names added.
#
# [examples]
# statistics <- add_variable_names(
#   statistics, all_effects, effectsN, effects,
#   interactions
# )
add_variable_names <- function(statistics, effectNames, effects, interactions, scaling) {
  # Helper function to add variable name to effect
  add_variable_name <- function(effect, variable) {
    if (!is.null(variable)) {
      paste0(effect, "_", variable)
    } else {
      effect
    }
  }

  # Dimnames statistics
  dimnames(statistics) <- list(NULL, NULL, effectNames)

  # Add variable name to exogenous statistics
  exogenous_indices <- which(sapply(effects, function(x) {
    "variable" %in% names(x)
  }))

  dimnames(statistics)[[3]][exogenous_indices] <- sapply(
    effects[exogenous_indices],
    function(x) add_variable_name(x$effect, x$variable)
  )

  # Add counter to tie name
  tie_effects <- grepl("tie", dimnames(statistics)[[3]])
  if (sum(tie_effects) > 1) {
    dimnames(statistics)[[3]][tie_effects] <- paste0("tie", 1:sum(tie_effects))
  }

  # Add counter to event name
  event_effects <- grepl("event", dimnames(statistics)[[3]])
  if (sum(event_effects) > 1) {
    dimnames(statistics)[[3]][event_effects] <- paste0("event", 1:sum(event_effects))
  }

  # Add .unique 
  unique_effects <- sapply(effects, function(x) {
    ifelse("scaling" %in% names(x), isTRUE(grepl("unique", x$scaling)), FALSE)
  })
  if (any(unique_effects)) {
    dimnames(statistics)[[3]][unique_effects] <- paste0(dimnames(statistics)[[3]][unique_effects], ".unique")
  }

  # Add .type 
  type_effects <- sapply(effects, function(x) isTRUE(x$consider_type))
  if (any(type_effects)) {
    dimnames(statistics)[[3]][type_effects] <- paste0(dimnames(statistics)[[3]][type_effects], ".type")
  }
  

  # Add variable name to interaction statistics
  interaction_index <- which(effectNames == "interact")
  dimnames(statistics)[[3]][interaction_index] <- sapply(
    interactions[interaction_index],
    function(x) {
      paste0(
        dimnames(statistics)[[3]][as.numeric(x[1])],
        ":",
        dimnames(statistics)[[3]][as.numeric(x[2])]
      )
    }
  )
  interactionNames <- vector(length = length(interaction_index))
  for (i in 1:length(interaction_index)) {

  	spl <- unlist(strsplit(dimnames(statistics)[[3]][interaction_index[i]], ":"))
  	for (ss in 1:2) {
  		ind <- match(spl[ss], dimnames(statistics)[[3]][-interaction_index])
  		scaling[ind] <- gsub("_", ".", scaling[ind], fixed = TRUE)
  		spl[ss] <- paste0(spl[ss], ".", scaling[ind])
  	}
  	interactionNames[i] <- paste0(spl, collapse = ":")
  }
  
  # now add the scaling, leave out the baseline
  dimnamesForJasp <- dimnames(statistics)[[3]][-1]
  # somehow unique is duplicated after the scaling
  scaling <- gsub("_unique", "", scaling)
  scaling <- gsub("_", ".", scaling, fixed = TRUE)
  dimnamesForJasp <- paste0(dimnamesForJasp, ".", scaling[-1])
  dimnamesForJasp[interaction_index-1] <- interactionNames

  attr(statistics, "jaspnames") <- dimnamesForJasp
  statistics # Return the modified statistics object
}

# Prepare exogenous effect
#
# This function prepares an exogenous effect by matching the scaling, checking
# the presence of the variable in the attr_data object, and collecting the
# necessary information in a data frame.
#
# [param] effect The effect name.
# [param] variable The variable name.
# [param] attr_data A data frame containing attribute data for the actors.
# [param] scaling The scaling option for the effect, which can be "as.is" or
# "std".
#
# [return] A list containing the prepared exogenous effect information.
#
# [examples]
# exo_effect <- prep_exo(effect, variable, attr_data, scaling)
prep_exo <- function(effect, variable, attr_data, scaling) {

  # Check attr_data object
  if(is.function(attr_data)) {
    stop("Cannot find the correct 'attr_data' object. Possible naming conflict: consider renaming the 'attr_data' object.")
  }

  # Check variable
  if (!is.character(variable)) {
    stop("The 'variable' argument should be a string.")
  }

  # Prepare effect
  if (is.null(attr_data)) {
    list(
      effect = effect,
      variable = variable,
      x = NULL,
      scaling = scaling
    )
  } else {
    # Check if the variable name is in the attr_data object
    if (!(variable %in% colnames(attr_data))) {
      stop(paste0("Variable '", variable, "' not in attr_data object for the '", effect, "' effect.")) # nolint
    }

    # Check if the time variable is available
    if (!("time" %in% colnames(attr_data))) {
      stop(paste0("time variable is missing in attr_data object for the '", effect, "' effect.")) # nolint
    }
    if (anyNA(attr_data$time)) {
      stop("time variable in attr_data cannot have missing values")
    }
  	
  	if (!("name" %in% names(attr_data))) {
  		stop("The column containing the actors should be named 'name'")
  	}

    # Collect the information in a data.frame
    dat <- data.frame(
      name = attr_data$name,
      time = attr_data$time,
      x = attr_data[, variable]
    )

    # Warning for missing values
    if (anyNA(dat)) {
      warning("Missing values in the attr_data object for the '", effect, "' effect can cause unexpected behavior.")
    }

    # Set the third column name equal to the variable name
    colnames(dat)[3] <- variable

    # Output
    list(
      effect = effect,
      variable = variable,
      x = dat,
      scaling = scaling
    )
  }
}

# Parse formula
#
# This function parses a formula and extracts the effects specified in the
# formula.
#
# [param] formula The formula specifying the effects.
# [param] type The type of model being used, either "rem" (for the tie-oriented
# model) or "rateEffects" or "choiceEffects".
# [param] ordinal Logical indicating whether the event data is ordinal.
#
# [return] A list containing the parsed effects.
#
# [examples]
# parsed_effects <- parse_formula(formula, type, ordinal)
parse_formula <- function(formula, type, ordinal = FALSE) {
  ft <- stats::terms(formula)

  var <- attr(ft, "variables")
  var <- as.list(var)[-1]

  effects <- lapply(var, function(y) {
    eval(y)
  })

  if (type == "rem" & !ordinal & attr(ft, "intercept") == 1) {
    effects <- append(
      effects,
      list(list(effect = "baseline")), 0
    )
  }
  if (type == "rateEffects" & attr(ft, "intercept") == 1) {
    effects <- append(
      effects,
      list(list(effect = "baseline")), 0
    )
  }

  attributes(effects)$model <- type
  effects
}

# Parse interaction terms
#
# This function parses a formula and extracts the interaction terms specified
# in the formula.
#
# [param] formula The formula specifying the interaction terms.
# [param] type The type of model being used, either "rem" or "rateEffects".
# [param] effects The list of effects parsed from the formula.
# [param] ordinal Logical indicating whether the event data is ordinal.
#
# [return] A list containing the parsed interaction terms.
#
# [examples]
# parsed_interactions <- parse_int(formula, type, effects, ordinal)
parse_int <- function(formula, type, effects, ordinal = FALSE) {
  ft <- stats::terms(formula)
  ft.order <- attr(ft, "order")
  ft.factor <- attr(ft, "factors")
  interactions <- which(ft.order > 1)
  interactions <- lapply(interactions, function(i) {
    if (type == "rem" & !ordinal & attr(ft, "intercept") == 1) {
      which(ft.factor[, i] > 0) + 1
    } else if (type == "rateEffects" & attr(ft, "intercept") == 1) {
      which(ft.factor[, i] > 0) + 1
    } else {
      which(ft.factor[, i] > 0)
    }
  })

  interactions <- if (any(sapply(effects, function(x) x$effect == "FEtype")) &
    any(ft.order > 1)) {
    pos <- which(sapply(effects, function(x) x$effect == "FEtype"))
    unlist(lapply(1:length(interactions), function(i) {
      if (any(interactions[[i]] %in% pos)) {
        change <- which(!interactions[[i]] == min(pos))
        x <- interactions[[i]]
        x[change] <- x[change] + length(pos) - 1
        FE <- which(interactions[[i]] == min(pos))
        lapply(pos, function(p) {
          x[FE] <- p
          x
        })
      } else {
        list(interactions[[i]])
      }
    }), recursive = F)
  } else {
    interactions
  }

  attributes(interactions)$model <- type
  interactions
}

# Parse tie information
#
# This function parses the tie information provided in a list and prepares it
# for further analysis.
#
# [param] List The list containing the tie information.
# [param] reh The reh object representing the event history data.
#
# [return] A matrix representing the parsed tie information.
#
# [examples]
# parsed_tie <- parse_tie(List, reh)
parse_tie <- function(List, reh) {
  x <- List$x
  dictionary <- attr(reh, "dictionary")$actors
  # First column: actorName
  # Second column: actorID

  # Error message in the case of missing rownames
  if (is.null(rownames(x)) | is.null(colnames(x))) {
    if(nrow(x) != nrow(dictionary) | ncol(x) != nrow(dictionary)) {
      stop("Rows and columns of 'x' in tie() should match number of actors in the network")
    }
    rownames(x) <- colnames(x) <- dictionary[,1]
  }

  # Error message in the case of missing actors
  if ((!all(dictionary$actorName %in% rownames(x))) |
    (!all(dictionary$actorName %in% colnames(x)))) {
    stop("'x' in tie() should include values for all actors in the network")
  }

  # Recode
  rownames(x) <- dictionary[match(rownames(x), dictionary[, 1]), 2]
  colnames(x) <- dictionary[match(colnames(x), dictionary[, 1]), 2]

  # Reorder
  x <- x[order(as.numeric(rownames(x))), ]
  x <- x[, order(as.numeric(colnames(x)))]

  # Undirected events
  if (!attr(reh, "directed")) {
    if (!isSymmetric(x)) {
      if (all(is.na(x[upper.tri(x)]))) {
        x[upper.tri(x)] <- t(x)[upper.tri(x)]
      } else if (all(is.na(x[lower.tri(x)]))) {
        x[lower.tri(x)] <- t(x)[lower.tri(x)]
      } else {
        stop("Matrix 'x' in tie() is expected to be symmetric when directed is FALSE.") # note: one triangle with only NA values is also allowed
      }
    }
  }

  # Error message in the case of missing values
  if (anyNA(x[lower.tri(x)]) | anyNA(x[upper.tri(x)])) {
    stop("Matrix 'x' in tie() contains missing values.")
  }

  as.matrix(x)
}

# Modify Riskset
#
# This function modifies the riskset object by mapping actor and type IDs to their corresponding names in the event history data.
#
# [param] riskset The riskset object to be modified.
# [param] reh The reh object representing the event history data.
# [param] actors The actors information.
# [param] types The types information.
#
# [return] The modified riskset object.
#
# [examples]
# modified_riskset <- modify_riskset(riskset, reh, actors, types)
modify_riskset <- function(riskset, reh, actors, types) {
  riskset <- as.data.frame(riskset)

  if (attr(reh, "directed")) {
    colnames(riskset) <- c("sender", "receiver", "type", "id")
    riskset$sender <- actors$actorName[match(riskset$sender, actors$actorID)]
    riskset$receiver <- actors$actorName[match(riskset$receiver, actors$actorID)]
    riskset$type <- types$typeName[match(riskset$type, types$typeID)]
  } else {
    colnames(riskset) <- c("actor1", "actor2", "type", "id")
    riskset$actor1 <- actors$actorName[match(riskset$actor1, actors$actorID)]
    riskset$actor2 <- actors$actorName[match(riskset$actor2, actors$actorID)]
    riskset$type <- types$typeName[match(riskset$type, types$typeID)]
  }

  riskset$id <- riskset$id + 1

  if (length(unique(riskset$type)) == 1) {
    riskset <- riskset[, -3]
  }

  riskset # Return the modified riskset object
}

# Process Covariate
#
# This function processes the covariates for different exogenous effects in
# the tie-oriented model.
#
# [param] effects A list of effects containing information about the covariates.
# [param] attr_data The attr_data object containing the covariate data.
# [param] actors The actors information.
# [param] edgelist The edgelist representing the network.
# [param] reh The reh object representing the event history data.
# [param] prepR The prepR object representing the risk set.
#
# [return] A list of processed covariates for each effect.
#
# [examples]
# covariates <- process_covariate(effects, attr_data, actors, edgelist, reh, prepR)
process_covariate <- function(effects, attr_data, actors, edgelist, reh,
                              prepR) {
  lapply(effects, function(x) {
    effect <- x$effect
    if (effect %in% c("send", "receive", "same", "difference", "average", "minimum", "maximum")) {
      if (is.null(x$x)) {
        variable <- x$variable

        if (!(variable %in% colnames(attr_data))) {
          stop(paste0("Variable '", variable, "' not in attr_data object for the '", effect, "' effect."))
        }
        if (!("time" %in% colnames(attr_data))) {
          stop("time variable is missing in attr_data object")
        }
        if (anyNA(attr_data$time)) {
          stop("time variable in attr_data cannot have missing values")
        }
        if (!("name" %in% names(attr_data))) {
        	stop("The column in attr_data containing the actors should be named 'name'")
        }

        dat <- data.frame(
          name = attr_data$name,
          time = attr_data$time,
          x = attr_data[, variable],
          stringsAsFactors = FALSE
        )

        if (anyNA(dat)) {
          warning(paste0("Missing values in the attr_data object for the '", effect, "' effect can cause unexpected behavior."))
        }

        dat$name <- as.character(dat$name)

        if (!all(actors[, 1] %in% dat$name)) {
          stop("Missing actors in the attr_data object.")
        }

        dat$name <- actors[match(dat$name, actors[, 1]), 2]
        colnames(dat)[3] <- variable
        dat <- dat[order(as.numeric(dat$name)), ]

        as.matrix(dat)
      } else {
        dat <- x$x

        dat$name <- as.character(dat$name)

        if (!all(actors[, 1] %in% dat$name)) {
          stop("Missing actors in the attr_data object.")
        }

        dat$name <- actors[match(dat$name, actors[, 1]), 2]
        dat <- dat[order(as.numeric(dat$name)), ]
      }

      if (any(is.na(dat$name))) {
        warning(paste0("attr_data contain actors that are not in the risk set. These are not included in the computation of the statistics."))
        dat <- dat[!is.na(dat$name), ]
      }

      as.matrix(dat)
    } else if (effect %in% c("tie", "event", "FEtype", "ccp")) {
      if (effect == "tie") {
        parse_tie(x, reh)
      } else if (effect == "event") {
        if (length(x$x) != nrow(edgelist)) {
          stop("Length of vector 'x' in event() does not match number of events in edgelist")
        }
        if(!is.numeric(x$x)) {
          x$x <- (as.numeric(factor(x$x)) - 1)
        }
        as.matrix(x$x)
      } else if (effect == "FEtype") {
        as.matrix(x$typeID)
      } else if (effect == "ccp") {
        as.matrix(x$x)
      }
    } else if (effect == "userStat") {
      if (NROW(x$x) != nrow(edgelist)) {
        stop("Number of rows of matrix 'x' in userStat() does not match number of events in edgelist")
      }

      if (NCOL(x$x) != nrow(prepR)) {
        stop("Number of columns of matrix 'x' in userStat() does not match number of dyads in risk set")
      }

      as.matrix(x$x)
    } else {
      matrix()
    }
  })
}
