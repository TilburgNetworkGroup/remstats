prep_exo <- function(effect, variable, attributes, scaling) {
    # Match scaling
    scaling <- match(scaling, c("as.is", "std"))

    # Prepare effect
    if (is.null(attributes)) {
        list(
            effect = effect,
            variable = variable,
            x = NULL,
            scaling = scaling
        )
    } else {
        # Check if the variable name is in the attributes object
        if (!(variable %in% colnames(attributes))) {
            stop(paste0("Variable '", variable, "' not in attributes object for the '", effect, "' effect.")) # nolint
        }

        # Check if the time variable is available
        if (!("time" %in% colnames(attributes))) {
            stop(paste0("time variable is missing in attributes object for the '", effect, "' effect.")) # nolint
        }
        if (anyNA(attributes$time)) {
            stop("time variable in attributes cannot have missing values")
        }

        # Collect the information in a data.frame
        dat <- data.frame(
            name = attributes$name,
            time = attributes$time,
            x = attributes[, variable]
        )

        # Warning for missing values
        if (anyNA(dat)) {
            warning("Missing values in the attributes object for the '", effect, "' effect can cause unexpected behavior.") # nolint
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

parse_formula <- function(formula, type, ordinal = FALSE) {
    ft <- stats::terms(formula)

    var <- attr(ft, "variables")
    var <- as.list(var)[-1]

    effects <- lapply(var, eval.parent, n = 4)

    if (type == "rem" & !ordinal & attr(ft, "intercept") == 1) {
        effects <- append(
            effects,
            list(list(effect = "baseline", scaling = 1)), 0
        )
    }
    if (type == "rateEffects" & attr(ft, "intercept") == 1) {
        effects <- append(
            effects,
            list(list(effect = "baseline", scaling = 1)), 0
        )
    }

    attributes(effects)$model <- type
    effects
}

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

parse_tie <- function(List, prep) {
    x <- List$x
    dictionary <- attr(prep, "dictionary")$actors
    # First column: actorName
    # Second column: actorID

    # Error message in the case of missing rownames
    if (is.null(rownames(x)) | is.null(colnames(x))) {
        stop("Name rows and columns of 'x' in tie() with the respective actors in the network")
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
    if (!attr(prep, "directed")) {
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
