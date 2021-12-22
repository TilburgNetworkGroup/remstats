parse_formula <- function(formula, type, ordinal = FALSE) {
	ft <- stats::terms(formula)
	
	var <- attr(ft, "variables")
	var <- as.list(var)[-1]
	
	effects <- lapply(var, eval)
	
    if(type == "rem" & !ordinal & attr(ft, "intercept") == 1) {
        effects <- append(effects, 
			list(list(effect = "baseline", scaling = 1)), 0)
    }
	if(type == "rateEffects" & attr(ft, "intercept") == 1) {
		effects <- append(effects, 
			list(list(effect = "baseline", scaling = 1)), 0)
	}
	
	attributes(effects)$model <- type
	effects
}

parse_int <- function(formula, type, effects, ordinal = FALSE) {
	ft <- stats::terms(formula)
	ft.order <- attr(ft, "order")
	ft.factor <- attr(ft, "factor")
	interactions <- which(ft.order > 1)
	interactions <- lapply(interactions, function(i) {
		if(type == "rem" & !ordinal & attr(ft, "intercept") == 1) {
			which(ft.factor[,i] > 0) + 1
		}
		else if(type == "rateEffects" & attr(ft, "intercept") == 1) {
			which(ft.factor[,i] > 0) + 1
		} else {
			which(ft.factor[,i] > 0) 
		}	
	})
	
	interactions <- if(any(sapply(effects, function(x) x$effect == "FEtype")) &
		any(ft.order > 1)) {
		pos <- which(sapply(effects, function(x) x$effect == "FEtype"))
		unlist(lapply(1:length(interactions), function(i) {
			if(any(interactions[[i]] %in% pos)) {
				change <- which(!interactions[[i]] == min(pos))
				x <- interactions[[i]]
				x[change] <- x[change] + length(pos)-1
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
	
	# Error message in the case of missing actors
	m1 <- "One or more actors are missing from the input given to tie()."
	if(nrow(x) < nrow(dictionary)) {
		stop(m1)
	}
	
	if(ncol(x) < nrow(dictionary)) {
		stop(m1)
	}
	
	# Error message/actions in the case of redundant actors
	w1 <- FALSE
	m2 <- "Not all actors from the input given to `x` in tie() are in the dictionary. 
	Remove the redundant actors from `x`, or supply the actor names to the row and column 
	names of `x` so tie() knows which rows and columns to select."
	m3 <- "Not all actors from the input given to `x` in tie() are in the dictionary: 
	Redundant actors were removed from `x`."
	if(nrow(x) > nrow(dictionary)) {
		if(is.null(rownames(x))) {
			stop(m2)
		} else {
			x <- x[rownames(x) %in% dictionary[,1],]
			warning(m3)
			w1 <- TRUE
		}
	}
	
	if(ncol(x) > nrow(dictionary)) {
		if(is.null(colnames(x))) {
			stop(m2)
		} else {
			x <- x[,colnames(x) %in% dictionary[,1]]
			if(!w1) {warning(m3)}
		}
	}
	
	if(is.null(rownames(x))) {
		rownames(x) <- dictionary[,2]
	} else if(!all(rownames(x) %in% dictionary[,1])) {
		stop("rownames `x` in tie don't match actor names in dictionary")
	} else {
		rownames(x) <- dictionary[match(rownames(x), dictionary[,1]),2]
	}
	
	if(is.null(colnames(x))) {
		colnames(x) <- dictionary[,2]
	} else if(!all(colnames(x) %in% dictionary[,1])) {
		stop("colnames `x` in tie don't match actor ids")
	} else {
		colnames(x) <- dictionary[match(colnames(x), dictionary[,1]),2]
	}	
	
	x <- x[order(as.numeric(rownames(x))),]
	x <- x[,order(as.numeric(colnames(x)))]
	
	if(!attr(prep, "directed")) {
		if(!isSymmetric(x)) {
			if(all(is.na(x[upper.tri(x)]))) {
				x[upper.tri(x)] <- t(x)[upper.tri(x)]
			} else if(all(is.na(x[lower.tri(x)]))) {
				x[lower.tri(x)] <- t(x)[lower.tri(x)]
			} else {
				stop("Matrix 'x' in tie() is expected to be symmetric or include only NA values for one of the triangles when directed is FALSE.")
			}
		}
	}
	
	as.matrix(x)
}
