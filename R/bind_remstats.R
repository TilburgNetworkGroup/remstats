#' Combine two or more remstats objects 
#' 
#' Function to bind any number of remstats objects into one while duplicated
#' statistics in the combined object are removed.
#' 
#' @param ... Any number of \code{\link{remstats}} objects. All the 
#' \code{remstats} objects must have matching dimensions, except for the third 
#' dimension.
#' 
#' @return \code{statistics } array with the combined statistics, where rows
#' refer to time points, columns refer to potential relational event (i.e.,
#' potential edges) in the risk set and slices refer to statistics
#'
#' @examples
#' library(remstats)
#' reh <- remify::remify(edgelist = history, model = "tie")
#' effects1 <- ~ inertia():send("extraversion") + otp()
#' stats1 <- tomstats(effects1, reh = reh, attr_data = info)
#' effects2 <- ~ reciprocity()
#' stats2 <- tomstats(effects2, reh = reh, attr_data = info)
#' statsC <- bind_remstats(stats1, stats2)
#' 
#' @export
bind_remstats <- function(...) {

    arg.list <- list(...)

    # Check class
    stopifnot("All objects should be of class remstats" = 
        all(sapply(arg.list, function(x) any(class(x) == "remstats"))))
    
    firstclass <- class(arg.list[[1]])[1]
    if(!all(sapply(arg.list, function(x) class(x)[1] == firstclass))) {
        stop(paste0("All objects should be of class ", firstclass))
    }
    
    # Check risk set
    firstrs <- attr(arg.list[[1]], "riskset")
    stopifnot("Risk sets of the remstats objects should be equal" = 
            all(sapply(arg.list, function(x) attr(x, "riskset") == firstrs)))
    
    # Combine statistics
    statistics <- combine_arrays(arg.list, along = 3)
    
    # Names of the statistics
    stat_names <- lapply(arg.list, function(x) dimnames(x)[[3]])
    stat_names <- do.call(c, stat_names)
    stat_names <- stat_names[!duplicated(stat_names)]
    dimnames(statistics) <- list(NULL, NULL, stat_names)

    # Combine formula
    form.list <- sapply(arg.list, function(x) attr(x, "form"))
    processed_formulas <- sapply(form.list, function(formula) {
        formula_string <- as.character(formula)  
        formula_string <- gsub("~", "", formula_string[-1])  
        formula_string  
    })
    combined_formula <- paste(processed_formulas, collapse = " + ")
    final_formula <- paste("~", combined_formula)
    final_formula <- stats::as.formula(final_formula)

    # Format output
    class(statistics) <- class(arg.list[[1]])
    attr(statistics, "model") <- attr(arg.list[[1]], "model")
    attr(statistics, "formula") <- final_formula
    attr(statistics, "riskset") <- attr(arg.list[[1]], "riskset")
    attr(statistics, "adjmat") <-  attr(arg.list[[1]], "adjmat")
    
    # Output
    statistics
}

