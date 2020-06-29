# prepExoVar: Helper function to specify exogenous statistics. 
# 
# Parameters:
# * type: string value. indicates the type of effect. used to name the list.
# * variables: character vector. indicates the variables used for the exogenous 
# statistics. each variable needs to correspond to a column name in the 
# covariates dataframe. 
# * covariates: dataframe. first column should contain the actor id', second 
# column the time for the exogenous covariates and subsequent columns the 
# covariates. 
prepExoVar <- function(type, variables, covariates) {

    # Warning for missing values
    if(anyNA(covariates[,variables])) {
        warning("Missing values in covariates object.")
    }

    # Number of variables
    p <- seq_len(length(variables))

    # Check if the variable names are in the covariates object
    if(!all(variables%in%colnames(covariates))) {
        stop(paste("variable", which(!(variables %in% covariates)), "not in covariates"))
    }

    # Per variable, prepare the output. Collect in a list. 
    out <- lapply(p, function(y) {
        # Collect the information in a dataframe
        dat <- data.frame(
            id = covariates[,1], 
            time = covariates[,2], 
            x = covariates[,variables[y]])
        # Set the third column name equal to the variable name
        colnames(dat)[3] <- variables[y]
        # Order the dataframe 
        dat <- dat[order(dat$id, dat$time),]
        # Give the dataframe an effect attribute with the effect type
        attributes(dat)$effect <- type
        # Output the dataframe
        list(x = dat)
    })

    # Set the names of the list object equal to the effect type. 
    names(out) <- rep(type, length(out))

    # Output
    out
}

# prepEndoVar: Helper function to specify endogenous statistics.
#
# Parameters:
# * type: string value. indicates the type of effect. used to name the output 
# list. 
# * scaling: string value. indicates the type of scaling. can be different per # effect. 
# * memory_value: numeric value. indicates how long events should be 
# remembered/included in the count. 
# * with_type: logical value. indicates whether a distinction should be made 
# between events of different types. 
# * event_weights: vector with numeric values that indicates the intensity of 
# the events in the edgelist. 

prepEndoVar <- function(type, scaling, memory_value, with_type, event_weights) {

    if(length(scaling) > 1) {scaling <- scaling[1]}

    out <- list(
        effect = list(
            scaling = scaling, 
            memory_value = memory_value,
            with_type = with_type,
            event_weights = event_weights
        )
    )

    names(out) <- type
    out
}