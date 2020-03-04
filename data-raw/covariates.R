rm(list = ls())

set.seed(54688)

# Time-varying covariate information
# Set up the data-frame
covar <- data.frame(id = rep(letters, each = 3), 
    time = rep(c(0, 5, 10), times = 26),
    x1 = round(runif(26*3, -2, 2), 1), 
    x2 = round(rnorm(26*3, 0, 2)))

# Sample measurements
covar <- covar[-sort(sample(c(seq(2, nrow(covar), 3), 
    seq(3, nrow(covar), 3)), 1.2*26)),]

# Add randomness to time
covar$time <- ifelse(covar$time != 0, 
    covar$time+runif(nrow(covar), min = -1, max = 1), 
	covar$time)

# Tidy up dataframe
covar$time <- round(covar$time, 2)
rownames(covar) <- NULL
names(covar) <- c("id", "change_time", "x1", "x2")

usethis::use_data(covar, overwrite = TRUE)



