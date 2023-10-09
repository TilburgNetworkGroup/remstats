rm(list=ls())

set.seed(8787)

# Create actors and exogenous information on them
info <- data.frame(name = sample(101:115, 10))
info$time <- 0
info$age <- sample(c(0,1), 10, replace = T, prob = c(0.8, 0.2))
info$sex <- sample(c(0,1), 10, replace = T, prob = c(0.8, 0.2))
info$extraversion <- round(rnorm(10, mean = 4, sd = 1), 2)
info$agreeableness <- round(rnorm(10, mean = 3.5, sd = 1), 2)
info$extraversion <- ifelse(info$extraversion < 1, 1, info$extraversion)
info$extraversion <- ifelse(info$extraversion > 7, 7, info$extraversion)
info$agreeableness <- ifelse(info$agreeableness < 1, 1, info$agreeableness)
info$agreeableness <- ifelse(info$agreeableness > 7, 7, info$agreeableness)

# Sample events between actors
history <- data.frame(time = cumsum(rexp(125, rate = 0.004)))
history$actor1 <- sample(info$name, 125, replace = T)
history$actor2 <- sample(info$name, 125, replace = T)
history <- history[history$actor1!=history$actor2,]
history$setting <- sample(c("work", "social"), size = nrow(history), prob = c(0.7, 0.3), replace = T)
history$weight <- rexp(nrow(history), 2)
history$weight <- history$weight + 1
history$weight <- round(history$weight, 2)
history$time <- round(history$time)
rownames(history) <- NULL

# Create info that vary over time
time <- seq(0, max(history$time), length.out = 4)[-4]
info <- lapply(time, function(x) {
	
	dat <- info
	dat$time <- x
	
	if(x != 0) {
		dat$extraversion <- dat$extraversion + rnorm(length(dat$extraversion), 0, 0.2)
		dat$agreeableness <- dat$agreeableness + rnorm(length(dat$agreeableness), 0, 0.2)
	} 
	
	dat
	
})

info <- do.call(rbind, info)
info$extraversion <- (info$extraversion-mean(info$extraversion)
)/sd(info$extraversion)
info$agreeableness <- (info$agreeableness-mean(info$agreeableness)
)/sd(info$agreeableness)
info$extraversion <- round(info$extraversion, 2)
info$agreeableness <- round(info$agreeableness, 2)
info <- info[order(info$name, info$time),]
rownames(info) <- NULL

# Create tie/dyad exogenous objects
actors <- unique(info$name)
sex <- info[match(actors, info$name), "sex"]
both_male_wide <- sapply(seq_along(actors), function(i) {
	sapply(seq_along(actors), function(j) {
		ifelse(sex[i] == 0 & sex[j] == 0, 1, 0)
	})
})
rownames(both_male_wide) <- colnames(both_male_wide) <- actors

# Get row and column names
row_ids <- rownames(both_male_wide)
col_ids <- colnames(both_male_wide)

# Initialize empty vectors to store data
row_data <- character(0)
col_data <- character(0)
value_data <- numeric(0)

# Iterate through the matrix 
for (i in 1:nrow(both_male_wide)) {
	for (j in 1:ncol(both_male_wide)) {
		row_data <- c(row_data, row_ids[i])
		col_data <- c(col_data, col_ids[j])
		value_data <- c(value_data, both_male_wide[i, j])
	}
}

# Create a data frame
both_male_long <- data.frame(actor1 = row_data, actor2 = col_data, both_male = value_data)
 
usethis::use_data(info, history, both_male_wide, both_male_long, overwrite = TRUE)
