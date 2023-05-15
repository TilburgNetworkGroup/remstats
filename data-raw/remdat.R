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

usethis::use_data(info, history, overwrite = TRUE)
