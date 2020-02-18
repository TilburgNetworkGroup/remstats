rm(list=ls())

set.seed(343234)

M <- 1000
actors <- letters

# Try out different classes
edgelist <- data.frame(
	time = cumsum(rexp(n = M)),
	sender = sample(actors, M, replace = TRUE),
	receiver = sample(actors, M, replace = TRUE)
)

edgelist <- edgelist[edgelist$sender!=edgelist$receiver,]

edgelistF <- edgelist
edgelist$sender <- as.character(edgelist$sender)
edgelist$receiver <- as.character(edgelist$receiver)
edgelistC <- edgelist
edgelist$sender <- as.integer(edgelistF$sender)
edgelist$receiver <- as.integer(edgelistF$receiver)
edgelistI <- edgelist
edgelist$sender <- as.numeric(edgelistF$sender)
edgelist$receiver <- as.numeric(edgelistF$receiver)
edgelistN <- edgelist

usethis::use_data("edgelists_of_different_classes")
