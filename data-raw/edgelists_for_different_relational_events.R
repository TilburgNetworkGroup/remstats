rm(list=ls())

set.seed(343234)

M <- 1000
actors <- letters

# Different types of relational events 
# Directed edgelist 
edgelistD <- data.frame(
	time = cumsum(rexp(n = M)),
	sender = sample(actors, M, replace = TRUE),
	receiver = sample(actors, M, replace = TRUE)
)

edgelistD <- edgelistD[edgelistD$sender!=edgelistD$receiver,]

# Undirected edgelist 
edgelistU <- edgelistD
edgelistU[,c(2,3)] <- t(apply(edgelistU, 1, function(x) sort(x[c(2,3)])))
colnames(edgelistU)[c(2,3)] <- c("actor1", "actor2")

# Directed and typed edgelist 
types <- 1:3

edgelistDT <- edgelistD
edgelistDT$type <- sample(types, nrow(edgelistDT), replace = TRUE)

# Undirected and typed edgelist 
edgelistUT <- edgelistDT 
edgelistUT[,c(2,3)] <- t(apply(edgelistUT, 1, function(x) sort(x[c(2,3)])))
colnames(edgelistUT)[c(2,3)] <- c("actor1", "actor2")

usethis::use_data("edgelists_for_different_relational_events")
