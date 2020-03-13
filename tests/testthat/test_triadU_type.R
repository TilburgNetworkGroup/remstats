context("triadU_type")

library(remstats)

# Prepare for undirected relational events with types
data(edgelistUT)

out <- prepER(edgelistUT, directed = FALSE, type = TRUE)
el <- out$edgelist
rs <- out$riskset
ac <- out$actors

# Statistics
statA <- triadU_type(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = FALSE, standardize = FALSE)
statB <- triadU_type(actors = ac[,1], edgelist = el, riskset = rs, 
    unique_sp = TRUE, standardize = FALSE)

# Tests
test_that("dimensions", {
    expect_output(str(statA), "num[1:nrow(el), 1:nrow(rs)]")
    expect_output(str(statB), "num[1:nrow(el), 1:nrow(rs)]")
})

test_that("the final counts are correct", {
    sel <- el[1:(nrow(el)-1),]

    expect_equal(apply(rs, 1, function(x) {
        del1 <- sel[(sel[,2] == x[1] | sel[,3] == x[1]) & sel[,4] == x[3],]
        del2 <- sel[(sel[,2] == x[2] | sel[,3] == x[2]) & sel[,4] == x[3],]
        p1 <- c(del1[,2], del1[,3])
        p2 <- c(del2[,2], del2[,3])
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sp <- 0
        for(i in unique(p1)) {
            sp <- sp + min(c(sum(p1==i), sum(p2==i)))
        }
        sp}), statA[nrow(statA),])

    expect_equal(apply(rs, 1, function(x) {
        del1 <- sel[(sel[,2] == x[1] | sel[,3] == x[1]) & sel[,4] == x[3],]
        del2 <- sel[(sel[,2] == x[2] | sel[,3] == x[2]) & sel[,4] == x[3],]
        p1 <- unique(c(del1[,2], del1[,3]))
        p2 <- unique(c(del2[,2], del2[,3]))
        p1 <- p1[!(p1==x[1] | p1 == x[2])]
        p2 <- p2[!(p2==x[1] | p2 == x[2])]
        sum(p1 %in% p2)}), statB[nrow(statB),])
})