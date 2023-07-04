library(tinytest)
library(remstats)
tinytest::test_package("remstats", testdir="tinytest")

# test-endogenous-stats: 
# Condition 1: Directed events, tie-oriented model
# Condition 2: Undirected events, tie-oriented model
# Condition 3: Directed evens with types, tie-oriented model
# Condition 4: Undirected events with types, tie-oriented model
# Condition 5: Directed events, tie-oriented model with active risk set
# Condition 7: Directed evens with types, tie-oriented model with active risk set
# Condition 9: Actor-oriented model