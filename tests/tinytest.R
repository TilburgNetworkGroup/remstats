
if ( requireNamespace("tinytest", quietly=TRUE) ){
	tinytest::test_package("remstats")
}

# test-endogenous-stats / test-exogenous-stats: 
# Condition 1: Directed events, tie-oriented model
# Condition 2: Undirected events, tie-oriented model
# Condition 3: Directed evens with types, tie-oriented model
# Condition 4: Undirected events with types, tie-oriented model
# Condition 5: Actor-oriented model