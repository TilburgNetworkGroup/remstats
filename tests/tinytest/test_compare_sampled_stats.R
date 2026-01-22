

library("tinytest")

test_tomstats2_sampling_matches_full_endogenous <- function() {
	
	data(history, package = "remstats")
	data(info, package = "remstats")
	
	# Endogenous-only model (adjust names to the exact remstats API you have)
	effects <- ~
		inertia() +
		reciprocity() +
		indegreeSender() + outdegreeSender() +   # degree (sender-based)
		indegreeReceiver() + outdegreeReceiver() + # degree (receiver-based), if available
		otp() + itp() +                            # triadic closure examples
		recencySendReceiver() +                                # dyad recency, if this is the name you use
		recencyReceiveReceiver() +                             # rank-based recency, if available
		psABBA() + psABXY() + psABAY()
	
	reh <- remify::remify(edgelist = history, model = "tie", riskset = "active")
	
	set.seed(1)
	ts_samp <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, method = "pt",
		sampling = TRUE, samp_num = 10
	)
	
	ts_full <- remstats::tomstats2(
		effects, reh = reh, attr_actors = info, method = "pt",
		sampling = FALSE
	)
	
	sample_map <- attr(ts_samp, "sample_map")
	expect_true(!is.null(sample_map))
	
	riskset <- attr(ts_full, "riskset")
	if (is.null(riskset)) riskset <- reh$riskset
	expect_true(!is.null(riskset))
	
	dyad_id <- as.integer(riskset[, ncol(riskset)])
	if (min(dyad_id, na.rm = TRUE) == 0L) {
		dyad_id_key <- dyad_id
	} else {
		dyad_id_key <- dyad_id - 1L
	}
	
	col_index_by_dyad <- setNames(seq_along(dyad_id_key), dyad_id_key)
	
	M <- dim(ts_samp)[1]
	S <- dim(ts_samp)[2]
	
	fails <- list()
	k <- 0L
	
	for (m in seq_len(M)) {
		for (s in seq_len(S)) {
			d <- as.integer(sample_map[m, s])
			j <- unname(col_index_by_dyad[as.character(d)])
			expect_true(!is.na(j))
			
			a <- as.numeric(ts_samp[m, s, ])
			b <- as.numeric(ts_full[m, j, ])
			
			diff_idx <- which(abs(a - b) > 1e-12)
			
			if (length(diff_idx) > 0L) {
				k <- k + 1L
				fails[[k]] <- list(
					m = m, s = s, d = d, j = j,
					diff_idx = diff_idx,
					a = a[diff_idx],
					b = b[diff_idx]
				)
			}
		}
	}
	
	if (k > 0L) {
		cat("\nFound", k, "mismatches.\n")
		
		# 1) Which effect positions mismatch most often
		freq <- table(unlist(lapply(fails, `[[`, "diff_idx")))
		freq <- sort(freq, decreasing = TRUE)
		cat("\nMost frequent mismatching effect indices (top 20):\n")
		print(head(freq, 20))
		
		# 2) Show first few mismatches with their differing positions and values
		cat("\nFirst 10 mismatches with differing indices:\n")
		for (ii in seq_len(min(20L, k))) {
			f <- fails[[ii]]
			cat("\n#", ii, " m=", f$m, " s=", f$s, " d=", f$d, " j=", f$j, "\n", sep="")
			cat("idx: ", paste(f$diff_idx, collapse=","), "\n", sep="")
			cat("a:   ", paste(signif(f$a, 16), collapse=","), "\n", sep="")
			cat("b:   ", paste(signif(f$b, 16), collapse=","), "\n", sep="")
		}
		
		stop("Mismatches found between sampled and full tomstats2 outputs")
	}
	
}

test_tomstats2_sampling_matches_full_endogenous()



