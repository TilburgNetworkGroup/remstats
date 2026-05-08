#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
#include <progress.hpp>
#include <progress_bar.hpp>
#include <algorithm> // std::min, std::max

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]

/*
  calculate_active_stats

  Computes active-state statistics for Duration Relational Event Models (DuREM).
  These statistics capture properties of the currently active event network —
  i.e., events that have started but not yet ended — at each time point in the
  dual-event sequence.

  At each time point t the statistic is recorded BEFORE the event at t is
  processed, consistent with the convention used in tomstats (history strictly
  before t).

  Input edgelist columns: [time, actor1_id, actor2_id, status]
    status: 0 = start event (interaction begins)
            1 = end event   (interaction terminates)
  Actor IDs are 0-indexed integers matching the rows/cols of risksetMatrix.

  stat_type:

  Directed networks:
    1  activeTie              is there a currently active event from i to j?
    2  activeOutdegreeSender  how many active events does i currently send?
    3  activeIndegreeReceiver how many active events does j currently receive?
    4  activeTotaldegreeSender   active out + in degree of i
    5  activeTotaldegreeReceiver active out + in degree of j
    6  activeSharedPartners_otp  #h: i→h active AND h→j active
    7  activeSharedPartners_itp  #h: j→h active AND h→i active
    8  activeSharedPartners_osp  #h: i→h active AND j→h active
    9  activeSharedPartners_isp  #h: h→i active AND h→j active

  Undirected networks (riskset only has canonical dyads i < j):
    1  activeTie              is there a currently active event between i and j?
    2  activeDegreeActor1     how many active events does i (row actor) have?
    3  activeDegreeActor2     how many active events does j (col actor) have?
    4  activeSharedPartners   #h: (i,h) active AND (j,h) active

  Params:
  - edgelist:       matrix [time, actor1_id, actor2_id, status]
  - risksetMatrix:  N x (N*C) matrix, cell = dyad_id or -999 if absent
  - stat_type:      integer selecting which statistic (see above)
  - directed:       whether the network is directed
  - start:          index of first unique time point to compute (0-based)
  - stop:           index of last  unique time point to compute (0-based)
  - display_progress: show progress bar
*/

// [[Rcpp::export]]
arma::mat calculate_active_stats(
    const arma::mat &edgelist,
    const arma::mat &risksetMatrix,
    int stat_type,
    bool directed,
    int start,
    int stop,
    bool display_progress
) {
    int N = risksetMatrix.n_rows;

    // Unique time points in the dual edgelist
    arma::vec all_times  = arma::unique(edgelist.col(0));
    arma::vec time_points = all_times.subvec(start, stop);
    arma::uword M = time_points.n_elem;
    arma::uword D = (arma::uword)(risksetMatrix.max() + 1);

    // Output: M time points x D dyads
    arma::mat stat(M, D, arma::fill::zeros);

    // ── Active-state bookkeeping ───────────────────────────────────────────────
    // active_adj(i,j): count of currently active events from i to j
    //   directed  — full N×N; only (i,j) with i≠j meaningful
    //   undirected — stored in canonical form: active_adj(lo,hi) with lo<hi
    // active_out(i): number of active events where i is actor1 (out-degree, or
    //               total degree for undirected — each event counted once per actor)
    // active_in(i):  number of active events where i is actor2 (in-degree,
    //               directed only)
    arma::mat active_adj(N, N, arma::fill::zeros);
    arma::vec active_out(N, arma::fill::zeros);
    arma::vec active_in (N, arma::fill::zeros);

    // Helper lambda: apply a delta (+1 start, -1 end) to the active state
    auto apply_delta = [&](int a1, int a2, double delta) {
        if (directed) {
            active_adj(a1, a2) = std::max(0.0, active_adj(a1, a2) + delta);
            active_out(a1)     = std::max(0.0, active_out(a1)     + delta);
            active_in (a2)     = std::max(0.0, active_in (a2)     + delta);
        } else {
            int lo = std::min(a1, a2), hi = std::max(a1, a2);
            active_adj(lo, hi) = std::max(0.0, active_adj(lo, hi) + delta);
            active_out(a1)     = std::max(0.0, active_out(a1)     + delta);
            active_out(a2)     = std::max(0.0, active_out(a2)     + delta);
        }
    };

    // Initialise active state from events before the first computed time point
    double first_time = time_points(0);
    for (arma::uword k = 0; k < edgelist.n_rows; ++k) {
        if (edgelist(k, 0) >= first_time) break; // edgelist is time-sorted
        int  a1    = (int)edgelist(k, 1);
        int  a2    = (int)edgelist(k, 2);
        int  st    = (int)edgelist(k, 3);        // 0=start, 1=end
        apply_delta(a1, a2, (st == 0) ? 1.0 : -1.0);
    }

    Progress p(M, display_progress);

    for (arma::uword m = 0; m < M; ++m) {
        double t = time_points(m);

        // ── 1. Record stat for all dyads at current active state ──────────────
        // For degree stats the inner loop is O(N^2); for shared-partner stats it
        // is O(N^3). Both can be optimised later; correctness first.
        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                if (i == j) continue;
                int dyad = (int)risksetMatrix(i, j);
                if (dyad < 0) continue; // not in risk set

                double val = 0.0;

                if (directed) {
                    switch (stat_type) {
                    case 1: // activeTie
                        val = (active_adj(i, j) > 0) ? 1.0 : 0.0;
                        break;
                    case 2: // activeOutdegreeSender
                        val = active_out(i);
                        break;
                    case 3: // activeIndegreeReceiver
                        val = active_in(j);
                        break;
                    case 4: // activeTotaldegreeSender
                        val = active_out(i) + active_in(i);
                        break;
                    case 5: // activeTotaldegreeReceiver
                        val = active_out(j) + active_in(j);
                        break;
                    case 6: // otp: #h: i→h AND h→j
                        for (int h = 0; h < N; ++h) {
                            if (h == i || h == j) continue;
                            if (active_adj(i, h) > 0 && active_adj(h, j) > 0)
                                val += 1.0;
                        }
                        break;
                    case 7: // itp: #h: j→h AND h→i
                        for (int h = 0; h < N; ++h) {
                            if (h == i || h == j) continue;
                            if (active_adj(j, h) > 0 && active_adj(h, i) > 0)
                                val += 1.0;
                        }
                        break;
                    case 8: // osp: #h: i→h AND j→h
                        for (int h = 0; h < N; ++h) {
                            if (h == i || h == j) continue;
                            if (active_adj(i, h) > 0 && active_adj(j, h) > 0)
                                val += 1.0;
                        }
                        break;
                    case 9: // isp: #h: h→i AND h→j
                        for (int h = 0; h < N; ++h) {
                            if (h == i || h == j) continue;
                            if (active_adj(h, i) > 0 && active_adj(h, j) > 0)
                                val += 1.0;
                        }
                        break;
                    }
                } else {
                    // Undirected: risksetMatrix only has entries for i < j, so
                    // the dyad < 0 guard above already skips the i > j case.
                    int lo = std::min(i, j), hi = std::max(i, j);
                    switch (stat_type) {
                    case 1: // activeTie
                        val = (active_adj(lo, hi) > 0) ? 1.0 : 0.0;
                        break;
                    case 2: // activeDegreeActor1
                        val = active_out(i);
                        break;
                    case 3: // activeDegreeActor2
                        val = active_out(j);
                        break;
                    case 4: // activeSharedPartners: #h: (i,h) AND (j,h) active
                        for (int h = 0; h < N; ++h) {
                            if (h == i || h == j) continue;
                            int lo_ih = std::min(i, h), hi_ih = std::max(i, h);
                            int lo_jh = std::min(j, h), hi_jh = std::max(j, h);
                            if (active_adj(lo_ih, hi_ih) > 0 &&
                                active_adj(lo_jh, hi_jh) > 0)
                                val += 1.0;
                        }
                        break;
                    }
                }

                stat(m, (arma::uword)dyad) = val;
            }
        }

        // ── 2. Update active state with events at time t ──────────────────────
        // Scan forward through edgelist rows at time t (edgelist is sorted)
        for (arma::uword k = 0; k < edgelist.n_rows; ++k) {
            if (edgelist(k, 0) < t)  continue;
            if (edgelist(k, 0) > t)  break;
            int a1 = (int)edgelist(k, 1);
            int a2 = (int)edgelist(k, 2);
            int st = (int)edgelist(k, 3);
            apply_delta(a1, a2, (st == 0) ? 1.0 : -1.0);
        }

        p.increment();
    }
    
    // ── Final-state row: active state after all events ────────────────────────
    // Needed so the R caller can map time points beyond the last event.
    stat.resize(M + 1, D);
    for (int i = 0; i < N; ++i) {
    	for (int j = 0; j < N; ++j) {
    		if (i == j) continue;
    		int dyad = (int)risksetMatrix(i, j);
    		if (dyad < 0) continue;
    		// same switch as above — or factor into a helper
    	}
    }

    return stat;
}
