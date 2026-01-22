
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <vector>
#include <deque>
#include <cmath>
#include <map>


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]

// [[Rcpp::export]]
arma::mat calculate_FEtype_sampled(const arma::mat &covariates,
                                   const arma::mat &edgelist,
                                   const arma::mat &riskset,
                                   int start, int stop,
                                   Rcpp::String method,
                                   const arma::imat &sample_map) // M x S, 0-based dyad IDs
{
	if (covariates.n_elem == 0) Rcpp::stop("FEtype_sampled: covariates is empty.");
	double target_type = covariates(0, 0);
	
	// Time points (same as elsewhere)
	arma::vec time_points;
	if (method == "pt") {
		time_points = arma::unique(edgelist.col(0));
	} else if (method == "pe") {
		time_points = edgelist.col(0);
	} else {
		Rcpp::stop("FEtype_sampled: unknown method (must be 'pt' or 'pe').");
	}
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	arma::mat stat(M, S, arma::fill::zeros);
	
	// Expect riskset columns: sender, receiver, type, dyad_id (0-based).
	// In your printed example: riskset has 4 cols and col 3 (0-based index 3) is dyad id 0..D-1.
	if (riskset.n_cols < 4) {
		Rcpp::stop("FEtype_sampled: riskset must have >= 4 columns (sender, receiver, type, dyad_id).");
	}
	
	// Determine D from riskset dyad_id column (ignore negatives like -999)
	int max_dyad = -1;
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = static_cast<int>(riskset(r, 3));
		if (d > max_dyad) max_dyad = d;
	}
	if (max_dyad < 0) Rcpp::stop("FEtype_sampled: could not determine dyad id range from riskset.");
	const arma::uword D = static_cast<arma::uword>(max_dyad + 1);
	
	// Build dyad_id -> type lookup
	std::vector<double> type_by_dyad(D, NA_REAL);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = static_cast<int>(riskset(r, 3));
		if (d < 0) continue;
		arma::uword du = static_cast<arma::uword>(d);
		if (du >= D) continue;
		type_by_dyad[du] = riskset(r, 2);
	}
	
	// Fill sampled output
	for (arma::uword m = 0; m < M; ++m) {
		for (arma::uword s = 0; s < S; ++s) {
			
			int d = sample_map(m, s); // 0-based dyad id
			if (d < 0 || d >= (int)D)
				Rcpp::stop("sample_map out of bounds");
			arma::uword du = (arma::uword)d;
			if (du >= D) Rcpp::stop("FEtype_sampled: sample_map dyad id out of bounds.");
			stat(m, s) = (type_by_dyad[du] == target_type) ? 1.0 : 0.0;
		}
	}
	
	return stat;
}


// [[Rcpp::export]]
arma::mat calculate_exo_actor_sampled(std::string type,
                                      const arma::mat &edgelist,
                                      const arma::mat &riskset,     // cols: sender, receiver, type, dyad_id
                                      const arma::mat &covariates,  // [actor, time, value]
                                      int start, int stop,
                                      bool display_progress,
                                      Rcpp::String method,
                                      const arma::imat &sample_map) // M x S, 0-based dyad_id
{
	if (display_progress) {
		Rcpp::Rcout << "Calculating " << type << " statistic (sampled)" << std::endl;
	}
	if (type != "send" && type != "receive") {
		Rcpp::stop("calculate_exo_actor_sampled: type must be 'send' or 'receive'.");
	}
	if (covariates.n_cols < 3) {
		Rcpp::stop("calculate_exo_actor_sampled: covariates must have >=3 columns [actor,time,value].");
	}
	if (riskset.n_cols < 4) {
		Rcpp::stop("calculate_exo_actor_sampled: riskset must have >=4 cols (sender, receiver, type, dyad_id).");
	}
	if (!(method == "pt" || method == "pe")) {
		Rcpp::stop("calculate_exo_actor_sampled: method must be 'pt' or 'pe'.");
	}
	
	// --- time points ---
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else                time_points = edgelist.col(0);
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	arma::mat stat(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return stat;
	
	// --- infer N from covariates actor column ---
	if (covariates.n_rows == 0) return stat;
	const arma::uword N = static_cast<arma::uword>(covariates.col(0).max() + 1.0);
	
	// --- dyad_id range from riskset and dyad_id -> sender/receiver ---
	const arma::uword D = static_cast<arma::uword>(arma::max(riskset.col(3)) + 1.0);
	
	std::vector<int> sender_by_dyad(D, -1), receiver_by_dyad(D, -1);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = (int)riskset(r, 3);
		if (d < 0 || (arma::uword)d >= D) continue;
		sender_by_dyad[(arma::uword)d]   = (int)riskset(r, 0);
		receiver_by_dyad[(arma::uword)d] = (int)riskset(r, 1);
	}
	
	// --- sort covariates by time once (stable), then stream updates ---
	arma::uvec ord = arma::sort_index(covariates.col(1));
	arma::mat covs = covariates.rows(ord);
	
	// current actor values at first timepoint t0
	const double t0 = time_points(0);
	arma::vec current(N, arma::fill::zeros);
	
	// apply all changes with time <= t0
	arma::uword ptr = 0;
	while (ptr < covs.n_rows && covs(ptr, 1) <= t0) {
		int a = (int)covs(ptr, 0);
		if (a >= 0 && (arma::uword)a < N) current((arma::uword)a) = covs(ptr, 2);
		++ptr;
	}
	
	auto actor_for_dyad = [&](int d) -> int {
		if (d < 0 || (arma::uword)d >= D) return -1;
		return (type == "send") ? sender_by_dyad[(arma::uword)d]
		: receiver_by_dyad[(arma::uword)d];
	};
	
	// row 0
	for (arma::uword s = 0; s < S; ++s) {
		int d = sample_map(0, s);
		if (d < 0 || (arma::uword)d >= D) Rcpp::stop("calculate_exo_actor_sampled: sample_map out of bounds.");
		int a = actor_for_dyad(d);
		if (a < 0 || (arma::uword)a >= N) Rcpp::stop("calculate_exo_actor_sampled: invalid actor index for dyad.");
		stat(0, s) = current((arma::uword)a);
	}
	
	// subsequent rows
	for (arma::uword m = 1; m < M; ++m) {
		double now = time_points(m);
		
		// apply covariate changes up to 'now'
		while (ptr < covs.n_rows && covs(ptr, 1) <= now) {
			int a = (int)covs(ptr, 0);
			if (a >= 0 && (arma::uword)a < N) current((arma::uword)a) = covs(ptr, 2);
			++ptr;
		}
		
		for (arma::uword s = 0; s < S; ++s) {
			int d = sample_map(m, s);
			if (d < 0 || (arma::uword)d >= D) Rcpp::stop("calculate_exo_actor_sampled: sample_map out of bounds.");
			int a = actor_for_dyad(d);
			if (a < 0 || (arma::uword)a >= N) Rcpp::stop("calculate_exo_actor_sampled: invalid actor index for dyad.");
			stat(m, s) = current((arma::uword)a);
		}
	}
	
	return stat;
}



arma::mat calculate_exo_tie_sampled(const arma::mat &covariates,
                                    const arma::mat &edgelist,
                                    const arma::mat &risksetMatrix,
                                    const arma::mat &riskset,
                                    int start, int stop,
                                    bool display_progress,
                                    Rcpp::String method,
                                    const arma::imat &sample_map)
{
	if (display_progress) Rcpp::Rcout << "Calculating tie statistic (sampled)" << std::endl;
	
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("calculate_exo_tie_sampled: unknown method.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	int N = risksetMatrix.n_rows;
	int C = risksetMatrix.n_cols / N;
	
	arma::mat out(M, S, arma::fill::zeros);
	
	// dyad -> positions in sample_map row m (unordered_map per row)
	std::vector<std::unordered_map<arma::uword, arma::uword>> pos(M);
	for (arma::uword m = 0; m < M; ++m) {
		pos[m].reserve(static_cast<size_t>(S * 2));
		for (arma::uword s = 0; s < S; ++s) pos[m][sample_map(m, s)] = s;
	}
	
	arma::vec covar_times = arma::unique(covariates.col(2));
	Progress p(covar_times.n_elem, display_progress);
	
	for (arma::uword t = 0; t < covar_times.n_elem; ++t) {
		double time = covar_times(t);
		arma::uvec covar_idx = arma::find(covariates.col(2) == time);
		arma::uvec rows_to_set = arma::find(time_points >= time);
		
		for (arma::uword ii = 0; ii < covar_idx.n_elem; ++ii) {
			arma::uword r = covar_idx(ii);
			int a1 = static_cast<int>(covariates(r, 0));
			int a2 = static_cast<int>(covariates(r, 1));
			double value = covariates(r, 3);
			
			for (int k = 0; k < C; ++k) {
				int dyad = risksetMatrix(a1, a2 + N * k);
				if (dyad < 0) continue;
				arma::uword d = static_cast<arma::uword>(dyad);
				
				for (arma::uword jj = 0; jj < rows_to_set.n_elem; ++jj) {
					arma::uword m = rows_to_set(jj);
					auto it = pos[m].find(d);
					if (it != pos[m].end()) out(m, it->second) = value;
				}
			}
		}
		p.increment();
	}
	
	return out;
}


arma::mat calculate_exo_event_sampled(const arma::mat &covariates,
                                      const arma::mat &edgelist,
                                      int start, int stop,
                                      Rcpp::String method,
                                      const arma::imat &sample_map)
{
	arma::vec event_times;
	if (method == "pt") event_times = arma::unique(edgelist.col(0));
	else if (method == "pe") event_times = edgelist.col(0);
	else Rcpp::stop("calculate_exo_event_sampled: unknown method.");
	event_times = event_times.subvec(start, stop);
	
	const arma::uword M = event_times.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	arma::mat cov_slice = covariates.rows(start, stop);
	if (static_cast<arma::uword>(cov_slice.n_rows) != M)
		Rcpp::stop("calculate_exo_event_sampled: covariates rows(start,stop) must match M.");
	if (cov_slice.n_cols < 1)
		Rcpp::stop("calculate_exo_event_sampled: covariates must have >=1 column.");
	
	arma::mat out(M, S, arma::fill::zeros);
	for (arma::uword m = 0; m < M; ++m) out.row(m).fill(cov_slice(m, 0));
	return out;
}


arma::mat calculate_exo_dyad_sampled(std::string type,
                                     const arma::mat &edgelist,
                                     const arma::mat &riskset,
                                     const arma::mat &covariates,
                                     int start, int stop,
                                     bool display_progress,
                                     Rcpp::String method,
                                     const arma::imat &sample_map)
{
	if (display_progress) Rcpp::Rcout << "Calculating " << type << " statistic (sampled)" << std::endl;
	
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("calculate_exo_dyad_sampled: unknown method.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	if (riskset.n_cols < 4) {
		Rcpp::stop("calculate_exo_dyad_sampled: riskset must have >=4 cols (sender, receiver, type, dyad_id).");
	}
	
	// D from dyad_id column
	int max_dyad = -1;
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = static_cast<int>(riskset(r, 3));
		if (d > max_dyad) max_dyad = d;
	}
	if (max_dyad < 0) Rcpp::stop("calculate_exo_dyad_sampled: could not determine dyad id range from riskset.");
	const arma::uword D = static_cast<arma::uword>(max_dyad + 1);
	
	// dyad_id -> (a1,a2)
	std::vector<int> a1_by_dyad(D, -1);
	std::vector<int> a2_by_dyad(D, -1);
	int max_actor = -1;
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = static_cast<int>(riskset(r, 3));
		if (d < 0) continue;
		arma::uword du = static_cast<arma::uword>(d);
		if (du >= D) continue;
		
		int a1 = static_cast<int>(riskset(r, 0));
		int a2 = static_cast<int>(riskset(r, 1));
		a1_by_dyad[du] = a1;
		a2_by_dyad[du] = a2;
		if (a1 > max_actor) max_actor = a1;
		if (a2 > max_actor) max_actor = a2;
	}
	if (max_actor < 0) return out;
	const arma::uword N = static_cast<arma::uword>(max_actor + 1);
	
	// current actor values
	arma::vec current_actor(N, arma::fill::zeros);
	
	// initialize at first time
	double t0 = time_points(0);
	for (arma::uword a = 0; a < N; ++a) {
		arma::uvec idx = arma::find(covariates.col(0) == static_cast<double>(a) &&
			covariates.col(1) <= t0);
		if (idx.n_elem == 0) {
			current_actor(a) = 0.0;
		} else {
			arma::mat vals = covariates.rows(idx);
			arma::uword k = index_max(vals.col(1));
			current_actor(a) = vals(k, 2);
		}
	}
	
	arma::vec changetimes = arma::unique(covariates.col(1));
	arma::uword counter = 0;
	
	Progress p(M, display_progress);
	
	for (arma::uword m = 0; m < M; ++m) {
		
		while (counter < changetimes.n_elem && time_points(m) >= changetimes(counter)) {
			double ct = changetimes(counter);
			arma::uvec upd = arma::find(covariates.col(1) == ct);
			for (arma::uword j = 0; j < upd.n_elem; ++j) {
				arma::uword r = upd(j);
				int a = static_cast<int>(covariates(r, 0));
				double v = covariates(r, 2);
				if (a >= 0 && static_cast<arma::uword>(a) < N) current_actor(static_cast<arma::uword>(a)) = v;
			}
			counter += 1;
		}
		
		for (arma::uword s = 0; s < S; ++s) {
			int d = static_cast<int>(sample_map(m, s));
			if (d < 0 || static_cast<arma::uword>(d) >= D) Rcpp::stop("calculate_exo_dyad_sampled: sample_map out of bounds.");
			arma::uword du = static_cast<arma::uword>(d);
			
			int a1 = a1_by_dyad[du];
			int a2 = a2_by_dyad[du];
			if (a1 < 0 || a2 < 0) Rcpp::stop("calculate_exo_dyad_sampled: missing actor mapping for dyad id.");
			
			double v1 = current_actor(static_cast<arma::uword>(a1));
			double v2 = current_actor(static_cast<arma::uword>(a2));
			
			if (type == "same") out(m, s) = (v1 == v2) ? 1.0 : 0.0;
			else if (type == "difference") out(m, s) = (v1 - v2);
			else if (type == "average") out(m, s) = 0.5 * (v1 + v2);
			else if (type == "minimum") out(m, s) = std::min(v1, v2);
			else if (type == "maximum") out(m, s) = std::max(v1, v2);
			else Rcpp::stop("calculate_exo_dyad_sampled: unknown type.");
		}
		
		p.increment();
	}
	
	return out;
}



static void update_lastActive(double time, const arma::vec &dyads, arma::vec &lastActive)
{
	for (arma::uword d = 0; d < dyads.n_elem; ++d)
	{
		int dyad = static_cast<int>(dyads(d));
		if (dyad >= 0) lastActive(static_cast<arma::uword>(dyad)) = time;
	}
}

arma::mat calculate_recency_sampled(std::string type,
                                    const arma::mat &edgelist,
                                    const arma::mat &risksetMatrix,
                                    const arma::mat &riskset,
                                    int start, int stop,
                                    bool consider_type,
                                    bool display_progress,
                                    Rcpp::String method,
                                    const arma::imat &sample_map)
{
	if (display_progress) Rcpp::Rcout << "Calculating " << type << " statistic (sampled)" << std::endl;
	
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("calculate_recency_sampled: unknown method.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	int N = risksetMatrix.n_rows;
	int C = risksetMatrix.n_cols / N;
	const arma::uword D = static_cast<arma::uword>(arma::max(riskset.col(3)) + 1);
	
	arma::mat out(M, S, arma::fill::zeros);
	
	arma::vec lastActive(D);
	lastActive.fill(arma::datum::inf);
	
	// initialize from past
	double first_time = edgelist(start, 0);
	arma::uvec event_indices = arma::find(edgelist.col(0) < first_time);
	
	auto update_from_event = [&](arma::uword event)
	{
		double time = edgelist(event, 0);
		arma::uword sender = edgelist(event, 1);
		arma::uword receiver = edgelist(event, 2);
		arma::uword event_type = 0;
		if (C > 1) event_type = edgelist(event, 3);
		
		if (type == "recencyContinue")
		{
			if (consider_type)
			{
				int dyad = risksetMatrix(sender, receiver + event_type * N);
				if (dyad >= 0) lastActive(static_cast<arma::uword>(dyad)) = time;
			}
			else
			{
				for (int k = 0; k < C; ++k)
				{
					int dyad = risksetMatrix(sender, receiver + k * N);
					if (dyad >= 0) lastActive(static_cast<arma::uword>(dyad)) = time;
				}
			}
			return;
		}
		
		if (type == "recencySendSender")
		{
			if (consider_type)
			{
				arma::vec temp = risksetMatrix.row(sender).t();
				arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
				update_lastActive(time, dyads, lastActive);
			}
			else
			{
				arma::vec dyads = risksetMatrix.row(sender).t();
				update_lastActive(time, dyads, lastActive);
			}
			return;
		}
		
		if (type == "recencySendReceiver")
		{
			if (consider_type)
			{
				arma::vec dyads = risksetMatrix.col(sender + event_type * N);
				update_lastActive(time, dyads, lastActive);
			}
			else
			{
				arma::vec dyads;
				for (int k = 0; k < C; ++k) dyads = arma::join_cols(dyads, risksetMatrix.col(sender + k * N));
				update_lastActive(time, dyads, lastActive);
			}
			return;
		}
		
		if (type == "recencyReceiveSender")
		{
			if (consider_type)
			{
				arma::vec temp = risksetMatrix.row(receiver).t();
				arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
				update_lastActive(time, dyads, lastActive);
			}
			else
			{
				arma::vec dyads = risksetMatrix.row(receiver).t();
				update_lastActive(time, dyads, lastActive);
			}
			return;
		}
		
		if (type == "recencyReceiveReceiver")
		{
			if (consider_type)
			{
				arma::vec dyads = risksetMatrix.col(receiver + event_type * N);
				update_lastActive(time, dyads, lastActive);
			}
			else
			{
				arma::vec dyads;
				for (int k = 0; k < C; ++k) dyads = arma::join_cols(dyads, risksetMatrix.col(receiver + k * N));
				update_lastActive(time, dyads, lastActive);
			}
			return;
		}
	};
	
	for (arma::uword k = 0; k < event_indices.n_elem; ++k) update_from_event(event_indices(k));
	
	Progress p(M, display_progress);
	
	for (arma::uword m = 0; m < M; ++m)
	{
		double current_time = time_points(m);
		
		for (arma::uword s = 0; s < S; ++s) {
			arma::uword d = sample_map(m, s);
			if (d < 0 || (arma::uword)d >= D)
				Rcpp::stop("calculate_recency_sampled: sample_map out of bounds.");
			out(m, s) = 1.0 / ((current_time - lastActive(d)) + 1.0);
		}
		
		// which events to apply after computing row m?
		if (method == "pt")
		{
			double next_time = (m < (M - 1)) ? time_points(m + 1) : current_time;
			event_indices = arma::find(edgelist.col(0) >= current_time && edgelist.col(0) < next_time);
		}
		else
		{
			event_indices.set_size(1);
			event_indices(0) = start + m;
		}
		
		for (arma::uword k = 0; k < event_indices.n_elem; ++k) update_from_event(event_indices(k));
		
		p.increment();
	}
	
	return out;
}


static inline void mark_dyad(std::vector<int> &marked, int dyad) {
	if (dyad >= 0) marked.push_back(dyad);
}

static inline void mark_dyads_from_vec(std::vector<int> &marked,
                                       const arma::vec &dyads,
                                       arma::uword D) {
	for (arma::uword j = 0; j < dyads.n_elem; ++j) {
		int d = static_cast<int>(dyads(j));
		if (d >= 0 && static_cast<arma::uword>(d) < D)
			marked.push_back(d);
	}
}


static inline void mark_dyads_from_mat_as_col(std::vector<int> &marked,
                                              const arma::mat &dyadsMat,
                                              arma::uword D) {
	arma::vec dyads = arma::vectorise(dyadsMat); // safer/clearer than as_col()
	mark_dyads_from_vec(marked, dyads, D);
}


// [[Rcpp::export]]
arma::mat calculate_pshift_sampled(std::string type,
                                   const arma::mat &edgelist,
                                   const arma::mat &risksetMatrix,
                                   const arma::mat &riskset,
                                   int start, int stop,
                                   bool directed,
                                   bool consider_type,
                                   bool display_progress,
                                   Rcpp::String method,
                                   const arma::imat &sample_map)
{
	if (display_progress) {
		Rcpp::Rcout << "Calculating pshift " << type << " statistic (sampled)" << std::endl;
	}
	
	// time_points
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("calculate_pshift_sampled: method must be 'pt' or 'pe'.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	int N = risksetMatrix.n_rows;
	int C = risksetMatrix.n_cols / N;
	arma::uword D = static_cast<arma::uword>(risksetMatrix.max() + 1);
	
	arma::mat out(M, S, arma::fill::zeros);
	Progress p(M, display_progress);
	
	// helper: emit row i from marked dyads
	auto emit_row = [&](arma::uword i, const std::vector<int> &marked) {
		std::unordered_set<arma::uword> hit;
		hit.reserve(marked.size() * 2);
		
		for (int d : marked) {
			if (d >= 0) hit.insert(static_cast<arma::uword>(d));
		}
		
		for (arma::uword s = 0; s < S; ++s) {
			arma::uword d0 = sample_map(i, s);
			if (d0 >= D) Rcpp::stop("calculate_pshift_sampled: sample_map out of bounds.");
			out(i, s) = (hit.find(d0) != hit.end()) ? 1.0 : 0.0;
		}
	};
	
	for (arma::uword i = 0; i < M; ++i)
	{
		double current_time = time_points(i);
		
		// event_indices at row i
		arma::uvec event_indices;
		double previous_time = 0.0;
		if (i > 0) previous_time = time_points(i - 1);
		
		if (method == "pt") {
			event_indices = arma::find(edgelist.col(0) >= previous_time &&
				edgelist.col(0) < current_time);
		} else { // pe
			if (i == 0) {
				event_indices = arma::find(edgelist.col(0) < current_time);
				if (event_indices.n_elem > 0) {
					arma::uword last_event = arma::max(event_indices);
					event_indices.set_size(1);
					event_indices(0) = last_event;
				} else {
					event_indices.reset(); // empty
				}
			} else {
				event_indices.set_size(1);
				event_indices(0) = start + i - 1;
			}
		}
		
		std::vector<int> marked;
		marked.reserve(128);
		
		for (arma::uword m = 0; m < event_indices.n_elem; ++m)
		{
			arma::uword event = event_indices(m);
			arma::uword sender = static_cast<arma::uword>(edgelist(event, 1));
			arma::uword receiver = static_cast<arma::uword>(edgelist(event, 2));
			arma::uword event_type = 0;
			if (C > 1) event_type = static_cast<arma::uword>(edgelist(event, 3));
			
			// AB-BA
			if (type == "AB-BA")
			{
				if (consider_type)
				{
					int dyad = risksetMatrix(receiver, sender + event_type * N);
					mark_dyad(marked, dyad);
				}
				else
				{
					for (int k = 0; k < C; ++k)
					{
						int dyad = risksetMatrix(receiver, sender + k * N);
						mark_dyad(marked, dyad);
					}
				}
			}
			
			// AB-BY
			else if (type == "AB-BY")
			{
				if (consider_type)
				{
					arma::vec temp = risksetMatrix.row(receiver).t();
					arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
					dyads.shed_row(sender);
					mark_dyads_from_vec(marked, dyads, D);
				}
				else
				{
					arma::vec dyads = risksetMatrix.row(receiver).t();
					arma::uvec shed_indices(C);
					for (int k = 0; k < C; ++k) shed_indices(k) = sender + k * N;
					dyads.shed_rows(shed_indices);
					mark_dyads_from_vec(marked, dyads, D);
				}
			}
			
			// AB-XA
			else if (type == "AB-XA")
			{
				if (consider_type)
				{
					arma::vec dyads = risksetMatrix.col(sender + event_type * N);
					dyads.shed_row(receiver);
					mark_dyads_from_vec(marked, dyads, D);
				}
				else
				{
					for (int k = 0; k < C; ++k)
					{
						arma::vec dyads = risksetMatrix.col(sender + k * N);
						dyads.shed_row(receiver);
						mark_dyads_from_vec(marked, dyads, D);
					}
				}
			}
			
			// AB-XB
			else if (type == "AB-XB")
			{
				if (consider_type)
				{
					arma::vec dyads = risksetMatrix.col(receiver + event_type * N);
					dyads.shed_row(sender);
					mark_dyads_from_vec(marked, dyads, D);
				}
				else
				{
					for (int k = 0; k < C; ++k)
					{
						arma::vec dyads = risksetMatrix.col(receiver + k * N);
						dyads.shed_row(sender);
						mark_dyads_from_vec(marked, dyads, D);
					}
				}
			}
			
			// AB-XY
			else if (type == "AB-XY")
			{
				if (consider_type)
				{
					arma::mat dyadsMat = risksetMatrix.cols(event_type * N, ((event_type + 1) * N) - 1);
					arma::uvec idx = { sender, receiver };
					dyadsMat.shed_rows(idx);
					dyadsMat.shed_cols(idx);
					mark_dyads_from_mat_as_col(marked, dyadsMat, D);
				}
				else
				{
					arma::mat dyadsMat = risksetMatrix;
					arma::uvec row_idx = { sender, receiver };
					dyadsMat.shed_rows(row_idx);
					
					arma::uvec col_idx(C * 2);
					for (int k = 0; k < C; ++k)
					{
						col_idx(k) = sender + k * N;
						col_idx(k + C) = receiver + k * N;
					}
					dyadsMat.shed_cols(col_idx);
					mark_dyads_from_mat_as_col(marked, dyadsMat, D);
				}
			}
			
			// AB-AY
			else if (type == "AB-AY")
			{
				if (consider_type)
				{
					// A as sender
					arma::vec temp = risksetMatrix.row(sender).t();
					arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
					dyads.shed_row(receiver);
					mark_dyads_from_vec(marked, dyads, D);
					
					if (!directed)
					{
						// A as second actor
						dyads = risksetMatrix.col(sender + event_type * N);
						dyads.shed_row(receiver);
						mark_dyads_from_vec(marked, dyads, D);
						
						// B as sender
						temp = risksetMatrix.row(receiver).t();
						dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
						dyads.shed_row(sender);
						mark_dyads_from_vec(marked, dyads, D);
						
						// B as second actor
						dyads = risksetMatrix.col(receiver + event_type * N);
						dyads.shed_row(sender);
						mark_dyads_from_vec(marked, dyads, D);
					}
				}
				else
				{
					// A as sender
					arma::vec dyads = risksetMatrix.row(sender).t();
					arma::uvec shed_indices(C);
					for (int k = 0; k < C; ++k) shed_indices(k) = receiver + k * N;
					dyads.shed_rows(shed_indices);
					mark_dyads_from_vec(marked, dyads, D);
					
					if (!directed)
					{
						// A as second actor
						for (int k = 0; k < C; ++k)
						{
							arma::vec dy2 = risksetMatrix.col(sender + k * N);
							dy2.shed_row(receiver);
							mark_dyads_from_vec(marked, dy2, D);
						}
						
						// B as sender
						dyads = risksetMatrix.row(receiver).t();
						for (int k = 0; k < C; ++k) shed_indices(k) = sender + k * N;
						dyads.shed_rows(shed_indices);
						mark_dyads_from_vec(marked, dyads, D);
						
						// B as second actor
						for (int k = 0; k < C; ++k)
						{
							arma::vec dy2 = risksetMatrix.col(receiver + k * N);
							dy2.shed_row(sender);
							mark_dyads_from_vec(marked, dy2, D);
						}
					}
				}
			}
			
			// AB-AB
			else if (type == "AB-AB")
			{
				if (consider_type)
				{
					int dyad = risksetMatrix(sender, receiver + event_type * N);
					mark_dyad(marked, dyad);
				}
				else
				{
					for (int k = 0; k < C; ++k)
					{
						int dyad = risksetMatrix(sender, receiver + k * N);
						mark_dyad(marked, dyad);
					}
				}
			}
			else
			{
				Rcpp::stop("calculate_pshift_sampled: unknown pshift type.");
			}
		} // end loop over event_indices
		
		emit_row(i, marked);
		p.increment();
	}
	
	return out;
}


// assumes rankR(rowvec, N) exists and returns a rowvec of ranks length N
// arma::rowvec rankR(const arma::rowvec& x, int N);

// static arma::rowvec rankR(arma::rowvec x, int N)
// {
// 	arma::uvec ranksU = N - sort_index(sort_index(x));
// 	arma::rowvec ranks = arma::conv_to<arma::rowvec>::from(ranksU);
// 	arma::uvec indices = arma::find(x == 0);
// 	arma::rowvec reps(indices.n_elem, arma::fill::zeros);
// 	ranks(indices) = reps;
// 	return ranks;
// }

// static inline std::uint64_t pack_key(int a1, int et) {
// 	return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(a1)) << 32) |
// 		static_cast<std::uint32_t>(et);
// }


// rank among a vector of lastTimes; return ranks where 0-time gets rank 0
static inline std::vector<int> rank_times_desc(const std::vector<double>& times) {
	const int n = static_cast<int>(times.size());
	std::vector<int> idx(n);
	for (int i = 0; i < n; ++i) idx[i] = i;
	
	std::sort(idx.begin(), idx.end(),
           [&](int a, int b) { return times[a] > times[b]; });
	
	std::vector<int> rank(n, 0);
	for (int pos = 0; pos < n; ++pos) {
		int i = idx[pos];
		if (times[i] == 0.0) rank[i] = 0;
		else rank[i] = n - pos; // highest time -> n
	}
	return rank;
}

// [[Rcpp::export]]
arma::mat calculate_rrank_sampled(int type,
                                  const arma::mat &edgelist,
                                  const arma::mat &risksetMatrix,
                                  const arma::mat &riskset,     // cols: sender, receiver, type, dyad_id
                                  int start, int stop,
                                  bool consider_type,
                                  bool display_progress,
                                  Rcpp::String method,
                                  const arma::imat &sample_map) // dyad IDs (0-based)
{
	if (type != 1 && type != 2) Rcpp::stop("calculate_rrank_sampled: type must be 1 (send) or 2 (receive).");
	
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("calculate_rrank_sampled: method must be 'pt' or 'pe'.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	if (riskset.n_cols < 4) Rcpp::stop("calculate_rrank_sampled: riskset must have >=4 cols (sender, receiver, type, dyad_id).");
	
	// D from risksetMatrix
	const arma::uword D = static_cast<arma::uword>(risksetMatrix.max() + 1);
	
	// dyad_id -> (sender, receiver, et)
	std::vector<int> a1_by_dyad(D, -1), a2_by_dyad(D, -1), et_by_dyad(D, 0);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = static_cast<int>(riskset(r, 3));
		if (d < 0 || static_cast<arma::uword>(d) >= D) continue;
		a1_by_dyad[d] = static_cast<int>(riskset(r, 0));
		a2_by_dyad[d] = static_cast<int>(riskset(r, 1));
		et_by_dyad[d] = (riskset.n_cols >= 3) ? static_cast<int>(riskset(r, 2)) : 0;
	}
	
	// Build admissible dyads per "row-group":
	// if consider_type: group = (sender, et)
	// else: group = (sender) pooling types
	int N = risksetMatrix.n_rows;
	int C = risksetMatrix.n_cols / N;
	const int K = consider_type ? C : 1;
	
	// group key -> list of dyad_ids
	// key = sender*K + (consider_type?et:0)
	std::vector<std::vector<int>> group_dyads(static_cast<size_t>(N * K));
	for (arma::uword d = 0; d < D; ++d) {
		int a1 = a1_by_dyad[d];
		if (a1 < 0) continue;
		int et = consider_type ? et_by_dyad[d] : 0;
		if (consider_type && (et < 0 || et >= C)) continue;
		group_dyads[static_cast<size_t>(a1 * K + et)].push_back(static_cast<int>(d));
	}
	
	// last time per dyad_id (and if consider_type=FALSE we still store per dyad; pooling happens via group)
	std::vector<double> lastTime(D, 0.0);
	
	// init from past
	double first_time = edgelist(start, 0);
	arma::uvec event_indices = arma::find(edgelist.col(0) < first_time);
	
	for (arma::uword m = 0; m < event_indices.n_elem; ++m) {
		arma::uword ev = event_indices(m);
		double t = edgelist(ev, 0);
		int sender = static_cast<int>(edgelist(ev, 1));
		int receiver = static_cast<int>(edgelist(ev, 2));
		int et = (C > 1) ? static_cast<int>(edgelist(ev, 3)) : 0;
		
		int a_from = (type == 1) ? sender : receiver;
		int a_to   = (type == 1) ? receiver : sender;
		
		// update matching dyad(s)
		if (consider_type) {
			int d = risksetMatrix(a_from, a_to + et * N);
			if (d >= 0) lastTime[static_cast<arma::uword>(d)] = t;
		} else {
			for (int k = 0; k < C; ++k) {
				int d = risksetMatrix(a_from, a_to + k * N);
				if (d >= 0) lastTime[static_cast<arma::uword>(d)] = t;
			}
		}
	}
	
	Progress p(M, display_progress);
	
	// per-row cache: group_key -> ranks vector aligned with group_dyads list indices
	for (arma::uword i = 0; i < M; ++i) {
		
		std::unordered_map<int, std::vector<int>> rank_cache;
		rank_cache.reserve(static_cast<size_t>(S * 2));
		
		auto get_rank_for = [&](int a1, int et, int dyad_id) -> int {
			int g = a1 * K + et;
			auto it = rank_cache.find(g);
			if (it == rank_cache.end()) {
				const auto& dyads = group_dyads[static_cast<size_t>(g)];
				std::vector<double> times;
				times.reserve(dyads.size());
				for (int d : dyads) times.push_back(lastTime[static_cast<arma::uword>(d)]);
				std::vector<int> ranks = rank_times_desc(times);
				it = rank_cache.emplace(g, std::move(ranks)).first;
			}
			// find dyad_id position within group list (linear; you can preindex if needed)
			const auto& dyads = group_dyads[static_cast<size_t>(g)];
			for (size_t pos = 0; pos < dyads.size(); ++pos) {
				if (dyads[pos] == dyad_id) return it->second[pos];
			}
			return 0;
		};
		
		// emit sampled
		for (arma::uword s = 0; s < S; ++s) {
			int d = static_cast<int>(sample_map(i, s));
			if (d < 0 || static_cast<arma::uword>(d) >= D) Rcpp::stop("calculate_rrank_sampled: sample_map out of bounds.");
			
			int a1 = a1_by_dyad[static_cast<arma::uword>(d)];
			int et = consider_type ? et_by_dyad[static_cast<arma::uword>(d)] : 0;
			if (a1 < 0) Rcpp::stop("calculate_rrank_sampled: dyad not found in riskset mapping.");
			
			int rk = get_rank_for(a1, et, d);
			out(i, s) = (rk > 0) ? (1.0 / static_cast<double>(rk)) : 0.0;
		}
		
		// update lastTime with events at this row
		double current_time = time_points(i);
		if (method == "pt") {
			double next_time = (i < (M - 1)) ? time_points(i + 1) : current_time;
			event_indices = arma::find(edgelist.col(0) >= current_time && edgelist.col(0) < next_time);
		} else {
			event_indices.set_size(1);
			event_indices(0) = start + i;
		}
		
		for (arma::uword m = 0; m < event_indices.n_elem; ++m) {
			arma::uword ev = event_indices(m);
			double t = edgelist(ev, 0);
			int sender = static_cast<int>(edgelist(ev, 1));
			int receiver = static_cast<int>(edgelist(ev, 2));
			int et = (C > 1) ? static_cast<int>(edgelist(ev, 3)) : 0;
			
			int a_from = (type == 1) ? sender : receiver;
			int a_to   = (type == 1) ? receiver : sender;
			
			if (consider_type) {
				int d = risksetMatrix(a_from, a_to + et * N);
				if (d >= 0) lastTime[static_cast<arma::uword>(d)] = t;
			} else {
				for (int k = 0; k < C; ++k) {
					int d = risksetMatrix(a_from, a_to + k * N);
					if (d >= 0) lastTime[static_cast<arma::uword>(d)] = t;
				}
			}
		}
		
		p.increment();
	}
	
	return out;
}


// [[Rcpp::export]]
arma::mat calculate_inertia_sampled(const arma::mat &edgelist,
                                    const arma::vec &weights,
                                    const arma::mat &risksetMatrix,
                                    Rcpp::String memory,
                                    const arma::vec &memory_value,
                                    int start, int stop,
                                    bool display_progress,
                                    Rcpp::String method,
                                    const arma::imat &sample_map)
{
	if (display_progress) {
		Rcpp::Rcout << "Calculating inertia statistic (sampled)" << std::endl;
	}
	
	// time points
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("calculate_inertia_sampled: method must be 'pt' or 'pe'.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	
	int N = risksetMatrix.n_rows;
	int C = risksetMatrix.n_cols / N;
	const arma::uword D = static_cast<arma::uword>(risksetMatrix.max() + 1);
	
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	// --- event pointer helpers (process each event once, in time order) ---
	auto event_time = [&](arma::uword ev) -> double { return edgelist(ev, 0); };
	
	// map event -> dyad_id (typed)
	auto dyad_of_event = [&](arma::uword ev) -> int {
		int a1 = static_cast<int>(edgelist(ev, 1));
		int a2 = static_cast<int>(edgelist(ev, 2));
		int et = 0;
		if (C > 1) et = static_cast<int>(edgelist(ev, 3));
		int dyad = static_cast<int>(risksetMatrix(a1, a2 + N * et));
		return dyad; // may be -999 / <0
	};
	
	// --- state for each memory mode ---
	// full: cumulative sum per dyad
	std::vector<double> full_sum;
	// window: deque per dyad + running sum
	std::vector<std::deque<std::pair<double,double>>> win_q;
	std::vector<double> win_sum;
	// interval: deque per dyad (kept up to max window), compute interval sum on demand
	std::vector<std::deque<std::pair<double,double>>> int_q;
	// decay: lazy exponential state per dyad
	std::vector<double> dec_state, dec_last_t;
	
	// parse memory_value
	double win_len = NA_REAL;
	double int_min = NA_REAL, int_max = NA_REAL;
	double half_life = NA_REAL;
	double lambda = NA_REAL;
	
	if (memory == "full") {
		full_sum.assign(D, 0.0);
	} else if (memory == "window") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_inertia_sampled: window requires memory_value length 1.");
		win_len = memory_value(0);
		if (win_len < 0) Rcpp::stop("calculate_inertia_sampled: window length must be >= 0.");
		win_q.resize(D);
		win_sum.assign(D, 0.0);
	} else if (memory == "interval") {
		if (memory_value.n_elem < 2) Rcpp::stop("calculate_inertia_sampled: interval requires memory_value length 2 (min,max).");
		int_min = memory_value(0);
		int_max = memory_value(1);
		if (int_min < 0 || int_max < int_min) Rcpp::stop("calculate_inertia_sampled: invalid interval memory_value.");
		int_q.resize(D);
	} else if (memory == "decay") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_inertia_sampled: decay requires memory_value length 1 (half-life).");
		half_life = memory_value(0);
		if (half_life <= 0) Rcpp::stop("calculate_inertia_sampled: decay half-life must be > 0.");
		lambda = std::log(2.0) / half_life;
		dec_state.assign(D, 0.0);
		dec_last_t.assign(D, 0.0); // will be initialized on first touch
		// We'll set dec_last_t[d] at first update/query to that time.
		for (arma::uword d = 0; d < D; ++d) dec_last_t[d] = NAN;
	} else {
		Rcpp::stop("calculate_inertia_sampled: unknown memory (full/window/interval/decay).");
	}
	
	// update functions
	auto update_full = [&](arma::uword ev) {
		int d = dyad_of_event(ev);
		if (d >= 0) full_sum[static_cast<arma::uword>(d)] += weights(ev);
	};
	
	auto prune_window = [&](arma::uword d, double now) {
		auto &q = win_q[d];
		double cutoff = now - win_len;
		while (!q.empty() && q.front().first < cutoff) {
			win_sum[d] -= q.front().second;
			q.pop_front();
		}
	};
	
	auto update_window = [&](arma::uword ev) {
		int d = dyad_of_event(ev);
		if (d < 0) return;
		arma::uword du = static_cast<arma::uword>(d);
		double t = event_time(ev);
		double w = weights(ev);
		win_q[du].push_back({t, w});
		win_sum[du] += w;
	};
	
	auto prune_interval = [&](arma::uword d, double now) {
		// keep only events with time >= now - int_max (older can never contribute)
		auto &q = int_q[d];
		double oldest_allowed = now - int_max;
		while (!q.empty() && q.front().first < oldest_allowed) q.pop_front();
	};
	
	auto update_interval = [&](arma::uword ev) {
		int d = dyad_of_event(ev);
		if (d < 0) return;
		arma::uword du = static_cast<arma::uword>(d);
		double t = event_time(ev);
		double w = weights(ev);
		int_q[du].push_back({t, w});
	};
	
	auto decay_touch = [&](arma::uword d, double t_now) {
		if (std::isnan(dec_last_t[d])) {
			dec_last_t[d] = t_now;
			return;
		}
		double dt = t_now - dec_last_t[d];
		if (dt > 0) {
			dec_state[d] *= std::exp(-lambda * dt);
			dec_last_t[d] = t_now;
		} else {
			// if dt==0 or negative, do nothing (assumes nondecreasing time)
			dec_last_t[d] = t_now;
		}
	};
	
	auto update_decay = [&](arma::uword ev) {
		int d = dyad_of_event(ev);
		if (d < 0) return;
		arma::uword du = static_cast<arma::uword>(d);
		double t = event_time(ev);
		decay_touch(du, t);
		dec_state[du] += weights(ev);
	};
	
	// --- initialize event pointer to include history before first selected time ---
	arma::uword ev_ptr = 0;
	double first_t = time_points(0);
	
	if (method == "pe") {
		// for pe, time_points are edgelist times; "history before first_t" means events with time < first_t
		while (ev_ptr < static_cast<arma::uword>(edgelist.n_rows) && event_time(ev_ptr) < first_t) {
			if (memory == "full") update_full(ev_ptr);
			else if (memory == "window") update_window(ev_ptr);
			else if (memory == "interval") update_interval(ev_ptr);
			else if (memory == "decay") update_decay(ev_ptr);
			ev_ptr++;
		}
	} else { // pt
		// same: process all events with time < first selected timepoint
		while (ev_ptr < static_cast<arma::uword>(edgelist.n_rows) && event_time(ev_ptr) < first_t) {
			if (memory == "full") update_full(ev_ptr);
			else if (memory == "window") update_window(ev_ptr);
			else if (memory == "interval") update_interval(ev_ptr);
			else if (memory == "decay") update_decay(ev_ptr);
			ev_ptr++;
		}
	}
	
	Progress p(M, display_progress);
	
	for (arma::uword m = 0; m < M; ++m) {
		double now = time_points(m);
		
		// For pt: before emitting at time now, ensure all events with time < now have been processed.
		// For pe: time_points are event times; the initialization + incremental pointer already ensures this invariant.
		while (ev_ptr < static_cast<arma::uword>(edgelist.n_rows) && event_time(ev_ptr) < now) {
			if (memory == "full") update_full(ev_ptr);
			else if (memory == "window") update_window(ev_ptr);
			else if (memory == "interval") update_interval(ev_ptr);
			else if (memory == "decay") update_decay(ev_ptr);
			ev_ptr++;
		}
		
		// emit only sampled dyads
		for (arma::uword s = 0; s < S; ++s) {
			arma::uword d = sample_map(m, s);
			if (d >= D) Rcpp::stop("calculate_inertia_sampled: sample_map out of bounds.");
			
			if (memory == "full") {
				out(m, s) = full_sum[d];
			} else if (memory == "window") {
				prune_window(d, now);
				out(m, s) = win_sum[d];
			} else if (memory == "interval") {
				prune_interval(d, now);
				// sum weights for events with time < now - int_min (and >= now - int_max due to pruning)
				double upper = now - int_min;
				double acc = 0.0;
				const auto &q = int_q[d];
				for (const auto &tw : q) {
					if (tw.first < upper) acc += tw.second;
					else break; // times are nondecreasing
				}
				out(m, s) = acc;
			} else { // decay
				decay_touch(d, now);
				out(m, s) = dec_state[d];
			}
		}
		
		// For pe, move pointer to include the event at index (start + m) after emitting row m,
		// so next row sees it as "past". This matches the intended "< current_time" history.
		if (method == "pe") {
			arma::uword ev = static_cast<arma::uword>(start) + m;
			if (ev < static_cast<arma::uword>(edgelist.n_rows)) {
				// only add if its time == now (it should) or <= now; adding here makes it past for next row
				if (memory == "full") update_full(ev);
				else if (memory == "window") update_window(ev);
				else if (memory == "interval") update_interval(ev);
				else if (memory == "decay") update_decay(ev);
				// and keep ev_ptr consistent if we haven't reached it by time-based advancement
				if (ev >= ev_ptr) ev_ptr = ev + 1;
			}
		}
		
		p.increment();
	}
	
	return out;
}


// indegree of the sender of each sampled dyad (same memory semantics as reciprocity)
static inline arma::mat indegree_sender_sampled(const arma::mat &edgelist,
                                                const arma::vec &weights,
                                                const arma::mat &riskset,
                                                Rcpp::String memory,
                                                const arma::vec &memory_value,
                                                int start, int stop,
                                                bool consider_type,
                                                Rcpp::String method,
                                                const arma::imat &sample_map,
                                                int N, int C)
{
	// time points
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("indegree_sender_sampled: method must be 'pt' or 'pe'.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	// dyad_id -> (sender, receiver, type)
	arma::uword D = static_cast<arma::uword>(arma::max(riskset.col(3)) + 1);
	std::vector<int> a1_by_d(D, -1), a2_by_d(D, -1), et_by_d(D, 0);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = (int)riskset(r, 3);
		if (d < 0 || (arma::uword)d >= D) continue;
		a1_by_d[d] = (int)riskset(r, 0);
		a2_by_d[d] = (int)riskset(r, 1);
		et_by_d[d] = (int)riskset(r, 2);
	}
	
	const int K = consider_type ? C : 1;
	auto key = [&](int actor, int et){ return actor * K + (consider_type ? et : 0); };
	
	// state per (receiver[,type])
	std::vector<double> state((size_t)N * (size_t)K, 0.0);
	
	// decay bookkeeping (only used if memory=="decay")
	std::vector<double> last_t;
	double lambda = 0.0;
	if (memory == "decay") {
		lambda = std::log(2.0) / memory_value(0);
		last_t.assign(state.size(), NAN);
	}
	
	auto decay_touch = [&](size_t idx, double t_now){
		double &lt = last_t[idx];
		if (std::isnan(lt)) { lt = t_now; return; }
		double dt = t_now - lt;
		if (dt > 0) state[idx] *= std::exp(-lambda * dt);
		lt = t_now;
	};
	
	// For window/interval we keep per-key queues (sparse, only touched keys)
	std::vector<std::deque<std::pair<double,double>>> q;
	std::vector<double> qsum;
	double win_len=0.0, int_min=0.0, int_max=0.0;
	if (memory == "window" || memory == "interval") {
		q.resize(state.size());
		if (memory == "window") { win_len = memory_value(0); qsum.assign(state.size(), 0.0); }
		if (memory == "interval") { int_min = memory_value(0); int_max = memory_value(1); }
	}
	
	auto update = [&](arma::uword ev){
		double t = edgelist(ev, 0);
		//int sender = (int)edgelist(ev, 1);
		int receiver = (int)edgelist(ev, 2);
		int et = (C > 1) ? (int)edgelist(ev, 3) : 0;
		
		int idx = key(receiver, et);
		double w = weights(ev);
		
		if (memory == "full") state[(size_t)idx] += w;
		else if (memory == "decay") { decay_touch((size_t)idx, t); state[(size_t)idx] += w; }
		else if (memory == "window") { q[(size_t)idx].push_back({t,w}); qsum[(size_t)idx] += w; }
		else if (memory == "interval") { q[(size_t)idx].push_back({t,w}); }
	};
	
	auto query = [&](int receiver, int et, double now)->double{
		size_t idx = (size_t)key(receiver, et);
		
		if (memory == "full") return state[idx];
		if (memory == "decay") { decay_touch(idx, now); return state[idx]; }
		if (memory == "window") {
			double cutoff = now - win_len;
			auto &dq = q[idx];
			while (!dq.empty() && dq.front().first < cutoff) { qsum[idx] -= dq.front().second; dq.pop_front(); }
			return qsum[idx];
		}
		// interval
		auto &dq = q[idx];
		double oldest = now - int_max;
		while (!dq.empty() && dq.front().first < oldest) dq.pop_front();
		double upper = now - int_min;
		double acc = 0.0;
		for (auto &tw : dq) { if (tw.first < upper) acc += tw.second; else break; }
		return acc;
	};
	
	// init from past (< first time)
	arma::uword ev_ptr = 0;
	double first_t = time_points(0);
	while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < first_t) update(ev_ptr++);
	
	for (arma::uword m = 0; m < M; ++m) {
		double now = time_points(m);
		
		if (method == "pt") {
			while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < now) update(ev_ptr++);
		}
		
		for (arma::uword s = 0; s < S; ++s) {
			int d = (int)sample_map(m,s);
			if (d < 0 || (arma::uword)d >= D) Rcpp::stop("indegree_sender_sampled: sample_map dyad id out of bounds.");
			int sender = a1_by_d[(arma::uword)d];
			int et = consider_type ? et_by_d[(arma::uword)d] : 0;
			out(m,s) = query(sender, et, now);
		}
		
		if (method == "pe") {
			arma::uword ev = (arma::uword)start + m;
			if (ev < (arma::uword)edgelist.n_rows) update(ev);
		}
	}
	
	return out;
}


// [[Rcpp::export]]
arma::mat calculate_reciprocity_sampled(const arma::mat &edgelist,
                                        const arma::vec &weights,
                                        const arma::mat &risksetMatrix,
                                        const arma::mat &riskset,          // cols: sender, receiver, type, dyad_id (0-based)
                                        Rcpp::String memory,               // full/window/interval/decay
                                        const arma::vec &memory_value,     // window: [L], interval: [min,max], decay: [half_life]
                                        int start, int stop,
                                        bool consider_type,                // TRUE: same type only; FALSE: sum over types
                                        bool display_progress,
                                        Rcpp::String method,               // pt/pe
                                        const arma::imat &sample_map)      // MxS, 0-based dyad_id
{
	if (riskset.n_cols < 4) Rcpp::stop("calculate_reciprocity_sampled: riskset must have >=4 cols (sender, receiver, type, dyad_id).");
	
	// time points
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("calculate_reciprocity_sampled: method must be 'pt' or 'pe'.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	const int N = risksetMatrix.n_rows;
	const int C = risksetMatrix.n_cols / N;
	const arma::uword D = static_cast<arma::uword>(risksetMatrix.max() + 1);
	
	// --- dyad_id -> (a1,a2,et) lookup from riskset ---
	std::vector<int> a1_by_d(D, -1), a2_by_d(D, -1), et_by_d(D, 0);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = static_cast<int>(riskset(r, 3));
		if (d < 0 || static_cast<arma::uword>(d) >= D) continue;
		a1_by_d[d] = static_cast<int>(riskset(r, 0));
		a2_by_d[d] = static_cast<int>(riskset(r, 1));
		et_by_d[d] = static_cast<int>(riskset(r, 2));
	}
	
	// --- collect unique sampled dyads across all rows/cols ---
	std::unordered_set<int> sampled_dyads;
	sampled_dyads.reserve(static_cast<size_t>(M * S / 4 + 1));
	for (arma::uword m = 0; m < M; ++m) {
		for (arma::uword s = 0; s < S; ++s) {
			int d = static_cast<int>(sample_map(m, s));
			if (d < 0 || static_cast<arma::uword>(d) >= D) Rcpp::stop("calculate_reciprocity_sampled: sample_map out of bounds.");
			sampled_dyads.insert(d);
		}
	}
	
	// --- for each sampled dyad, compute reverse dyad list; collect tracked reverse dyads ---
	std::unordered_map<int, std::vector<int>> rev_of_sampled;
	rev_of_sampled.reserve(sampled_dyads.size() * 2);
	
	std::unordered_set<int> tracked;
	tracked.reserve(sampled_dyads.size() * 2);
	
	for (int d : sampled_dyads) {
		int a1 = a1_by_d[d], a2 = a2_by_d[d], et = et_by_d[d];
		if (a1 < 0 || a2 < 0) Rcpp::stop("calculate_reciprocity_sampled: sampled dyad not found in riskset mapping.");
		
		std::vector<int> revs;
		if (consider_type) {
			int dr = static_cast<int>(risksetMatrix(a2, a1 + et * N));
			if (dr >= 0) {
				revs.push_back(dr);
				tracked.insert(dr);
			}
		} else {
			revs.reserve(static_cast<size_t>(C));
			for (int k = 0; k < C; ++k) {
				int dr = static_cast<int>(risksetMatrix(a2, a1 + k * N));
				if (dr >= 0) {
					revs.push_back(dr);
					tracked.insert(dr);
				}
			}
		}
		rev_of_sampled.emplace(d, std::move(revs));
	}
	
	// compact index for tracked dyads (avoid D-sized state)
	std::unordered_map<int, int> idx_of_d;
	idx_of_d.reserve(tracked.size() * 2);
	int Q = 0;
	for (int d : tracked) idx_of_d.emplace(d, Q++);
	
	// --- parse memory ---
	if (!(memory == "full" || memory == "window" || memory == "interval" || memory == "decay"))
		Rcpp::stop("calculate_reciprocity_sampled: memory must be full/window/interval/decay.");
	
	double win_len = 0.0, int_min = 0.0, int_max = 0.0, half_life = 0.0, lambda = 0.0;
	
	if (memory == "window") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_reciprocity_sampled: window requires memory_value length 1.");
		win_len = memory_value(0);
		if (win_len < 0) Rcpp::stop("calculate_reciprocity_sampled: window length must be >= 0.");
	} else if (memory == "interval") {
		if (memory_value.n_elem < 2) Rcpp::stop("calculate_reciprocity_sampled: interval requires memory_value length 2 (min,max).");
		int_min = memory_value(0);
		int_max = memory_value(1);
		if (int_min < 0 || int_max < int_min) Rcpp::stop("calculate_reciprocity_sampled: invalid interval memory_value.");
	} else if (memory == "decay") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_reciprocity_sampled: decay requires memory_value length 1 (half-life).");
		half_life = memory_value(0);
		if (half_life <= 0) Rcpp::stop("calculate_reciprocity_sampled: decay half-life must be > 0.");
		lambda = std::log(2.0) / half_life;
	}
	
	// --- state (size Q only) ---
	std::vector<double> full_sum;
	std::vector<std::deque<std::pair<double,double>>> q_ev; // (time, weight) for window/interval
	std::vector<double> win_sum;
	std::vector<double> dec_state, dec_last_t;
	
	if (memory == "full") {
		full_sum.assign(static_cast<size_t>(Q), 0.0);
	} else if (memory == "window") {
		q_ev.resize(static_cast<size_t>(Q));
		win_sum.assign(static_cast<size_t>(Q), 0.0);
	} else if (memory == "interval") {
		q_ev.resize(static_cast<size_t>(Q));
	} else { // decay
		dec_state.assign(static_cast<size_t>(Q), 0.0);
		dec_last_t.assign(static_cast<size_t>(Q), NAN);
	}
	
	auto decay_touch = [&](int qi, double t_now) {
		double &lt = dec_last_t[static_cast<size_t>(qi)];
		if (std::isnan(lt)) { lt = t_now; return; }
		double dt = t_now - lt;
		if (dt > 0) dec_state[static_cast<size_t>(qi)] *= std::exp(-lambda * dt);
		lt = t_now;
	};
	
	// update state with one event (only if tracked)
	auto update_with_event = [&](arma::uword ev) {
		int sender = static_cast<int>(edgelist(ev, 1));
		int receiver = static_cast<int>(edgelist(ev, 2));
		int et = (C > 1) ? static_cast<int>(edgelist(ev, 3)) : 0;
		
		int d = static_cast<int>(risksetMatrix(sender, receiver + et * N));
		if (d < 0) return;
		
		auto it = idx_of_d.find(d);
		if (it == idx_of_d.end()) return;
		
		int qi = it->second;
		double t = edgelist(ev, 0);
		double w = weights(ev);
		
		if (memory == "full") {
			full_sum[static_cast<size_t>(qi)] += w;
		} else if (memory == "window") {
			q_ev[static_cast<size_t>(qi)].push_back({t, w});
			win_sum[static_cast<size_t>(qi)] += w;
		} else if (memory == "interval") {
			q_ev[static_cast<size_t>(qi)].push_back({t, w});
		} else { // decay
			decay_touch(qi, t);
			dec_state[static_cast<size_t>(qi)] += w;
		}
	};
	
	auto prune_window = [&](int qi, double now) {
		auto &dq = q_ev[static_cast<size_t>(qi)];
		double cutoff = now - win_len;
		while (!dq.empty() && dq.front().first < cutoff) {
			win_sum[static_cast<size_t>(qi)] -= dq.front().second;
			dq.pop_front();
		}
	};
	
	auto prune_interval = [&](int qi, double now) {
		auto &dq = q_ev[static_cast<size_t>(qi)];
		double oldest = now - int_max;
		while (!dq.empty() && dq.front().first < oldest) dq.pop_front();
	};
	
	auto query_state = [&](int d, double now) -> double {
		auto it = idx_of_d.find(d);
		if (it == idx_of_d.end()) return 0.0;
		int qi = it->second;
		
		if (memory == "full") {
			return full_sum[static_cast<size_t>(qi)];
		} else if (memory == "window") {
			prune_window(qi, now);
			return win_sum[static_cast<size_t>(qi)];
		} else if (memory == "interval") {
			prune_interval(qi, now);
			double upper = now - int_min; // include events with t < upper
			double acc = 0.0;
			const auto &dq = q_ev[static_cast<size_t>(qi)];
			for (const auto &tw : dq) {
				if (tw.first < upper) acc += tw.second;
				else break;
			}
			return acc;
		} else { // decay
			decay_touch(qi, now);
			return dec_state[static_cast<size_t>(qi)];
		}
	};
	
	// --- initialize from events before first selected time ---
	arma::uword ev_ptr = 0;
	double first_t = time_points(0);
	while (ev_ptr < static_cast<arma::uword>(edgelist.n_rows) && edgelist(ev_ptr, 0) < first_t) {
		update_with_event(ev_ptr);
		++ev_ptr;
	}
	
	Progress p(M, display_progress);
	
	for (arma::uword m = 0; m < M; ++m) {
		double now = time_points(m);
		
		// pt: advance state to include all events strictly before now
		if (method == "pt") {
			while (ev_ptr < static_cast<arma::uword>(edgelist.n_rows) &&
          edgelist(ev_ptr, 0) < now) {
				update_with_event(ev_ptr);
				++ev_ptr;
			}
		}
		
		// emit sampled reciprocity at time now
		for (arma::uword s = 0; s < S; ++s) {
			int d = static_cast<int>(sample_map(m, s));
			const auto it = rev_of_sampled.find(d);
			if (it == rev_of_sampled.end()) { out(m, s) = 0.0; continue; }
			
			double acc = 0.0;
			for (int dr : it->second) acc += query_state(dr, now);
			out(m, s) = acc;
		}
		
		// pe: add the current event AFTER emitting, so it becomes past for next row
		if (method == "pe") {
			arma::uword ev = static_cast<arma::uword>(start) + m;
			if (ev < static_cast<arma::uword>(edgelist.n_rows)) update_with_event(ev);
		}
		
		p.increment();
	}
	
	return out;
}


// build dyad_id -> sender map once (use in multiple stats)
static inline std::vector<int> sender_by_dyad_from_riskset(const arma::mat &riskset,
                                                           arma::uword D)
{
	if (riskset.n_cols < 4) Rcpp::stop("riskset must have >=4 cols (sender, receiver, type, dyad_id).");
	std::vector<int> sender_by_dyad(D, -1);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = static_cast<int>(riskset(r, 3));
		if (d < 0 || static_cast<arma::uword>(d) >= D) continue;
		sender_by_dyad[static_cast<arma::uword>(d)] = static_cast<int>(riskset(r, 0));
	}
	return sender_by_dyad;
}

static inline arma::mat normalize_inertia_sampled_prop(const arma::mat &stat_in,   // MxS (already inertia)
                                                       const arma::mat &inertia,   // MxS (same as stat_in)
                                                       const arma::mat &riskset,
                                                       const arma::imat &sample_map,
                                                       arma::uword D,
                                                       int N)
{
	const arma::uword M = stat_in.n_rows;
	const arma::uword S = stat_in.n_cols;
	
	arma::mat out = stat_in;
	
	std::vector<int> sender_by_dyad = sender_by_dyad_from_riskset(riskset, D);
	
	std::vector<double> deg_out(static_cast<size_t>(N), 0.0);
	
	for (arma::uword m = 0; m < M; ++m) {
		std::fill(deg_out.begin(), deg_out.end(), 0.0);
		
		// compute sampled outdegree per sender at row m
		for (arma::uword s = 0; s < S; ++s) {
			arma::uword d = sample_map(m, s);
			if (d >= D) Rcpp::stop("normalize_inertia_sampled_prop: sample_map out of bounds.");
			int a1 = sender_by_dyad[d];
			if (a1 < 0 || a1 >= N) Rcpp::stop("normalize_inertia_sampled_prop: invalid sender mapping.");
			deg_out[static_cast<size_t>(a1)] += inertia(m, s);
		}
		
		// divide each sampled dyad by its sender outdegree
		for (arma::uword s = 0; s < S; ++s) {
			arma::uword d = sample_map(m, s);
			int a1 = sender_by_dyad[d];
			double denom = deg_out[static_cast<size_t>(a1)];
			
			if (denom > 0.0 && std::isfinite(denom)) {
				out(m, s) = out(m, s) / denom;
			} else {
				// match original spirit: replace NaN with 1/(N-1)
				out(m, s) = 1.0 / (static_cast<double>(N) - 1.0);
			}
		}
	}
	
	return out;
}

static inline arma::mat standardize_rows_sampled(const arma::mat &x)
{
	arma::mat out = x;
	const arma::uword M = x.n_rows;
	//const arma::uword S = x.n_cols;
	
	for (arma::uword m = 0; m < M; ++m) {
		double mean = arma::mean(x.row(m));
		arma::rowvec centered = x.row(m) - mean;
		double sd = std::sqrt(arma::as_scalar(arma::mean(arma::square(centered))));
		if (sd > 0.0 && std::isfinite(sd)) out.row(m) = centered / sd;
		else out.row(m).zeros();
	}
	return out;
}


inline arma::uword dyad_from_sample(const arma::imat &sample_map, arma::uword m, arma::uword s) {
	return sample_map(m, s); // assumes 0..D-1 already
}

// helper: per-row denominator = weighted number of past events under memory semantics
static inline arma::vec past_events_denom(const arma::mat &edgelist,
                                          const arma::vec &weights,
                                          Rcpp::String memory,
                                          const arma::vec &memory_value,
                                          int start, int stop,
                                          bool display_progress,
                                          Rcpp::String method)
{
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else if (method == "pe") time_points = edgelist.col(0);
	else Rcpp::stop("past_events_denom: method must be 'pt' or 'pe'.");
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	arma::vec den(M, arma::fill::zeros);
	if (M == 0) return den;
	
	arma::uword ev_ptr = 0;
	
	// decay
	double lambda = 0.0;
	double last_t = NAN;
	double state = 0.0;
	
	// window / interval
	std::deque<std::pair<double,double>> q;
	double qsum = 0.0;
	double win_len = 0.0, int_min = 0.0, int_max = 0.0;
	if (memory == "window") win_len = memory_value(0);
	if (memory == "interval") { int_min = memory_value(0); int_max = memory_value(1); }
	
	auto decay_touch = [&](double t_now){
		if (std::isnan(last_t)) { last_t = t_now; return; }
		double dt = t_now - last_t;
		if (dt > 0) state *= std::exp(-lambda * dt);
		last_t = t_now;
	};
	
	auto add_event = [&](arma::uword ev){
		double t = edgelist(ev, 0);
		double w = weights(ev);
		
		if (memory == "full") {
			state += w;
		} else if (memory == "decay") {
			decay_touch(t);
			state += w;
		} else if (memory == "window") {
			q.push_back({t,w});
			qsum += w;
		} else if (memory == "interval") {
			q.push_back({t,w});
		} else {
			Rcpp::stop("past_events_denom: unknown memory.");
		}
	};
	
	auto query = [&](double now)->double{
		if (memory == "full") return state;
		if (memory == "decay") { decay_touch(now); return state; }
		
		if (memory == "window") {
			double cutoff = now - win_len;
			while (!q.empty() && q.front().first < cutoff) { qsum -= q.front().second; q.pop_front(); }
			return qsum;
		}
		
		// interval
		double oldest = now - int_max;
		while (!q.empty() && q.front().first < oldest) q.pop_front();
		double upper = now - int_min;
		double acc = 0.0;
		for (auto &tw : q) { if (tw.first < upper) acc += tw.second; else break; }
		return acc;
	};
	
	// init from events strictly before first timepoint
	double first_t = time_points(0);
	while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < first_t) add_event(ev_ptr++);
	
	if (memory == "decay") lambda = std::log(2.0) / memory_value(0);
	
	Progress p(M, display_progress);
	
	for (arma::uword m = 0; m < M; ++m) {
		double now = time_points(m);
		
		if (method == "pt") {
			while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < now) add_event(ev_ptr++);
		}
		
		den(m) = query(now);
		
		if (method == "pe") {
			arma::uword ev = (arma::uword)start + m;
			if (ev < (arma::uword)edgelist.n_rows) add_event(ev);
		}
		
		p.increment();
	}
	
	return den;
}

// node-degree prop: divide by #past events; if 0 => 1/N
static inline arma::mat normalize_degree_sampled_prop_node(arma::mat x,
                                                           const arma::mat &edgelist,
                                                           const arma::vec &weights,
                                                           Rcpp::String memory,
                                                           const arma::vec &memory_value,
                                                           int start, int stop,
                                                           bool display_progress,
                                                           Rcpp::String method,
                                                           int N)
{
	arma::vec den = past_events_denom(edgelist, weights, memory, memory_value,
                                   start, stop, display_progress, method);
	
	for (arma::uword m = 0; m < x.n_rows; ++m) {
		double d = den(m);
		if (d > 0) x.row(m) /= d;
		else x.row(m).fill(1.0 / static_cast<double>(N));
	}
	x.replace(arma::datum::nan, 1.0 / static_cast<double>(N));
	x.replace(arma::datum::inf, 1.0 / static_cast<double>(N));
	return x;
}

// total-degree prop: divide by 2*#past events; if 0 => 1/N
static inline arma::mat normalize_degree_sampled_prop_total(arma::mat x,
                                                            const arma::mat &edgelist,
                                                            const arma::vec &weights,
                                                            Rcpp::String memory,
                                                            const arma::vec &memory_value,
                                                            int start, int stop,
                                                            bool display_progress,
                                                            Rcpp::String method,
                                                            int N)
{
	arma::vec den = past_events_denom(edgelist, weights, memory, memory_value,
                                   start, stop, display_progress, method);
	
	for (arma::uword m = 0; m < x.n_rows; ++m) {
		double d = 2.0 * den(m);
		if (d > 0) x.row(m) /= d;
		else x.row(m).fill(1.0 / static_cast<double>(N));
	}
	x.replace(arma::datum::nan, 1.0 / static_cast<double>(N));
	x.replace(arma::datum::inf, 1.0 / static_cast<double>(N));
	return x;
}


// [[Rcpp::export]]
arma::mat calculate_degree_actor_sampled(int type,                 // 1..6 as in original
                                         const arma::mat &edgelist,
                                         const arma::vec &weights,
                                         const arma::mat &riskset,  // cols: sender, receiver, type, dyad_id (0-based)
                                         Rcpp::String memory,
                                         const arma::vec &memory_value,
                                         int start, int stop,
                                         bool consider_type,
                                         bool display_progress,
                                         Rcpp::String method,
                                         const arma::imat &sample_map) // MxS dyad_id (0-based)
{
	if (riskset.n_cols < 4) Rcpp::stop("calculate_degree_actor_sampled: riskset must have >=4 cols (sender, receiver, type, dyad_id).");
	if (!(type >= 1 && type <= 6)) Rcpp::stop("calculate_degree_actor_sampled: type must be in 1..6.");
	if (!(method == "pt" || method == "pe")) Rcpp::stop("calculate_degree_actor_sampled: method must be 'pt' or 'pe'.");
	if (!(memory == "full" || memory == "window" || memory == "interval" || memory == "decay"))
		Rcpp::stop("calculate_degree_actor_sampled: memory must be full/window/interval/decay.");
	if ((arma::uword)edgelist.n_rows != (arma::uword)weights.n_elem)
		Rcpp::stop("calculate_degree_actor_sampled: weights length must equal edgelist.n_rows.");
	
	// time points
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else                time_points = edgelist.col(0);
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	// infer N and C
	int max_actor = (int)arma::max(arma::max(riskset.cols(0,1)));
	int N = max_actor + 1;
	int C = 1;
	if (consider_type) {
		int max_type = (int)arma::max(riskset.col(2));
		C = max_type + 1;
	}
	const int K = consider_type ? C : 1;
	
	auto key = [&](int actor, int et) -> size_t {
		return (size_t)actor * (size_t)K + (size_t)(consider_type ? et : 0);
	};
	
	// dyad_id -> (sender, receiver, et)
	int max_dyad = (int)arma::max(riskset.col(3));
	if (max_dyad < 0) Rcpp::stop("calculate_degree_actor_sampled: invalid dyad_id column.");
	const arma::uword D = (arma::uword)(max_dyad + 1);
	
	std::vector<int> a1_by_d(D, -1), a2_by_d(D, -1), et_by_d(D, 0);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = (int)riskset(r, 3);
		if (d < 0 || (arma::uword)d >= D) continue;
		a1_by_d[d] = (int)riskset(r, 0);
		a2_by_d[d] = (int)riskset(r, 1);
		et_by_d[d] = (int)riskset(r, 2);
	}
	
	// state per actor (and type if consider_type)
	const size_t SZ = (size_t)N * (size_t)K;
	std::vector<double> state(SZ, 0.0);
	
	// decay bookkeeping
	std::vector<double> last_t;
	double lambda = 0.0;
	if (memory == "decay") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_degree_actor_sampled: decay requires memory_value length 1 (half-life).");
		double half_life = memory_value(0);
		if (half_life <= 0) Rcpp::stop("calculate_degree_actor_sampled: decay half-life must be > 0.");
		lambda = std::log(2.0) / half_life;
		last_t.assign(SZ, NAN);
	}
	
	auto decay_touch = [&](size_t idx, double t_now) {
		double &lt = last_t[idx];
		if (std::isnan(lt)) { lt = t_now; return; }
		double dt = t_now - lt;
		if (dt > 0) state[idx] *= std::exp(-lambda * dt);
		lt = t_now;
	};
	
	// window / interval queues
	std::vector<std::deque<std::pair<double,double>>> q;
	std::vector<double> qsum;
	double win_len = 0.0, int_min = 0.0, int_max = 0.0;
	
	if (memory == "window") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_degree_actor_sampled: window requires memory_value length 1.");
		win_len = memory_value(0);
		if (win_len < 0) Rcpp::stop("calculate_degree_actor_sampled: window length must be >=0.");
		q.resize(SZ);
		qsum.assign(SZ, 0.0);
	} else if (memory == "interval") {
		if (memory_value.n_elem < 2) Rcpp::stop("calculate_degree_actor_sampled: interval requires memory_value length 2 (min,max).");
		int_min = memory_value(0);
		int_max = memory_value(1);
		if (int_min < 0 || int_max < int_min) Rcpp::stop("calculate_degree_actor_sampled: invalid interval memory_value.");
		q.resize(SZ);
	}
	
	auto add_to_state = [&](int actor, int et, double t, double w) {
		if (actor < 0 || actor >= N) return;
		if (consider_type && (et < 0 || et >= C)) return;
		size_t idx = key(actor, et);
		
		if (memory == "full") state[idx] += w;
		else if (memory == "decay") { decay_touch(idx, t); state[idx] += w; }
		else if (memory == "window") { q[idx].push_back({t,w}); qsum[idx] += w; }
		else { q[idx].push_back({t,w}); } // interval
	};
	
	auto query_state = [&](int actor, int et, double now) -> double {
		if (actor < 0 || actor >= N) return 0.0;
		if (consider_type && (et < 0 || et >= C)) return 0.0;
		size_t idx = key(actor, et);
		
		if (memory == "full") return state[idx];
		if (memory == "decay") { decay_touch(idx, now); return state[idx]; }
		
		if (memory == "window") {
			double cutoff = now - win_len;
			auto &dq = q[idx];
			while (!dq.empty() && dq.front().first < cutoff) {
				qsum[idx] -= dq.front().second;
				dq.pop_front();
			}
			return qsum[idx];
		}
		
		// interval
		auto &dq = q[idx];
		double oldest = now - int_max;
		while (!dq.empty() && dq.front().first < oldest) dq.pop_front();
		double upper = now - int_min;
		double acc = 0.0;
		for (auto &tw : dq) {
			if (tw.first < upper) acc += tw.second;
			else break;
		}
		return acc;
	};
	
	auto update_with_event = [&](arma::uword ev) {
		double t = edgelist(ev, 0);
		int sender = (int)edgelist(ev, 1);
		int receiver = (int)edgelist(ev, 2);
		int et = (edgelist.n_cols > 3) ? (int)edgelist(ev, 3) : 0;
		double w = weights(ev);
		
		// indegree updates
		if (type == 1 || type == 2 || type == 5 || type == 6) {
			add_to_state(receiver, et, t, w);
		}
		// outdegree updates
		if (type == 3 || type == 4 || type == 5 || type == 6) {
			add_to_state(sender, et, t, w);
		}
	};
	
	// init from events before first selected time
	arma::uword ev_ptr = 0;
	double first_t = time_points(0);
	while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < first_t) {
		update_with_event(ev_ptr++);
	}
	
	Progress p(M, display_progress);
	
	for (arma::uword m = 0; m < M; ++m) {
		double now = time_points(m);
		
		if (method == "pt") {
			while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < now) {
				update_with_event(ev_ptr++);
			}
		}
		
		for (arma::uword s = 0; s < S; ++s) {
			int d = (int)sample_map(m,s);
			if (d < 0 || (arma::uword)d >= D) Rcpp::stop("calculate_degree_actor_sampled: sample_map out of bounds.");
			
			int a1 = a1_by_d[(arma::uword)d];
			int a2 = a2_by_d[(arma::uword)d];
			int et = consider_type ? et_by_d[(arma::uword)d] : 0;
			
			int actor = -1;
			if (type == 1 || type == 3 || type == 5) actor = a1; // Sender
			else                                      actor = a2; // Receiver
			
			// For indegreeSender / totaldegreeSender: query indegree/total for sender.
			// For indegreeReceiver / totaldegreeReceiver: query indegree/total for receiver.
			// Here "state" already corresponds to the correct degree family based on type, so we just query it.
			double v = 0.0;
			if (!consider_type) {
				// pool types: sum across types by construction (K=1)
				v = query_state(actor, 0, now);
			} else {
				// consider_type: degree is for that dyad’s type only (matches original semantics)
				v = query_state(actor, et, now);
			}
			out(m,s) = v;
		}
		
		if (method == "pe") {
			arma::uword ev = (arma::uword)start + m;
			if (ev < (arma::uword)edgelist.n_rows) update_with_event(ev);
		}
		
		p.increment();
	}
	
	return out;
}

// [[Rcpp::export]]
arma::mat calculate_degree_dyad_sampled(int type,                 // 1=min, 2=max, 3=diff(abs), 4=sum
                                        const arma::mat &edgelist,
                                        const arma::vec &weights,
                                        const arma::mat &riskset,  // sender, receiver, type, dyad_id (0-based)
                                        Rcpp::String memory,
                                        const arma::vec &memory_value,
                                        int start, int stop,
                                        bool consider_type,
                                        bool display_progress,
                                        Rcpp::String method,
                                        const arma::imat &sample_map)
{
	if (!(type >= 1 && type <= 4)) Rcpp::stop("calculate_degree_dyad_sampled: type must be 1..4.");
	if (riskset.n_cols < 4) Rcpp::stop("calculate_degree_dyad_sampled: riskset must have >=4 cols (sender, receiver, type, dyad_id).");
	
	// totaldegree of sender and receiver (MxS)
	arma::mat td_sender = calculate_degree_actor_sampled(5, edgelist, weights, riskset,
                                                      memory, memory_value,
                                                      start, stop,
                                                      consider_type,
                                                      false, method, sample_map);
	
	arma::mat td_receiver = calculate_degree_actor_sampled(6, edgelist, weights, riskset,
                                                        memory, memory_value,
                                                        start, stop,
                                                        consider_type,
                                                        false, method, sample_map);
	
	arma::mat out;
	if (type == 1)      out = arma::min(td_sender, td_receiver);
	else if (type == 2) out = arma::max(td_sender, td_receiver);
	else if (type == 3) out = arma::abs(td_sender - td_receiver);
	else                out = td_sender + td_receiver;
	
	// optional progress message (cheap)
	if (display_progress) {
		if (type == 1) Rcpp::Rcout << "Calculating degreeMin statistic (sampled)" << std::endl;
		if (type == 2) Rcpp::Rcout << "Calculating degreeMax statistic (sampled)" << std::endl;
		if (type == 3) Rcpp::Rcout << "Calculating degreeDiff statistic (sampled)" << std::endl;
		if (type == 4) Rcpp::Rcout << "Calculating totaldegreeDyad statistic (sampled)" << std::endl;
	}
	
	return out;
}

// [[Rcpp::export]]
arma::mat calculate_triad_sampled(int type,                      // 1..5 as in calculate_triad()
                                  const arma::mat &edgelist,
                                  const arma::vec &weights,
                                  const arma::mat &riskset,      // cols: sender, receiver, type, dyad_id
                                  const arma::mat &risksetMatrix,// N x (N*C), maps (s,r,c)->dyad_id
                                  Rcpp::String memory,
                                  const arma::vec &memory_value,
                                  int start, int stop,
                                  Rcpp::String scaling,          // "none","std","none_unique","std_unique"
                                  bool consider_type,
                                  bool display_progress,
                                  Rcpp::String method,           // "pt" or "pe"
                                  const arma::imat &sample_map)  // M x S dyad_id
{
	if (!(type >= 1 && type <= 5)) Rcpp::stop("calculate_triad_sampled: type must be 1..5.");
	if (riskset.n_cols < 4) Rcpp::stop("calculate_triad_sampled: riskset must have >=4 cols.");
	if (!(method == "pt" || method == "pe")) Rcpp::stop("calculate_triad_sampled: method must be 'pt' or 'pe'.");
	if (!(memory == "full" || memory == "window" || memory == "interval" || memory == "decay"))
		Rcpp::stop("calculate_triad_sampled: memory must be full/window/interval/decay.");
	if ((arma::uword)edgelist.n_rows != (arma::uword)weights.n_elem)
		Rcpp::stop("calculate_triad_sampled: weights length must equal edgelist.n_rows.");
	
	const arma::uword N = risksetMatrix.n_rows;
	const arma::uword C = risksetMatrix.n_cols / N;
	
	// time points
	arma::vec time_points;
	if (method == "pt") time_points = arma::unique(edgelist.col(0));
	else                time_points = edgelist.col(0);
	time_points = time_points.subvec(start, stop);
	
	const arma::uword M = time_points.n_elem;
	const arma::uword S = sample_map.n_cols;
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	// dyad_id -> (sender, receiver, et)
	int max_dyad = (int)arma::max(riskset.col(3));
	const arma::uword D = (arma::uword)(max_dyad + 1);
	std::vector<int> a1_by_d(D, -1), a2_by_d(D, -1), et_by_d(D, 0);
	for (arma::uword r = 0; r < riskset.n_rows; ++r) {
		int d = (int)riskset(r, 3);
		if (d < 0 || (arma::uword)d >= D) continue;
		a1_by_d[d] = (int)riskset(r, 0);
		a2_by_d[d] = (int)riskset(r, 1);
		et_by_d[d] = (int)riskset(r, 2);
	}
	
	// dyad state (sparse queues for window/interval; dense numeric state for full/decay)
	std::vector<double> state(D, 0.0);
	
	// decay
	std::vector<double> last_t;
	double lambda = 0.0;
	if (memory == "decay") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_triad_sampled: decay requires memory_value length 1.");
		double half_life = memory_value(0);
		if (half_life <= 0) Rcpp::stop("calculate_triad_sampled: decay half-life must be >0.");
		lambda = std::log(2.0) / half_life;
		last_t.assign(D, NAN);
	}
	auto decay_touch = [&](arma::uword idx, double t_now) {
		double &lt = last_t[idx];
		if (std::isnan(lt)) { lt = t_now; return; }
		double dt = t_now - lt;
		if (dt > 0) state[idx] *= std::exp(-lambda * dt);
		lt = t_now;
	};
	
	// window/interval sparse queues
	double win_len = 0.0, int_min = 0.0, int_max = 0.0;
	std::unordered_map<int, std::deque<std::pair<double,double>>> q;
	std::unordered_map<int, double> qsum;
	
	if (memory == "window") {
		if (memory_value.n_elem < 1) Rcpp::stop("calculate_triad_sampled: window requires memory_value length 1.");
		win_len = memory_value(0);
		if (win_len < 0) Rcpp::stop("calculate_triad_sampled: window length must be >=0.");
	} else if (memory == "interval") {
		if (memory_value.n_elem < 2) Rcpp::stop("calculate_triad_sampled: interval requires memory_value length 2.");
		int_min = memory_value(0);
		int_max = memory_value(1);
		if (int_min < 0 || int_max < int_min) Rcpp::stop("calculate_triad_sampled: invalid interval memory_value.");
	}
	
	auto add_to_state = [&](int dyad, double t, double w) {
		if (dyad < 0 || (arma::uword)dyad >= D) return;
		arma::uword idx = (arma::uword)dyad;
		if (memory == "full") state[idx] += w;
		else if (memory == "decay") { decay_touch(idx, t); state[idx] += w; }
		else if (memory == "window") { q[dyad].push_back({t,w}); qsum[dyad] += w; }
		else { q[dyad].push_back({t,w}); } // interval
	};
	
	auto query_state = [&](int dyad, double now) -> double {
		if (dyad < 0 || (arma::uword)dyad >= D) return 0.0;
		arma::uword idx = (arma::uword)dyad;
		if (memory == "full") return state[idx];
		if (memory == "decay") { decay_touch(idx, now); return state[idx]; }
		
		auto it = q.find(dyad);
		if (it == q.end()) return 0.0;
		auto &dq = it->second;
		
		if (memory == "window") {
			double cutoff = now - win_len;
			double &s = qsum[dyad];
			while (!dq.empty() && dq.front().first < cutoff) {
				s -= dq.front().second;
				dq.pop_front();
			}
			return s;
		}
		
		// interval
		double oldest = now - int_max;
		while (!dq.empty() && dq.front().first < oldest) dq.pop_front();
		double upper = now - int_min;
		double acc = 0.0;
		for (auto &tw : dq) {
			if (tw.first < upper) acc += tw.second;
			else break;
		}
		return acc;
	};
	
	auto event_dyad_id = [&](int s, int r, int et) -> int {
		if (s < 0 || r < 0 || (arma::uword)s >= N || (arma::uword)r >= N) return -1;
		if (et < 0 || (arma::uword)et >= C) return -1;
		return (int)risksetMatrix((arma::uword)s, (arma::uword)r + N * (arma::uword)et);
	};
	
	auto update_with_event = [&](arma::uword ev) {
		double t = edgelist(ev, 0);
		int s = (int)edgelist(ev, 1);
		int r = (int)edgelist(ev, 2);
		int et = (edgelist.n_cols > 3) ? (int)edgelist(ev, 3) : 0;
		double w = weights(ev);
		int d = event_dyad_id(s, r, et);
		if (d >= 0) add_to_state(d, t, w);
	};
	
	// init from events before first selected time
	arma::uword ev_ptr = 0;
	double first_t = time_points(0);
	while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < first_t) {
		update_with_event(ev_ptr++);
	}
	
	const bool unique_mode = (scaling == "none_unique" || scaling == "std_unique");
	
	Progress p(M, display_progress);
	
	for (arma::uword m = 0; m < M; ++m) {
		double now = time_points(m);
		
		if (method == "pt") {
			while (ev_ptr < (arma::uword)edgelist.n_rows && edgelist(ev_ptr,0) < now) {
				update_with_event(ev_ptr++);
			}
		}
		
		for (arma::uword sidx = 0; sidx < S; ++sidx) {
			int d_this = (int)sample_map(m, sidx);
			if (d_this < 0 || (arma::uword)d_this >= D) Rcpp::stop("calculate_triad_sampled: sample_map out of bounds.");
			
			int s0 = a1_by_d[(arma::uword)d_this];
			int r0 = a2_by_d[(arma::uword)d_this];
			int c0 = et_by_d[(arma::uword)d_this];
			
			double acc = 0.0;
			
			for (arma::uword h = 0; h < N; ++h) {
				if ((int)h == s0 || (int)h == r0) continue;
				
				auto path_value = [&](int ps, int pr, int c_fixed) -> double {
					if (consider_type) {
						int d = (type == 5)
						? event_dyad_id(std::min(ps, pr), std::max(ps, pr), c_fixed)
							: event_dyad_id(ps, pr, c_fixed);
						double v = query_state(d, now);
						return unique_mode ? (v > 0.0 ? 1.0 : 0.0) : v;
					} else {
						double vtot = 0.0;
						for (arma::uword k = 0; k < C; ++k) {
							int d = (type == 5)
							? event_dyad_id(std::min(ps, pr), std::max(ps, pr), (int)k)
								: event_dyad_id(ps, pr, (int)k);
							double v = query_state(d, now);
							vtot += (unique_mode ? (v > 0.0 ? 1.0 : 0.0) : v);
						}
						return vtot;
					}
				};
				
				double v1 = 0.0, v2 = 0.0;
				
				switch (type) {
				case 1: // otp: s->h, h->r
					v1 = path_value(s0, (int)h, c0);
					v2 = path_value((int)h, r0, c0);
					break;
				case 2: // itp: r->h, h->s
					v1 = path_value(r0, (int)h, c0);
					v2 = path_value((int)h, s0, c0);
					break;
				case 3: // osp: s->h, r->h
					v1 = path_value(s0, (int)h, c0);
					v2 = path_value(r0, (int)h, c0);
					break;
				case 4: // isp: h->s, h->r
					v1 = path_value((int)h, s0, c0);
					v2 = path_value((int)h, r0, c0);
					break;
				case 5: // sp (undirected): {s,h}, {r,h}
					v1 = path_value(s0, (int)h, c0);
					v2 = path_value(r0, (int)h, c0);
					break;
				}
				
				acc += std::min(v1, v2);
			}
			
			out(m, sidx) = acc;
		}
		
		if (method == "pe") {
			arma::uword ev = (arma::uword)start + m;
			if (ev < (arma::uword)edgelist.n_rows) update_with_event(ev);
		}
		
		p.increment();
	}
	
	// standardization requested for triads (matches docs)
	if (scaling == "std" || scaling == "std_unique") {
		out = standardize_rows_sampled(out);
	}
	
	return out;
}



static int getEffectNumber(Rcpp::String effect)
{
	std::map<std::string, int> effectsMap;
	
	// Baseline
	effectsMap["baseline"] = 1;
	effectsMap["FEtype"] = 2;
	
	// Exogenous stats
	effectsMap["send"] = 11;
	effectsMap["receive"] = 12;
	effectsMap["tie"] = 13;
	effectsMap["same"] = 14;
	effectsMap["difference"] = 15;
	effectsMap["average"] = 16;
	effectsMap["minimum"] = 17;
	effectsMap["maximum"] = 18;
	effectsMap["event"] = 19;
	
	// Endogenous stats
	effectsMap["inertia"] = 101;
	effectsMap["reciprocity"] = 102;
	
	effectsMap["indegreeSender"] = 111;
	effectsMap["indegreeReceiver"] = 112;
	effectsMap["outdegreeSender"] = 113;
	effectsMap["outdegreeReceiver"] = 114;
	effectsMap["totaldegreeSender"] = 115;
	effectsMap["totaldegreeReceiver"] = 116;
	
	effectsMap["totaldegreeDyad"] = 117;
	effectsMap["degreeMin"] = 118;
	effectsMap["degreeMax"] = 119;
	effectsMap["degreeDiff"] = 120;
	
	effectsMap["otp"] = 131;
	effectsMap["itp"] = 132;
	effectsMap["osp"] = 133;
	effectsMap["isp"] = 134;
	effectsMap["sp"] = 135;
	
	effectsMap["psABBA"] = 141;
	effectsMap["psABBY"] = 142;
	effectsMap["psABXA"] = 143;
	effectsMap["psABXB"] = 144;
	effectsMap["psABXY"] = 145;
	effectsMap["psABAY"] = 146;
	effectsMap["psABAB"] = 147;
	
	effectsMap["rrankSend"] = 151;
	effectsMap["rrankReceive"] = 152;
	
	effectsMap["recencyContinue"] = 161;
	effectsMap["recencySendSender"] = 162;
	effectsMap["recencySendReceiver"] = 163;
	effectsMap["recencyReceiveSender"] = 164;
	effectsMap["recencyReceiveReceiver"] = 165;
	
	effectsMap["userStat"] = 888;
	effectsMap["interact"] = 999;
	
	std::string key = effect.get_cstring();
	auto result = effectsMap.find(key);
	
	if (result != effectsMap.end()) return result->second;
	
	Rcpp::Rcout << "Effect not found in the map." << std::endl;
	return 0;
}


// [[Rcpp::export]]
arma::mat get_userstat_sampled(SEXP covariatesSEXP,
                               const arma::mat &edgelist,
                               int start, int stop,
                               bool display_progress,
                               Rcpp::String method,
                               const arma::imat &sample_map)
{
	if (display_progress) {
		Rcpp::Rcout << "Calculating userstat statistic (sampled)" << std::endl;
	}
	if (!(method == "pt" || method == "pe")) {
		Rcpp::stop("get_userstat_sampled: method must be 'pt' or 'pe'.");
	}
	
	// Build time grid length (like unsampled get_userstat) for dimension checks
	arma::vec event_times;
	if (method == "pt") event_times = arma::unique(edgelist.col(0));
	else                event_times = edgelist.col(0);
	
	const arma::uword T = event_times.n_elem;            // full grid length
	const arma::uword M = (stop - start + 1);            // requested slice length
	const arma::uword S = sample_map.n_cols;
	
	arma::mat out(M, S, arma::fill::zeros);
	if (M == 0 || S == 0) return out;
	
	// covariates must be a numeric matrix; NA placeholders will not be
	if (TYPEOF(covariatesSEXP) != REALSXP && TYPEOF(covariatesSEXP) != INTSXP) {
		Rcpp::stop("get_userstat_sampled: userStat covariate must be a numeric matrix aligned to the time grid.");
	}
	
	arma::mat cov = Rcpp::as<arma::mat>(covariatesSEXP);
	if (cov.n_cols != 1) {
		Rcpp::stop("get_userstat_sampled: expected a single-column numeric matrix for userStat.");
	}
	
	arma::vec stat1;
	
	// Accept either full-length (T x 1) or already-sliced (M x 1)
	if (cov.n_rows == T) {
		if ((arma::uword)stop >= cov.n_rows) {
			Rcpp::stop("get_userstat_sampled: stop index exceeds covariate rows.");
		}
		stat1 = cov.col(0).subvec((arma::uword)start, (arma::uword)stop); // length M
	} else if (cov.n_rows == M) {
		stat1 = cov.col(0); // already sliced
	} else {
		Rcpp::stop("get_userstat_sampled: covariate rows must equal full time grid length or (stop-start+1).");
	}
	
	// Broadcast Mx1 -> MxS
	out.each_col() = stat1;
	return out;
}




//[[Rcpp::export]]
arma::cube compute_stats_tie_sampled(Rcpp::CharacterVector effects,
                             const arma::mat &edgelist,
                             const arma::vec &weights,
                             const arma::mat &riskset,
                             const arma::mat &risksetMatrix,
                             const Rcpp::List &covariates,
                             const Rcpp::List &interactions,
                             Rcpp::String memory,
                             const arma::vec &memory_value,
                             Rcpp::CharacterVector &scaling,
                             Rcpp::LogicalVector &consider_type,
                             int start, int stop,
                             bool directed,
                             bool display_progress,
                             Rcpp::String method,
                             const arma::imat &sample_map)
{
	
	// Time points: Depending on the method, get ...
	arma::vec time_points;
	if (method == "pt")
	{
		// ... the unique time points
		time_points = arma::unique(edgelist.col(0));
	}
	else if (method == "pe")
	{
		// ... all event times
		time_points = edgelist.col(0);
	}
	time_points = time_points.subvec(start, stop);
	
	// Get number of actors (N) and types (C) in the network
	int N = risksetMatrix.n_rows;
	// int C = risksetMatrix.n_cols / N;
	
	// Initialize saving space
	//arma::cube stats(time_points.n_elem, riskset.n_rows, effects.size());
	arma::uword S = sample_map.n_cols;
	arma::cube stats(time_points.n_elem, S, effects.size());
	
	// Loop over effects
	for (int i = 0; i < effects.size(); ++i)
	{
		// Get case number
		Rcpp::String effectName = effects(i);
		int effect = getEffectNumber(effectName);
		
		// Compute effect
		switch (effect)
		{
			
			// baseline
		case 1:
			stats.slice(i).fill(1);
			break;
			
			// FEtype
		case 2:
			stats.slice(i) = calculate_FEtype_sampled(covariates[i], edgelist, riskset,
               start, stop, method, sample_map);
			break;
			
			// send
		case 11:
			stats.slice(i) = calculate_exo_actor_sampled("send", edgelist, riskset,
               covariates[i], start, stop,
               display_progress, method,
               sample_map);
			break;
			
			// receive
		case 12:
			stats.slice(i) = calculate_exo_actor_sampled("receive", edgelist, riskset,
               covariates[i], start, stop,
               display_progress, method,
               sample_map);
			break;
			
			// tie
		case 13:
			// Compute statistic
			//stats.slice(i) = calculate_exo_tie(covariates[i], edgelist, risksetMatrix, start, stop, display_progress, method);
			stats.slice(i) = calculate_exo_tie_sampled(covariates[i], edgelist, risksetMatrix,
               riskset, start, stop,
               display_progress, method, sample_map);
			break;
			
			// same
		case 14:
			// Compute statistic
			//stats.slice(i) = calculate_exo_dyad("same", edgelist, riskset, covariates[i], start, stop, display_progress, method);
			stats.slice(i) = calculate_exo_dyad_sampled("same", edgelist, riskset,
               covariates[i], start, stop,
               display_progress, method, sample_map);
			break;
			
			// difference
		case 15:
			// Compute statistic
			//stats.slice(i) = calculate_exo_dyad("difference", edgelist, riskset, covariates[i], start, stop, display_progress, method);
			stats.slice(i) = calculate_exo_dyad_sampled("difference", edgelist, riskset,
               covariates[i], start, stop,
               display_progress, method, sample_map);
			// Absolute values
			if ((scaling(i) == "none_abs") || (scaling(i) == "std_abs"))
				stats.slice(i) = arma::abs(stats.slice(i));
			break;
			
			// average
		case 16:
			// Compute statistic
			//stats.slice(i) = calculate_exo_dyad("average", edgelist, riskset, covariates[i], start, stop, display_progress, method);
			stats.slice(i) = calculate_exo_dyad_sampled("average", edgelist, riskset,
               covariates[i], start, stop,
               display_progress, method, sample_map);
			break;
			
			// minimum
		case 17:
			// Compute statistic
			//stats.slice(i) = calculate_exo_dyad("minimum", edgelist, riskset, covariates[i], start, stop, display_progress, method);
			stats.slice(i) = calculate_exo_dyad_sampled("minimum", edgelist, riskset,
               covariates[i], start, stop,
               display_progress, method, sample_map);
			break;
			
			// maximum
		case 18:
			// Compute statistic
			//stats.slice(i) = calculate_exo_dyad("maximum", edgelist, riskset, covariates[i], start, stop, display_progress, method);
			stats.slice(i) = calculate_exo_dyad_sampled("maximum", edgelist, riskset,
               covariates[i], start, stop,
               display_progress, method, sample_map);
			break;
			
			// event
		case 19:
			// Compute statistic
			//stats.slice(i) = calculate_exo_event(covariates[i], edgelist, riskset, start, stop, display_progress, method);
			stats.slice(i) = calculate_exo_event_sampled(covariates[i], edgelist,
               start, stop, method, sample_map);
			break;
			
			// inertia
		case 101: {
				arma::mat in = calculate_inertia_sampled(edgelist, weights, risksetMatrix,
                                             memory, memory_value,
                                             start, stop,
                                             display_progress, method,
                                             sample_map);
				
				stats.slice(i) = in; // MxS
				
				if (scaling(i) == "prop") {
					arma::uword D = static_cast<arma::uword>(risksetMatrix.max() + 1);
					stats.slice(i) = normalize_inertia_sampled_prop(stats.slice(i), in, riskset, sample_map, D, N);
				} else if (scaling(i) == "std") {
					stats.slice(i) = standardize_rows_sampled(stats.slice(i));
				}
				break;
			}
			
			// reciprocity
		case 102: {
			arma::mat rec = calculate_reciprocity_sampled(edgelist, weights,
                                                 risksetMatrix, riskset,
                                                 memory, memory_value,
                                                 start, stop,
                                                 consider_type(i),
                                                 display_progress, method,
                                                 sample_map);
			if (scaling(i) == "prop") {
				int N = risksetMatrix.n_rows;
				int C = risksetMatrix.n_cols / N;
				arma::mat denom = indegree_sender_sampled(edgelist, weights, riskset,
                                              memory, memory_value,
                                              start, stop,
                                              consider_type(i),
                                              method, sample_map,
                                              N, C);
				rec /= denom;
				rec.replace(arma::datum::inf, 1.0 / (N - 1.0));
				rec.replace(arma::datum::nan, 1.0 / (N - 1.0));
			} else if (scaling(i) == "std") {
				rec = standardize_rows_sampled(rec);
			}
			
			stats.slice(i) = rec;
			break;
		}
			
			
			// indegreeSender
		case 111: {
			arma::mat deg = calculate_degree_actor_sampled(
				/*type=*/1,
				edgelist, weights, riskset,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				stats.slice(i) = normalize_degree_sampled_prop_node(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			
			// indegreeReceiver
		case 112: {
			arma::mat deg = calculate_degree_actor_sampled(
				/*type=*/2,
				edgelist, weights, riskset,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				// node-degree prop: divide by #past events; if 0 => 1/N
				stats.slice(i) = normalize_degree_sampled_prop_node(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			// outdegreeSender
		case 113: {
			arma::mat deg = calculate_degree_actor_sampled(
				/*type=*/3,
				edgelist, weights, riskset,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				// node-degree prop: divide by #past events; if 0 => 1/N
				stats.slice(i) = normalize_degree_sampled_prop_node(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			// outdegreeReceiver
		case 114: {
			arma::mat deg = calculate_degree_actor_sampled(
				/*type=*/4,
				edgelist, weights, riskset,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				// node-degree prop: divide by #past events; if 0 => 1/N
				stats.slice(i) = normalize_degree_sampled_prop_node(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			// totaldegreeSender
		case 115: {
			arma::mat deg = calculate_degree_actor_sampled(
				/*type=*/5,
				edgelist, weights, riskset,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				// total-degree prop: divide by 2*#past events; if 0 => 1/N
				stats.slice(i) = normalize_degree_sampled_prop_total(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			// totaldegreeReceiver
		case 116: {
			arma::mat deg = calculate_degree_actor_sampled(
				/*type=*/6,
				edgelist, weights, riskset,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				// total-degree prop: divide by 2*#past events; if 0 => 1/N
				stats.slice(i) = normalize_degree_sampled_prop_total(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			
			// totaldegreeDyad
		case 117: {
			arma::mat deg = calculate_degree_dyad_sampled(
				/*type=*/4, // sum(sender_totaldegree, receiver_totaldegree)
				edgelist, weights, risksetMatrix,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				// dyad totals are totals-of-totals => use total-degree prop denominator: 2*#past events
				stats.slice(i) = normalize_degree_sampled_prop_total(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			// degreeMin
		case 118: {
			arma::mat deg = calculate_degree_dyad_sampled(
				/*type=*/1, // min(sender_totaldegree, receiver_totaldegree)
				edgelist, weights, risksetMatrix,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				stats.slice(i) = normalize_degree_sampled_prop_total(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			// degreeMax
		case 119: {
			arma::mat deg = calculate_degree_dyad_sampled(
				/*type=*/2, // max(sender_totaldegree, receiver_totaldegree)
				edgelist, weights, risksetMatrix,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				stats.slice(i) = normalize_degree_sampled_prop_total(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			// degreeDiff
		case 120: {
			arma::mat deg = calculate_degree_dyad_sampled(
				/*type=*/3, // abs(sender_totaldegree - receiver_totaldegree)
				edgelist, weights, risksetMatrix,
				memory, memory_value,
				start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			); // M x S raw counts
			
			stats.slice(i) = deg;
			
			if (scaling(i) == "prop") {
				stats.slice(i) = normalize_degree_sampled_prop_total(
					stats.slice(i),
					edgelist, weights,
					memory, memory_value,
					start, stop,
					display_progress, method,
					N
				);
			} else if (scaling(i) == "std") {
				stats.slice(i) = standardize_rows_sampled(stats.slice(i));
			}
			break;
		}
			
			
			// otp
		case 131: {
			stats.slice(i) = calculate_triad_sampled(
				/*type=*/1,
				edgelist, weights,
				riskset, risksetMatrix,
				memory, memory_value,
				start, stop,
				scaling(i),
				consider_type(i),
				display_progress, method,
				sample_map
			);
			break;
		}
			
			// itp
		case 132: {
			stats.slice(i) = calculate_triad_sampled(
				/*type=*/2,
				edgelist, weights,
				riskset, risksetMatrix,
				memory, memory_value,
				start, stop,
				scaling(i),
				consider_type(i),
				display_progress, method,
				sample_map
			);
			break;
		}
			
			// osp
		case 133: {
			stats.slice(i) = calculate_triad_sampled(
				/*type=*/3,
				edgelist, weights,
				riskset, risksetMatrix,
				memory, memory_value,
				start, stop,
				scaling(i),
				consider_type(i),
				display_progress, method,
				sample_map
			);
			break;
		}
			
			// isp
		case 134: {
			stats.slice(i) = calculate_triad_sampled(
				/*type=*/4,
				edgelist, weights,
				riskset, risksetMatrix,
				memory, memory_value,
				start, stop,
				scaling(i),
				consider_type(i),
				display_progress, method,
				sample_map
			);
			break;
		}
			
			// sp
		case 135: {
			stats.slice(i) = calculate_triad_sampled(
				/*type=*/5,
				edgelist, weights,
				riskset, risksetMatrix,
				memory, memory_value,
				start, stop,
				scaling(i),
				consider_type(i),
				display_progress, method,
				sample_map
			);
			break;
		}
			
			// psABBA
		case 141:
			stats.slice(i) = calculate_pshift_sampled("AB-BA", edgelist, risksetMatrix, riskset,
               start, stop, directed,
               consider_type(i),
               display_progress, method, sample_map);
			break;
			// ... etc for 142..147
			
			// psABBY
		case 142:
			stats.slice(i) = calculate_pshift_sampled("AB-BY", edgelist, risksetMatrix, riskset,
               start, stop, directed,
               consider_type(i), display_progress,
               method, sample_map);
			break;
			// ... etc for 142..147
			
			// psABXA
		case 143:
			// Compute statistic
			stats.slice(i) = calculate_pshift_sampled("AB-XA", edgelist, risksetMatrix, riskset,
               start, stop, directed,
               consider_type(i), display_progress,
               method, sample_map);
			break;
			
			// psABXB
		case 144:
			// Compute statistic
			stats.slice(i) = calculate_pshift_sampled("AB-XB", edgelist, risksetMatrix, riskset,
               start, stop, directed,
               consider_type(i), display_progress,
               method, sample_map);
			break;
			
			// psABXY
		case 145:
			// Compute statistic
			//stats.slice(i) = calculate_pshift("AB-XY", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
			stats.slice(i) = calculate_pshift_sampled("AB-XY", edgelist, risksetMatrix, riskset,
               start, stop, directed,
               consider_type(i),
               display_progress, method, sample_map);
			break;
			
			// psABAY
		case 146:
			// Compute statistic
			//stats.slice(i) = calculate_pshift("AB-AY", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
			stats.slice(i) = calculate_pshift_sampled("AB-AY", edgelist, risksetMatrix, riskset,
               start, stop, directed,
               consider_type(i),
               display_progress, method, sample_map);
			break;
			
			// psABAB
		case 147:
			// Compute statistic
			//stats.slice(i) = calculate_pshift("AB-AB", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
			stats.slice(i) = calculate_pshift_sampled("AB-AB", edgelist, risksetMatrix, riskset,
               start, stop, directed,
               consider_type(i),
               display_progress, method, sample_map);
			break;
			
			// rrankSend
		case 151: {
				stats.slice(i) = calculate_rrank_sampled(
					1,
					edgelist,
					risksetMatrix,
					riskset,
					start, stop,
					consider_type(i),
					display_progress,
					method,
					sample_map
				);
				break;
			}
			
			// rrankReceive
		case 152: {
			stats.slice(i) = calculate_rrank_sampled(
				2,
				edgelist,
				risksetMatrix,
				riskset,
				start, stop,
				consider_type(i),
				display_progress,
				method,
				sample_map
			);
			break;
		}
			
			// recencyContinue
		case 161:
			// Compute statistic
			//stats.slice(i) = calculate_recency("recencyContinue", edgelist, risksetMatrix, start, stop, consider_type(i), display_progress, method);
			stats.slice(i) = calculate_recency_sampled("recencyContinue", edgelist, risksetMatrix,
               riskset, start, stop,
               consider_type(i),
               display_progress, method, sample_map);
			break;
			
			// recencySendSender
		case 162:
			// Compute statistic
			//stats.slice(i) = calculate_recency("recencySendSender", edgelist, risksetMatrix, start, stop, consider_type(i), display_progress, method);
			stats.slice(i) = calculate_recency_sampled("recencySendSender", edgelist, risksetMatrix,
               riskset, start, stop,
               consider_type(i),
               display_progress, method, sample_map);
			break;
			
			// recencySendReceiver
		case 163: {
				stats.slice(i) = calculate_recency_sampled(
					"recencySendReceiver",
					edgelist, risksetMatrix,
					riskset, start, stop,
					consider_type(i),
					display_progress, method,
					sample_map
				);
				break;
			}
			
			// recencyReceiveSender
		case 164: {
			stats.slice(i) = calculate_recency_sampled(
				"recencyReceiveSender",
				edgelist, risksetMatrix,
				riskset, start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			);
			break;
		}
			
			// recencyReceiveReceiver
		case 165: {
			stats.slice(i) = calculate_recency_sampled(
				"recencyReceiveReceiver",
				edgelist, risksetMatrix,
				riskset, start, stop,
				consider_type(i),
				display_progress, method,
				sample_map
			);
			break;
		}
			
			// userStat
		case 888:
			//stats.slice(i) = get_userstat(covariates[i], edgelist, start, stop, display_progress, method);
			stats.slice(i) = get_userstat_sampled(covariates[i], edgelist,
               start, stop, display_progress, method, sample_map);
			break;
			
			// interact
		case 999:
			// Get the indices of the statistics slices (+1) with the
			// statistics for which an interaction needs to be computed.
			arma::vec x = interactions[i];
			int main1 = x(0);
			int main2 = x(1);
			// Element-wise multiplication
			stats.slice(i) = stats.slice(main1 - 1) % stats.slice(main2 - 1);
			break;
		}
		
	}
	
	return stats;
}