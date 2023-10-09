#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include <map>

using namespace Rcpp;

// degree
//
// Function to compute the degree statistics for the actor-oriented model.
//
// type: string, "in" = indegree, "out" = outdegree, "total" = total degree
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
// weights: vector
//
//
arma::mat degree_aom(std::string type,
                     const arma::mat &edgelist,
                     const arma::vec &actors,
                     const arma::vec &weights,
                     std::string memory,
                     arma::vec memory_value,
                     Rcpp::String scaling,
                     int start,
                     int stop,
                     bool display_progress)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Computing " << type << "degree statistic" << std::endl;
  }

  // Initialize saving space and fill with zeros
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);
  arma::mat ideg;
  arma::mat odeg;
  if ((type == "in") | (type == "total"))
  {
    ideg.set_size((stop - start + 1), actors.n_elem);
    ideg.zeros();
  }

  if ((type == "out") | (type == "total"))
  {
    odeg.set_size((stop - start + 1), actors.n_elem);
    odeg.zeros();
  }

  // Set up for memory changes later
  if (memory == "window")
  {
    arma::vec temp_memory_value(2);
    temp_memory_value(0) = 0;
    temp_memory_value(1) = memory_value(0);
    memory_value = temp_memory_value;
    memory = "interval";
  }

  // Full memory
  if (memory == "full")
  {
    // Progress bar
    Progress p(stop, display_progress);

    // (1) Initialize
    // Select the past
    double time = edgelist(start, 0);
    arma::uvec past = arma::find(edgelist.col(0) < time);

    // For loop over the past
    for (arma::uword j = 0; j < past.n_elem; ++j)
    {
      // Event indicator
      int event = past(j);
      // Add event weight to in-degree count
      if ((type == "in") | (type == "total"))
      {
        int receiver = edgelist(event, 2);
        ideg(0, receiver) += weights(event);
      }
      // Add event weight to out-degree count
      if ((type == "out") | (type == "total"))
      {
        int sender = edgelist(event, 1);
        odeg(0, sender) += weights(event);
      }
      p.increment();
    }

    // (2) For loop over timepoints
    for (int i = 1; i < (stop - start + 1); ++i)
    {
      // Indicator of the *previous(!)* event
      int event = start + i - 1;
      if ((type == "in") | (type == "total"))
      {
        // Copy previous row
        ideg.row(i) = ideg.row(i - 1);
        // Receiver of the *previous(!)* event
        int receiver = edgelist(event, 2);
        // Add event weight previous event
        ideg(i, receiver) += weights(event);
      }
      if ((type == "out") | (type == "total"))
      {
        // Copy previous row
        odeg.row(i) = odeg.row(i - 1);
        // Sender of the *previous(!)* event
        int sender = edgelist(event, 1);
        // Add event weight previous event
        odeg(i, sender) += weights(event);
      }
      p.increment();
    }
  }

  // Interval memory
  if (memory == "interval")
  {
    Progress p((stop - start + 1), display_progress);
    // For loop over timepoints
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      // Select the past
      double time_max = edgelist(start + i, 0) - memory_value(0);
      double time_min = edgelist(start + i, 0) - memory_value(1);
      arma::uvec past = arma::find(edgelist.col(0) < time_max &&
                                   edgelist.col(0) >= time_min);

      // For loop over the past
      for (arma::uword j = 0; j < past.n_rows; ++j)
      {
        // Event indicator
        int event = past(j);
        // Add event weight to in-degree count
        if ((type == "in") | (type == "total"))
        {
          int receiver = edgelist(event, 2);
          ideg(i, receiver) += weights(event);
        }
        // Add event weight to out-degree count
        if ((type == "out") | (type == "total"))
        {
          int sender = edgelist(event, 1);
          odeg(i, sender) += weights(event);
        }
      }
      p.increment();
    }
  }

  // Exponential decay memory
  if (memory == "decay")
  {
    Progress p((stop - start + 1), display_progress);
    // For loop over timepoints
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      // Current time
      double time = edgelist(start + i, 0);

      // Past events
      arma::uvec past = arma::find(edgelist.col(0) < time);

      // For loop over the past
      for (arma::uword j = 0; j < past.n_rows; ++j)
      {
        // Event indicator
        int event = past(j);
        // Weight of the event
        double we = weights(event);
        // Time of the event
        double te = edgelist(event, 0);
        // Brandes weight
        double bw = we *
                    exp(-(time - te) * (log(2) / memory_value(0))) *
                    (log(2) / memory_value(0));

        // Add event weight to in-degree count
        if ((type == "in") | (type == "total"))
        {
          int receiver = edgelist(event, 2);
          ideg(i, receiver) += bw;
        }
        // Add event weight to out-degree count
        if ((type == "out") | (type == "total"))
        {
          int sender = edgelist(event, 1);
          odeg(i, sender) += bw;
        }
      }
      p.increment();
    }
  }

  if (type == "in")
  {
    stat = ideg;
  }
  if (type == "out")
  {
    stat = odeg;
  }
  if (type == "total")
  {
    stat = ideg + odeg;
  }

  // Scaling in *choice(!)* model
  // Divide by the number of past events
  if (scaling == "prop")
  {
    for (arma::uword t = 0; t < stat.n_rows; ++t)
    {
      stat.row(t) = stat.row(t) / sum(stat.row(t));
    }
    stat.replace(arma::datum::nan, 0);
    // First row
    if (start == 0)
    {
      arma::rowvec rep = arma::rowvec(stat.n_cols);
      rep.fill(1.0 / actors.n_elem);
      stat.row(0) = rep;
    }
  }
  // Standardize
  if (scaling == "std")
  {
    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      int event = m + start;
      arma::uword sender = edgelist(event, 1);

      arma::rowvec statrow = stat.row(m);
      arma::vec statrowMin = statrow(arma::find(actors != sender));

      // For loop over receivers
      for (arma::uword r = 0; r < actors.n_elem; ++r)
      {
        if (sender == r)
        {
          stat(m, r) = 0;
        }
        else
        {
          stat(m, r) = (stat(m, r) - mean(statrowMin)) /
                       stddev(statrowMin);
        }
      }

      stat.replace(arma::datum::nan, 0);
    }
  }

  return stat;
}

// exo_actor
//
// Computes (or transforms/obtains) the exogenous actor statistic (sender
// effect) for the rate step and (receiver effect) choice step in the
// actor-oriented model.
//
// covariates: matrix: actorID [0:(N-1)], time, value
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
//
//
arma::mat exo_actor_aom(const arma::mat &covariates,
                        const arma::mat &edgelist,
                        const arma::vec &actors,
                        int start,
                        int stop,
                        Rcpp::String scaling)
{
  // Initialize saving space
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);

  // Initialize statistic
  double time = edgelist(start, 0);
  for (arma::uword actor = 0; actor < actors.n_elem; ++actor)
  {
    arma::uvec index = find(covariates.col(0) == actor &&
                            covariates.col(1) <= time);
    arma::mat actorcovar = covariates.rows(index);
    arma::uword max_index = index_max(actorcovar.col(1));
    stat(0, actor) = actorcovar(max_index, 2);
  }

  // Find the unique change timepoints
  arma::vec changetimes = sort(unique(covariates.col(1)));
  changetimes = changetimes(find(changetimes != 0));
  arma::uword counter = 0;

  // Iterate over events
  for (int m = 1; m < (stop - start + 1); ++m)
  {
    // Copy the previous row
    arma::rowvec thisrow = stat.row(m - 1);

    // Update the statistic if required
    // Do not update after the last changetime
    if (counter < changetimes.n_elem)
    {
      // Update if the time of the event is larger than the current
      // changetime
      if (edgelist(m + start, 0) >= changetimes(counter))
      {
        // Update all changes in between
        while ((counter < changetimes.n_elem) &&
               (edgelist(m + start, 0) >= changetimes(counter)))
        {
          // For loop over actors
          for (arma::uword j = 0; j < actors.n_elem; ++j)
          {
            arma::uvec index = find(covariates.col(0) == j &&
                                    covariates.col(1) == changetimes(counter));
            // Update if a new value exists
            if (index.n_elem == 1)
            {
              double value = covariates(index(0), 2);
              thisrow(j) = value;
            }
          }

          // Update the counter
          counter += 1;
        }
      }
    }

    // Save the row
    stat.row(m) = thisrow;
  }

  // Scaling in *choice(!)* model
  if (scaling == "std")
  {
    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      int event = m + start;
      arma::uword sender = edgelist(event, 1);

      arma::rowvec statrow = stat.row(m);
      arma::vec statrowMin = statrow(arma::find(actors != sender));

      // For loop over receivers
      for (arma::uword r = 0; r < actors.n_elem; ++r)
      {
        if (sender == r)
        {
          stat(m, r) = 0;
        }
        else
        {
          stat(m, r) = (stat(m, r) - mean(statrowMin)) /
                       stddev(statrowMin);
        }
      }

      stat.replace(arma::datum::nan, 0);
    }
  }

  // Return
  return stat;
}

// recency
//
// A function for computing the recency statistics, as in  Vu et al. (2017)
// and Mulder and Leenders (2019) for the actor-oriented model.
//
// type: string, "Continue" = recencyContinue, "Send" = recencySend,
//   "Receive" = recencyReceive
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
//
//
arma::mat recency_aom(std::string type,
                      const arma::mat &edgelist,
                      const arma::vec &actors,
                      int start,
                      int stop,
                      bool display_progress)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Computing recency" << type << " statistic" << std::endl;
  }

  // Progress bar
  Progress p(stop, display_progress);

  // Initialize statistic
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);

  if (type == "Continue")
  {
    // Initialize matrix with times the dyads were last active
    arma::mat lastActive(actors.n_elem, actors.n_elem);
    lastActive.fill(arma::datum::inf);

    // Select the past
    double time = edgelist(start, 0);
    arma::uvec past = arma::find(edgelist.col(0) < time);

    // For loop over the past
    for (arma::uword m = 0; m < past.n_elem; ++m)
    {
      // Event indicator
      int event = past(m);
      // Sender and receiver of the event
      int s = edgelist(event, 1);
      int r = edgelist(event, 2);
      // Event time
      double time = edgelist(event, 0);
      // Last time dyad was active
      lastActive(s, r) = time;

      p.increment();
    }

    // For loop over time points
    for (int m = 0; m < (stop - start + 1); ++m)
    {

      // Event indicator
      int event = start + m;
      // Sender and receiver of the event
      arma::uword s = edgelist(event, 1);
      arma::uword r = edgelist(event, 2);

      // Event time
      double time = edgelist(event, 0);

      // Compute the statistic
      arma::rowvec fr = 1 / ((time - lastActive.row(s)) + 1);
      stat.row(m) = fr;

      // Last time dyad was active
      lastActive(s, r) = time;
    }

    p.increment();
  }
  else
  {
    // Initialize vector with times the actors/dyads were last active
    arma::rowvec lastActive(actors.n_elem);
    lastActive.fill(arma::datum::inf);

    // Select the past
    double time = edgelist(start, 0);
    arma::uvec past = arma::find(edgelist.col(0) < time);

    // For loop over the past
    for (arma::uword m = 0; m < past.n_elem; ++m)
    {
      // Event indicator
      int event = past(m);
      // Sender and receiver of the event
      int s = edgelist(event, 1);
      int r = edgelist(event, 2);

      // Event time
      double time = edgelist(event, 0);

      // Find respective dyads
      if (type == "Send")
      {
        // Last time the actor was active as sender
        lastActive(s) = time;
      }
      if (type == "Receive")
      {
        // Last time the actor was active as sender
        lastActive(r) = time;
      }

      p.increment();
    }

    // For loop over time points
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      // Event indicator
      int event = start + m;
      // Sender and receiver of the event
      arma::uword s = edgelist(event, 1);
      arma::uword r = edgelist(event, 2);

      // Event time
      double time = edgelist(event, 0);

      // Compute the statistic
      arma::rowvec fr = 1 / ((time - lastActive) + 1);
      stat.row(m) = fr;

      // Update last active
      if (type == "Send")
      {
        // Last time the actor was active as sender
        lastActive(s) = time;
      }
      if (type == "Receive")
      {
        // Last time the actor was active as sender
        lastActive(r) = time;
      }

      p.increment();
    }
  }

  return stat;
}

// exo_dyad_aom
//
// Function to compute the dyadic exogenous statistics 'same', 'difference' and
// 'average' in the 'Choice'-step of the actor-oriented model.
//
// type: string (same, difference, or average)
// covariates: matrix: actorID [0:(N-1)], time, value
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
//
//
arma::mat exo_dyad_aom(std::string type,
                       const arma::mat &covariates,
                       const arma::mat &edgelist,
                       const arma::vec &actors,
                       int start,
                       int stop,
                       Rcpp::String scaling)
{

  // Initialize saving space
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);

  // Iterate over events
  for (int m = 0; m < (stop - start + 1); ++m)
  {
    // Event indicator
    int event = m + start;
    // Sender of the event
    int sender = edgelist(event, 1);

    // The sender's current exogenous value
    double time = edgelist(event, 0);

    arma::uvec indexSender = find(covariates.col(0) == sender &&
                                  covariates.col(1) <= time);
    arma::mat senderCovar = covariates.rows(indexSender);
    arma::uword senderMax = index_max(senderCovar.col(1));
    double senderValue = senderCovar(senderMax, 2);

    // For loop over receivers
    for (arma::uword r = 0; r < actors.n_elem; ++r)
    {
      // The receiver's current exogenous value
      arma::uvec indexReceiver = find(covariates.col(0) == r &&
                                      covariates.col(1) <= time);
      arma::mat receiverCovar = covariates.rows(indexReceiver);
      arma::uword receiverMax = index_max(receiverCovar.col(1));
      double receiverValue = receiverCovar(receiverMax, 2);

      // 1: Same
      if (type == "same")
      {
        int same = {senderValue == receiverValue};
        stat(m, r) = same;
      }
      // 2: Difference
      if (type == "difference")
      {
        stat(m, r) = {senderValue - receiverValue};
      }

      arma::vec both = {senderValue, receiverValue};
      // 3: Average
      if (type == "average")
      {
        stat(m, r) = mean(both);
      }
      // 4: Minimum
      if (type == "minimum")
      {
        stat(m, r) = min(both);
      }
      // 5: Maximum
      if (type == "maximum")
      {
        stat(m, r) = max(both);
      }
    }
  }

  if (type == "difference")
  {
    // Absolute scaling
    if ((scaling == "none_abs") || (scaling == "std_abs"))
    {
      stat = abs(stat);
    }
  }

  // Standardization
  if ((scaling == "std") || (scaling == "std_abs"))
  {
    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      int event = m + start;
      arma::uword sender = edgelist(event, 1);

      arma::rowvec statrow = stat.row(m);
      arma::vec statrowMin = statrow(arma::find(actors != sender));

      // For loop over receivers
      for (arma::uword r = 0; r < actors.n_elem; ++r)
      {
        if (sender == r)
        {
          stat(m, r) = 0;
        }
        else
        {
          stat(m, r) = (stat(m, r) - mean(statrowMin)) /
                       stddev(statrowMin);
        }
      }

      stat.replace(arma::datum::nan, 0);
    }
  }

  return stat;
}

/**
    Calculates tie statistics

    @param covariates Matrix containing covariate information (actor1, actor2, time, value).
    @param edgelist Matrix representing edges between actors at specific time points (time, actor1, actor2, weight).
    @param actors Vector with actors.
    @param start Starting index for the tie statistic calculation.
    @param stop Ending index for the tie statistic calculation.
    @return Matrix containing tie statistics for the specified range of indices.
*/
// [[Rcpp::export]]
arma::mat calculate_aom_tie_statistic(
    const arma::mat &covariates,
    const arma::mat &edgelist,
    const arma::vec &actors,
    int start, int stop,
    Rcpp::String scaling,
    bool display_progress)
{
  if (display_progress)
  {
    Rcpp::Rcout << "Computing tie statistic" << std::endl;
  }

  // Initialise statistic
  arma::mat stat(stop - start + 1, actors.n_elem);

  // Unique time points in the covariates
  arma::vec time_points = covariates.col(2);
  time_points = unique(time_points);
  time_points = sort(time_points);

  // Initialise objects
  double time, value;
  arma::uvec time_indices, edgelist_indices, sender_time_indices;
  arma::uvec receiver_index;
  int sender, receiver;
  arma::mat time_covariates, sender_time_covariates;

  // Progress bar
  Progress p(time_points.n_elem * actors.n_elem, display_progress);

  // Iterate over unique time_points
  for (arma::uword t = 0; t < time_points.n_elem; ++t)
  {
    time = time_points(t);
    time_indices = arma::find(covariates.col(2) == time);
    time_covariates = covariates.rows(time_indices);

    // Iterate over senders
    for (arma::uword s = 0; s < actors.n_elem; ++s)
    {
      sender = actors(s);
      sender_time_indices = arma::find(time_covariates.col(0) == sender);
      sender_time_covariates = time_covariates.rows(sender_time_indices);

      edgelist_indices = arma::find(edgelist.col(0) >= time &&
                                    edgelist.col(1) == sender);
      
      edgelist_indices = edgelist_indices(arma::find(edgelist_indices >= start && edgelist_indices <= stop));
      edgelist_indices = edgelist_indices - start;

      // Iterate over receivers
      for (arma::uword r = 0; r < actors.n_elem; ++r)
      {
        receiver = actors(r);
        receiver_index = arma::find(sender_time_covariates.col(1) == receiver);
        if (receiver_index.n_elem == 1)
        {
          value = sender_time_covariates(receiver_index(0), 3);
          if (receiver_index(0) >= 0)
          {
            // Fill the submatrix for the specific receiver and time indices 
            // with the 'value'
            arma::uvec receiver_indices(1);
            receiver_indices(0) = static_cast<arma::uword>(receiver);
            stat(edgelist_indices, receiver_indices).fill(value);
          }
        }
      }

      p.increment();
    }
  }

  // Standardize
  if (scaling == "std")
  {
    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      int event = m + start;
      arma::uword sender = edgelist(event, 1);
      arma::rowvec statrow = stat.row(m);
      arma::vec statrowMin = statrow(arma::find(actors != sender));
      // For loop over receivers
      for (arma::uword r = 0; r < actors.n_elem; r++)
      {
        if (sender == r)
        {
          stat(m, r) = 0;
        }
        else
        {
          stat(m, r) = (stat(m, r) - mean(statrowMin)) /
                       stddev(statrowMin);
        }
      }
    }
    stat.replace(arma::datum::nan, 0);
  }

  return stat;
}

// inertia
//
// Computes the statistic for an inertia effect in the actor-oriented model.
//
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
// weights: vector
//
//
arma::mat inertia_aom(const arma::mat &edgelist,
                      const arma::vec &actors,
                      const arma::vec &weights,
                      std::string memory,
                      arma::vec memory_value,
                      Rcpp::String scaling,
                      int start,
                      int stop,
                      bool display_progress)
{

  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Computing inertia statistic" << std::endl;
  }

  // Initialize saving space and fill with zeros
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);

  // Prepare for self-events in the future
  bool self_events = false;

  // Set up for memory changes later
  if (memory == "window")
  {
    arma::vec temp_memory_value(2);
    temp_memory_value(0) = 0;
    temp_memory_value(1) = memory_value(0);
    memory_value = temp_memory_value;
    memory = "interval";
  }

  // Full memory
  if (memory == "full")
  {
    // Initialize saving space
    arma::mat inertia(actors.n_elem, actors.n_elem, arma::fill::zeros);

    // Progress bar
    Progress p(stop, display_progress);

    // Select the past
    double time = edgelist(start, 0);
    arma::uvec past = arma::find(edgelist.col(0) < time);

    // Iterate over the past
    for (arma::uword i = 0; i < past.n_elem; i++)
    {
      int event = past(i);
      int sender = edgelist(event, 1);
      int receiver = edgelist(event, 2);
      inertia(sender, receiver) += weights(event); // Add event weight

      p.increment();
    }

    // Iterate over time points
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      // Statistic at this time
      int event = start + i;
      int sender = edgelist(event, 1);
      stat.row(i) = inertia.row(sender);

      // Update inertia with event
      int receiver = edgelist(event, 2);
      inertia(sender, receiver) += weights(event);

      p.increment();
    }
  }
  else
  {
    // Progress bar
    Progress p((stop - start + 1), display_progress);

    // Iterate over time points
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      // Sender and time of the event
      int event = start + i;
      int sender = edgelist(event, 1);
      double time = edgelist(event, 0);

      // Select the past
      arma::uvec past;
      if (memory == "decay")
      {
        past = arma::find(edgelist.col(0) < time &&
                          edgelist.col(1) == sender);
      }
      else if (memory == "interval")
      {
        double time_max = time - memory_value(0);
        double time_min = time - memory_value(1);
        past = arma::find(edgelist.col(0) < time_max &&
                          edgelist.col(0) >= time_min &&
                          edgelist.col(1) == sender);
      }

      // Initialize saving space
      arma::rowvec inertia(actors.n_elem, arma::fill::zeros);

      // Iterate over the past
      for (arma::uword j = 0; j < past.n_elem; j++)
      {
        int receiver = edgelist(past(j), 2);
        if (memory == "decay")
        {
          // Compute the event weight
          double we = weights(past(j));
          double te = edgelist(past(j), 0);
          double bw = we *
                      exp(-(time - te) * (log(2) / memory_value(0))) *
                      (log(2) / memory_value(0));
          // Add the computed event weight
          inertia(receiver) += bw;
        }
        else
        {
          // Simply add the event weight
          inertia(receiver) += weights(past(j));
        }
      }
      stat.row(i) = inertia;
      p.increment();
    }
  }

  // Scale by the outdegree of the sender >> the fraction of messages i sent
  // to j >> if i hasn't sent any messages yet, than all n-1 actors are
  // equally likely to get a message
  if (scaling == "prop")
  {
    arma::mat deg = degree_aom("out", edgelist, actors, weights, memory, memory_value, scaling = 1, start, stop, false);

    // Iterate over the sequence
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      int event = m + start;
      arma::uword sender = edgelist(event, 1);
      stat.row(m) = stat.row(m) / deg(m, sender);
      double rep = 1.0 / (actors.n_elem - 1.0);
      stat.replace(arma::datum::nan, rep);
      if (!self_events)
      {
        stat(m, sender) = 0;
      }
    }
  }
  // Standardize
  if (scaling == "std")
  {
    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      if (self_events)
      {
        stat.row(m) = stat.row(m) - mean(stat.row(m)) / stddev(stat.row(m));
      }
      else if (!self_events)
      {
        int event = m + start;
        arma::uword sender = edgelist(event, 1);
        arma::rowvec statrow = stat.row(m);
        arma::vec statrowMin = statrow(arma::find(actors != sender));
        // For loop over receivers
        for (arma::uword r = 0; r < actors.n_elem; r++)
        {
          if (sender == r)
          {
            stat(m, r) = 0;
          }
          else
          {
            stat(m, r) = (stat(m, r) - mean(statrowMin)) /
                         stddev(statrowMin);
          }
        }
      }
    }
    stat.replace(arma::datum::nan, 0);
  }

  return stat;
}

// reciprocity
//
// Computes the statistic for an reciprocity effect in the actor-oriented model.
//
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
// weights: vector
//
//
arma::mat reciprocity_aom(const arma::mat &edgelist,
                          const arma::vec &actors,
                          const arma::vec &weights,
                          std::string memory,
                          arma::vec memory_value,
                          Rcpp::String scaling,
                          int start,
                          int stop,
                          bool display_progress)
{

  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Computing reciprocity statistic" << std::endl;
  }

  // Initialize saving space and fill with zeros
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);

  // Prepare for self-events in the future
  bool self_events = false;

  // Set up for memory changes later
  if (memory == "window")
  {
    arma::vec temp_memory_value(2);
    temp_memory_value(0) = 0;
    temp_memory_value(1) = memory_value(0);
    memory_value = temp_memory_value;
    memory = "interval";
  }

  // Full memory
  if (memory == "full")
  {
    // Initialize saving space
    arma::mat recip(actors.n_elem, actors.n_elem, arma::fill::zeros);

    // Progress bar
    Progress p(stop, display_progress);

    // Select the past
    double time = edgelist(start, 0);
    arma::uvec past = arma::find(edgelist.col(0) < time);

    // Iterate over the past
    for (arma::uword i = 0; i < past.n_elem; i++)
    {
      int event = past(i);
      int sender = edgelist(event, 1);
      int receiver = edgelist(event, 2);
      recip(receiver, sender) += weights(event); // Add event weight

      p.increment();
    }

    // Iterate over time points
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      // Statistic at this time
      int event = start + i;
      int sender = edgelist(event, 1);
      stat.row(i) = recip.row(sender);

      // Update reciprocity with event
      int receiver = edgelist(event, 2);
      recip(receiver, sender) += weights(event);

      p.increment();
    }
  }
  else
  {
    // Progress bar
    Progress p((stop - start + 1), display_progress);

    // Iterate over time points
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      // Sender and time of the event
      int event = start + i;
      int sender = edgelist(event, 1);
      double time = edgelist(event, 0);

      // Select the past
      arma::uvec past;
      if (memory == "decay")
      {
        past = arma::find(edgelist.col(0) < time &&
                          edgelist.col(2) == sender);
      }
      else if (memory == "interval")
      {
        double time_max = time - memory_value(0);
        double time_min = time - memory_value(1);
        past = arma::find(edgelist.col(0) < time_max &&
                          edgelist.col(0) >= time_min &&
                          edgelist.col(2) == sender);
      }

      // Initialize saving space
      arma::rowvec recip(actors.n_elem, arma::fill::zeros);

      // Iterate over the past
      for (arma::uword j = 0; j < past.n_elem; j++)
      {
        int receiver = edgelist(past(j), 1);
        if (memory == "decay")
        {
          // Compute the event weight
          double we = weights(past(j));
          double te = edgelist(past(j), 0);
          double bw = we *
                      exp(-(time - te) * (log(2) / memory_value(0))) *
                      (log(2) / memory_value(0));
          // Add the computed event weight
          recip(receiver) += bw;
        }
        else
        {
          // Simply add the event weight
          recip(receiver) += weights(past(j));
        }
      }
      stat.row(i) = recip;
      p.increment();
    }
  }

  // Scale by the indegree of the sender >> the fraction of messages j sent to
  // i >> if i hasn't received any messages yet, than all n-1 actors are
  // equally likely to get a message
  if (scaling == "prop")
  {
    arma::mat deg = degree_aom("in", edgelist, actors, weights, memory, memory_value, scaling = "none", start, stop, false);

    // Iterate over the sequence
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      int event = m + start;
      arma::uword sender = edgelist(event, 1);
      stat.row(m) = stat.row(m) / deg(m, sender);
      double rep = 1.0 / (actors.n_elem - 1.0);
      stat.replace(arma::datum::nan, rep);
      if (!self_events)
      {
        stat(m, sender) = 0;
      }
    }
  }
  // Standardize
  if (scaling == "std")
  {
    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
      if (self_events)
      {
        stat.row(m) = stat.row(m) - mean(stat.row(m)) / stddev(stat.row(m));
      }
      else if (!self_events)
      {
        int event = m + start;
        arma::uword sender = edgelist(event, 1);
        arma::rowvec statrow = stat.row(m);
        arma::vec statrowMin = statrow(arma::find(actors != sender));
        // For loop over receivers
        for (arma::uword r = 0; r < actors.n_elem; r++)
        {
          if (sender == r)
          {
            stat(m, r) = 0;
          }
          else
          {
            stat(m, r) = (stat(m, r) - mean(statrowMin)) /
                         stddev(statrowMin);
          }
        }
      }
    }
    stat.replace(arma::datum::nan, 0);
  }

  return stat;
}

// rrank
//
// Computes statistic for a recency-rank effect (rrankSend, rrankReceive) in
// the tie-oriented model.
//
// type: string (Send or Receive)
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
//
//
arma::mat rrank_aom(std::string type,
                    const arma::mat &edgelist,
                    const arma::vec &actors,
                    int start,
                    int stop,
                    bool display_progress)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Computing rrank" << type << " statistic" << std::endl;
  }

  // Progress bar
  Progress p(stop, display_progress);

  // Initialize saving space
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);
  arma::mat ranks(actors.n_elem, actors.n_elem, arma::fill::zeros);

  // Determine the ranks at the first timepoint
  double time = edgelist(start, 0);
  arma::uvec past = arma::find(edgelist.col(0) < time);

  for (arma::uword j = 0; j < past.n_elem; ++j)
  {
    // Sender and receiver of the event
    int event = past(j);
    int sender = edgelist(event, 1);
    int receiver = edgelist(event, 2);

    // rrankSend
    if (type == "Send")
    {
      // To whom the sender has most recently send events
      int rank = ranks(sender, receiver);
      if (rank == 1)
      {
        // If the current actor is the most recent actor:
        // nothing changes
        continue;
      }
      else
      {
        // Find all elements that should be changed
        arma::uvec change = {0};
        if (rank == 0)
        {
          // All non-zero elements
          change = find(ranks.row(sender) > 0);
        }
        else
        {
          // All non-zero elements that are smaller than the
          // rank of the current actor
          change = find(ranks.row(sender) > 0 &&
                        ranks.row(sender) < rank);
        }
        // Add one to all elements that should be changed
        arma::rowvec rowranks = ranks.row(sender);
        rowranks(change) += 1;
        // Set the rank of the current actor to one
        rowranks(receiver) = 1;
        // Update ranks
        ranks.row(sender) = rowranks;
      }
    }

    // rrankReceive
    if (type == "Receive")
    {
      // From whom the sender has most recently received events
      int rank = ranks(receiver, sender);
      if (rank == 1)
      {
        // If the current actor is the most recent actor:
        // nothing changes
        continue;
      }
      else
      {
        // Find all elements that should be changed
        arma::uvec change = {0};
        if (rank == 0)
        {
          // All non-zero elements
          change = find(ranks.row(receiver) > 0);
        }
        else
        {
          // All non-zero elements that are smaller than the
          // rank of the current actor
          change = find(ranks.row(receiver) > 0 &&
                        ranks.row(receiver) < rank);
        }
        // Add one to all elements that should be changed
        arma::rowvec rowranks = ranks.row(receiver);
        rowranks(change) += 1;
        // Set the rank of the current actor to one
        rowranks(sender) = 1;
        // Update ranks
        ranks.row(receiver) = rowranks;
      }
    }

    p.increment();
  }

  // Iterate over time points
  for (int m = 0; m < (stop - start + 1); ++m)
  {
    // Compute the statistic based on the current ranks
    int event = start + m;
    int s = edgelist(event, 1);
    for (arma::uword j = 0; j < actors.n_elem; ++j)
    {
      stat(m, j) = 1 / ranks(s, j);
      stat.replace(arma::datum::inf, 0);
    }

    // Update the ranks
    // Sender, receiver and type of the event
    int r = edgelist(event, 2);

    if (type == "Send")
    {
      // To whom the sender has most recently send events
      int rank = ranks(s, r);
      if (rank == 1)
      {
        // If the current actor is the most recent actor: nothing
        // changes
        continue;
      }
      else
      {
        // Find all elements that should be changed
        arma::uvec change = {0};
        if (rank == 0)
        {
          // All non-zero elements
          change = find(ranks.row(s) > 0);
        }
        else
        {
          // All non-zero elements that are smaller than the rank
          // of the current actor
          change = find(ranks.row(s) > 0 &&
                        ranks.row(s) < rank);
        }
        // Add one to all elements that should be changed
        arma::rowvec rowranks = ranks.row(s);
        rowranks(change) += 1;
        // Set the rank of the current actor to one
        rowranks(r) = 1;
        // Update ranks
        ranks.row(s) = rowranks;
      }
    }
    if (type == "Receive")
    {
      // From whom the sender has most recently received events
      int rank = ranks(r, s);
      if (rank == 1)
      {
        // If the current actor is the most recent actor: nothing
        // changes
        continue;
      }
      else
      {
        // Find all elements that should be changed
        arma::uvec change = {0};
        if (rank == 0)
        {
          // All non-zero elements
          change = find(ranks.row(r) > 0);
        }
        else
        {
          // All non-zero elements that are smaller than the rank
          // of the current actor
          change = find(ranks.row(r) > 0 &&
                        ranks.row(r) < rank);
        }
        // Add one to all elements that should be changed
        arma::rowvec rowranks = ranks.row(r);
        rowranks(change) += 1;
        // Set the rank of the current actor to one
        rowranks(s) = 1;
        // Update ranks
        ranks.row(r) = rowranks;
      }
    }
  }

  p.increment();

  return stat;
}

// triad
//
// Computes the triad statistics for the choice step in the actor-oriented
// model.
//
// type: string (otp, itp, osp, isp)
// edgelist: matrix: time, actorID_1 [0:(N-1)], actorID_2 [0:(N-1)]
// actors: vector, actor ids [0:(N-1)]
// weights: vector
//
//
arma::mat triad_aom(std::string type,
                    const arma::mat &edgelist,
                    const arma::vec &actors,
                    const arma::vec &weights,
                    std::string memory,
                    arma::vec memory_value,
                    Rcpp::String scaling,
                    int start,
                    int stop,
                    bool display_progress)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Computing " << type << " statistic" << std::endl;
  }

  // Initialize saving space and fill with zeros
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);

  // Prepare for self-events in the future
  bool self_events = false;

  // Set up for memory changes later
  if (memory == "window")
  {
    arma::vec temp_memory_value(2);
    temp_memory_value(0) = 0;
    temp_memory_value(1) = memory_value(0);
    memory_value = temp_memory_value;
    memory = "interval";
  }

  // Full memory
  if (memory == "full")
  {
    // Initialize saving space
    arma::mat adjmat(actors.n_elem, actors.n_elem, arma::fill::zeros);

    // Progress bar
    Progress p(stop, display_progress);

    // Select the past
    double time = edgelist(start, 0);
    arma::uvec past = arma::find(edgelist.col(0) < time);

    // Iterate over the past
    for (arma::uword i = 0; i < past.n_elem; i++)
    {
      int event = past(i);
      int sender = edgelist(event, 1);
      int receiver = edgelist(event, 2);
      adjmat(sender, receiver) += weights(event); // Add event weight

      p.increment();
    }

    // Convert to adjmat if greater than 0
    if ((scaling == "none_unique") || (scaling == "std_unique"))
    {
      // Convert elements in selectedCols1 to 1 if greater than 0
      adjmat = arma::conv_to<arma::mat>::from(adjmat > 0);
    }

    // Iterate over time points
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      int event = start + i;
      int sender = edgelist(event, 1);

      // Initialize objects
      arma::vec count1, count2;
      arma::mat counts;
      arma::vec min_counts;

      // For loop over receivers
      for (arma::uword r = 0; r < actors.n_elem; r++)
      {
        // otp
        if (type == "otp")
        {
          count1 = adjmat.row(sender).t();
          count2 = adjmat.col(r);
        }

        // itp
        if (type == "itp")
        {
          count1 = adjmat.col(sender);
          count2 = adjmat.row(r).t();
        }

        // osp
        if (type == "osp")
        {
          count1 = adjmat.row(sender).t();
          count2 = adjmat.row(r).t();
        }

        // isp
        if (type == "isp")
        {
          count1 = adjmat.col(sender);
          count2 = adjmat.col(r);
        }

        counts = arma::join_rows(count1, count2);
        min_counts = min(counts, 1);
        stat(i, r) = sum(min_counts);
      }

      // Correct for the absence of self-events
      if (!self_events)
      {
        stat(i, sender) = 0;
      }

      // Update adjmat with event
      int receiver = edgelist(event, 2);
      adjmat(sender, receiver) += weights(event);

      // Convert to adjmat if greater than 0
      if ((scaling == "none_unique") || (scaling == "std_unique"))
      {
        // Convert elements in selectedCols1 to 1 if greater than 0
        adjmat = arma::conv_to<arma::mat>::from(adjmat > 0);
      }

      p.increment();
    }
  }
  else
  {
    // Progress bar
    Progress p((stop - start + 1), display_progress);

    // Iterate over time points
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      // Initialize saving space
      arma::mat adjmat(actors.n_elem, actors.n_elem, arma::fill::zeros);

      // Event info
      int event = start + i;
      int sender = edgelist(event, 1);
      double time = edgelist(event, 0);

      // Select the past with the current actor
      arma::uvec past;
      if (memory == "decay")
      {
        past = arma::find(edgelist.col(0) < time);
      }
      else if (memory == "interval")
      {
        double time_max = time - memory_value(0);
        double time_min = time - memory_value(1);
        past = arma::find(edgelist.col(0) < time_max &&
                          edgelist.col(0) >= time_min);
      }

      // Iterate over the past
      for (arma::uword j = 0; j < past.n_elem; j++)
      {
        int s = edgelist(past(j), 1);
        int r = edgelist(past(j), 2);
        double event_weight = weights(j);
        if (memory == "decay")
        {
          double time_event = edgelist(past(j), 0);
          event_weight = event_weight *
                         exp(-(time - time_event) * (log(2) / memory_value(0))) *
                         (log(2) / memory_value(0));
        }
        adjmat(s, r) += event_weight;
      }

      // Initialize objects
      arma::vec count1, count2;
      arma::mat counts;
      arma::vec min_counts;

      // Convert to adjmat if greater than 0
      if ((scaling == "none_unique") || (scaling == "std_unique"))
      {
        // Convert elements in selectedCols1 to 1 if greater than 0
        adjmat = arma::conv_to<arma::mat>::from(adjmat > 0);
      }

      // For loop over receivers
      for (arma::uword r = 0; r < actors.n_elem; r++)
      {
        // otp
        if (type == "otp")
        {
          count1 = adjmat.row(sender).t();
          count2 = adjmat.col(r);
        }

        // itp
        if (type == "itp")
        {
          count1 = adjmat.col(sender);
          count2 = adjmat.row(r).t();
        }

        // osp
        if (type == "osp")
        {
          count1 = adjmat.row(sender).t();
          count2 = adjmat.row(r).t();
        }

        // isp
        if (type == "isp")
        {
          count1 = adjmat.col(sender);
          count2 = adjmat.col(r);
        }

        counts = arma::join_rows(count1, count2);
        min_counts = min(counts, 1);
        stat(i, r) = sum(min_counts);
      }

      // Correct for the absence of self-events
      if (!self_events)
      {
        stat(i, sender) = 0;
      }

      p.increment();
    }
  }

  // Scaling
  if ((scaling == "std") || (scaling == "std_unique"))
  {
    // Iterate over time points
    for (int i = 0; i < (stop - start + 1); ++i)
    {
      if (self_events)
      {
        stat.row(i) = stat.row(i) - mean(stat.row(i)) / stddev(stat.row(i));
      }
      else if (!self_events)
      {
        int event = start + i;
        arma::uword sender = edgelist(event, 1);
        arma::rowvec statrow = stat.row(i);
        arma::vec statrowMin = statrow(arma::find(actors != sender));

        // For loop over receivers
        for (arma::uword r = 0; r < actors.n_elem; ++r)
        {
          if (sender == r)
          {
            stat(i, r) = 0;
          }
          else
          {
            stat(i, r) = (stat(i, r) - mean(statrowMin)) /
                         stddev(statrowMin);
          }
        }

        stat.replace(arma::datum::nan, 0);
      }
    }
  }

  return stat;
}

// standardize
//
// Helper function that performs scales the statistic through standardization
// per time point.
//
// Per time point, the mean Mt and standard deviation SDt of the statistic Xt
// is computed. The statistics is scaled by subtracting the mean of the values
// and divide by the standard deviation: Xt = (Xt - Mt)/SDt.
//
// *param [stat] statistic matrix. The rows in this matrix always refer to the
// timepoints. In the case of the tie-oriented model, the columns refer to the
// possible relational events in the risk set. In the case of the
// actor-oriented model, the columns refer to all possible senders in the rate
// model and to all possible receivers in the choice model.
//
// *return [stat] statistic matrix standardized per time point, i.e., per row.
arma::mat scale(arma::mat stat)
{

  // For loop over timepoints, i.e., rows
  for (arma::uword i = 0; i < stat.n_rows; ++i)
  {
    // Subtract the row mean and divide by the row standard deviation
    stat.row(i) = (stat.row(i) - mean(stat.row(i))) /
                  stddev(stat.row(i));
  }

  // If the standard deviation is 0, the resulting values are NaN, replace
  // these values with 0
  stat.replace(arma::datum::nan, 0);

  // Return standardized statistic matrix
  return stat;
}

// get_user_stat
arma::mat get_user_stat2(const arma::mat &covariates, int start, int stop)
{
  arma::mat stat = covariates.rows(start, stop);
  return (stat);
}

/*
   Function: calc_pshift_sender

   Description: Computes p-shift statistics for the sender step in the actor-oriented model

   Parameters:
    - type: An integer indicating the type of statistic to compute.
        1 for AB-A,
        2 for AB-B,
        3 for AB-X,
    - edgelist: A matrix with the observed relational event history. Each row represents an observed event and contains four columns: time, sender, receiver, weight
    - actors: A vector with the actors that can potentially interact.
    - memory: String with the type of memory effect. The default is to consider the "full" memory. Further, only the "interval" memory has an effect, since this statistic only considers the event that occurred last.
    - memory_value: Numeric vector indicating the start and end of the memory interval.
    - start: An integer indicating the first row in the edgelist for which the statistic must be computed.
    - stop: An integer indicating the last row in the edgelist for which statistic must be computed.
    - display_progress: A boolean value indicating whether to display the statistic calculation progress bar (true) or not (false).

   Returns:
     A matrix with the p-shift statistic. Rows refer to the time points, and columns refer to the actors.
*/
arma::mat calc_pshift_sender(int type,
                             const arma::mat &edgelist,
                             const arma::vec &actors,
                             std::string memory,
                             arma::vec memory_value,
                             int start,
                             int stop,
                             bool display_progress)
{
  // Progress update
  if (display_progress)
  {
    // Define the statistic names
    Rcpp::CharacterVector statisticNames = Rcpp::CharacterVector::create("psABA", "psABB", "psABX");

    // Check if the 'type' variable is within valid range
    if (type >= 1 && type <= statisticNames.size())
    {
      Rcpp::Rcout << "Computing " << statisticNames[type - 1] << " statistic" << std::endl;
    }
    else
    {
      Rcpp::Rcerr << "Invalid type value. Please provide a valid type." << std::endl;
    }
  }

  // Progress bar
  Progress p(stop - start + 1, display_progress);

  // Initialize statistic
  arma::mat stat((stop - start + 1), actors.n_elem);
  if (type == 1 || type == 2)
  {
    stat.zeros();
  }
  else if (type == 3)
  {
    stat.ones();
  }

  int actor = 0;
  int value = 1;
  if (type == 3)
  {
    value = 0;
  }

  if (start > 0)
  {

    bool update = true;

    if (memory == "interval")
    {
      // Did the last event occur within the interval?
      double current_time = edgelist(start, 0);
      double last_time = edgelist(start - 1, 0);
      update = (last_time > (current_time - memory_value(1))) && (last_time < (current_time - memory_value(0)));
    }

    if (update)
    {
      if (type == 1 || type == 3)
      {
        actor = edgelist(start - 1, 1); // sender
        stat(0, actor) = value;
      }
      if (type == 2 || type == 3)
      {
        actor = edgelist(start - 1, 2); // receiver
        stat(0, actor) = value;
      }
    }

    p.increment();
  }

  // Iterate over time points
  for (int i = start; i < (stop - start); ++i)
  {

    if (memory == "interval")
    {
      bool update = true;
      // Does the current event occur within the interval of the next?
      double new_time = edgelist(i + 1, 0);
      double current_time = edgelist(i, 0);
      update = (current_time > (new_time - memory_value(1))) && (current_time < (new_time - memory_value(0)));

      if (!update)
      {
        p.increment();
        continue;
      }
    }

    if (type == 1 || type == 3)
    {
      actor = edgelist(i, 1); // sender
      stat(i + 1, actor) = value;
    }
    if (type == 2 || type == 3)
    {
      actor = edgelist(i, 2); // receiver
      stat(i + 1, actor) = value;
    }
    p.increment();
  }

  return stat;
}

/*
   Function: calc_pshift_receiver
   Description: Computes p-shift statistics for the receiver step in the actor-oriented model

   Parameters:
    - type: An integer indicating the type of statistic to compute.
        1 for AB-AB,
        2 for AB-BA,
        3 for AB-XB,
        4 for AB-XA,
        5 for AB-AY,
        6 for AB-BY,
        7 for AB-XY
    - edgelist: A matrix with the observed relational event history. Each row represents an observed event and contains four columns: time, sender, receiver, weight
    - actors: A vector with the actors that can potentially interact.
    - memory: String with the type of memory effect. The default is to consider the "full" memory. Further, only the "interval" memory has an effect, since this statistic only considers the event that occurred last.
    - memory_value: Numeric vector indicating the start and end of the memory interval.
    - start: An integer indicating the first row in the edgelist for which the statistic must be computed.
    - stop: An integer indicating the last row in the edgelist for which statistic must be computed.
    - display_progress: A boolean value indicating whether to display the statistic calculation progress bar (true) or not (false).

   Returns:
     A matrix with the p-shift statistic. Rows refer to the time points, and columns refer to the actors.
*/
arma::mat calc_pshift_receiver(int type,
                               const arma::mat &edgelist,
                               const arma::vec &actors,
                               std::string memory,
                               arma::vec memory_value,
                               int start,
                               int stop,
                               bool display_progress)
{
  // Progress update
  if (display_progress)
  {
    // Define the statistic names
    Rcpp::CharacterVector statisticNames = Rcpp::CharacterVector::create("psABAB", "psABBA", "psABXB", "psABXA", "psABAY", "psABBY", "psABXY");

    // Check if the 'type' variable is within valid range
    if (type >= 1 && type <= statisticNames.size())
    {
      Rcpp::Rcout << "Computing " << statisticNames[type - 1] << " statistic" << std::endl;
    }
    else
    {
      Rcpp::Rcerr << "Invalid type value. Please provide a valid type." << std::endl;
    }
  }

  // Progress bar
  Progress p(stop - start + 1, display_progress);

  // Initialize saving space and fill with zeros
  arma::mat stat((stop - start + 1), actors.n_elem, arma::fill::zeros);

  // Initialize statistic
  int sender_prev = 0;
  int receiver_prev = 0;
  int sender_next = 0;

  if (start > 0)
  {

    bool update = true;

    if (memory == "interval")
    {
      // Did the last event occur within the interval?
      double current_time = edgelist(start, 0);
      double last_time = edgelist(start - 1, 0);
      update = (last_time > (current_time - memory_value(1))) && (last_time < (current_time - memory_value(0)));
    }

    if (update)
    {
      sender_prev = edgelist(start - 1, 1);
      receiver_prev = edgelist(start - 1, 2);
      sender_next = edgelist(start, 1);

      if (type == 1)
      {
        if (sender_prev == sender_next)
        {
          stat(0, receiver_prev) = 1;
        }
      }
      else if (type == 2)
      {
        if (receiver_prev == sender_next)
        {
          stat(0, sender_prev) = 1;
        }
      }
      else if (type == 3)
      {
        if (receiver_prev != sender_next && sender_prev != sender_next)
        {
          stat(0, receiver_prev) = 1;
        }
      }
      else if (type == 4)
      {
        if (receiver_prev != sender_next && sender_prev != sender_next)
        {
          stat(0, sender_prev) = 1;
        }
      }
      else if (type == 5)
      {
        if (sender_prev == sender_next)
        {
          stat.row(0).ones();
          stat(0, sender_prev) = 0;
          stat(0, receiver_prev) = 0;
        }
      }
      else if (type == 6)
      {
        if (receiver_prev == sender_next)
        {
          stat.row(0).ones();
          stat(0, sender_prev) = 0;
          stat(0, receiver_prev) = 0;
        }
      }
      else if (type == 7)
      {
        if (sender_prev != sender_next && receiver_prev != sender_next)
        {
          stat.row(0).ones();
          stat(0, sender_prev) = 0;
          stat(0, receiver_prev) = 0;
        }
      }
    }

    p.increment();
  }
  else
  {
    if (type == 7)
    {
      stat.row(0).ones();
    }
  }

  // Iterate over time points
  for (int i = start; i < (stop - start); ++i)
  {

    if (memory == "interval")
    {
      bool update = true;
      // Does the current event occur within the interval of the next?
      double new_time = edgelist(i + 1, 0);
      double current_time = edgelist(i, 0);
      update = (current_time > (new_time - memory_value(1))) && (current_time < (new_time - memory_value(0)));

      if (!update)
      {
        p.increment();
        continue;
      }
    }

    sender_prev = edgelist(i, 1);
    receiver_prev = edgelist(i, 2);
    sender_next = edgelist(i + 1, 1);

    if (type == 1)
    {
      if (sender_prev == sender_next)
      {
        stat(i + 1, receiver_prev) = 1;
      }
    }
    else if (type == 2)
    {
      if (receiver_prev == sender_next)
      {
        stat(i + 1, sender_prev) = 1;
      }
    }
    else if (type == 3)
    {
      if (receiver_prev != sender_next && sender_prev != sender_next)
      {
        stat(i + 1, receiver_prev) = 1;
      }
    }
    else if (type == 4)
    {
      if (receiver_prev != sender_next && sender_prev != sender_next)
      {
        stat(i + 1, sender_prev) = 1;
      }
    }
    else if (type == 5)
    {
      if (sender_prev == sender_next)
      {
        stat.row(i + 1).ones();
        stat(i + 1, sender_prev) = 0;
        stat(i + 1, receiver_prev) = 0;
      }
    }
    else if (type == 6)
    {
      if (receiver_prev == sender_next)
      {
        stat.row(i + 1).ones();
        stat(i + 1, sender_prev) = 0;
        stat(i + 1, receiver_prev) = 0;
      }
    }
    else if (type == 7)
    {
      if (sender_prev != sender_next && receiver_prev != sender_next)
      {
        stat.row(i + 1).ones();
        stat(i + 1, sender_prev) = 0;
        stat(i + 1, receiver_prev) = 0;
      }
    }
    p.increment();
  }

  return stat;
}

int getRateEffectNumber(std::string effect)
{

  std::map<std::string, int> effectsMap;

  // Baseline
  effectsMap["baseline"] = 1;

  // Exogenous stats
  effectsMap["send"] = 2;

  // Endogenous stats
  effectsMap["indegreeSender"] = 3;
  effectsMap["outdegreeSender"] = 4;
  effectsMap["totaldegreeSender"] = 5;
  effectsMap["recencySendSender"] = 6;
  effectsMap["recencyReceiveSender"] = 7;
  effectsMap["psABA"] = 8;
  effectsMap["psABB"] = 9;
  effectsMap["psABX"] = 10;

  // userStat
  effectsMap["userStat"] = 888;

  // interaction effects
  effectsMap["interact"] = 999;

  // find effect number
  auto result = effectsMap.find(effect);
  int numericValue = 0;
  if (result != effectsMap.end())
  {
    numericValue = result->second; // Access the second element
  }
  else
  {
    Rcpp::Rcout << "Effect not found in the map." << std::endl;
  }

  return numericValue;
}

//[[Rcpp::export]]
arma::cube compute_stats_rate(Rcpp::CharacterVector &effects,
                              const arma::mat &edgelist,
                              const arma::vec &actors,
                              const arma::vec &weights,
                              const Rcpp::List &covariates,
                              const Rcpp::List &interactions,
                              std::string memory,
                              const arma::vec memory_value,
                              Rcpp::CharacterVector &scaling,
                              int start,
                              int stop,
                              bool display_progress)
{

  // Initialize saving space
  arma::cube rateStats((stop - start + 1), actors.n_elem, effects.size());

  // For loop over effects
  for (int i = 0; i < effects.size(); ++i)
  {
    // Get case number
    Rcpp::String effectName = effects(i);
    int effect = getRateEffectNumber(effectName);

    // Initialize saving space
    arma::mat stat(rateStats.n_rows, rateStats.n_cols, arma::fill::zeros);

    // Compute effect
    switch (effect)
    {
    // 1 baseline
    case 1:
      stat.fill(1);
      break;
    // 2 send
    case 2:
      // Compute statistic
      stat = exo_actor_aom(covariates[i], edgelist, actors, start, stop, 1);
      break;
    // 3 in-degree
    case 3:
      stat = degree_aom("in", edgelist, actors, weights, memory,
                        memory_value, 1, start, stop, display_progress);
      // Divide by the number of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / sum(stat.row(t));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols);
          rep.fill(1.0 / actors.n_elem);
          stat.row(0) = rep;
        }
      }
      break;
    // 4 out-degree
    case 4:
      stat = degree_aom("out", edgelist, actors, weights, memory,
                        memory_value, 1, start, stop, display_progress);
      // Divide by the number of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / sum(stat.row(t));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols);
          rep.fill(1.0 / actors.n_elem);
          stat.row(0) = rep;
        }
      }
      break;
    // 5 total-degree
    case 5:
      stat = degree_aom("total", edgelist, actors, weights, memory,
                        memory_value, 1, start, stop, display_progress);
      // Divide by two times the number of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / (sum(stat.row(t)));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols);
          rep.fill(1.0 / actors.n_elem);
          stat.row(0) = rep;
        }
      }
      break;
    // 6 recencySendSender
    case 6:
      // Compute statistic
      stat = recency_aom("Send", edgelist, actors, start, stop,
                         display_progress);
      break;
    // 7 recencyReceiveSender
    case 7:
      // Compute statistic
      stat = recency_aom("Receive", edgelist, actors, start, stop,
                         display_progress);
      break;
    // 8 psABA
    case 8:
      stat = calc_pshift_sender(1, edgelist, actors, memory,
                                memory_value, start, stop,
                                display_progress);
      break;
    // 9 psABB
    case 9:
      stat = calc_pshift_sender(2, edgelist, actors, memory,
                                memory_value, start, stop,
                                display_progress);
      break;
    // 10 psABX
    case 10:
      stat = calc_pshift_sender(3, edgelist, actors, memory,
                                memory_value, start, stop,
                                display_progress);
      break;
    // userStat
    case 888:
      stat = get_user_stat2(covariates[i], start, stop);
      break;
    // 999 interact
    case 999:
      // Get the indices of the statistics slices (+1) with the
      // statistics for which an interaction needs to be computed.
      arma::vec x = interactions[i];
      int main1 = x(0);
      int main2 = x(1);
      // Element-wise multiplication
      stat = rateStats.slice(main1 - 1) % rateStats.slice(main2 - 1);
      break;
    }

    // Standardize
    if (scaling(i) == "std")
    {
      stat = scale(stat);
    }

    // Save statistic
    rateStats.slice(i) = stat;
  }

  return rateStats;
}

int getChoiceEffectNumber(std::string effect)
{

  std::map<std::string, int> effectsMap;

  // Exogenous stats
  effectsMap["receive"] = 1;
  effectsMap["same"] = 2;
  effectsMap["difference"] = 3;
  effectsMap["average"] = 4;
  effectsMap["tie"] = 5;

  // Endogenous stats
  effectsMap["inertia"] = 6;
  effectsMap["reciprocity"] = 7;
  effectsMap["indegreeReceiver"] = 8;
  effectsMap["outdegreeReceiver"] = 9;
  effectsMap["totaldegreeReceiver"] = 10;
  effectsMap["otp"] = 11;
  effectsMap["itp"] = 12;
  effectsMap["osp"] = 13;
  effectsMap["isp"] = 14;
  effectsMap["rrankSend"] = 15;
  effectsMap["rrankReceive"] = 16;
  effectsMap["recencySendReceiver"] = 17;
  effectsMap["recencyReceiveReceiver"] = 18;
  effectsMap["recencyContinue"] = 19;
  effectsMap["psABAB"] = 20;
  effectsMap["psABBA"] = 21;
  effectsMap["psABXB"] = 22;
  effectsMap["psABXA"] = 23;
  effectsMap["psABAY"] = 24;
  effectsMap["psABBY"] = 25;
  effectsMap["psABXY"] = 26;

  // userStat
  effectsMap["userStat"] = 888;

  // interaction effects
  effectsMap["interact"] = 999;

  // find effect number
  auto result = effectsMap.find(effect);
  int numericValue = 0;
  if (result != effectsMap.end())
  {
    numericValue = result->second; // Access the second element
  }
  else
  {
    Rcpp::Rcout << "Effect not found in the map." << std::endl;
  }

  return numericValue;
}

//[[Rcpp::export]]
arma::cube compute_stats_choice(Rcpp::CharacterVector &effects,
                                const arma::mat &edgelist,
                                const arma::vec &actors,
                                const arma::vec &weights,
                                const Rcpp::List &covariates,
                                const Rcpp::List &interactions,
                                std::string memory,
                                const arma::vec memory_value,
                                Rcpp::CharacterVector &scaling,
                                int start,
                                int stop,
                                bool display_progress)
{

  // Initialize saving space
  arma::cube choiceStats((stop - start + 1), actors.n_elem, effects.size());

  // For loop over effects
  for (int i = 0; i < effects.size(); ++i)
  {
    // Get case number
    Rcpp::String effectName = effects(i);
    int effect = getChoiceEffectNumber(effectName);

    // Initialize saving space
    arma::mat stat(choiceStats.n_rows, choiceStats.n_cols, arma::fill::zeros);

    // Compute effect
    switch (effect)
    {
    // 1 receive
    case 1:
      // Compute statistic
      stat = exo_actor_aom(covariates[i], edgelist, actors, start, stop,
                           scaling(i));
      break;
    // 2 same
    case 2:
      // Compute statistic
      stat = exo_dyad_aom("same", covariates[i], edgelist, actors, start,
                          stop, 1);
      break;
    // 3 difference
    case 3:
      // Compute statistic
      stat = exo_dyad_aom("difference", covariates[i], edgelist, actors,
                          start, stop, scaling(i));
      break;
    // 4 average
    case 4:
      // Compute statistic
      stat = exo_dyad_aom("average", covariates[i], edgelist, actors,
                          start, stop, scaling(i));
      break;
    // 5 tie
    case 5:
      // Compute statistic
      stat = calculate_aom_tie_statistic(covariates[i], edgelist, actors,
                                         start, stop, scaling(i), display_progress);
      break;
    // 6 inertia
    case 6:
      // Compute statistic
      stat = inertia_aom(edgelist, actors, weights, memory, memory_value,
                         scaling(i), start, stop, display_progress);
      break;
    // 7 reciprocity
    case 7:
      // Compute statistic
      stat = reciprocity_aom(edgelist, actors, weights, memory,
                             memory_value, scaling(i), start, stop, display_progress);
      break;
    // 8 in-degree
    case 8:
      stat = degree_aom("in", edgelist, actors, weights, memory,
                        memory_value, scaling(i), start, stop, display_progress);
      break;
    // 9 out-degree
    case 9:
      stat = degree_aom("out", edgelist, actors, weights, memory,
                        memory_value, scaling(i), start, stop, display_progress);
      break;
    // 10 total-degree
    case 10:
      stat = degree_aom("total", edgelist, actors, weights, memory,
                        memory_value, scaling(i), start, stop, display_progress);
      break;
    // 11 otp
    case 11:
      stat = triad_aom("otp", edgelist, actors, weights, memory,
                       memory_value, scaling(i), start, stop,
                       display_progress);
      break;
    // 12 itp
    case 12:
      stat = triad_aom("itp", edgelist, actors, weights, memory,
                       memory_value, scaling(i), start, stop,
                       display_progress);
      break;
    // 13 osp
    case 13:
      stat = triad_aom("osp", edgelist, actors, weights, memory,
                       memory_value, scaling(i), start, stop,
                       display_progress);
      break;
    // 14 isp
    case 14:
      stat = triad_aom("isp", edgelist, actors, weights, memory,
                       memory_value, scaling(i), start, stop,
                       display_progress);
      break;
    // 15 rrankSend
    case 15:
      stat = rrank_aom("Send", edgelist, actors, start, stop,
                       display_progress);
      break;
    // 16 rrankReceive
    case 16:
      stat = rrank_aom("Receive", edgelist, actors, start, stop,
                       display_progress);
      break;
    // 17 recencySendReceiver
    case 17:
      stat = recency_aom("Send", edgelist, actors, start, stop,
                         display_progress);
      break;
    // 18 recencyReceiveReceiver
    case 18:
      stat = recency_aom("Receive", edgelist, actors, start, stop,
                         display_progress);
      break;
    // 19 recencyContinue
    case 19:
      stat = recency_aom("Continue", edgelist, actors, start, stop,
                         display_progress);
      break;
    // 20 psABAB
    case 20:
      stat = calc_pshift_receiver(1, edgelist, actors, memory,
                                  memory_value, start, stop,
                                  display_progress);
      break;
    // 21 psABBA
    case 21:
      stat = calc_pshift_receiver(2, edgelist, actors, memory,
                                  memory_value, start, stop,
                                  display_progress);
      break;
    // 22 psABXB
    case 22:
      stat = calc_pshift_receiver(3, edgelist, actors, memory,
                                  memory_value, start, stop,
                                  display_progress);
      break;
    // 23 psABXA
    case 23:
      stat = calc_pshift_receiver(4, edgelist, actors, memory,
                                  memory_value, start, stop,
                                  display_progress);
      break;
    // 24 psABAY
    case 24:
      stat = calc_pshift_receiver(5, edgelist, actors, memory,
                                  memory_value, start, stop,
                                  display_progress);
      break;
    // 25 psABBY
    case 25:
      stat = calc_pshift_receiver(6, edgelist, actors, memory,
                                  memory_value, start, stop,
                                  display_progress);
      break;
    // 26 psABXY
    case 26:
      stat = calc_pshift_receiver(7, edgelist, actors, memory,
                                  memory_value, start, stop,
                                  display_progress);
      break;
    // userStat
    case 888:
      stat = get_user_stat2(covariates[i], start, stop);
      break;
    // 99 interact
    case 999:
      // Get the indices of the statistics slices (+1) with the
      // statistics for which an interaction needs to be computed.
      arma::vec x = interactions[i];
      int main1 = x(0);
      int main2 = x(1);
      // Element-wise multiplication
      stat = choiceStats.slice(main1 - 1) % choiceStats.slice(main2 - 1);
      break;
    }

    // Save statistic
    choiceStats.slice(i) = stat;
  }

  return choiceStats;
}