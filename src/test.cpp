#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
#include <stdexcept> // Include the necessary header for std::runtime_error
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include <map>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]

// [[Rcpp::export]]
arma::mat get_riskset(arma::uvec actorID, arma::uvec typeID,
                      arma::uword N, arma::uword C, bool directed)
{
  switch (directed)
  {
  case 0:
  { // for undirected network
    arma::uword i, j, c;
    arma::uword col_index = 0;
    arma::mat riskset(((N * (N - 1)) / 2) * C, 4);
    for (c = 0; c < C; c++)
    {
      for (i = 0; i < N; i++)
      {
        for (j = (i + 1); j < N; j++)
        {
          // unit increase col_index
          riskset(col_index, 0) = actorID(i);
          riskset(col_index, 1) = actorID(j);
          riskset(col_index, 2) = typeID(c);
          riskset(col_index, 3) = col_index;
          col_index += 1;
        }
      }
    }
    return riskset;
  }

  case 1:
  { // for directed network
    arma::uword i, j, c;
    arma::mat riskset(N * N * C, 4);
    arma::uvec indices_to_shed(N * C); // this is the vector where to store the indices of selfedges to remove at the end of the function
    indices_to_shed.fill(N * N * C);
    for (c = 0; c < C; c++)
    {
      for (i = 0; i < N; i++)
      {
        for (j = 0; j < N; j++)
        {
          if (j != i)
          {
            riskset(j + i * N + c * (N * N), 0) = actorID(i);
            riskset(j + i * N + c * (N * N), 1) = actorID(j);
            riskset(j + i * N + c * (N * N), 2) = typeID(c);
          }
          else
          {
            indices_to_shed(j + c * N) = (j + i * N + c * (N * N));
          }
        }
      }
    }
    riskset.shed_rows(indices_to_shed);
    riskset.col(3) = arma::linspace(0, riskset.n_rows - 1, riskset.n_rows);
    return riskset;
  }
  }
}

// [[Rcpp::export]]
arma::mat convert_to_risksetMatrix(arma::mat riskset, int N, int C)
{

  arma::mat risksetMatrix(N, N * C);
  risksetMatrix.fill(-999);

  int sender;
  int receiver;
  int type;
  int dyad;

  for (arma::uword i = 0; i < riskset.n_rows; ++i)
  {
    sender = riskset(i, 0);
    receiver = riskset(i, 1);
    type = riskset(i, 2);
    dyad = riskset(i, 3);
    risksetMatrix(sender, receiver + (N * type)) = dyad;
  }

  return risksetMatrix;
}

arma::rowvec update_inertia(arma::uvec update, const arma::mat &edgelist,
                            const arma::mat &risksetMatrix,
                            arma::rowvec inertia_row, const arma::vec &weights)
{

  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  for (arma::uword j = 0; j < update.n_elem; ++j)
  {
    arma::uword event = update(j);
    int actor1 = edgelist(event, 1);
    int actor2 = edgelist(event, 2);
    int event_type = 0;
    if (C > 1)
    {
      event_type = edgelist(event, 3);
    }
    arma::uword dyad = risksetMatrix(actor1, actor2 + (N * event_type));
    inertia_row(dyad) += weights(event);
  }

  return inertia_row;
}

arma::vec get_decay_weights(double current_time, arma::uvec update,
                            const arma::vec &weights,
                            const arma::mat &edgelist, double mem_val)
{

  arma::vec decay_weights = weights;

  for (arma::uword j = 0; j < update.n_elem; ++j)
  {
    arma::uword event = update(j);
    double event_time = edgelist(event, 0);
    double event_weight = weights(event);
    double decay_weight = event_weight * exp(-(current_time - event_time) * (log(2) / mem_val)) * (log(2) / mem_val);

    decay_weights(event) = decay_weight;
  }

  return decay_weights;
}

// [[Rcpp::export]]
arma::mat calculate_inertia(const arma::mat &edgelist,
                            const arma::vec &weights,
                            const arma::mat &risksetMatrix,
                            Rcpp::String memory,
                            const arma::vec &memory_value,
                            int start, int stop,
                            bool display_progress,
                            Rcpp::String method = "pt")
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating inertia statistic/building block" << std::endl;
  }

  // Slice the edgelist according to 'start' and 'stop'
  arma::mat slice = edgelist.rows(start, stop);

  arma::uword D = risksetMatrix.max() + 1;
  // Initialize inertia
  arma::mat inertia;

  if (method == "pt")
  {
    // Unique time points
    arma::vec unique_time_points = unique(slice.col(0));

    // Progress bar
    Progress p(unique_time_points.n_elem, display_progress);

    // Re-size stat
    inertia.set_size(unique_time_points.n_elem, D);
    inertia.zeros();

    // Calculate inertia
    for (arma::uword i = 0; i < unique_time_points.n_elem; ++i)
    {
      double current_time = unique_time_points(i);

      // Indices of the events with which inertia is updated
      arma::uvec update;

      if (memory == "full")
      {
        // Only update with events that happened since the previous time point
        double previous_time = 0;
        if (i > 0)
        {
          previous_time = unique_time_points(i - 1);
          inertia.row(i) = inertia.row(i - 1);
        }

        update = arma::find(edgelist.col(0) >= previous_time &&
                            edgelist.col(0) < current_time);
      }
      else if (memory == "interval")
      {
        // Only update with events that happened between two time points
        double min_time = current_time - memory_value(1);
        double max_time = current_time - memory_value(0);

        update = arma::find(edgelist.col(0) >= min_time &&
                            edgelist.col(0) < max_time);
      }
      else if (memory == "decay")
      {
        // Update with events that happened before the current time
        update = arma::find(edgelist.col(0) < current_time);
      }

      // Update this inertia row
      if (memory == "decay")
      {
        if (memory_value(0) <= 0)
        {
          throw std::runtime_error("Invalid memory_value: must be a positive non-zero value.");
        }

        arma::vec decay_weights = get_decay_weights(current_time, update,
                                                    weights, edgelist,
                                                    memory_value(0));
        inertia.row(i) = update_inertia(update, edgelist, risksetMatrix,
                                        inertia.row(i), decay_weights);
      }
      else
      {
        inertia.row(i) = update_inertia(update, edgelist, risksetMatrix,
                                        inertia.row(i), weights);
      }

      p.increment();
    }
  }
  else if (method == "pe")
  {
    // Progress bar
    Progress p(slice.n_rows, display_progress);

    // Re-size stat
    inertia.set_size(slice.n_rows, D);
    inertia.zeros();

    if (memory == "full")
    {
      // Initialize inertia
      double current_time = slice(0, 0);
      arma::uvec update = arma::find(edgelist.col(0) < current_time);
      inertia.row(0) = update_inertia(update, edgelist, risksetMatrix,
                                      inertia.row(0), weights);

      int N = risksetMatrix.n_rows;
      int C = risksetMatrix.n_cols / N;

      // Update with each new event
      for (arma::uword i = 1; i < slice.n_rows; ++i)
      {
        // Copy previous row
        inertia.row(i) = inertia.row(i - 1);
        // Update with the new event
        arma::uword event = start + i - 1;
        int actor1 = edgelist(event, 1);
        int actor2 = edgelist(event, 2);
        int event_type = 0;
        if (C > 1)
        {
          event_type = edgelist(event, 3);
        }
        arma::uword dyad = risksetMatrix(actor1, actor2 + (N * event_type));
        inertia(i, dyad) += weights(event);

        p.increment();
      }
    }
    else if (memory == "interval")
    {
      // Loop over timepoints
      for (arma::uword i = 0; i < slice.n_rows; ++i)
      {
        double current_time = slice(i, 0);
        double min_time = current_time - memory_value(1);
        double max_time = current_time - memory_value(0);

        // Indices of the events with which inertia is calculated
        arma::uvec update = arma::find(edgelist.col(0) >= min_time &&
                                       edgelist.col(0) < max_time);

        // Calculate inertia for the given time point
        inertia.row(i) = update_inertia(update, edgelist, risksetMatrix,
                                        inertia.row(i), weights);

        p.increment();
      }
    }
    else if (memory == "decay")
    {

      if (memory_value(0) <= 0)
      {
        throw std::runtime_error("Invalid memory_value: must be a positive non-zero value.");
      }

      // Loop over time points
      for (arma::uword i = 0; i < slice.n_rows; ++i)
      {
        double current_time = slice(i, 0);

        // Indices of the events with which inertia is calculated
        arma::uvec update = arma::find(edgelist.col(0) < current_time);

        // Weights with which inertia is calculated
        arma::vec decay_weights = get_decay_weights(current_time, update, weights, edgelist, memory_value(0));

        // Calculate inertia for the given time point
        inertia.row(i) = update_inertia(update, edgelist, risksetMatrix,
                                        inertia.row(i), decay_weights);

        p.increment();
      }
    }
  }

  // Return
  return inertia;
}

/*
  Transform the inertia building block when event types are in the dependent variable, but consider_type is FALSE.
*/
// [[Rcpp::export]]
arma::mat transform_inertia(const arma::mat &inertia,
                            const arma::mat &risksetMatrix,
                            bool display_progress)
{
  if (display_progress)
  {
    Rcpp::Rcout << "Aggregate inertia over event types" << std::endl;
  }

  // Initialize
  arma::mat new_inertia(inertia.n_rows, inertia.n_cols);
  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // Loop over 'senders'
  for (int s = 0; s < N; ++s)
  {
    // Loop over 'receivers'
    for (int r = 0; r < N; ++r)
    {
      // Find the dyads with these actors
      std::vector<int> dyads_valid;

      for (int c = 0; c < C; ++c)
      {
        int dyad = risksetMatrix(s, r + (N * c));
        if (dyad >= 0)
        {
          dyads_valid.push_back(dyad);
        }
      }

      // Sum inertia for these dyads
      arma::uvec dyads_idx = arma::conv_to<arma::uvec>::from(dyads_valid);
      arma::vec dyad_inertia = sum(inertia.cols(dyads_idx), 1);

      for (int d = 0; d < dyads_valid.size(); ++d)
      {
        int dyad = dyads_valid[d];
        new_inertia.col(dyad) = dyad_inertia;
      }
    }
  }

  return new_inertia;
}

// [[Rcpp::export]]
arma::mat calculate_degree(int type, const arma::mat &inertia,
                           const arma::mat &risksetMatrix,
                           bool display_progress)
{
  // Progress update
  Rcpp::CharacterVector types = Rcpp::CharacterVector::create(
      "indegreeSender", "indegreeReceiver",
      "outdegreeSender", "outdegreeReceiver",
      "totaldegreeSender", "totaldegreeReceiver");

  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << types(type - 1) << " statistic" << std::endl;
  }

  // Initialize statistic
  arma::mat degree_stat(inertia.n_rows, inertia.n_cols, arma::fill::zeros);

  // Prep
  arma::uword N = risksetMatrix.n_rows;
  arma::uword C = risksetMatrix.n_cols / N;
  arma::uvec exist, dyads_idx;
  arma::vec dyads_valid, saving_dyads;
  arma::mat actor_inertia, actor_degree;

  // Progress bar
  Progress p(N, display_progress);

  // Loop over actors
  for (arma::uword i = 0; i < N; ++i)
  {
    // Loop over event types
    for (arma::uword c = 0; c < C; ++c)
    {
      // Compute the actor's indegree value per time point
      if (type == 1 || type == 2 || type == 5 || type == 6)
      {
        // Find the dyads in which actor i is the receiver
        arma::vec dyads = risksetMatrix.col(i + (N * c));
        exist = arma::find(dyads >= 0);
        dyads_valid = dyads.elem(exist);

        // Compute actor i's indegree for each timepoint
        dyads_idx = arma::conv_to<arma::uvec>::from(dyads_valid);
        actor_inertia = inertia.cols(dyads_idx);
        actor_degree = sum(actor_inertia, 1);

        // Find the dyads in which actor i is the receiver
        if (type == 2 || type == 6)
        {
          saving_dyads = dyads_valid;
        }
        // Find the dyads in which actor i is the sender
        if (type == 1 || type == 5)
        {
          arma::rowvec dyadsSender = risksetMatrix.row(i);
          exist = arma::find(dyadsSender >= 0);
          saving_dyads = dyadsSender.elem(exist);
        }

        // Saving actor i's indegree
        for (arma::uword j = 0; j < saving_dyads.n_elem; ++j)
        {
          arma::uword col_idx = saving_dyads(j);
          degree_stat.col(col_idx) += actor_degree;
        }
      }

      // Compute the actor's outdegree value per time point
      if (type == 3 || type == 4 || type == 5 || type == 6)
      {
        // Find the dyads in which actor i is the sender
        arma::rowvec dyads = risksetMatrix.row(i);
        exist = arma::find(dyads >= 0);
        dyads_valid = dyads.elem(exist);

        // Compute actor i's outdegree for each timepoint
        dyads_idx = arma::conv_to<arma::uvec>::from(dyads_valid);
        actor_inertia = inertia.cols(dyads_idx);
        actor_degree = sum(actor_inertia, 1);

        // Find the dyads in which actor i is the sender
        if (type == 3 || type == 5)
        {
          saving_dyads = dyads_valid;
        }
        // Find the dyads in which actor i is the receiver
        if (type == 4 || type == 6)
        {
          arma::vec dyadsReceiver = risksetMatrix.col(i + N * c);
          exist = arma::find(dyadsReceiver >= 0);
          saving_dyads = dyadsReceiver.elem(exist);
        }

        // Saving actor i's outdegree
        for (arma::uword j = 0; j < saving_dyads.n_elem; ++j)
        {
          arma::uword col_idx = saving_dyads(j);
          degree_stat.col(col_idx) += actor_degree;
        }
      }
    }

    p.increment();
  }

  return degree_stat;
}

// [[Rcpp::export]]
arma::mat calculate_reciprocity(const arma::mat &inertia,
                                const arma::mat &risksetMatrix,
                                bool display_progress)
{
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating reciprocity statistic" << std::endl;
  }

  // Initialize
  arma::mat reciprocity(inertia.n_rows, inertia.n_cols, arma::fill::zeros);
  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // Progress bar
  Progress p(N, display_progress);

  // Loop over 'senders'
  for (int s = 0; s < N; ++s)
  {
    // Loop over 'receivers'
    for (int r = 0; r < N; ++r)
    {
      // Loop over event 'types'
      for (int c = 0; c < C; ++c)
      {
        int this_dyad = risksetMatrix(s, r + (N * c));
        int rev_dyad = risksetMatrix(r, s + (N * c));
        if (this_dyad >= 0 && rev_dyad >= 0)
        {
          reciprocity.col(this_dyad) = inertia.col(rev_dyad);
        }
      }
    }

    p.increment();
  }

  // Return
  return reciprocity;
}

// [[Rcpp::export]]
arma::mat calculate_triad(int type, const arma::mat &inertia,
                          const arma::mat &risksetMatrix,
                          Rcpp::String scaling,
                          bool display_progress)
{
  // Progress update
  Rcpp::CharacterVector types = Rcpp::CharacterVector::create(
      "otp", "itp", "osp", "isp");

  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << types(type - 1) << " statistic" << std::endl;
  }

  // Initialize statistic
  arma::mat triad(inertia.n_rows, inertia.n_cols, arma::fill::zeros);

  // Network info
  arma::uword N = risksetMatrix.n_rows;
  arma::uword C = risksetMatrix.n_cols / N;

  // Declare variables
  int path1_dyad, path2_dyad;

  // Progress bar
  Progress p(N, display_progress);

  // Loop over 'senders'
  for (int s = 0; s < N; ++s)
  {
    // Loop over 'receivers'
    for (int r = 0; r < N; ++r)
    {
      // Loop over event 'types'
      for (int c = 0; c < C; ++c)
      {
        int this_dyad = risksetMatrix(s, r + (N * c));
        if (this_dyad < 0)
          continue;
        // Loop over 'third actors'
        for (int h = 0; h < N; ++h)
        {
          if (h == s || h == r)
            continue;

          // get path dyads
          switch (type)
          {
          // otp
          case 1:
            // s -> h
            path1_dyad = risksetMatrix(s, h + (N * c));
            // h -> r
            path2_dyad = risksetMatrix(h, r + (N * c));
            break;
          // itp
          case 2:
            // r -> h
            path1_dyad = risksetMatrix(r, h + (N * c));
            // h -> s
            path2_dyad = risksetMatrix(h, s + (N * c));
            break;
          // osp
          case 3:
            // s -> h
            path1_dyad = risksetMatrix(s, h + (N * c));
            // r -> h
            path2_dyad = risksetMatrix(r, h + (N * c));
            break;
          // isp
          case 4:
            // h -> s
            path1_dyad = risksetMatrix(h, s + (N * c));
            // h -> r
            path2_dyad = risksetMatrix(h, r + (N * c));
            break;
          }

          // get path events
          if (path1_dyad < 0)
            continue;
          arma::mat path1_events = inertia.col(path1_dyad);
          if (path2_dyad < 0)
            continue;
          arma::mat path2_events = inertia.col(path2_dyad);
          // join
          arma::mat events = join_rows(path1_events, path2_events);
          // convert elements to 1 if greater than 0
          if ((scaling == "none_unique") || (scaling == "std_unique"))
          {
            events = arma::conv_to<arma::mat>::from(events > 0);
          }
          // compute the minimum
          arma::vec stat = min(events, 1);
          // save
          triad.col(this_dyad) += stat;
        }
      }
    }

    p.increment();
  }

  // Return statistic
  return triad;
}

/* To do: Document when memory is decay? */

void process_pshift(const arma::vec &dyads, arma::mat &pshift, arma::uword i)
{
  for (arma::uword d = 0; d < dyads.n_elem; ++d)
  {
    int dyad = dyads(d);
    if (dyad >= 0)
    {
      pshift(i, dyad) = 1;
    }
  }
}

arma::uvec pshift_event_indices(const arma::mat &edgelist,
                                const arma::vec &time_points,
                                int start, int i,
                                Rcpp::String method)
{
  arma::uvec event_indices;

  double current_time = time_points(i);
  double previous_time = 0;
  if (i > 0)
  {
    previous_time = time_points(i - 1);
  }

  if (method == "pt")
  {

    // Only compute with events that happened since the previous time point
    event_indices = arma::find(edgelist.col(0) >= previous_time &&
                               edgelist.col(0) < current_time);
  }
  else if (method == "pe")
  {
    if (i == 0)
    {
      event_indices = arma::find(edgelist.col(0) < current_time);
      if (event_indices.n_elem > 0)
      {
        int last_event = arma::max(event_indices);
        event_indices.resize(1);
        event_indices(0) = last_event;
      }
    }
    else
    {
      event_indices.resize(1);
      event_indices(0) = start + i - 1;
    }
  }
  return event_indices;
}

// [[Rcpp::export]]
arma::mat calculate_pshift(std::string type, const arma::mat &edgelist,
                           const arma::mat &risksetMatrix,
                           int start, int stop, bool display_progress,
                           Rcpp::String method = "pt")
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating pshift " << type << " statistic" << std::endl;
  }

  // Slice the edgelist according to 'start' and 'stop'
  arma::mat slice = edgelist.rows(start, stop);

  // Time points: Depending on the method, get ...
  arma::vec time_points;
  if (method == "pt")
  {
    // ... the unique time points
    time_points = unique(slice.col(0));
  }
  else if (method == "pe")
  {
    // ... all event times
    time_points = slice.col(0);
  }

  // Initialize pshift matrix
  arma::mat pshift(time_points.n_elem, risksetMatrix.max() + 1,
                   arma::fill::zeros);

  // Get number of actors (N) and types (C) in the network
  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // Progress bar
  Progress p(time_points.n_elem, display_progress);

  // Loop over time points
  for (arma::uword i = 0; i < time_points.n_elem; ++i)
  {
    // Indices of the event with which the respective pshift is computed
    arma::uvec event_indices = pshift_event_indices(edgelist, time_points, start, i, method);

    // Loop over event_indices
    for (arma::uword m = 0; m < event_indices.n_elem; ++m)
    {
      // Get the event information
      arma::uword event = event_indices(m);
      arma::uword sender = edgelist(event, 1);
      arma::uword receiver = edgelist(event, 2);
      arma::uword event_type = 0;
      if (C > 1)
        event_type = edgelist(event, 3);

      // For each pshift type: Get the respective dyad(s) that create(s) the pshift and process it in the statistic
      if (type == "AB-BA")
      {
        int dyad = risksetMatrix(receiver, sender + event_type * C);
        if (dyad >= 0)
        {
          pshift(i, dyad) = 1;
        }
      }

      if (type == "AB-BY")
      {
        arma::vec temp = risksetMatrix.row(receiver).t();
        arma::vec dyads = temp.subvec(event_type * N,
                                      ((event_type + 1) * N) - 1);
        dyads.shed_row(sender);
        process_pshift(dyads, pshift, i);
      }

      if (type == "AB-XA")
      {
        arma::vec dyads = risksetMatrix.col(sender + event_type * C);
        dyads.shed_row(receiver);
        process_pshift(dyads, pshift, i);
      }

      if (type == "AB-XB")
      {
        arma::vec dyads = risksetMatrix.col(receiver + event_type * C);
        dyads.shed_row(sender);
        process_pshift(dyads, pshift, i);
      }

      if (type == "AB-XY")
      {
        arma::mat dyadsMat = risksetMatrix.cols(event_type * N,
                                                ((event_type + 1) * N) - 1);
        arma::uvec idx = {sender, receiver};
        dyadsMat.shed_rows(idx);
        dyadsMat.shed_cols(idx);
        arma::vec dyads = dyadsMat.as_col();
        process_pshift(dyads, pshift, i);
      }

      if (type == "AB-AY")
      {
        arma::vec temp = risksetMatrix.row(sender).t();
        arma::vec dyads = temp.subvec(event_type * N,
                                      ((event_type + 1) * N) - 1);
        dyads.shed_row(receiver);
        process_pshift(dyads, pshift, i);
      }

      if (type == "AB-AB")
      {
        int dyad = risksetMatrix(sender, receiver + event_type * C);
        if (dyad >= 0)
        {
          pshift(i, dyad) = 1;
        }
      }
    }

    p.increment(); // progress update
  }

  return pshift;
}

void update_lastActive(double time, const arma::vec &dyads,
                       arma::vec &lastActive)
{
  for (arma::uword d = 0; d < dyads.n_elem; ++d)
  {
    int dyad = dyads(d);
    if (dyad >= 0)
    {
      lastActive(dyad) = time;
    }
  }
}

// [[Rcpp::export]]
arma::mat calculate_recency(std::string type, const arma::mat &edgelist,
                            const arma::mat &risksetMatrix,
                            int start, int stop, bool display_progress,
                            Rcpp::String method = "pt")
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << type << " statistic" << std::endl;
  }

  // Slice the edgelist according to 'start' and 'stop'
  arma::mat slice = edgelist.rows(start, stop);

  // Time points: Depending on the method, get ...
  arma::vec time_points;
  if (method == "pt")
  {
    // ... the unique time points
    time_points = unique(slice.col(0));
  }
  else if (method == "pe")
  {
    // ... all event times
    time_points = slice.col(0);
  }

  // Initialize recency matrix
  arma::mat recency(time_points.n_elem, risksetMatrix.max() + 1,
                    arma::fill::zeros);

  // Initialize vector with times the dyads were last active
  arma::vec lastActive(recency.n_cols);
  lastActive.fill(arma::datum::inf);

  // Progress bar
  Progress p(time_points.n_elem, display_progress);

  // Get number of actors (N) and types (C) in the network
  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // Select the events in the past to initialize 'lastActive'
  double first_time = edgelist(start, 0);
  arma::uvec event_indices = arma::find(edgelist.col(0) < first_time);

  // For loop over the past
  for (arma::uword m = 0; m < event_indices.n_elem; ++m)
  {
    // Get the event information
    arma::uword event = event_indices(m);
    double time = edgelist(event, 0);
    arma::uword sender = edgelist(event, 1);
    arma::uword receiver = edgelist(event, 2);
    arma::uword event_type = 0;
    if (C > 1)
    {
      event_type = edgelist(event, 3);
    }

    // Find respective dyads and update lastActive
    if (type == "recencyContinue")
    {
      int dyad = risksetMatrix(sender, receiver + event_type * N);
      if (dyad >= 0)
      {
        lastActive(dyad) = time;
      }
    }

    if (type == "recencySendSender")
    {
      arma::vec temp = risksetMatrix.row(sender).t();
      arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
      update_lastActive(time, dyads, lastActive);
    }

    if (type == "recencySendReceiver")
    {
      arma::vec dyads = risksetMatrix.col(sender + event_type * N);
      update_lastActive(time, dyads, lastActive);
    }

    if (type == "recencyReceiveSender")
    {
      arma::vec temp = risksetMatrix.row(receiver).t();
      arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
      update_lastActive(time, dyads, lastActive);
    }

    if (type == "recencyReceiveReceiver")
    {
      arma::vec dyads = risksetMatrix.col(receiver + event_type * N);
      update_lastActive(time, dyads, lastActive);
    }
  }

  // Loop over time points
  for (arma::uword m = 0; m < time_points.n_elem; ++m)
  {

    // Event_time
    double current_time = time_points(m);

    // Compute stat
    arma::vec fr = 1 / ((current_time - lastActive) + 1);
    recency.row(m) = fr.t();

    // Get the indices of the events with which lastActive needs to be updated
    if (method == "pt")
    {
      // With all events at the current time
      double next_time = 0;
      if (m < (time_points.n_elem - 1))
      {
        next_time = time_points(m + 1);
      }
      else
      {
        next_time = current_time;
      }
      event_indices = arma::find(edgelist.col(0) >= current_time &&
                                 edgelist.col(0) < next_time);
    }
    else if (method == "pe")
    {
      // Only with the current event
      event_indices.resize(1);
      event_indices(0) = start + m;
    }

    // For loop over event_indices
    for (arma::uword i = 0; i < event_indices.n_elem; ++i)
    {
      // Get the event information
      arma::uword event = event_indices(i);
      double time = edgelist(event, 0);
      arma::uword sender = edgelist(event, 1);
      arma::uword receiver = edgelist(event, 2);
      arma::uword event_type = 0;
      if (C > 1)
      {
        event_type = edgelist(event, 3);
      }

      // Find respective dyads and update lastActive
      if (type == "recencyContinue")
      {
        int dyad = risksetMatrix(sender, receiver + event_type * N);
        if (dyad >= 0)
        {
          lastActive(dyad) = time;
        }
      }

      if (type == "recencySendSender")
      {
        arma::vec temp = risksetMatrix.row(sender).t();
        arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
        update_lastActive(time, dyads, lastActive);
      }

      if (type == "recencySendReceiver")
      {
        arma::vec dyads = risksetMatrix.col(sender + event_type * N);
        update_lastActive(time, dyads, lastActive);
      }

      if (type == "recencyReceiveSender")
      {
        arma::vec temp = risksetMatrix.row(receiver).t();
        arma::vec dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
        update_lastActive(time, dyads, lastActive);
      }

      if (type == "recencyReceiveReceiver")
      {
        arma::vec dyads = risksetMatrix.col(receiver + event_type * N);
        update_lastActive(time, dyads, lastActive);
      }
    }

    p.increment();
  }

  return recency;
}

arma::rowvec rankR(arma::rowvec x, int N)
{
  arma::uvec ranksU = N - sort_index(sort_index(x));
  arma::rowvec ranks = arma::conv_to<arma::rowvec>::from(ranksU);
  arma::uvec indices = arma::find(x == 0);
  arma::rowvec reps(indices.n_elem, arma::fill::zeros);
  ranks(indices) = reps;
  return ranks;
}

// [[Rcpp::export]]
arma::mat calculate_rrank(int type, const arma::mat &edgelist,
                          const arma::mat &riskset, int N, int C,
                          int start, int stop,
                          bool consider_type, bool display_progress,
                          Rcpp::String method = "pt")
{
  // Progress update
  if (display_progress)
  {
    if (type == 1)
    {
      Rcpp::Rcout << "Calculating rrankSend statistic" << std::endl;
    }
    else if (type == 2)
    {
      Rcpp::Rcout << "Calculating rrankReceive statistic" << std::endl;
    }
  }

  // Slice the edgelist according to 'start' and 'stop'
  arma::mat slice = edgelist.rows(start, stop);

  // Time points: Depending on the method, get ...
  arma::vec time_points;
  if (method == "pt")
  {
    // ... the unique time points
    time_points = unique(slice.col(0));
  }
  else if (method == "pe")
  {
    // ... all event times
    time_points = slice.col(0);
  }

  // Initialize recency matrix
  arma::mat rrank(time_points.n_elem, riskset.n_rows, arma::fill::zeros);

  // Progress bar
  Progress p(time_points.n_elem, display_progress);

  // Select the events in the past to initialize 'lastActive'
  double first_time = edgelist(start, 0);
  arma::uvec event_indices = arma::find(edgelist.col(0) < first_time);

  // Initialize lastTime array
  arma::cube lastTime(N, N, C);

  // For loop over the past
  for (arma::uword m = 0; m < event_indices.n_elem; ++m)
  {
    // Get relevant event info
    arma::uword event = event_indices(m);
    double event_time = edgelist(event, 0);
    arma::uword sender = edgelist(event, 1);
    arma::uword receiver = edgelist(event, 2);
    arma::uword event_type = 0;
    if (C > 1 && consider_type)
    {
      event_type = edgelist(event, 3);
    }

    // rrankSend: to whom the sender most recently has send event
    // (most recent times)
    if (type == 1)
    {
      lastTime(sender, receiver, event_type) = event_time;
    }
    // rrankReceive: from whom the sender has most recently received events
    // (most recent times)
    if (type == 2)
    {
      lastTime(receiver, sender, event_type) = event_time;
    }
  }

  // Loop over time points
  for (arma::uword i = 0; i < time_points.n_elem; ++i)
  {
    // Compute ranks based on lastTime
    arma::cube ranks(N, N, C, arma::fill::zeros);

    // Loop over event types
    arma::uword K = 1;
    if (consider_type)
    {
      K = C;
    }
    for (arma::uword c = 0; c < K; ++c)
    {
      // Loop over senders
      for (arma::uword j = 0; j < N; ++j)
      {
        ranks.slice(c).row(j) = rankR(lastTime.slice(c).row(j), N);
      }
    }

    // Statistic values
    arma::cube values = 1 / ranks;
    values.replace(arma::datum::inf, 0);

    // Transform to rrank matrix
    for (arma::uword j = 0; j < riskset.n_rows; ++j)
    {
      if (consider_type)
      {
        rrank(i, j) = values(riskset(j, 0), riskset(j, 1), riskset(j, 2));
      }
      else
      {
        rrank(i, j) = values(riskset(j, 0), riskset(j, 1), 0);
      }
    }

    // Get the indices of the events with which lastTime needs to be updated
    double current_time = time_points(i);
    if (method == "pt")
    {
      // With all events at the current time
      double next_time = 0;
      if (i < (time_points.n_elem - 1))
      {
        next_time = time_points(i + 1);
      }
      else
      {
        next_time = current_time;
      }
      event_indices = arma::find(edgelist.col(0) >= current_time &&
                                 edgelist.col(0) < next_time);
    }
    else if (method == "pe")
    {
      // Only with the current event
      event_indices.resize(1);
      event_indices(0) = start + i;
    }

    // Loop over event_indices to update lastTime
    for (arma::uword m = 0; m < event_indices.n_elem; ++m)
    {
      // Get relevant event info
      arma::uword event = event_indices(m);
      double event_time = edgelist(event, 0);
      arma::uword sender = edgelist(event, 1);
      arma::uword receiver = edgelist(event, 2);
      arma::uword event_type = 0;
      if (C > 1 && consider_type)
      {
        event_type = edgelist(event, 3);
      }

      // rrankSend: to whom the sender most recently has send event
      // (most recent times)
      if (type == 1)
      {
        lastTime(sender, receiver, event_type) = event_time;
      }
      // rrankReceive: from whom the sender has most recently received events
      // (most recent times)
      if (type == 2)
      {
        lastTime(receiver, sender, event_type) = event_time;
      }
    }
  }

  return rrank;
}

void update_exo_actor(double value, arma::mat &stat, arma::vec dyads, int i)
{
  // Loop over dyads
  for (arma::uword d = 0; d < dyads.n_elem; ++d)
  {
    int dyad = dyads(d);
    if (dyad >= 0)
    {
      stat(i, dyad) = value;
    }
  }
}

/*
   Function: calculate_exo_actor

   Description: Computes (or transforms/obtains) the exogenous actor statistics for the tie-oriented model.

   Parameters:
     - type: An string indicating the type of statistic to compute: 'send' for sender effect, 'receive' for receiver effect.
     - edgelist: A matrix with the observed relational event history. Rows refer to the observed relational events. The first column refers to the time, the second column to the sender, the third column to the receiver.
     - risksetMatrix: A matrix where rows refer to potential senders, columns to potential receivers x event_type, and entries give the respective dyad ids. 
     - start: An integer indicating the first row in the edgelist for which statistics have to be computed.
     - stop: An integer indicating the last row in the edgelist for which statistics have to be computed.

   Returns:
     A matrix with the exogenous actor statistics. Rows refer to the time points, and columns refer to the dyads.
*/

// [[Rcpp::export]]
arma::mat calculate_exo_actor(std::string type,
                              const arma::mat &edgelist,
                              const arma::mat &risksetMatrix,
                              const arma::mat &covariates,
                              int start, int stop,
                              bool display_progress,
                              Rcpp::String method = "pt")
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << type << " statistic" << std::endl;
  }

  // Slice the edgelist according to 'start' and 'stop'
  arma::mat slice = edgelist.rows(start, stop);

  // Time points: Depending on the method, get ...
  arma::vec time_points;
  if (method == "pt")
  {
    // ... the unique time points
    time_points = unique(slice.col(0));
  }
  else if (method == "pe")
  {
    // ... all event times
    time_points = slice.col(0);
  }

  // Initialize saving space
  arma::mat stat(time_points.n_elem, risksetMatrix.max() + 1,
                 arma::fill::zeros);

  // Get number of actors (N) and types (C) in the network
  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // First time point
  double time = time_points(0);

  // Loop over actors
  for (arma::uword k = 0; k < N; ++k)
  {
    // Find all the attribute values for actor k before the first timepoint
    arma::uvec indices = arma::find(covariates.col(0) == k &&
                                    covariates.col(1) <= time);
    arma::mat actor_covariates = covariates.rows(indices);

    // Find the last attribute value for actor k before the first timepoint
    double value = actor_covariates.col(1).max();

    // Add the value to the correct place in the stat
    if (type == "send")
    {
      arma::vec dyads = risksetMatrix.row(k).t();
      update_exo_actor(value, stat, dyads, 0);
    }
    if (type == "receive")
    {
      arma::vec dyads(risksetMatrix.n_rows * C);
      for (int c = 0; c < C; ++c)
      {
        arma::vec temp = risksetMatrix.col(k + (N * c));
        dyads.subvec(N * c, (N * (c + 1)) - 1) = temp;
      }
      update_exo_actor(value, stat, dyads, 0);
    }
  }

  // Find the times when the covariates change
  arma::vec changetimes = unique(covariates.col(1));

  // Start counter
  arma::uword counter = 0;

  // Loop over the time points
  for (arma::uword m = 0; m < time_points.n_elem; ++m)
  {
    // Copy the previous row
    if (m > 0)
    {
      stat.row(m) = stat.row(m - 1);
    }

    // Check if the statistic needs to be updated
    if (counter < changetimes.n_elem)
    {
      if (time_points(m) >= changetimes(counter))
      {
        // Update all changes in between
        while (counter < changetimes.n_elem &&
               time_points(m) >= changetimes(counter))
        {
          // Loop over actors
          for (arma::uword k = 0; k < N; ++k)
          {
            // Find all the attribute values for actor k at the change timepoint
            arma::uvec indices = arma::find(covariates.col(0) == k && covariates.col(1) == changetimes(counter));

            // Update if a new value exists
            if (indices.n_elem == 1)
            {
              double value = covariates(indices(0), 2);
              // Add the value to the correct place in the stat
              if (type == "send")
              {
                arma::vec dyads = risksetMatrix.row(k).t();
                update_exo_actor(value, stat, dyads, m);
              }
              if (type == "receive")
              {
                arma::vec dyads(risksetMatrix.n_rows * C);
                for (int c = 0; c < C; ++c)
                {
                  arma::vec temp = risksetMatrix.col(k + (N * c));
                  dyads.subvec(N * c, (N * (c + 1)) - 1) = temp;
                }
                update_exo_actor(value, stat, dyads, m);
              }
            }
          }

          // Update the counter
          counter += 1;
        }
      }
    }
  }

  return stat;
}
