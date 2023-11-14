#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
#include <stdexcept> // std::runtime_error
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include <map>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]

/* get_riskset

Source (originally): remify.

Create the risk set from all potential actors and event types in the network.

Param:
- actorID: vector with actor identification numbers
- typeID: vector with event type identification numbers
- directed: whether the events in the network are directed

Output: matrix with sender, receiver, event type, dyad ID
*/

// [[Rcpp::export]]
arma::mat get_riskset(arma::uvec actorID, arma::uvec typeID, bool directed)
{
  // Network information
  arma::uword N = actorID.n_elem;
  arma::uword C = typeID.n_elem;

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

/* convert_to_risksetMatrix

Converts the 'long' riskset format (sender, receiver, event type, dyad ID) to a 'wide' format.

Param:
- riskset: matrix with sender, receiver, event type, dyad ID
- N: number of actors in the network
- C: number of event types in the network

Output: matrix where rows refer to senders, columns refer to (receivers + N * event type) and entries refer to dyad IDs
*/

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

/* inertia_event_indices

Helper for calculate_inertia. Returns the indices for the events that should be used to calculate inertia, depending on the 'method' and 'memory' settings.

Note: This function assumes that when memory is set to 'full,' the current inertia row (i) is initialized with the values from the previous row (i - 1). Subsequently, this row is efficiently updated with new events corresponding to the indices obtained through this function. In other memory cases, the current inertia row starts empty and must be computed using events obtained from the indices provided by this function.
*/

arma::uvec inertia_event_indices(const arma::mat &edgelist,
                                 const arma::vec &time_points,
                                 int start, int i,
                                 const std::string &memory,
                                 const arma::vec &memory_value,
                                 const std::string &method)
{
  // Declare event_indices variable
  arma::uvec event_indices;

  // Get the current event time
  double current_time = time_points(i);

  if (memory == "full")
  {
    if (method == "pt")
    {
      // Declare the previous event time variable
      double previous_time = 0;
      if (i > 0)
      {
        // Get the previous event time if the index i is larger than 0
        previous_time = time_points(i - 1);
      }

      // Events that happened since the previous time point
      event_indices = arma::find(edgelist.col(0) >= previous_time && edgelist.col(0) < current_time);
    }
    else if (method == "pe")
    {
      if (i == 0 && start > 0)
      {
        // All previous events
        event_indices = arma::regspace<arma::uvec>(0, start - 1);
      }
      else if (i > 0)
      {
        // Only the one previous event
        event_indices.set_size(1);
        event_indices(0) = start + i - 1;
      }
    }
  }
  else if (memory == "interval")
  {
    // Define the interval times
    double min_time = current_time - memory_value(1);
    double max_time = current_time - memory_value(0);
    // Only update with events that happened between two time points
    event_indices = arma::find(edgelist.col(0) >= min_time &&
                               edgelist.col(0) < max_time);
  }
  else if (memory == "decay")
  {
    if (method == "pt")
    {
      // Update with events that happened before the current time
      event_indices = arma::find(edgelist.col(0) < current_time);
    }
    else if (method == "pe")
    {
      // Update with all previous events
      if (i > 0)
      {
        event_indices = arma::regspace<arma::uvec>(0, i - 1);
      }
    }
  }
  return event_indices;
}

/* get_decay_weights

Helper for calculate_inertia. Computes the decay weights for all events for a given time point.
*/
arma::vec get_decay_weights(double previous_time,
                            arma::uvec event_indices,
                            const arma::vec &weights,
                            const arma::mat &edgelist,
                            double mem_val)
{

  arma::vec decay_weights = weights;

  for (arma::uword j = 0; j < event_indices.n_elem; ++j)
  {
    arma::uword event = event_indices(j);
    double event_time = edgelist(event, 0);
    double event_weight = weights(event);
    double decay_weight = event_weight * exp(-(previous_time - event_time) * (log(2) / mem_val)) * (log(2) / mem_val);

    decay_weights(event) = decay_weight;
  }

  return decay_weights;
}

void update_inertia(arma::uvec event_indices, int i,
                    arma::mat &inertia,
                    const arma::mat &edgelist,
                    const arma::mat &risksetMatrix,
                    int N, int C,
                    const arma::vec &weights)
{
  for (arma::uword j = 0; j < event_indices.n_elem; ++j)
  {
    arma::uword event = event_indices(j);
    int actor1 = edgelist(event, 1);
    int actor2 = edgelist(event, 2);
    int event_type = 0;
    if (C > 1)
    {
      event_type = edgelist(event, 3);
    }
    arma::uword dyad = risksetMatrix(actor1, actor2 + (N * event_type));
    inertia(i, dyad) += weights(event);
  }
}

/* calculate_inertia

Calculates the inertia statistic (that is also used as a building block) where per time (in the rows) and per dyad (in the columns) the number of previous events is given.

Param:
- edgelist:
- weights:
- risksetMatrix:
- memory:
- memory_value:
- start:
- stop:
- display_progress:
- method:
*/

// [[Rcpp::export]]
arma::mat calculate_inertia(const arma::mat &edgelist,
                            const arma::vec &weights,
                            const arma::mat &risksetMatrix,
                            Rcpp::String memory,
                            const arma::vec &memory_value,
                            int start, int stop,
                            bool display_progress,
                            Rcpp::String method)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating inertia statistic/building block" << std::endl;
  }

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

  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // Initialize inertia
  arma::mat inertia(time_points.n_elem, risksetMatrix.max() + 1,
                    arma::fill::zeros);

  // Progress bar
  Progress p(time_points.n_elem, display_progress);

  // Calculate inertia
  for (arma::uword i = 0; i < time_points.n_elem; ++i)
  {
    // Indices of the events with which inertia is updated
    arma::uvec event_indices = inertia_event_indices(edgelist, time_points, start, i, memory, memory_value, method);

    if (memory == "full")
    {
      // Set the current row equal to the previous
      if (i > 0)
      {
        inertia.row(i) = inertia.row(i - 1);
      }
    }

    // Update inertia
    if (memory == "full" || memory == "interval")
    {
      update_inertia(event_indices, i, inertia, edgelist, risksetMatrix, N, C, weights);
    }
    else if (memory == "decay")
    {
      // Declare the previous event time variable
      double previous_time = 0;
      if (i == 0 && start > 0)
      {
        // Get the previous event time if start is larger than 0
        arma::vec event_times;
        if (method == "pt")
        {
          event_times = arma::unique(edgelist.col(0));
        }
        else if (method == "pe")
        {
          event_times = edgelist.col(0);
        }
        previous_time = arma::max(event_times.subvec(0, start - 1));
      }
      else if (i > 0)
      {
        // Get the previous event time if the index i is larger than 0
        previous_time = time_points(i - 1);
      }

      // Update decay weights
      arma::vec decay_weights = get_decay_weights(previous_time, event_indices, weights, edgelist, memory_value(0));

      // Update inertia
      update_inertia(event_indices, i, inertia, edgelist, risksetMatrix, N, C, decay_weights);
    }

    p.increment();
  }

  // Return
  return inertia;
}

/*
  Transform the inertia building block when event types are in the dependent variable, but consider_type is FALSE.
*/
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

  // Progress bar
  Progress p(N, display_progress);

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

      for (arma::uword d = 0; d < dyads_valid.size(); ++d)
      {
        int dyad = dyads_valid[d];
        new_inertia.col(dyad) = dyad_inertia;
      }
    }

    p.increment();
  }

  return new_inertia;
}

arma::mat calculate_degree_actor(int type, const arma::mat &inertia,
                                 const arma::mat &risksetMatrix,
                                 bool consider_type,
                                 bool display_progress)
{
  // Progress update
  std::string type_name;
  switch (type)
  {
  case 1:
    type_name = "indegreeSender";
    break;

  case 2:
    type_name = "indegreeReceiver";
    break;

  case 3:
    type_name = "outdegreeSender";
    break;

  case 4:
    type_name = "outdegreeReceiver";
    break;

  case 5:
    type_name = "totaldegreeSender";
    break;

  case 6:
    type_name = "totaldegreeReceiver";
    break;
  }

  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << type_name << " statistic" << std::endl;
  }

  // Initialize statistic
  arma::mat degree_stat(inertia.n_rows, inertia.n_cols, arma::fill::zeros);

  // Network information
  arma::uword N = risksetMatrix.n_rows;
  arma::uword C = risksetMatrix.n_cols / N;

  // Declare variables
  arma::vec dyads_col, dyads;
  arma::uvec exist, indices;
  arma::mat actor_inertia;
  arma::vec actor_degree;
  arma::vec saving_dyads;

  // Progress bar
  Progress p(N, display_progress);

  // Loop over actors
  for (arma::uword i = 0; i < N; ++i)
  {
    // Loop over event types
    for (arma::uword c = 0; c < C; ++c)
    {
      // Compute the actor's **indegree** value per time point
      if (type == 1 || type == 2 || type == 5 || type == 6)
      {
        // Find the dyads in which actor i is the receiver
        dyads_col = risksetMatrix.col(i + (N * c));
        exist = arma::find(dyads_col >= 0);
        dyads = dyads_col.elem(exist);

        // Compute actor i's indegree for each timepoint
        indices = arma::conv_to<arma::uvec>::from(dyads);
        actor_inertia = inertia.cols(indices);
        actor_degree = sum(actor_inertia, 1);

        // Find the dyads in which actor i is the receiver
        if (type == 2 || type == 6)
        {
          if (consider_type)
          {
            saving_dyads = dyads;
          }
          else
          {
            // Initialize an empty vector
            arma::vec temp;
            // Loop again over event types
            for (arma::uword k = 0; k < C; ++k)
            {
              arma::vec new_dyads = risksetMatrix.col(i + (N * k));
              temp = arma::join_cols(temp, new_dyads);
            }
            saving_dyads = temp;
          }
        }
        // Find the dyads in which actor i is the sender
        if (type == 1 || type == 5)
        {
          arma::vec temp = risksetMatrix.row(i).t();
          if (consider_type)
          {
            saving_dyads = temp.subvec(c * N, ((c + 1) * N) - 1);
          }
          else
          {
            saving_dyads = temp;
          }
        }

        // Saving actor i's indegree
        for (arma::uword j = 0; j < saving_dyads.n_elem; ++j)
        {
          int col_idx = saving_dyads(j);
          if (col_idx >= 0)
          {
            degree_stat.col(col_idx) += actor_degree;
          }
        }
      }

      // Compute the actor's outdegree value per time point
      if (type == 3 || type == 4 || type == 5 || type == 6)
      {
        // Find the dyads in which actor i is the sender
        arma::vec temp = risksetMatrix.row(i).t();
        dyads_col = temp.subvec(c * N, ((c + 1) * N) - 1);
        exist = arma::find(dyads_col >= 0);
        dyads = dyads_col.elem(exist);

        // Compute actor i's outdegree for each timepoint
        indices = arma::conv_to<arma::uvec>::from(dyads);
        actor_inertia = inertia.cols(indices);
        actor_degree = sum(actor_inertia, 1);

        // Find the dyads in which actor i is the sender
        if (type == 3 || type == 5)
        {
          if (consider_type)
          {
            saving_dyads = dyads;
          }
          else
          {
            saving_dyads = risksetMatrix.row(i).t();
          }
        }
        // Find the dyads in which actor i is the receiver
        if (type == 4 || type == 6)
        {
          if (consider_type)
          {
            saving_dyads = risksetMatrix.col(i + N * c);
          }
          else
          {
            // Initialize an empty vector
            arma::vec temp;
            // Loop again over event types
            for (arma::uword k = 0; k < C; ++k)
            {
              arma::vec new_dyads = risksetMatrix.col(i + (N * k));
              temp = arma::join_cols(temp, new_dyads);
            }
            saving_dyads = temp;
          }
        }

        // Saving actor i's outdegree
        for (arma::uword j = 0; j < saving_dyads.n_elem; ++j)
        {
          int col_idx = saving_dyads(j);
          if (col_idx >= 0)
          {
            degree_stat.col(col_idx) += actor_degree;
          }
        }
      }
    }

    p.increment();
  }

  return degree_stat;
}

arma::mat calculate_degree_dyad(int type, const arma::mat &inertia,
                                const arma::mat &risksetMatrix,
                                bool consider_type,
                                bool display_progress)
{
  // Progress update
  std::string type_name;
  switch (type)
  {
  case 1:
    type_name = "degreeMin";
    break;

  case 2:
    type_name = "degreeMax";
    break;

  case 3:
    type_name = "degreeDiff";
    break;

  case 4:
    type_name = "totaldegreeDyad";
    break;
  }

  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << type_name << " statistic" << std::endl;
  }

  // Degree first actor
  arma::mat totaldegreeSender = calculate_degree_actor(5, inertia, risksetMatrix, consider_type, false);
  // Degree second actor
  arma::mat totaldegreeReceiver = calculate_degree_actor(6, inertia, risksetMatrix, consider_type, false);

  arma::mat empty_degree; // Declare an empty matrix variable

  if (type == 1)
  {
    // Perform element-wise minimum operation
    arma::mat degreeMin = arma::min(totaldegreeSender, totaldegreeReceiver);
    return degreeMin;
  }
  else if (type == 2)
  {
    // Perform element-wise minimum operation
    arma::mat degreeMax = arma::max(totaldegreeSender, totaldegreeReceiver);
    return degreeMax;
  }
  else if (type == 3)
  {
    // Perform element-wise subtraction
    arma::mat degreeMin = arma::abs(totaldegreeSender - totaldegreeReceiver);
    ;
    return degreeMin;
  }
  else if (type == 4)
  {
    // Perform element-wise addition
    arma::mat totaldegreeDyad = totaldegreeSender + totaldegreeReceiver;
    return totaldegreeDyad;
  }
  else
  {
    Rcpp::Rcout << "Invalid type provided. Returning an empty matrix." << std::endl;
    empty_degree.set_size(inertia.n_rows, inertia.n_cols); // Set the size of the empty matrix
    return empty_degree;
  }
}

arma::mat calculate_reciprocity(const arma::mat &inertia,
                                const arma::mat &risksetMatrix,
                                bool consider_type,
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
        if (consider_type)
        {
          int this_dyad = risksetMatrix(s, r + (N * c));
          int rev_dyad = risksetMatrix(r, s + (N * c));
          if (this_dyad >= 0 && rev_dyad >= 0)
          {
            reciprocity.col(this_dyad) = inertia.col(rev_dyad);
          }
        }
        else
        {
          int this_dyad = risksetMatrix(s, r + (N * c));
          // Loop again over event 'types' to find existing reverse dyads
          for (int k = 0; k < C; ++k)
          {
            int rev_dyad = risksetMatrix(r, s + (N * k));
            if (this_dyad >= 0 && rev_dyad >= 0)
            {
              reciprocity.col(this_dyad) += inertia.col(rev_dyad);
            }
          }
        }
      }
    }

    p.increment();
  }

  // Return
  return reciprocity;
}

arma::mat calculate_triad(int type, const arma::mat &inertia,
                          const arma::mat &risksetMatrix,
                          Rcpp::String scaling,
                          bool consider_type,
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

  // Progress bar
  Progress p(N, display_progress);

  if (consider_type)
  {
    // Loop over 'senders'
    for (arma::uword s = 0; s < N; ++s)
    {
      // Loop over 'receivers'
      for (arma::uword r = 0; r < N; ++r)
      {
        // Loop over event 'types'
        for (arma::uword c = 0; c < C; ++c)
        {
          int this_dyad = risksetMatrix(s, r + (N * c));
          if (this_dyad < 0)
            continue;
          // Loop over 'third actors'
          for (arma::uword h = 0; h < N; ++h)
          {
            // Declare variables
            int path1_dyad = -1;
            int path2_dyad = -1;

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
            // sp
            case 5:
              int path1_actor1 = s;
              int path1_actor2 = h;
              if (h < s)
              {
                path1_actor1 = h;
                path1_actor2 = s;
              }
              int path2_actor1 = r;
              int path2_actor2 = h;
              if (h < r)
              {
                path2_actor1 = h;
                path2_actor2 = r;
              }
              // s -> h
              path1_dyad = risksetMatrix(path1_actor1, path1_actor2 + (N * c));
              // r -> h
              path2_dyad = risksetMatrix(path2_actor1, path2_actor2 + (N * c));
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
  }
  else
  {
    // Declare variables
    arma::vec path1_dyads(C), path2_dyads(C);

    // Loop over 'senders'
    for (arma::uword s = 0; s < N; ++s)
    {
      // Loop over 'receivers'
      for (arma::uword r = 0; r < N; ++r)
      {
        // Loop over event 'types'
        for (arma::uword c = 0; c < C; ++c)
        {
          int this_dyad = risksetMatrix(s, r + (N * c));
          if (this_dyad < 0)
            continue;
          // Loop over 'third actors'
          for (arma::uword h = 0; h < N; ++h)
          {
            if (h == s || h == r)
              continue;

            // get path dyads
            switch (type)
            {
            // otp
            case 1:
              // Loop over event types
              for (arma::uword k = 0; k < C; ++k)
              {
                // s -> h
                path1_dyads(k) = risksetMatrix(s, h + (N * k));
                // h -> r
                path2_dyads(k) = risksetMatrix(h, r + (N * k));
              }
              break;
            // itp
            case 2:
              // Loop over event types
              for (arma::uword k = 0; k < C; ++k)
              {
                // r -> h
                path1_dyads(k) = risksetMatrix(r, h + (N * k));
                // h -> s
                path2_dyads(k) = risksetMatrix(h, s + (N * k));
              }
              break;
            // osp
            case 3:
              // Loop over event types
              for (arma::uword k = 0; k < C; ++k)
              {
                // s -> h
                path1_dyads(k) = risksetMatrix(s, h + (N * k));
                // r -> h
                path2_dyads(k) = risksetMatrix(r, h + (N * k));
              }
              break;
            // isp
            case 4:
              // Loop over event types
              for (arma::uword k = 0; k < C; ++k)
              {
                // h -> s
                path1_dyads(k) = risksetMatrix(h, s + (N * k));
                // h -> r
                path2_dyads(k) = risksetMatrix(h, r + (N * k));
              }
              break;
            // sp (! undirected events assumed)
            case 5:
              int path1_actor1 = s;
              int path1_actor2 = h;
              if (h < s)
              {
                path1_actor1 = h;
                path1_actor2 = s;
              }
              int path2_actor1 = r;
              int path2_actor2 = h;
              if (h < r)
              {
                path2_actor1 = h;
                path2_actor2 = r;
              }
              // Loop over event types
              for (arma::uword k = 0; k < C; ++k)
              {
                // s -> h
                path1_dyads(k) = risksetMatrix(path1_actor1, path1_actor2 + (N * k));
                // r -> h
                path2_dyads(k) = risksetMatrix(path2_actor1, path2_actor2 + (N * k));
              }
            }

            // Loop over event types
            arma::mat path1_events(inertia.n_rows, 1);
            arma::mat path2_events(inertia.n_rows, 1);

            for (arma::uword k = 0; k < C; ++k)
            {
              // get path events
              if (path1_dyads(k) >= 0)
              {
                path1_events += inertia.col(path1_dyads(k));
              }
              if (path2_dyads(k) >= 0)
              {
                path2_events += inertia.col(path2_dyads(k));
              }
            }

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
        event_indices.set_size(1);
        event_indices(0) = last_event;
      }
    }
    else
    {
      event_indices.set_size(1);
      event_indices(0) = start + i - 1;
    }
  }
  return event_indices;
}

arma::mat calculate_pshift(std::string type, const arma::mat &edgelist,
                           const arma::mat &risksetMatrix,
                           int start, int stop, bool directed,
                           bool consider_type,
                           bool display_progress,
                           Rcpp::String method)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating pshift " << type << " statistic" << std::endl;
  }

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
        if (consider_type)
        {
          int dyad = risksetMatrix(receiver, sender + event_type * N);
          if (dyad >= 0)
          {
            pshift(i, dyad) = 1;
          }
        }
        else
        {
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            int dyad = risksetMatrix(receiver, sender + k * N);
            if (dyad >= 0)
            {
              pshift(i, dyad) = 1;
            }
          }
        }
      }

      if (type == "AB-BY")
      {
        if (consider_type)
        {
          arma::vec temp = risksetMatrix.row(receiver).t();
          arma::vec dyads = temp.subvec(event_type * N,
                                        ((event_type + 1) * N) - 1);
          dyads.shed_row(sender);
          process_pshift(dyads, pshift, i);
        }
        else
        {
          arma::vec dyads = risksetMatrix.row(receiver).t();
          arma::uvec shed_indices(C);
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            shed_indices(k) = sender + k * N;
          }
          dyads.shed_rows(shed_indices);
          process_pshift(dyads, pshift, i);
        }
      }

      if (type == "AB-XA")
      {
        if (consider_type)
        {
          arma::vec dyads = risksetMatrix.col(sender + event_type * N);
          dyads.shed_row(receiver);
          process_pshift(dyads, pshift, i);
        }
        else
        {
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            arma::vec dyads = risksetMatrix.col(sender + k * N);
            dyads.shed_row(receiver);
            process_pshift(dyads, pshift, i);
          }
        }
      }

      if (type == "AB-XB")
      {
        if (consider_type)
        {
          arma::vec dyads = risksetMatrix.col(receiver + event_type * N);
          dyads.shed_row(sender);
          process_pshift(dyads, pshift, i);
        }
        else
        {
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            arma::vec dyads = risksetMatrix.col(receiver + k * N);
            dyads.shed_row(sender);
            process_pshift(dyads, pshift, i);
          }
        }
      }

      if (type == "AB-XY")
      {
        if (consider_type)
        {
          arma::mat dyadsMat = risksetMatrix.cols(event_type * N,
                                                  ((event_type + 1) * N) - 1);
          arma::uvec idx = {sender, receiver};
          dyadsMat.shed_rows(idx);
          dyadsMat.shed_cols(idx);
          arma::vec dyads = dyadsMat.as_col();
          process_pshift(dyads, pshift, i);
        }
        else
        {
          arma::mat dyadsMat = risksetMatrix;
          arma::uvec row_idx = {sender, receiver};
          dyadsMat.shed_rows(row_idx);
          arma::uvec col_idx(C * 2);
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            col_idx(k) = sender + k * N;
            col_idx(k + C) = receiver + k * N;
          }
          dyadsMat.shed_cols(col_idx);
          arma::vec dyads = dyadsMat.as_col();
          process_pshift(dyads, pshift, i);
        }
      }

      if (type == "AB-AY")
      {
        if (consider_type)
        {
          // All events with actor A as sender (first actor)
          arma::vec temp = risksetMatrix.row(sender).t();
          arma::vec dyads = temp.subvec(event_type * N,
                                        ((event_type + 1) * N) - 1);
          dyads.shed_row(receiver);
          process_pshift(dyads, pshift, i);
          if (!directed)
          {
            // All events with actor A as second actor
            dyads = risksetMatrix.col(sender + event_type * N);
            dyads.shed_row(receiver);
            process_pshift(dyads, pshift, i);
            // All events with actor B as first actor
            temp = risksetMatrix.row(receiver).t();
            dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
            dyads.shed_row(sender);
            process_pshift(dyads, pshift, i);
            // All events with actor B as second actor
            dyads = risksetMatrix.col(receiver + event_type * N);
            dyads.shed_row(sender);
            process_pshift(dyads, pshift, i);
          }
        }
        else
        {
          // All events with actor A as sender (first actor)
          arma::vec dyads = risksetMatrix.row(sender).t();
          arma::uvec shed_indices(C);
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            shed_indices(k) = receiver + k * N;
          }
          dyads.shed_rows(shed_indices);
          process_pshift(dyads, pshift, i);
          if (!directed)
          {
            // All events with actor A as second actor
            for (int k = 0; k < C; ++k)
            {
              arma::vec dyads = risksetMatrix.col(sender + k * N);
              dyads.shed_row(receiver);
              process_pshift(dyads, pshift, i);
            }
            // All events with actor B as first actor
            dyads = risksetMatrix.row(receiver).t();
            for (int k = 0; k < C; ++k)
            {
              shed_indices(k) = sender + k * N;
            }
            dyads.shed_rows(shed_indices);
            process_pshift(dyads, pshift, i);
            // All events with actor B as second actor
            for (int k = 0; k < C; ++k)
            {
              arma::vec dyads = risksetMatrix.col(receiver + k * N);
              dyads.shed_row(sender);
              process_pshift(dyads, pshift, i);
            }
          }
        }
      }

      if (type == "AB-AB")
      {
        if (consider_type)
        {
          int dyad = risksetMatrix(sender, receiver + event_type * N);
          if (dyad >= 0)
          {
            pshift(i, dyad) = 1;
          }
        }
        else
        {
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            int dyad = risksetMatrix(sender, receiver + k * N);
            if (dyad >= 0)
            {
              pshift(i, dyad) = 1;
            }
          }
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

arma::mat calculate_recency(std::string type, const arma::mat &edgelist,
                            const arma::mat &risksetMatrix,
                            int start, int stop, bool consider_type,
                            bool display_progress,
                            Rcpp::String method)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << type << " statistic" << std::endl;
  }

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
      if (consider_type)
      {
        int dyad = risksetMatrix(sender, receiver + event_type * N);
        if (dyad >= 0)
        {
          lastActive(dyad) = time;
        }
      }
      else
      {
        // Loop over event types
        for (int k = 0; k < C; ++k)
        {
          int dyad = risksetMatrix(sender, receiver + k * N);
          if (dyad >= 0)
          {
            lastActive(dyad) = time;
          }
        }
      }
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
        for (int k = 0; k < C; ++k)
        {
          arma::vec temp = risksetMatrix.col(sender + k * N);
          dyads = arma::join_cols(dyads, temp);
        }
        update_lastActive(time, dyads, lastActive);
      }
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
        for (int k = 0; k < C; ++k)
        {
          arma::vec temp = risksetMatrix.col(receiver + k * N);
          dyads = arma::join_cols(dyads, temp);
        }
        update_lastActive(time, dyads, lastActive);
      }
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
      event_indices.set_size(1);
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
        if (consider_type)
        {
          int dyad = risksetMatrix(sender, receiver + event_type * N);
          if (dyad >= 0)
          {
            lastActive(dyad) = time;
          }
        }
        else
        {
          // Loop over event types
          for (int k = 0; k < C; ++k)
          {
            int dyad = risksetMatrix(sender, receiver + k * N);
            if (dyad >= 0)
            {
              lastActive(dyad) = time;
            }
          }
        }
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
          for (int k = 0; k < C; ++k)
          {
            arma::vec temp = risksetMatrix.col(sender + k * N);
            dyads = arma::join_cols(dyads, temp);
          }
          update_lastActive(time, dyads, lastActive);
        }
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
          for (int k = 0; k < C; ++k)
          {
            arma::vec temp = risksetMatrix.col(receiver + k * N);
            dyads = arma::join_cols(dyads, temp);
          }
          update_lastActive(time, dyads, lastActive);
        }
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

arma::mat calculate_rrank(int type, const arma::mat &edgelist,
                          const arma::mat &riskset, int N, int C,
                          int start, int stop,
                          bool consider_type, bool display_progress,
                          Rcpp::String method)
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
      for (int j = 0; j < N; ++j)
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
      event_indices.set_size(1);
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

arma::mat calculate_exo_actor(std::string type,
                              const arma::mat &edgelist,
                              const arma::mat &risksetMatrix,
                              const arma::mat &covariates,
                              int start, int stop,
                              bool display_progress,
                              Rcpp::String method)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << type << " statistic" << std::endl;
  }

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

  // Initialize saving space
  arma::mat stat(time_points.n_elem, risksetMatrix.max() + 1,
                 arma::fill::zeros);

  // Get number of actors (N) and types (C) in the network
  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // First time point
  double time = time_points(0);

  // Loop over actors
  for (int k = 0; k < N; ++k)
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
          for (int k = 0; k < N; ++k)
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

arma::mat calculate_exo_dyad(std::string type,
                             const arma::mat &edgelist,
                             const arma::mat &riskset,
                             const arma::mat &covariates,
                             int start, int stop,
                             bool display_progress,
                             Rcpp::String method)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating " << type << " statistic" << std::endl;
  }

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

  // Storage space for the current covariate values
  arma::vec current_ac1(riskset.n_rows, arma::fill::zeros);
  arma::vec current_ac2(riskset.n_rows, arma::fill::zeros);

  // Initialize saving space
  arma::mat stat(time_points.n_elem, riskset.n_rows, arma::fill::zeros);

  // First time point
  double time = time_points(0);

  // Loop over dyads
  for (arma::uword i = 0; i < riskset.n_rows; ++i)
  {
    // Find the relevant actors
    arma::uword actor1 = riskset(i, 0);
    arma::uword actor2 = riskset(i, 1);

    // All covariate values with 'actor1' before the current 'time' value
    arma::uvec actor1_indices = arma::find(covariates.col(0) == actor1 && covariates.col(1) <= time);
    arma::mat actor1_values = covariates.rows(actor1_indices);
    // The most recent covariate value with 'actor1' before the current 'time'
    arma::uword max_time_index_actor1 = index_max(actor1_values.col(1));
    current_ac1(i) = actor1_values(max_time_index_actor1, 2);

    // All covariate values with 'actor2' before the current 'time' value
    arma::uvec actor2_indices = arma::find(covariates.col(0) == actor2 && covariates.col(1) <= time);
    arma::mat actor2_values = covariates.rows(actor2_indices);
    // The most recent covariate value with 'actor2' before the current 'time'
    arma::uword max_time_index_actor2 = index_max(actor2_values.col(1));
    current_ac2(i) = actor2_values(max_time_index_actor2, 2);

    // Are these values equal?
    if (type == "same")
    {
      stat(0, i) = (current_ac1(i) == current_ac2(i));
    }
    // What is the difference between these values?
    if (type == "difference")
    {
      stat(0, i) = current_ac1(i) - current_ac2(i);
    }

    arma::vec both = {current_ac1(i), current_ac2(i)};
    // What is the mean value?
    if (type == "average")
    {
      stat(0, i) = mean(both);
    }
    // What is the minimum value?
    if (type == "minimum")
    {
      stat(0, i) = min(both);
    }
    // What is the maximum value?
    if (type == "maximum")
    {
      stat(0, i) = max(both);
    }
  }

  // Find the times when the covariates change
  arma::vec changetimes = arma::unique(covariates.col(1));

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
          // Loop over dyads
          for (arma::uword i = 0; i < riskset.n_rows; ++i)
          {
            // Find the relevant actors
            arma::uword actor1 = riskset(i, 0);
            arma::uword actor2 = riskset(i, 1);

            // All covariate values with 'actor1' before the current 'time' value
            arma::uvec actor1_indices = arma::find(covariates.col(0) == actor1 && covariates.col(1) == changetimes(counter));

            // All covariate values with 'actor2' before the current 'time' value
            arma::uvec actor2_indices = arma::find(covariates.col(0) == actor2 && covariates.col(1) == changetimes(counter));

            // Update if a new value exists
            if ((actor1_indices.n_elem == 1) || (actor2_indices.n_elem == 1))
            {
              if (actor1_indices.n_elem == 1)
              {
                current_ac1(i) = covariates(actor1_indices(0), 2);
              }
              if (actor2_indices.n_elem == 1)
              {
                current_ac2(i) = covariates(actor2_indices(0), 2);
              }

              // Are these values equal?
              if (type == "same")
              {
                stat(m, i) = (current_ac1(i) == current_ac2(i));
              }
              // What is the difference between these values?
              if (type == "difference")
              {
                stat(m, i) = current_ac1(i) - current_ac2(i);
              }

              arma::dvec both = {current_ac1(i), current_ac2(i)};
              // What is the mean value?
              if (type == "average")
              {
                stat(m, i) = mean(both);
              }
              // What is the minimum value?
              if (type == "minimum")
              {
                stat(m, i) = min(both);
              }
              // What is the maximum value?
              if (type == "maximum")
              {
                stat(m, i) = max(both);
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

/**
    Calculates tie statistics

    @param covariates Matrix containing covariate information (actor1, actor2, time, value).
    @param edgelist Matrix representing edges between actors at specific time points (time, actor1, actor2).
    @param risksetMatrix  A matrix where rows refer to potential senders, columns to potential receivers x event_type, and entries give the respective dyad ids.
    @param start Starting index for the tie statistic calculation.
    @param stop Ending index for the tie statistic calculation.
    @return Matrix containing tie statistics for the specified range of indices.
*/
arma::mat calculate_exo_tie(const arma::mat &covariates,
                            const arma::mat &edgelist,
                            const arma::mat &risksetMatrix,
                            int start, int stop,
                            bool display_progress,
                            Rcpp::String method)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating tie statistic" << std::endl;
  }

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

  // Initialize saving space
  arma::mat stat(time_points.n_elem, risksetMatrix.max() + 1,
                 arma::fill::zeros);

  // Get number of actors (N) and types (C) in the network
  int N = risksetMatrix.n_rows;
  int C = risksetMatrix.n_cols / N;

  // Covariate time points
  arma::vec covar_times = arma::unique(covariates.col(2));

  // Progress bar
  Progress p(covar_times.n_elem, display_progress);

  // Loop over unique covariates time points
  for (arma::uword t = 0; t < covar_times.n_elem; ++t)
  {

    double time = covar_times(t);
    // Covariates values with this timepoint
    arma::uvec covar_indices = arma::find(covariates.col(2) == time);
    // Saving indices
    arma::uvec saving_indices = arma::find(time_points >= time);

    // Loop over covar_indices
    for (arma::uword i = 0; i < covar_indices.n_elem; ++i)
    {
      int index = covar_indices(i);
      int actor1 = covariates(index, 0);
      int actor2 = covariates(index, 1);
      double value = covariates(index, 3);

      // Loop over event types
      for (int k = 0; k < C; ++k)
      {
        int dyad = risksetMatrix(actor1, actor2 + (N * k));

        if (dyad >= 0)
        {
          arma::uvec dyad_index = arma::uvec({static_cast<arma::uword>(dyad)});
          stat.submat(saving_indices, dyad_index).fill(value);
        }
      }
    }

    p.increment();
  }

  return stat;
}

arma::mat calculate_exo_event(const arma::mat &covariates, const arma::mat &edgelist, const arma::mat &riskset, int start, int stop, bool display_progress, Rcpp::String method)
{

  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating event statistic" << std::endl;
  }

  // Event time points: Depending on the method, get ...
  arma::vec event_times;
  if (method == "pt")
  {
    // ... the unique time points
    event_times = arma::unique(edgelist.col(0));
  }
  else if (method == "pe")
  {
    // ... all event times
    event_times = edgelist.col(0);
  }
  event_times = event_times.subvec(start, stop);

  // Repeat covariates matrix along columns to match riskset's rows
  arma::mat covRepeat = arma::repmat(covariates.rows(start, stop), 1, riskset.n_rows);

  // Check dimensions of input matrices
  if (covRepeat.n_rows != event_times.n_elem)
  {
    throw std::invalid_argument("Invalid dimensions: mismatch between 'event' covariate object and number of event times.");
  }

  return covRepeat;
}

// get_user_stat
arma::mat get_userstat(const arma::mat &covariates, const arma::mat &edgelist,
                       int start, int stop, bool display_progress,
                       Rcpp::String method)
{
  // Progress update
  if (display_progress)
  {
    Rcpp::Rcout << "Calculating userstat statistic" << std::endl;
  }

  // Event time points: Depending on the method, get ...
  arma::vec event_times;
  if (method == "pt")
  {
    // ... the unique time points
    event_times = arma::unique(edgelist.col(0));
  }
  else if (method == "pe")
  {
    // ... all event times
    event_times = edgelist.col(0);
  }
  event_times = event_times.subvec(start, stop);

  arma::mat stat = covariates.rows(start, stop);

  // Check dimensions of input matrices
  if (stat.n_rows != event_times.n_elem)
  {
    throw std::invalid_argument("Invalid dimensions: mismatch between 'userStat' covariate object and number of event times.");
  }

  return (stat);
}

arma::mat calculate_FEtype(const arma::mat &covariates,
                           const arma::mat &edgelist,
                           const arma::mat &riskset,
                           int start, int stop,
                           Rcpp::String method)
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

  // Initialize saving space
  arma::mat stat(time_points.n_elem, riskset.n_rows, arma::fill::zeros);

  // For loop over dyads
  for (arma::uword i = 0; i < riskset.n_rows; ++i)
  {
    arma::colvec val(time_points.n_elem, arma::fill::zeros);
    if (riskset(i, 2) == covariates(0, 0))
    {
      val.ones();
    }
    stat.col(i) = val;
  }

  // Output
  return (stat);
}

int getEffectNumber(std::string effect)
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
arma::mat standardize(arma::mat stat)
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

arma::mat normalize_degree(arma::mat stat, const arma::mat &inertia, int N,
                           int effect, int start)
{
  // Calculate row sums of inertia
  arma::vec rowSum = arma::sum(inertia, 1);
  if (effect == 115 || effect == 116 || effect == 117)
  {
    // Calculate two times row sums of inertia
    rowSum *= 2;
  }
  // Element-wise division
  stat.each_col() /= rowSum;
  // Replace NaN values with 0
  stat.replace(arma::datum::nan, 0);

  // First row
  if (start == 0)
  {
    stat.row(0).fill(1.0 / N);
  }

  return stat;
}

arma::mat normalize_inertia(arma::mat stat, const arma::mat &inertia,
                            const arma::mat &risksetMatrix, int N, bool consider_type)
{
  // Outdegree of the sender
  arma::mat degree = calculate_degree_actor(3, inertia, risksetMatrix, consider_type, false);
  // Element-wise division in-place
  stat /= degree;
  // Replace NaN values
  stat.replace(arma::datum::nan, 1.0 / (N - 1.0));

  return stat;
}

arma::mat normalize_reciprocity(arma::mat stat, const arma::mat &inertia,
                                const arma::mat &risksetMatrix, int N, bool consider_type)
{
  // Indegree of the sender
  arma::mat degree = calculate_degree_actor(1, inertia, risksetMatrix, consider_type, false);
  // Element-wise division in-place
  stat /= degree;
  // Replace NaN values
  stat.replace(arma::datum::nan, 1.0 / (N - 1.0));

  return stat;
}

//[[Rcpp::export]]
arma::cube compute_stats_tie(Rcpp::CharacterVector effects,
                             const arma::mat &edgelist,
                             const arma::mat &riskset,
                             const arma::mat &risksetMatrix,
                             const arma::mat &inertia,
                             const Rcpp::List &covariates,
                             const Rcpp::List &interactions,
                             Rcpp::String memory,
                             const arma::vec &memory_value,
                             Rcpp::CharacterVector &scaling,
                             Rcpp::LogicalVector &consider_type,
                             int start, int stop,
                             bool directed,
                             bool display_progress,
                             Rcpp::String method)
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
  int C = risksetMatrix.n_cols / N;

  // Initialize saving space
  arma::cube stats(time_points.n_elem, riskset.n_rows, effects.size());

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
      stats.slice(i) = calculate_FEtype(covariates[i], edgelist, riskset, start, stop, method);
      break;

    // send
    case 11:
      // Compute statistic
      stats.slice(i) = calculate_exo_actor("send", edgelist, risksetMatrix, covariates[i], start, stop, display_progress, method);
      break;

    // receive
    case 12:
      // Compute statistic
      stats.slice(i) = calculate_exo_actor("receive", edgelist, risksetMatrix, covariates[i], start, stop, display_progress, method);
      break;

    // tie
    case 13:
      // Compute statistic
      stats.slice(i) = calculate_exo_tie(covariates[i], edgelist, risksetMatrix, start, stop, display_progress, method);
      break;

    // same
    case 14:
      // Compute statistic
      stats.slice(i) = calculate_exo_dyad("same", edgelist, riskset, covariates[i], start, stop, display_progress, method);
      break;

    // difference
    case 15:
      // Compute statistic
      stats.slice(i) = calculate_exo_dyad("difference", edgelist, riskset, covariates[i], start, stop, display_progress, method);
      // Absolute values
      if ((scaling(i) == "none_abs") || (scaling(i) == "std_abs"))
      {
        stats.slice(i) = abs(stats.slice(i));
      }
      break;

    // average
    case 16:
      // Compute statistic
      stats.slice(i) = calculate_exo_dyad("average", edgelist, riskset, covariates[i], start, stop, display_progress, method);
      break;

    // minimum
    case 17:
      // Compute statistic
      stats.slice(i) = calculate_exo_dyad("minimum", edgelist, riskset, covariates[i], start, stop, display_progress, method);
      break;

    // maximum
    case 18:
      // Compute statistic
      stats.slice(i) = calculate_exo_dyad("maximum", edgelist, riskset, covariates[i], start, stop, display_progress, method);
      break;

    // event
    case 19:
      // Compute statistic
      stats.slice(i) = calculate_exo_event(covariates[i], edgelist, riskset, start, stop, display_progress, method);
      break;

    // inertia
    case 101:
      // Compute statistic
      if (consider_type(i))
      {
        stats.slice(i) = inertia;
      }
      else
      {
        stats.slice(i) = transform_inertia(inertia, risksetMatrix, display_progress);
      }
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_inertia(stats.slice(i), inertia, risksetMatrix, N, consider_type(i));
      }
      break;

    // reciprocity
    case 102:
      // Compute statistic
      stats.slice(i) = calculate_reciprocity(inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_reciprocity(stats.slice(i), inertia, risksetMatrix, N, consider_type(i));
      }
      break;

    // indegreeSender
    case 111:
      // Compute statistic
      stats.slice(i) = calculate_degree_actor(1, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // indegreeReceiver
    case 112:
      // Compute statistic
      stats.slice(i) = calculate_degree_actor(2, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // outdegreeSender
    case 113:
      // Compute statistic
      stats.slice(i) = calculate_degree_actor(3, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // outdegreeReceiver
    case 114:
      // Compute statistic
      stats.slice(i) = calculate_degree_actor(4, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // totaldegreeSender
    case 115:
      // Compute statistic
      stats.slice(i) = calculate_degree_actor(5, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // totaldegreeReceiver
    case 116:
      // Compute statistic
      stats.slice(i) = calculate_degree_actor(6, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // totaldegreeDyad
    case 117:
      // Compute statistic
      stats.slice(i) = calculate_degree_dyad(4, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // degreeMin
    case 118:
      // Compute statistic
      stats.slice(i) = calculate_degree_dyad(1, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // degreeMax
    case 119:
      // Compute statistic
      stats.slice(i) = calculate_degree_dyad(2, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // degreeDiff
    case 120:
      // Compute statistic
      stats.slice(i) = calculate_degree_dyad(3, inertia, risksetMatrix, consider_type(i), display_progress);
      // Proportional scaling
      if (scaling(i) == "prop")
      {
        stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
      }
      break;

    // otp
    case 131:
      // Compute statistic
      stats.slice(i) = calculate_triad(1, inertia, risksetMatrix, scaling(i), consider_type(i), display_progress);
      break;

    // itp
    case 132:
      // Compute statistic
      stats.slice(i) = calculate_triad(2, inertia, risksetMatrix, scaling(i), consider_type(i), display_progress);
      break;

    // osp
    case 133:
      // Compute statistic
      stats.slice(i) = calculate_triad(3, inertia, risksetMatrix, scaling(i), consider_type(i), display_progress);
      break;

    // isp
    case 134:
      // Compute statistic
      stats.slice(i) = calculate_triad(4, inertia, risksetMatrix, scaling(i), consider_type(i), display_progress);
      break;

      // sp
    case 135:
      // Compute statistic
      stats.slice(i) = calculate_triad(5, inertia, risksetMatrix, scaling(i), consider_type(i), display_progress);
      break;

    // psABBA
    case 141:
      // Compute statistic
      stats.slice(i) = calculate_pshift("AB-BA", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
      break;

    // psABBY
    case 142:
      // Compute statistic
      stats.slice(i) = calculate_pshift("AB-BY", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
      break;

    // psABXA
    case 143:
      // Compute statistic
      stats.slice(i) = calculate_pshift("AB-XA", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
      break;

    // psABXB
    case 144:
      // Compute statistic
      stats.slice(i) = calculate_pshift("AB-XB", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
      break;

    // psABXY
    case 145:
      // Compute statistic
      stats.slice(i) = calculate_pshift("AB-XY", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
      break;

    // psABAY
    case 146:
      // Compute statistic
      stats.slice(i) = calculate_pshift("AB-AY", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
      break;

    // psABAB
    case 147:
      // Compute statistic
      stats.slice(i) = calculate_pshift("AB-AB", edgelist, risksetMatrix, start, stop, directed, consider_type(i), display_progress, method);
      break;

    // rrankSend
    case 151:
      // Compute statistic
      stats.slice(i) = calculate_rrank(1, edgelist, riskset, N, C, start, stop, consider_type(i), display_progress, method);
      break;

    // rrankReceive
    case 152:
      // Compute statistic
      stats.slice(i) = calculate_rrank(2, edgelist, riskset, N, C, start, stop, consider_type(i), display_progress, method);
      break;

    // recencyContinue
    case 161:
      // Compute statistic
      stats.slice(i) = calculate_recency("recencyContinue", edgelist, risksetMatrix, start, stop, consider_type(i), display_progress, method);
      break;

    // recencySendSender
    case 162:
      // Compute statistic
      stats.slice(i) = calculate_recency("recencySendSender", edgelist, risksetMatrix, start, stop, consider_type(i), display_progress, method);
      break;

    // recencySendReceiver
    case 163:
      // Compute statistic
      stats.slice(i) = calculate_recency("recencySendReceiver", edgelist, risksetMatrix, start, stop, consider_type(i), display_progress, method);
      break;

    // recencyReceiveSender
    case 164:
      // Compute statistic
      stats.slice(i) = calculate_recency("recencyReceiveSender", edgelist, risksetMatrix, start, stop, consider_type(i), display_progress, method);
      break;

    // recencyReceiveReceiver
    case 165:
      // Compute statistic
      stats.slice(i) = calculate_recency("recencyReceiveReceiver", edgelist, risksetMatrix, start, stop, consider_type(i), display_progress, method);
      break;

    // userStat
    case 888:
      stats.slice(i) = get_userstat(covariates[i], edgelist, start, stop, display_progress, method);
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

    // Standardize
    if (scaling(i) == "std" || scaling(i) == "std_abs" || scaling(i) == "std_unique")
    {
      stats.slice(i) = standardize(stats.slice(i));
    }
  }

  return stats;
}