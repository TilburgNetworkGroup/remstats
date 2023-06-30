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

// Helper function to check if a value is null
inline bool isNull(int value)
{
  return value == NA_INTEGER;
}

/*
   Function: getDyadIDs

   Description: Filters a given riskset matrix based on the provided actor, type, and directed parameters. Returns a vector of dyad IDs that match the filtering criteria.

   Parameters:
     - riskset: A matrix representing the riskset data. Each row contains information about a dyad: actors involved, type and dyad ID.
     - actor1: An optional parameter specifying the first actor. If provided, the function filters the riskset to include only dyads where actor1 is involved.
     - actor2: An optional parameter specifying the second actor. If provided, the function filters the riskset to include only dyads where actor2 is involved.
     - type: An optional parameter specifying the type. If provided, the function filters the riskset to include only dyads of the specified type.
     - directed: A boolean parameter indicating whether the events are directed. If set to true, the function considers the direction of dyads during filtering.

   Returns:
     An IntegerVector containing the dyad IDs that match the filtering criteria.
*/
IntegerVector getDyadIDs(const arma::mat &riskset, int actor1 = NA_INTEGER, int actor2 = NA_INTEGER, int type = NA_INTEGER, bool directed = true)
{
  int numRows = riskset.n_rows;
  IntegerVector dyadIDs;

  for (int i = 0; i < numRows; i++)
  {
    bool match = true;

    if (!isNull(actor1))
    {
      int actor1ID = actor1;
      if (!directed && riskset(i, 0) != actor1ID && riskset(i, 1) != actor1ID)
      {
        match = false;
      }
      else if (directed && riskset(i, 0) != actor1ID)
      {
        match = false;
      }
    }

    if (!isNull(actor2))
    {
      int actor2ID = actor2;
      if (directed && riskset(i, 1) != actor2ID)
      {
        match = false;
      }
      else if (!directed && riskset(i, 1) != actor2ID && riskset(i, 0) != actor2ID)
      {
        match = false;
      }
    }

    if (!isNull(type))
    {
      int typeID = type;
      if (riskset(i, 2) != typeID)
      {
        match = false;
      }
    }

    if (match)
    {
      dyadIDs.push_back(riskset(i, 3));
    }
  }

  if (dyadIDs.size() == 0)
  {
    return Rcpp::IntegerVector::create(-999); // Return -999 if no matches were found
  }

  return dyadIDs;
}

// @title getRisksetMatrix (obtain permutations of actors' ids and event types).
//
// @param actorID vector of actors' id's.
// @param typeID vector of types' id's.
// @param N number of actors in the dataset.
// @param C number of event types
// @param directed boolean value: are events directed (1) or undirected (0)?
// source: remify
//
// @return matrix of possible dyadic events.
// [[Rcpp::export]]
arma::mat getRisksetMatrix(arma::uvec actorID, arma::uvec typeID,
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

// compute_adjmat
//
// Helper function that computes the adjacency matrix.
//
// Computes at each time point for every potential relational event, i.e.,
// potential edge, in the risk set its weight based on the past. Can account
// for memory effects by setting the `memory` argument together with the
// `memory_value` argument.
//
// *param [edgelist] matrix with the observed relational event history. Rows
// refers to the observed relational events. The first column refers to the
// time, the second column to the events and the third column to the event
// weight.
// *param [N] integer number referring to the total number of actors.
// *param [D] integer number referring to the total number of potential
// relational events in the risk set.
// *param [directed] boolean whether the events are directed
// *param [memory] integer number indicating the type of memory effect, i.e.,
// how past events influence future events/should be accounted for in the
// adjacency matrix count. 0 = full history, 1 = windowed memory, 2 =
// Brandes-type memory, i.e., exponential decay with a half-life parameter
// (see also memory_value).
// *param [memory_value] numeric value indicating the memory parameter, i.e.,
// the window width if memory = 1, and the half-life time if memory = 2
//
// *return [adjmat] adjacency matrix with per row the number of past events for
// the respective dyads in the columns
//
// [[Rcpp::export]]
arma::mat compute_adjmat(const arma::mat &edgelist, int D, bool directed,
                         Rcpp::String memory, arma::vec memory_value, int start,
                         int stop)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize memory and fill with zeros
  arma::mat adjmat(slice.n_rows, D, arma::fill::zeros);

  // Full memory
  if (memory == "full")
  {
    // (1) Initialize adjacency matrix
    // Select the past
    double time = slice(0, 0);
    arma::uvec pastkey = arma::find(edgelist.col(0) < time);
    arma::mat past = edgelist.rows(pastkey);

    // For loop over the past
    for (arma::uword j = 0; j < past.n_rows; ++j)
    {
      // Add event weight to adjacency matrix
      adjmat(0, past(j, 1)) += past(j, 2);
    }

    // (2) For loop over timepoints
    for (arma::uword i = 1; i < slice.n_rows; ++i)
    {
      // Copy previous row
      adjmat.row(i) = adjmat.row(i - 1);
      // Add event weight previous event to adjacency matrix
      adjmat(i, slice(i - 1, 1)) += slice(i - 1, 2);
    }
  }

  // Windowed memory
  if (memory == "window")
  {
    // For loop over timepoints
    for (arma::uword i = 1; i < slice.n_rows; ++i)
    {
      // Current time
      double t = slice(i, 0);

      // Past events
      arma::uvec pastkey = arma::find(edgelist.col(0) < t &&
                                      edgelist.col(0) >= (t - memory_value(0)));
      arma::mat past = edgelist.rows(pastkey);

      // For loop over the past
      for (arma::uword j = 0; j < past.n_rows; ++j)
      {
        // Add event weight to adjacency matrix
        adjmat(i, past(j, 1)) += past(j, 2);
      }
    }
  }

  // Interval memory
  if (memory == "interval")
  {
    // For loop over timepoints
    for (arma::uword i = 1; i < slice.n_rows; ++i)
    {
      // Current time
      double t = slice(i, 0);

      // Past events
      arma::uvec pastkey = arma::find(edgelist.col(0) < (t - memory_value(0)) &&
                                      edgelist.col(0) >= (t - memory_value(1)));
      arma::mat past = edgelist.rows(pastkey);

      // For loop over the past
      for (arma::uword j = 0; j < past.n_rows; ++j)
      {
        // Add event weight to adjacency matrix
        adjmat(i, past(j, 1)) += past(j, 2);
      }
    }
  }

  // Exponential decay memory
  if (memory == "decay")
  {
    // For loop over timepoints
    for (arma::uword i = 0; i < slice.n_rows; ++i)
    {

      // Current time
      double t = slice(i, 0);

      // Past events
      arma::uvec pastkey = arma::find(edgelist.col(0) < t);
      arma::mat past = edgelist.rows(pastkey);

      // For loop over the past
      for (arma::uword j = 0; j < past.n_rows; ++j)
      {
        // Weight of the event
        double we = past(j, 2);

        // Brandes weight
        double te = past(j, 0);
        we = we * exp(-(t - te) * (log(2) / memory_value(0))) *
             (log(2) / memory_value(0));

        // Add weight to adjacency matrix
        adjmat(i, past(j, 1)) += we;
      }
    }
  }

  // Output
  return adjmat;
}

/*
   Function: calc_actor_stats_exo

   Description: Computes (or transforms/obtains) the exogenous actor statistics for the tie-oriented model.

   Parameters:
     - type: An integer indicating the type of statistic to compute. 1 for sender effect, 2 for receiver effect.
     - covariates: A matrix with covariate values. The first column refers to the actors, the second column to the time point when the covariate value changes, and the third column to the covariate value.
     - edgelist: A matrix with the observed relational event history. Rows refer to the observed relational events. The first column refers to the time, the second column to the events, and the third column to the event weight.
     - actors: A vector with the actors that can potentially interact.
     - types: A vector with the types of events.
     - riskset: A matrix representing the riskset data. Each row contains information about a dyad: actors involved, type and dyad ID.
     - start: An integer indicating the first row in the edgelist for which statistics have to be computed.
     - stop: An integer indicating the last row in the edgelist for which statistics have to be computed.

   Returns:
     A matrix with the exogenous actor statistics. Rows refer to the time points, and columns refer to the actors.
*/
arma::mat calc_actor_stats_exo(int type,
                               const arma::mat &covariates,
                               const arma::mat &edgelist,
                               const arma::vec &actors,
                               const arma::vec &types,
                               const arma::mat &riskset,
                               int start, int stop)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  // First time point
  double time = slice(0, 0);

  // For loop over actors
  for (arma::uword k = 0; k < actors.n_elem; ++k)
  {
    // Actor
    int actor = actors(k);

    // Find all the attribute values for actor k before the first timepoint
    arma::uvec index = find(covariates.col(0) == actor &&
                            covariates.col(1) <= time);
    arma::mat actorcovar = covariates.rows(index);

    // Find the last attribute value for actor k before the first timepoint
    arma::uword max_index = index_max(actorcovar.col(1));
    double value = actorcovar(max_index, 2);

    // Add the value to the correct places in the stat
    // Sender effect
    if (type == 1)
    {
      IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, NA_INTEGER, true);
      for (int dyadID : dyadIDs)
      {
        stat(0, dyadID) = value;
      }
    }

    // Receiver effect
    if (type == 2)
    {
      IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, actor, NA_INTEGER, true);
      for (int dyadID : dyadIDs)
      {
        stat(0, dyadID) = value;
      }
    }
  }

  // Find the unique change timepoints
  arma::vec changetimes = sort(unique(covariates.col(1)));
  changetimes = changetimes(find(changetimes != 0));
  arma::uword counter = 0;

  // For loop over the sequence
  for (arma::uword m = 1; m < slice.n_rows; ++m)
  {
    // Copy the previous row
    arma::rowvec thisrow = stat.row(m - 1);

    // Update the statistic if required
    // Do not update after the last changetime
    if (counter < changetimes.n_elem)
    {
      // Update if the time of the event is larger than the current
      // changetime
      if (slice(m, 0) > changetimes(counter))
      {
        // Update all changes in between
        while ((counter < changetimes.n_elem) && (slice(m, 0) > changetimes(counter)))
        {
          // For loop over actors
          for (arma::uword k = 0; k < actors.n_elem; ++k)
          {
            // Actor
            int actor = actors(k);

            // Find attribute value for this actor at the change
            // timepoint
            arma::uvec index = find(covariates.col(0) == actor &&
                                    covariates.col(1) == changetimes(counter));

            // Update if a new value exists
            if (index.n_elem == 1)
            {
              double value = covariates(index(0), 2);

              // Add the value to the correct places in the stat
              // Sender effect
              if (type == 1)
              {
                IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, NA_INTEGER, true);
                for (int dyadID : dyadIDs)
                {
                  thisrow(dyadID) = value;
                }
              }

              // Receiver effect
              if (type == 2)
              {
                IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, actor, NA_INTEGER, true);
                for (int dyadID : dyadIDs)
                {
                  thisrow(dyadID) = value;
                }
              }
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

  // Return
  return stat;
}

// calc_dyad_stats_exo
//
// Function to compute the dyadic exogenous statistics 'same', 'difference',
// 'average', 'minimum', and 'maximum' in the tie-oriented model.
//
// Parameters:
//   - type: An integer indicating the type of statistic to compute.
//           1 = same, 2 = difference, 3 = average, 4 = minimum, 5 = maximum
//   - covariates: A matrix with covariate values. Each row represents an observation and contains three columns: actor ID, time, and covariate value.
//   - edgelist: A matrix with the observed relational event history. Each row represents an
//               observed event and contains three columns: time, event, and event weight.
//   - riskset: A matrix representing the riskset data. Each row contains information about a dyad:
//              actor1, actor2, type, and event.
//   - start: An integer indicating the first event in the edgelist for which the statistic is computed.
//   - stop: An integer indicating the last event in the edgelist for which the statistic is computed.
//
// Returns:
//   A matrix with the dyadic exogenous statistics. Rows correspond to time points, and columns correspond to dyads.
arma::mat calc_dyad_stats_exo(int type,
                              const arma::mat &covariates,
                              const arma::mat &edgelist,
                              const arma::mat &riskset,
                              int start,
                              int stop)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  // Storage space for the current covariate values
  arma::vec current_ac1(riskset.n_rows, arma::fill::zeros);
  arma::vec current_ac2(riskset.n_rows, arma::fill::zeros);

  // Initialize statistic
  double time = slice(0, 0);

  // For loop over dyads
  for (arma::uword i = 0; i < riskset.n_rows; ++i)
  {
    // Find the relevant actors
    arma::uword actor1 = riskset(i, 0);
    arma::uword actor2 = riskset(i, 1);

    // Find the covariate values for actor1
    arma::uvec index1 = find(covariates.col(0) == actor1 &&
                             covariates.col(1) <= time);
    arma::mat actor1_values = covariates.rows(index1);
    arma::uword max_index1 = index_max(actor1_values.col(1));
    current_ac1(i) = actor1_values(max_index1, 2);

    // Find the covariate values for actor2
    arma::uvec index2 = find(covariates.col(0) == actor2 &&
                             covariates.col(1) <= time);
    arma::mat actor2_values = covariates.rows(index2);
    arma::uword max_index2 = index_max(actor2_values.col(1));
    current_ac2(i) = actor2_values(max_index2, 2);

    // Are these values equal?
    if (type == 1)
    {
      stat(0, i) = (current_ac1(i) == current_ac2(i));
    }
    // What is the difference between these values?
    if (type == 2)
    {
      stat(0, i) = current_ac1(i) - current_ac2(i);
    }

    arma::vec both = {current_ac1(i), current_ac2(i)};
    // What is the mean value?
    if (type == 3)
    {
      stat(0, i) = mean(both);
    }
    // What is the minimum value?
    if (type == 4)
    {
      stat(0, i) = min(both);
    }
    // What is the maximum value?
    if (type == 5)
    {
      stat(0, i) = max(both);
    }
  }

  // Find the unique change timepoints
  arma::vec changetimes = sort(unique(covariates.col(1)));
  changetimes = changetimes(find(changetimes != 0));
  arma::uword counter = 0;

  // For loop over the sequence
  for (arma::uword m = 1; m < slice.n_rows; ++m)
  {

    // Copy the previous row
    arma::rowvec thisrow = stat.row(m - 1);

    // Update the statistic if required
    // Do not update after the last changetime
    if (counter < changetimes.n_elem)
    {
      // Update if the time of the event is larger than the current
      // changetime
      if (slice(m, 0) > changetimes(counter))
      {
        // Update all changes in between
        while ((counter < changetimes.n_elem) &&
               (slice(m, 0) > changetimes(counter)))
        {

          // For loop over dyads
          for (arma::uword i = 0; i < riskset.n_rows; ++i)
          {
            // Find the relevant actor
            arma::uword actor1 = riskset(i, 0);
            arma::uword actor2 = riskset(i, 1);

            // Find the values for these actor
            arma::uvec index1 =
                find((covariates.col(0) == actor1) && (covariates.col(1) == changetimes(counter)));
            arma::uvec index2 =
                find((covariates.col(0) == actor2) && (covariates.col(1) == changetimes(counter)));

            // Update if a new value exists
            if ((index1.n_elem == 1) || (index2.n_elem == 1))
            {
              if (index1.n_elem == 1)
              {
                current_ac1(i) = covariates(index1(0), 2);
              }
              if (index2.n_elem == 1)
              {
                current_ac2(i) = covariates(index2(0), 2);
              }

              // Are these values equal?
              if (type == 1)
              {
                thisrow(i) =
                    (current_ac1(i) == current_ac2(i));
              }
              // What is the difference between
              // these values?
              if (type == 2)
              {
                thisrow(i) =
                    current_ac1(i) - current_ac2(i);
              }

              arma::dvec both = {current_ac1(i),
                                 current_ac2(i)};
              // What is the mean value?
              if (type == 3)
              {
                thisrow(i) = mean(both);
              }
              // What is the minimum value?
              if (type == 4)
              {
                thisrow(i) = min(both);
              }
              // What is the maximum value?
              if (type == 5)
              {
                thisrow(i) = max(both);
              }
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

  return stat;
}

/*
   Function: calc_degree_directed

   Description: Computes degree statistics for the tie-oriented model.

   Parameters:
     - type: An integer indicating the type of statistic to compute.
        1 for indegreeSender,
        2 for indegreeReceiver,
        3 for outdegreeSender,
        4 for outdegreeReceiver,
        5 for totaldegreeSender,
        6 for totaldegreeReceiver,
        7 for totaldegreeDyad.
     - edgelist: A matrix with the observed relational event history. Rows refer to the observed relational events. The first column refers to the time, the second column to the events, and the third column to the event weight.
      - riskset: A matrix representing the riskset data. Each row contains information about a dyad: actors involved, type and dyad ID.
     - actors: A vector with the actors that can potentially interact.
     - types: A vector with the types of events.
     - start: An integer indicating the first row in the edgelist for which statistics have to be computed.
     - stop: An integer indicating the last row in the edgelist for which statistics have to be computed.

   Returns:
     A matrix with the degree statistics. Rows refer to the time points, and columns refer to the actors.
*/
arma::mat calc_degree_directed(int type,
                               const arma::mat &edgelist,
                               const arma::mat &riskset,
                               const arma::mat &adjmat,
                               const arma::vec &actors,
                               const arma::vec &types,
                               int start,
                               int stop,
                               bool consider_type,
                               bool directed)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, adjmat.n_cols, arma::fill::zeros);

  if (consider_type)
  {

    // Initialize saving space
    arma::cube indeg(slice.n_rows, actors.n_elem, types.n_elem,
                     arma::fill::zeros);
    arma::cube outdeg(slice.n_rows, actors.n_elem, types.n_elem,
                      arma::fill::zeros);
    arma::cube deg(slice.n_rows, actors.n_elem, types.n_elem,
                   arma::fill::zeros);

    // For loop over event types
    for (arma::uword c = 0; c < types.n_elem; ++c)
    {

      arma::mat indegType = indeg.slice(c);
      arma::mat outdegType = outdeg.slice(c);
      arma::mat degType = deg.slice(c);

      // For loop over actors
      for (arma::uword k = 0; k < actors.n_elem; k++)
      {

        // Actor
        int actor = actors(k);

        // Actor's indegree
        if ((type == 1) || (type == 2) || (type == 5) || (type == 6))
        {

          // Get the dyad IDs for "actor" as receiver and type "c"
          IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, actor, types(c), directed);
          for (int dyadID : dyadIDs)
          {
            indegType.col(actor) += adjmat.col(dyadID);
          }
        }

        // Actor's outdegree
        if ((type == 3) || (type == 4) || (type == 5) || (type == 6))
        {

          // Get the dyad IDs for "actor" as sender and type "c"
          IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, types(c), directed);
          for (int dyadID : dyadIDs)
          {
            outdegType.col(actor) += adjmat.col(dyadID);
          }
        }

        // Dyad's degree
        if (type == 7)
        {

          // Get the dyad IDs for "actor"
          IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, types(c), false);
          for (int dyadID : dyadIDs)
          {
            degType.col(actor) += adjmat.col(dyadID);
          }
        }

        // Save in the correct place
        IntegerVector save_dyadIDs = 0;
        if ((type == 1) || (type == 3) || (type == 5) || (type == 7))
        {
          // Get the dyad IDs for "actor" as sender and type "c"
          save_dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, types(c), directed);
        }

        if ((type == 2) || (type == 4) || (type == 6))
        {
          // Get the dyad IDs for "actor" as receiver and type "c"
          save_dyadIDs = getDyadIDs(riskset, NA_INTEGER, actor, types(c), directed);
        }

        if ((type == 1) || (type == 2))
        {
          for (int save_dyadID : save_dyadIDs)
          {
            stat.col(save_dyadID) = indegType.col(actor);
          }
        }

        if ((type == 3) || (type == 4))
        {
          for (int save_dyadID : save_dyadIDs)
          {
            stat.col(save_dyadID) = outdegType.col(actor);
          }
        }

        if ((type == 5) || (type == 6) || (type == 7))
        {
          for (int save_dyadID : save_dyadIDs)
          {
            stat.col(save_dyadID) = indegType.col(actor) +
                                    outdegType.col(actor);
          }
        }

        if (type == 7)
        {
          for (int save_dyadID : save_dyadIDs)
          {
            stat.col(save_dyadID) += degType.col(actor);
          }
        }
      }
    }
  }
  else
  {
    // Initialize saving space
    arma::mat indeg(slice.n_rows, actors.n_elem, arma::fill::zeros);
    arma::mat outdeg(slice.n_rows, actors.n_elem, arma::fill::zeros);
    arma::mat deg(slice.n_rows, actors.n_elem, arma::fill::zeros);

    // For loop over actors
    for (arma::uword k = 0; k < actors.n_elem; k++)
    {

      // Actor
      int actor = actors(k);

      // Actor's indegree
      if ((type == 1) || (type == 2) || (type == 5) || (type == 6))
      {

        // Get the dyad IDs for "actor" as receiver
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, actor, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          indeg.col(actor) += adjmat.col(dyadID);
        }
      }

      // Actor's outdegree
      if ((type == 3) || (type == 4) || (type == 5) || (type == 6))
      {

        // Get the dyad IDs for "actor" as sender
        IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          outdeg.col(actor) += adjmat.col(dyadID);
        }
      }

      // Actor's degree
      if (type == 7)
      {
        // Get the dyad IDs for "actor"
        IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, NA_INTEGER, false);
        for (int dyadID : dyadIDs)
        {
          deg.col(actor) += adjmat.col(dyadID);
        }
      }

      // Save in the correct place
      IntegerVector save_dyadIDs = 0;

      if ((type == 1) || (type == 3) || (type == 5) || (type == 7))
      {
        // Get the dyad IDs for "actor" as sender
        save_dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, NA_INTEGER, directed);
      }

      if ((type == 2) || (type == 4) || (type == 6))
      {
        // Get the dyad IDs for "actor" as receiver and type "c"
        save_dyadIDs = getDyadIDs(riskset, NA_INTEGER, actor, NA_INTEGER, directed);
      }

      if ((type == 1) | (type == 2))
      {
        for (int save_dyadID : save_dyadIDs)
        {
          stat.col(save_dyadID) = indeg.col(actor);
        }
      }

      if ((type == 3) | (type == 4))
      {
        for (int save_dyadID : save_dyadIDs)
        {
          stat.col(save_dyadID) = outdeg.col(actor);
        }
      }

      if ((type == 5) || (type == 6))
      {
        for (int save_dyadID : save_dyadIDs)
        {
          stat.col(save_dyadID) += indeg.col(actor) + outdeg.col(actor);
        }
      }

      if (type == 7)
      {
        for (int save_dyadID : save_dyadIDs)
        {
          stat.col(save_dyadID) += deg.col(actor);
        }
      }
    }
  }

  // Output the computed stat
  return stat;
}

/*
   Function: calc_degree_undirected

   Description: Computes degree statistics for the tie-oriented model with undirected events.

   Parameters:
     - type: An integer indicating the type of statistic to compute.
        1 for degreeMin,
        2 for degreeMax,
        3 for degreeDiff,
     - edgelist: A matrix with the observed relational event history. Rows refer to the observed relational events. The first column refers to the time, the second column to the events, and the third column to the event weight.
      - riskset: A matrix representing the riskset data. Each row contains information about a dyad: actors involved, type and dyad ID.
     - actors: A vector with the actors that can potentially interact.
     - types: A vector with the types of events.
     - start: An integer indicating the first row in the edgelist for which statistics have to be computed.
     - stop: An integer indicating the last row in the edgelist for which statistics have to be computed.

   Returns:
     A matrix with the degree statistics. Rows refer to the time points, and columns refer to the actors.
*/
arma::mat calc_degree_undirected(int type,
                                 const arma::mat &edgelist,
                                 const arma::mat &riskset,
                                 const arma::mat &adjmat,
                                 const arma::vec &actors,
                                 const arma::vec &types,
                                 int start,
                                 int stop,
                                 bool consider_type)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, adjmat.n_cols, arma::fill::zeros);

  // Consider event type?
  if (consider_type)
  {
    // Initialize saving space
    arma::cube deg(slice.n_rows, actors.n_elem, types.n_elem, arma::fill::zeros);

    // For loop over event types
    for (arma::uword c = 0; c < types.n_elem; ++c)
    {
      // Select the slice from the deg cube
      arma::mat degType = deg.slice(c);

      // (1) Step 1: Compute the degree
      // For loop over actors k
      for (arma::uword k = 0; k < actors.n_elem; ++k)
      {
        // Actor
        int actor = actors(k);

        // Get the dyad IDs for "actor"
        IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, types(c), false);
        for (int dyadID : dyadIDs)
        {
          degType.col(actor) += adjmat.col(dyadID);
        }
      }

      deg.slice(c) = degType;
    }

    // (2) Step 2: Save the degree in the correct place
    // For loop over dyads
    for (arma::uword d = 0; d < riskset.n_rows; ++d)
    {
      // Dyad info
      int actor1 = riskset(d, 0);
      int actor2 = riskset(d, 1);
      int event_type = riskset(d, 2);

      // Degree info
      arma::vec deg1 = deg.slice(event_type).col(actor1);
      arma::vec deg2 = deg.slice(event_type).col(actor2);

      switch (type)
      {
      case 1:
        stat.col(d) = min(deg1, deg2);
        break;

      case 2:
        stat.col(d) = max(deg1, deg2);
        break;

      case 3:
        stat.col(d) = abs(deg1 - deg2);
        break;
      }
    }
  }
  else
  {
    // Initialize saving space
    arma::mat deg(slice.n_rows, actors.n_elem, arma::fill::zeros);

    // (1) Step 1: Compute the degree
    // For loop over actors k
    for (arma::uword k = 0; k < actors.n_elem; k++)
    {
      // Actor
      int actor = actors(k);

      // Get the dyad IDs for "actor"
      IntegerVector dyadIDs = getDyadIDs(riskset, actor, NA_INTEGER, NA_INTEGER, false);
      for (int dyadID : dyadIDs)
      {
        deg.col(actor) += adjmat.col(dyadID);
      }
    }

    // (2) Step 2: Save the degree in the correct place
    // For loop over dyads
    for (arma::uword d = 0; d < riskset.n_rows; ++d)
    {
      // Dyad info
      int actor1 = riskset(d, 0);
      int actor2 = riskset(d, 1);

      // Degree info
      arma::vec deg1 = deg.col(actor1);
      arma::vec deg2 = deg.col(actor2);

      switch (type)
      {
      case 1:
        stat.col(d) = min(deg1, deg2);
        break;

      case 2:
        stat.col(d) = max(deg1, deg2);
        break;

      case 3:
        stat.col(d) = abs(deg1 - deg2);
        break;
      }
    }
  }

  // Output the computed stat
  return stat;
}

// calc_inertia
//
// Computes the statistic for an inertia effect in the tie-oriented model.
//
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// riskset: matrix, (actor1, actor2, type, event)
// N: integer, number of actors
// directed: boolean, whether events are directed or undirected
// types: vector, type ids
// start: integer, first event in the edgelist for which the statistic is
// computed
// stop: integer, last event in the edgelist for which the statistic is
// computed
// consider_type: boolean indicating whether to compute the inertia per
// event type (true) or sum across types (false)
arma::mat calc_inertia(const arma::mat &edgelist,
                       const arma::mat &adjmat,
                       const arma::mat &riskset,
                       const arma::vec &actors,
                       const arma::vec &types,
                       bool directed,
                       int start,
                       int stop,
                       bool consider_type,
                       Rcpp::String scaling)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  if (!consider_type)
  {
    // If there is only one event type, the adjmat is equal to the inertia
    // statistic
    if (types.n_elem == 1)
    {
      stat = adjmat;
    }
    else
    {
      // For loop over dyads
      for (arma::uword d = 0; d < riskset.n_rows; ++d)
      {
        // Actors
        int ac1 = riskset(d, 0);
        int ac2 = riskset(d, 1);

        // Get all events in the riskset
        IntegerVector dyadIDs = getDyadIDs(riskset, ac1, ac2, NA_INTEGER, directed);
        // Set the values
        for (int dyadID : dyadIDs)
        {
          stat.col(d) += adjmat.col(dyadID);
        }
      }
    }
  }
  else
  {
    stat = adjmat;
  }

  // Scale by the outdegree of the sender >> the fraction of messages i sent to
  // j >> if i hasn't sent any messages yet, than all n-1 actors are equally
  // likely to get a message
  if ((scaling == "prop") & (directed))
  {
    arma::mat deg = calc_degree_directed(3, edgelist, riskset, adjmat, actors, types, start, stop, consider_type, true);
    stat = stat / deg;
    double rep = 1.0 / (actors.n_elem - 1.0);
    stat.replace(arma::datum::nan, rep);
  }

  // Output the computed stat
  return stat;
}

// calc_reciprocity
//
// Computes the statistic for a reciprocity effect in the tie-oriented model.
//
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// riskset: matrix, (actor1, actor2, type, event)
// N: integer, number of actors
// types: vector, type ids
// start: integer, first event in the edgelist for which the statistic is
// computed
// stop: integer, last event in the edgelist for which the statistic is
// computed
// consider_type: boolean indicating whether to compute the degree per
// event type (true) or sum across types (false)
arma::mat calc_reciprocity(const arma::mat &edgelist,
                           const arma::mat &adjmat,
                           const arma::mat &riskset,
                           const arma::vec &actors,
                           const arma::vec &types,
                           int start,
                           int stop,
                           bool consider_type,
                           Rcpp::String scaling)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  if (!consider_type)
  {
    // For loop over dyads
    for (arma::uword j = 0; j < riskset.n_rows; j++)
    {
      // Actors
      int ac1 = riskset(j, 0);
      int ac2 = riskset(j, 1);

      // Get all events in the riskset
      IntegerVector dyadIDs = getDyadIDs(riskset, ac2, ac1, NA_INTEGER, true);
      // Set the values
      for (int dyadID : dyadIDs)
      {
        stat.col(j) += adjmat.col(dyadID);
      }
    }
  }
  else
  {
    // For loop over dyads
    for (arma::uword j = 0; j < riskset.n_rows; j++)
    {
      // Actors and event type
      int ac1 = riskset(j, 0);
      int ac2 = riskset(j, 1);
      int c = riskset(j, 2);

      // Find the position of the reverse dyad
      IntegerVector dyadIDs = getDyadIDs(riskset, ac2, ac1, c, true); // IntegerVector of length 1

      // Set the values
      stat.col(j) = adjmat.col(dyadIDs[0]);
    }
  }

  // Scale by the indegree of the sender >> the fraction of messages i received
  // from j >> if i hasn't received any messages yet, than all n-1 actors are
  // equally likely to get a message
  if (scaling == "prop")
  {
    arma::mat deg = calc_degree_directed(1, edgelist, riskset, adjmat, actors, types, start, stop, consider_type, true);
    stat = stat / deg;
    double rep = 1.0 / (actors.n_elem - 1.0);
    stat.replace(arma::datum::nan, rep);
  }

  // Output the computed stat
  return stat;
}

arma::uvec checkAndRemoveIndices(const arma::vec &vector1, const arma::vec &vector2, arma::uword i, arma::uword j)
{
  arma::uword n = vector1.size();
  arma::uvec result;

  for (arma::uword k = 0; k < n; k++)
  {
    if (vector1(k) != -999 && vector2(k) != -999 && k != i && k != j)
    {
      result.resize(result.n_elem + 1);
      result(result.n_elem - 1) = k;
    }
  }

  return result;
}

// computeTriadStatsNoTypes
//
// Computes the triad statistics for the tie-oriented model with directed
// dyadic events
//
// type: integer, 1 = otp, 2 = itp, 3 = osp, 4 = isp, 5 = sp, 6 = unique sp
// adjmat: matrix (events x dyads)
// start: integer, first event in the edgelist for which the statistic is
// computed
// stop: integer, last event in the edgelist for which the statistic is
// computed
arma::mat computeTriadStatsNoTypes(int type,
                                   const arma::mat &adjmat,
                                   arma::vec actors,
                                   const arma::mat &riskset)
{
  // Initialize saving space
  arma::mat stat(adjmat.n_rows, adjmat.n_cols, arma::fill::zeros);

  // Pre-compute dyad indices
  arma::mat dyadIndices(actors.n_elem, actors.n_elem);
  dyadIndices.fill(-999);
  for (arma::uword i = 0; i < actors.n_elem; ++i)
  {
    int actor1 = actors.at(i);

    for (arma::uword j = 0; j < actors.n_elem; ++j)
    {

      int actor2 = actors.at(j);

      if ((type == 5) | (type == 6))
      {
        int dyadID = getDyadIDs(riskset, actor1, actor2, 0, false)[0];
        dyadIndices(i, j) = dyadID;
      }
      else
      {
        int dyadID = getDyadIDs(riskset, actor1, actor2, 0, true)[0];
        dyadIndices(i, j) = dyadID;
      }
    }
  }

  // Initialize selectedCols1, selectedCols2, colIndices1, colIndices2,
  // dyads1, and dyads2 with their maximum required sizes
  arma::mat selectedCols1(adjmat.n_rows, actors.n_elem);
  arma::mat selectedCols2(adjmat.n_rows, actors.n_elem);
  arma::uvec colIndices1(actors.n_elem);
  arma::uvec colIndices2(actors.n_elem);
  arma::vec dyads1(actors.n_elem);
  arma::vec dyads2(actors.n_elem);

  // Initialize minMatrix and rowSums
  arma::mat minMatrix(adjmat.n_rows, actors.n_elem);
  arma::vec rowSums(adjmat.n_rows);

  // Initialize keepIndices
  arma::uvec keepIndices(actors.n_elem);

  // For loop over dyads
  for (arma::uword d = 0; d < adjmat.n_cols; ++d)
  {
    // Sender i and receiver j
    arma::uword i = riskset(d, 0);
    arma::uword j = riskset(d, 1);

    switch (type)
    {
    case 1:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.row(i).t();
      // Get the indices for dyads with receiver j
      dyads2 = dyadIndices.col(j);
      break;

    case 2:
      // Get the indices for dyads with sender j
      dyads1 = dyadIndices.row(j).t();
      // Get the indices for dyads with receiver i
      dyads2 = dyadIndices.col(i);
      break;

    case 3:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.row(i).t();
      // Get the indices for dyads with sender j
      dyads2 = dyadIndices.row(j).t();
      break;

    case 4:
      // Get the indices for dyads with receiver i
      dyads1 = dyadIndices.col(i);
      // Get the indices for dyads with receiver j
      dyads2 = dyadIndices.col(j);
      break;

    case 5:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.row(i).t();
      // Get the indices for dyads with sender j
      dyads2 = dyadIndices.row(j).t();
      break;

    case 6:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.row(i).t();
      // Get the indices for dyads with sender j
      dyads2 = dyadIndices.row(j).t();
      break;
    }

    // Remove any dyads not in the risk set
    keepIndices = checkAndRemoveIndices(dyads1, dyads2, i, j);

    // Get all past events sent or received by actor i (dependent on the condition)
    colIndices1 = arma::conv_to<arma::uvec>::from(dyads1(keepIndices));
    selectedCols1 = adjmat.cols(colIndices1);

    // Get all past events sent or received by actor j (dependent on the condition)
    colIndices2 = arma::conv_to<arma::uvec>::from(dyads2(keepIndices));
    selectedCols2 = adjmat.cols(colIndices2);

    if (type == 6)
    {
      // Convert elements in selectedCols1 to 1 if greater than 0
      selectedCols1 = arma::conv_to<arma::mat>::from(selectedCols1 > 0);
      // Convert elements in selectedCols2 to 1 if greater than 0
      selectedCols2 = arma::conv_to<arma::mat>::from(selectedCols2 > 0);
    }

    // Calculate the element-wise minimum
    arma::mat minMatrix = arma::min(selectedCols1, selectedCols2);

    // Calculate the sum per time point
    arma::vec rowSums = arma::sum(minMatrix, 1);
    stat.col(d) = rowSums;
  }

  return stat;
}

// computeTriadStatsTypesNotConsidered
//
// Computes the triad statistics for the tie-oriented model with
// types events, summing over event types (i.e., consider_type = false)
//
// type: integer, 1 = otp, 2 = itp, 3 = osp, 4 = isp
// adjmat: matrix (events x dyads)
// start: integer, first event in the edgelist for which the statistic is
// computed
// stop: integer, last event in the edgelist for which the statistic is
// computed
arma::mat computeTriadStatsTypesNotConsidered(
    int type,
    const arma::mat &adjmat,
    const arma::vec &actors,
    const arma::vec &types,
    const arma::mat &riskset)
{

  // Initialize saving space
  arma::mat stat(adjmat.n_rows, adjmat.n_cols, arma::fill::zeros);

  // Pre-compute dyad indices
  arma::cube dyadIndices(actors.n_elem, actors.n_elem, types.n_elem);
  for (arma::uword i = 0; i < actors.n_elem; ++i)
  {
    int actor1 = actors.at(i);

    for (arma::uword j = 0; j < actors.n_elem; ++j)
    {

      int actor2 = actors.at(j);

      for (arma::uword c = 0; c < types.n_elem; ++c)
      {

        int event_type = types.at(c);

        if ((type == 5) | (type == 6))
        {
          int dyadID = getDyadIDs(riskset, actor1, actor2, event_type, false)[0];
          dyadIndices(i, j, c) = dyadID;
        }
        else
        {
          int dyadID = getDyadIDs(riskset, actor1, actor2, event_type, true)[0];
          dyadIndices(i, j, c) = dyadID;
        }
      }
    }
  }

  // Initialize colIndices1, colIndices2, dyads1 and dyads2 with their
  // maximum required sizes
  arma::uvec colIndices1(actors.n_elem);
  arma::uvec colIndices2(actors.n_elem);
  arma::vec dyads1(actors.n_elem);
  arma::vec dyads2(actors.n_elem);

  // Initialize minMatrix, and rowSums
  arma::mat minMatrix(adjmat.n_rows, actors.n_elem);
  arma::vec rowSums(adjmat.n_rows);

  // Initialize keepIndices
  arma::uvec keepIndices(actors.n_elem);

  // For loop over dyads
  for (arma::uword d = 0; d < adjmat.n_cols; ++d)
  {
    // Initialize selectedCols1, selectedCols2
    arma::mat selectedCols1(adjmat.n_rows, actors.n_elem, arma::fill::zeros);
    arma::mat selectedCols2(adjmat.n_rows, actors.n_elem, arma::fill::zeros);

    // Sender i and receiver j
    arma::uword i = riskset(d, 0);
    arma::uword j = riskset(d, 1);

    // For loop over event types
    for (arma::uword c = 0; c < types.n_elem; ++c)
    {
      switch (type)
      {
      case 1:
        // Get the indices for dyads with sender i
        dyads1 = dyadIndices.slice(c).row(i).t();
        // Get the indices for dyads with receiver j
        dyads2 = dyadIndices.slice(c).col(j);
        break;

      case 2:
        // Get the indices for dyads with sender j
        dyads1 = dyadIndices.slice(c).row(j).t();
        // Get the indices for dyads with receiver i
        dyads2 = dyadIndices.slice(c).col(i);
        break;

      case 3:
        // Get the indices for dyads with sender i
        dyads1 = dyadIndices.slice(c).row(i).t();
        // Get the indices for dyads with sender j
        dyads2 = dyadIndices.slice(c).row(j).t();
        break;

      case 4:
        // Get the indices for dyads with receiver i
        dyads1 = dyadIndices.slice(c).col(i);
        // Get the indices for dyads with receiver j
        dyads2 = dyadIndices.slice(c).col(j);
        break;

      case 5:
        // Get the indices for dyads with sender i
        dyads1 = dyadIndices.slice(c).row(i).t();
        // Get the indices for dyads with sender j
        dyads2 = dyadIndices.slice(c).row(j).t();
        break;

      case 6:
        // Get the indices for dyads with sender i
        dyads1 = dyadIndices.slice(c).row(i).t();
        // Get the indices for dyads with sender j
        dyads2 = dyadIndices.slice(c).row(j).t();
        break;
      }

      // Remove any dyads not in the risk set
      keepIndices = checkAndRemoveIndices(dyads1, dyads2, i, j);

      // Loop over keepIndices
      for (arma::uword k = 0; k < keepIndices.n_elem; ++k)
      {
        selectedCols1.col(keepIndices.at(k)) += adjmat.col(dyads1.at(keepIndices.at(k)));
        selectedCols2.col(keepIndices.at(k)) += adjmat.col(dyads2.at(keepIndices.at(k)));
      }
    }

    if (type == 6)
    {
      // Convert elements in sumAcrossTypes1 to 1 if greater than 0
      selectedCols1 = arma::conv_to<arma::mat>::from(selectedCols1 > 0);
      // Convert elements in sumAcrossTypes2 to 1 if greater than 0
      selectedCols2 = arma::conv_to<arma::mat>::from(selectedCols2 > 0);
    }

    // Calculate the element-wise minimum
    minMatrix = arma::min(selectedCols1, selectedCols2);

    // Calculate the sum per time point
    rowSums = arma::sum(minMatrix, 1);
    stat.col(d) = rowSums;
  }
  return stat;
}

arma::mat computeTriadStatsTypesConsidered(
    int type,
    const arma::mat &adjmat,
    const arma::vec &actors,
    const arma::vec &types,
    const arma::mat &riskset)
{

  // Initialize saving space
  arma::mat stat(adjmat.n_rows, adjmat.n_cols, arma::fill::zeros);

  // Pre-compute dyad indices
  arma::cube dyadIndices(actors.n_elem, actors.n_elem, types.n_elem);
  for (arma::uword i = 0; i < actors.n_elem; ++i)
  {
    int actor1 = actors.at(i);

    for (arma::uword j = 0; j < actors.n_elem; ++j)
    {

      int actor2 = actors.at(j);

      for (arma::uword c = 0; c < types.n_elem; ++c)
      {

        int event_type = types.at(c);

        if ((type == 5) | (type == 6))
        {
          int dyadID = getDyadIDs(riskset, actor1, actor2, event_type, false)[0];
          dyadIndices(i, j, c) = dyadID;
        }
        else
        {
          int dyadID = getDyadIDs(riskset, actor1, actor2, event_type, true)[0];
          dyadIndices(i, j, c) = dyadID;
        }
      }
    }
  }

  // Initialize colIndices1, colIndices2, dyads1 and dyads2 with their
  // maximum required sizes
  arma::uvec colIndices1(actors.n_elem);
  arma::uvec colIndices2(actors.n_elem);
  arma::vec dyads1(actors.n_elem);
  arma::vec dyads2(actors.n_elem);

  // Initialize minMatrix, and rowSums
  arma::mat minMatrix(adjmat.n_rows, actors.n_elem);
  arma::vec rowSums(adjmat.n_rows);

  // Initialize keepIndices
  arma::uvec keepIndices(actors.n_elem);

  // For loop over dyads
  for (arma::uword d = 0; d < adjmat.n_cols; ++d)
  {
    // Initialize selectedCols1, selectedCols2
    arma::mat selectedCols1(adjmat.n_rows, actors.n_elem, arma::fill::zeros);
    arma::mat selectedCols2(adjmat.n_rows, actors.n_elem, arma::fill::zeros);

    // Sender i and receiver j
    arma::uword i = riskset(d, 0);
    arma::uword j = riskset(d, 1);
    arma::uword c = riskset(d, 2);

    switch (type)
    {
    case 1:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.slice(c).row(i).t();
      // Get the indices for dyads with receiver j
      dyads2 = dyadIndices.slice(c).col(j);
      break;

    case 2:
      // Get the indices for dyads with sender j
      dyads1 = dyadIndices.slice(c).row(j).t();
      // Get the indices for dyads with receiver i
      dyads2 = dyadIndices.slice(c).col(i);
      break;

    case 3:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.slice(c).row(i).t();
      // Get the indices for dyads with sender j
      dyads2 = dyadIndices.slice(c).row(j).t();
      break;

    case 4:
      // Get the indices for dyads with receiver i
      dyads1 = dyadIndices.slice(c).col(i);
      // Get the indices for dyads with receiver j
      dyads2 = dyadIndices.slice(c).col(j);
      break;

    case 5:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.slice(c).row(i).t();
      // Get the indices for dyads with sender j
      dyads2 = dyadIndices.slice(c).row(j).t();
      break;

    case 6:
      // Get the indices for dyads with sender i
      dyads1 = dyadIndices.slice(c).row(i).t();
      // Get the indices for dyads with sender j
      dyads2 = dyadIndices.slice(c).row(j).t();
      break;
    }

    // Remove any dyads not in the risk set
    keepIndices = checkAndRemoveIndices(dyads1, dyads2, i, j);

    // Loop over keepIndices
    for (arma::uword k = 0; k < keepIndices.n_elem; ++k)
    {
      selectedCols1.col(keepIndices.at(k)) = adjmat.col(dyads1.at(keepIndices.at(k)));
      selectedCols2.col(keepIndices.at(k)) = adjmat.col(dyads2.at(keepIndices.at(k)));
    }

    if (type == 6)
    {
      // Convert elements in sumAcrossTypes1 to 1 if greater than 0
      selectedCols1 = arma::conv_to<arma::mat>::from(selectedCols1 > 0);
      // Convert elements in sumAcrossTypes2 to 1 if greater than 0
      selectedCols2 = arma::conv_to<arma::mat>::from(selectedCols2 > 0);
    }

    // Calculate the element-wise minimum
    minMatrix = arma::min(selectedCols1, selectedCols2);

    // Calculate the sum per time point
    rowSums = arma::sum(minMatrix, 1);
    stat.col(d) = rowSums;
  }

  return stat;
}

// calc_pshift
//
// Computes statistic for a p-shift effect (AB-BA, AB-BY, AB-XA, AB-XB, AB-XY,
// AB-AY, AB-AB)
//
// *param [type] integer value that indicates the type of p-shift effect
// (1 = AB-BA, 2 = AB-BY, 3 = AB-XA, 4 = AB-XB, 5 = AB-XY, 6 = AB-AY, 7 = AB-AB)
// *param [edgelist] matrix with the observed relational event history. Rows
// refers to the observed relational events. The first column refers to the
// time, the second column to the events and the third column to the event
// weight.
// *param [D] integer value; the number of events in the risk set
// *param [directed] boolean value: are events directed (1) or undirected (0)?
// *param [start] integer number indicating the first row in the edgelist for
// which statistics have to be computed.
// *param [stop] integer number indicating the last row in the edgelist for
// which statistics have to be computed.
// *param [consider_type] boolean indicating whether to compute the pshift per
// event type (true) or across types (false)
arma::mat calc_pshift(int type,
                      const arma::mat &edgelist,
                      const arma::mat &riskset,
                      const arma::vec &actors,
                      const arma::vec &types,
                      bool directed,
                      int start,
                      int stop,
                      bool consider_type)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  // Pre-compute dyad indices
  arma::cube dyadIndices(actors.n_elem, actors.n_elem, types.n_elem);
  for (arma::uword i = 0; i < actors.n_elem; ++i)
  {
    int actor1 = actors.at(i);

    for (arma::uword j = 0; j < actors.n_elem; ++j)
    {

      int actor2 = actors.at(j);

      for (arma::uword c = 0; c < types.n_elem; ++c)
      {

        int event_type = types.at(c);
        int dyadID = getDyadIDs(riskset, actor1, actor2, event_type, directed)[0];
        dyadIndices(i, j, c) = dyadID;
      }
    }
  }

  // For loop over events
  for (arma::uword i = 0; i < slice.n_rows; ++i)
  {
    // Position of the current event in the full edgelist
    int current = start + i;
    // Position of the last event in the full edgelist
    int last = current - 1;

    // If the current event is the first event in the edgelist, continue to
    // the next iteration
    if (last < 0)
    {
      continue;
    }

    // Sender and receiver of the last event
    arma::uword s = riskset(edgelist(last, 1), 0);
    arma::uword r = riskset(edgelist(last, 1), 1);

    // Type of the last event
    arma::uword c = 0;
    if (consider_type)
    {
      c = riskset(edgelist(last, 1), 2);
    }

    // Find the dyads that would create the respective p-shift
    switch (type)
    {
    // AB-BA
    case 1:
      // Find the reverse dyad
      if (consider_type)
      {
        if (dyadIndices(r, s, c) >= 0)
        {
          stat(i, dyadIndices(r, s, c)) = 1.0;
        }
      }
      else
      {
        for (arma::uword k = 0; k < types.n_elem; ++k)
        {
          if (dyadIndices(r, s, k) >= 0)
          {
            stat(i, dyadIndices(r, s, k)) = 1.0;
          }
        }
      }
      break;

    // AB-BY
    case 2:
      // Find all BY dyads
      if (consider_type)
      {
        arma::vec dyadIDs = dyadIndices.slice(c).row(r).t();
        arma::uvec indicesToRemove = {s, r};
        dyadIDs.shed_rows(indicesToRemove);
        for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
        {
          if (dyadIDs(m) >= 0)
          {
            stat(i, dyadIDs(m)) = 1.0;
          }
        }
      }
      else
      {
        for (arma::uword k = 0; k < types.n_elem; ++k)
        {
          arma::vec dyadIDs = dyadIndices.slice(k).row(r).t();
          arma::uvec indicesToRemove = {s, r};
          dyadIDs.shed_rows(indicesToRemove);
          for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
          {
            if (dyadIDs(m) >= 0)
            {
              stat(i, dyadIDs(m)) = 1.0;
            }
          }
        }
      }
      break;

    // AB-XA
    case 3:
      if (consider_type)
      {
        arma::vec dyadIDs = dyadIndices.slice(c).col(s);
        arma::uvec indicesToRemove = {s, r};
        dyadIDs.shed_rows(indicesToRemove);
        for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
        {
          if (dyadIDs(m) >= 0)
          {
            stat(i, dyadIDs(m)) = 1.0;
          }
        }
      }
      else
      {
        for (arma::uword k = 0; k < types.n_elem; ++k)
        {
          arma::vec dyadIDs = dyadIndices.slice(k).col(s);
          arma::uvec indicesToRemove = {s, r};
          dyadIDs.shed_rows(indicesToRemove);
          for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
          {
            if (dyadIDs(m) >= 0)
            {
              stat(i, dyadIDs(m)) = 1.0;
            }
          }
        }
      }
      break;

    // AB-XB
    case 4:
      // Find all XB dyads
      if (consider_type)
      {
        arma::vec dyadIDs = dyadIndices.slice(c).col(r);
        arma::uvec indicesToRemove = {s, r};
        dyadIDs.shed_rows(indicesToRemove);
        for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
        {
          if (dyadIDs(m) >= 0)
          {
            stat(i, dyadIDs(m)) = 1.0;
          }
        }
      }
      else
      {
        for (arma::uword k = 0; k < types.n_elem; ++k)
        {
          arma::vec dyadIDs = dyadIndices.slice(k).col(r);
          arma::uvec indicesToRemove = {s, r};
          dyadIDs.shed_rows(indicesToRemove);
          for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
          {
            if (dyadIDs(m) >= 0)
            {
              stat(i, dyadIDs(m)) = 1.0;
            }
          }
        }
      }
      break;

    // AB-XY
    case 5:
      // Find all XY dyads
      if (consider_type)
      {
        arma::mat dyadIDs = dyadIndices.slice(c);
        arma::uvec indicesToRemove = {s, r};
        dyadIDs.shed_rows(indicesToRemove);
        dyadIDs.shed_cols(indicesToRemove);
        arma::vec dyadIDS_vec = arma::vectorise(dyadIDs);
        for (arma::uword m = 0; m < dyadIDS_vec.n_elem; ++m)
        {
          if (dyadIDS_vec(m) >= 0)
          {
            stat(i, dyadIDS_vec(m)) = 1.0;
          }
        }
      }
      else
      {
        for (arma::uword k = 0; k < types.n_elem; ++k)
        {
          arma::mat dyadIDs = dyadIndices.slice(k);
          arma::uvec indicesToRemove = {s, r};
          dyadIDs.shed_rows(indicesToRemove);
          dyadIDs.shed_cols(indicesToRemove);
          arma::vec dyadIDS_vec = arma::vectorise(dyadIDs);
          for (arma::uword m = 0; m < dyadIDS_vec.n_elem; ++m)
          {
            if (dyadIDS_vec(m) >= 0)
            {
              stat(i, dyadIDS_vec(m)) = 1.0;
            }
          }
        }
      }
      break;

    // AB-AY
    case 6:
      if (consider_type)
      {
        arma::vec dyadIDs = dyadIndices.slice(c).row(s).t();
        arma::uvec indicesToRemove = {s, r};
        dyadIDs.shed_rows(indicesToRemove);
        for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
        {
          if (dyadIDs(m) >= 0)
          {
            stat(i, dyadIDs(m)) = 1.0;
          }
        }
      }
      else
      {
        for (arma::uword k = 0; k < types.n_elem; ++k)
        {
          arma::vec dyadIDs = dyadIndices.slice(k).row(s).t();
          arma::uvec indicesToRemove = {s, r};
          dyadIDs.shed_rows(indicesToRemove);
          for (arma::uword m = 0; m < dyadIDs.n_elem; ++m)
          {
            if (dyadIDs(m) >= 0)
            {
              stat(i, dyadIDs(m)) = 1.0;
            }
          }
        }
      }
      break;

    // AB-AB
    case 7:
      // Find the same dyad
      if (consider_type)
      {
        if (dyadIndices(s, r, c) >= 0)
        {
          stat(i, dyadIndices(s, r, c)) = 1.0;
        }
      }
      else
      {
        for (arma::uword k = 0; k < types.n_elem; ++k)
          if (dyadIndices(s, r, k) >= 0)
          {
            stat(i, dyadIndices(s, r, k)) = 1.0;
          }
      }
      break;
    }
  }

  // Output the computed stat
  return stat;
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

// calc_rrank
//
// Computes statistic for a recency-rank effect (rrankSend, rrankReceive) in
// the tie-oriented model.
//
// type: integer, 1 = rrankSend, 2 = rrankReceive
// edgelist: matrix (time, event, weight)
// riskset: matrix (actor1, actor2, type, event)
// start: integer, first event in the edgelist for which the statistic is
// computed
// stop: integer, last event in the edgelist for which the statistic is
// computed
// consider_type: boolean indicating whether to compute the inertia per
// event type (true) or sum across types (false)
arma::mat calc_rrank(int type, const arma::mat &edgelist,
                     const arma::mat &riskset, arma::uword N, arma::uword C, int start,
                     int stop, bool consider_type)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat ESlice = edgelist.rows(start, stop);

  // Saving space
  arma::mat stat(ESlice.n_rows, riskset.n_rows, arma::fill::zeros);

  if (!consider_type)
  {
    // Initialize lastTime array
    arma::cube lastTime(N, N, ESlice.n_rows, arma::fill::zeros);
    if (start > 0)
    {
      arma::mat past = edgelist.rows(0, start - 1);
      for (arma::uword i = 0; i < past.n_rows; ++i)
      {
        // rrankSend: to whom the sender has most recently send events
        // (most recent times)
        int d = past(i, 1);
        if (type == 1)
        {
          lastTime(riskset(d, 0), riskset(d, 1), 0) = past(i, 0);
        }
        // rrankReceive: from whom the sender has most recently
        // received events (most recent times)
        if (type == 2)
        {
          lastTime(riskset(d, 1), riskset(d, 0), 0) = past(i, 0);
        }
      }
    }

    // Fill lastTime for every event in the sequence
    for (arma::uword i = 0; i < (ESlice.n_rows - 1); ++i)
    {
      // Update slice
      lastTime.slice(i + 1) = lastTime.slice(i);
      // rrankSend: to whom the sender has most recently send events
      // (most recent times)
      int d = ESlice(i, 1);
      if (type == 1)
      {
        lastTime(riskset(d, 0), riskset(d, 1), i + 1) = ESlice(i, 0);
      }
      // rrankReceive: from whom the sender has most recently received
      // events (most recent times)
      if (type == 2)
      {
        lastTime(riskset(d, 1), riskset(d, 0), i + 1) = ESlice(i, 0);
      }
    }

    // Compute ranks based on lastTime
    arma::cube ranks(N, N, ESlice.n_rows, arma::fill::zeros);
    for (arma::uword i = 0; i < ESlice.n_rows; ++i)
    {
      arma::mat lastTimeS = lastTime.slice(i);
      arma::mat ranksS = ranks.slice(i);
      for (arma::uword j = 0; j < N; ++j)
      {
        ranksS.row(j) = rankR(lastTimeS.row(j), N);
      }
      ranks.slice(i) = ranksS;
    }

    // Statistics value
    arma::cube values = 1 / ranks;
    values.replace(arma::datum::inf, 0);

    // Transform to statistics matrix
    for (arma::uword i = 0; i < riskset.n_rows; ++i)
    {
      stat.col(i) = arma::vectorise(values.tube(riskset(i, 0), riskset(i, 1)));
    }
  }
  else
  {
    // Initialize lastTime array
    arma::cube lastTime(N, N * C, ESlice.n_rows, arma::fill::zeros);
    if (start > 0)
    {
      arma::mat past = edgelist.rows(0, start - 1);
      for (arma::uword i = 0; i < past.n_rows; ++i)
      {
        int d = past(i, 1);
        // rrankSend: to whom the sender has most recently send events
        // of this type (most recent times)
        if (type == 1)
        {
          lastTime(riskset(d, 0), riskset(d, 1) + riskset(d, 2) * N, 0) =
              past(i, 0);
        }
        // rrankReceive: from whom the sender has most recently
        // received events of this type (most recent times)
        if (type == 2)
        {
          lastTime(riskset(d, 1), riskset(d, 0) + riskset(d, 2) * N, 0) =
              past(i, 0);
        }
      }
    }

    // Fill lastTime for every event in the sequence
    for (arma::uword i = 0; i < (ESlice.n_rows - 1); ++i)
    {
      // Update slice
      lastTime.slice(i + 1) = lastTime.slice(i);
      // rrankSend: to whom the sender has most recently send events
      // of this type (most recent times)
      int d = ESlice(i, 1);
      if (type == 1)
      {
        lastTime(riskset(d, 0), riskset(d, 1) + riskset(d, 2) * N, i + 1) =
            ESlice(i, 0);
      }
      // rrankReceive: from whom the sender has most recently received
      // of this type events (most recent times)
      if (type == 2)
      {
        lastTime(riskset(d, 1), riskset(d, 0) + riskset(d, 2) * N, i + 1) =
            ESlice(i, 0);
      }
    }

    // Compute ranks based on lastTime
    // Saving space
    arma::cube ranks(N, N * C, ESlice.n_rows, arma::fill::zeros);
    // For loop over timepoints
    for (arma::uword i = 0; i < ESlice.n_rows; ++i)
    {
      // Slice at the current timepoint
      arma::mat lastTimeS = lastTime.slice(i);
      arma::mat ranksS = ranks.slice(i);
      // For loop over senders
      for (arma::uword j = 0; j < N; ++j)
      {
        // Compute ranks
        ranksS.row(j) = rankR(lastTimeS.row(j), N * C);
      }
      // Save ranks
      ranks.slice(i) = ranksS;
    }

    // Statistics value
    arma::cube values = 1 / ranks;
    values.replace(arma::datum::inf, 0);

    // Transform to statistics matrix
    for (arma::uword i = 0; i < riskset.n_rows; ++i)
    {
      stat.col(i) = arma::vectorise(
          values.tube(riskset(i, 0), riskset(i, 1) + riskset(i, 2) * N));
    }
  }

  return stat;
}

// calc_recency
//
// A function for computing the recency statistics, as in  Vu et al. (2017)
// and Mulder and Leenders (2019).
//
// type: integer, 1 = recencyContinue, 2 = recencySendSender,
// 3 = recencySendReceiver, 4 = recencyReceiveSender, 5 = recencyReceiveReceiver
// edgelist: matrix (time, event, weight)
// riskset: matrix, (actor1, actor2, type, event)
// N: integer, number of actors
// C: integer, number of event types
// start: integer, first event in the edgelist for which the statistic is
// computed
// stop: integer, last event in the edgelist for which the statistic is
// computed
// consider_type: boolean indicating whether to compute the recency per
// event type (true) or sum across types (false)
// directed: boolean, whether events are directed or undirected
arma::mat calc_recency(int type,
                       const arma::mat &edgelist,
                       const arma::mat &riskset,
                       arma::uword N,
                       arma::uword C,
                       int start,
                       int stop,
                       bool consider_type,
                       bool directed)
{

  // Initialize vector with times the dyads were last active
  arma::vec lastActive(riskset.n_rows);
  lastActive.fill(arma::datum::inf);

  // Select the past
  double time = edgelist(start, 0);
  arma::uvec past = arma::find(edgelist.col(0) < time);

  // For loop over the past
  for (arma::uword m = 0; m < past.n_elem; ++m)
  {
    // Sender, receiver and event type
    int d = edgelist(past(m), 1);
    arma::uword s = riskset(d, 0);
    arma::uword r = riskset(d, 1);
    arma::uword c = riskset(d, 2);

    // Event time
    double time = edgelist(past(m), 0);

    // Find respective dyads
    switch (type)
    {
    case 1:
      // Last time active as dyad
      if (consider_type)
      {
        lastActive(d) = time;
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, s, r, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      break;

    case 2:
      // Last time the sender was active as sender
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, s, NA_INTEGER, c, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, s, NA_INTEGER, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      break;

    case 3:
      // Last time the receiver was active as sender
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, s, c, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, s, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      break;

    case 4:
      // Last time the sender was active as receiver
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, r, NA_INTEGER, c, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, r, NA_INTEGER, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      break;

    case 5:
      // Last time the receiver was active as receiver
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, r, c, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, r, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          lastActive(dyadID) = time;
        }
      }
      break;
    }
  }

  // Initialize statistic
  arma::mat stat((stop - start + 1), riskset.n_rows);

  // Helper
  arma::vec updateActive = lastActive;

  // For loop over time points
  for (int m = 0; m < (stop - start + 1); ++m)
  {
    // Event indicator
    int event = start + m;

    // Event time
    double time = edgelist(event, 0);

    // Compute statistic
    arma::vec frC = 1 / ((time - lastActive) + 1);
    arma::rowvec fr = arma::conv_to<arma::rowvec>::from(frC);
    stat.row(m) = fr;

    // Sender, receiver and event type
    int d = edgelist(event, 1);
    arma::uword s = riskset(d, 0);
    arma::uword r = riskset(d, 1);
    arma::uword c = riskset(d, 2);

    // Update updateActive
    // Find respective dyads
    switch (type)
    {
    case 1:
      // Last time active as dyad
      if (consider_type)
      {
        updateActive(d) = time;
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, s, r, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      break;

    case 2:
      // Last time the sender was active as sender
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, s, NA_INTEGER, c, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, s, NA_INTEGER, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      break;

    case 3:
      // Last time the receiver was active as sender
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, s, c, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, s, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      break;

    case 4:
      // Last time the sender was active as receiver
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, r, NA_INTEGER, c, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, r, NA_INTEGER, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      break;

    case 5:
      // Last time the receiver was active as receiver
      if (consider_type)
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, r, c, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      else
      {
        IntegerVector dyadIDs = getDyadIDs(riskset, NA_INTEGER, r, NA_INTEGER, directed);
        for (int dyadID : dyadIDs)
        {
          updateActive(dyadID) = time;
        }
      }
      break;
    }

    // Update lastActive?
    if (event < stop)
    {
      if (edgelist(event + 1, 0) > time)
      {
        lastActive = updateActive;
      }
    }
  }

  return stat;
}

// calc_tie_stats_exo
arma::mat calc_tie_stats_exo(const arma::mat &covariates, const arma::mat &edgelist,
                             const arma::mat &riskset, int start, int stop)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  // Saving space
  arma::rowvec thisrow(riskset.n_rows);

  // For loop over dyads
  for (arma::uword i = 0; i < riskset.n_rows; ++i)
  {

    // Find the relevant actors
    arma::uword actor1 = riskset(i, 0);
    arma::uword actor2 = riskset(i, 1);

    // Find the value
    double tieval = covariates(actor1, actor2);
    thisrow(i) = tieval;
  }

  // Save in stat
  for (arma::uword m = 0; m < slice.n_rows; ++m)
  {
    stat.row(m) = thisrow;
  }

  // Output
  return (stat);
}

// get_user_stat
arma::mat get_user_stat(const arma::mat &covariates, int start, int stop)
{
  arma::mat stat = covariates.rows(start, stop);
  return (stat);
}

// calc_event_stats_exo
arma::mat calc_event_stats_exo(const arma::mat &covariates, const arma::mat &edgelist,
                               const arma::mat &riskset, int start, int stop)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Slice the covariates according to "start" and "stop"
  arma::mat covS = covariates.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  // For loop over dyads
  for (arma::uword i = 0; i < riskset.n_rows; ++i)
  {
    stat.col(i) = covS.col(0);
  }

  // Output
  return (stat);
}

arma::mat calc_FEtype(const arma::mat &covariates,
                      const arma::mat &edgelist, const arma::mat &riskset, int start, int stop)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

  // For loop over dyads
  for (arma::uword i = 0; i < riskset.n_rows; ++i)
  {
    arma::colvec val(slice.n_rows, arma::fill::zeros);
    if (riskset(i, 2) == covariates(0, 0))
    {
      val.ones();
    }
    stat.col(i) = val;
  }

  // Output
  return (stat);
}

// current_common_partners (CPP)
//
// This statistic is currently only available for events with a duration, undirected and without event types!
arma::mat current_common_partners(const arma::mat &edgelist,
                                  const arma::mat &riskset, const arma::vec &actors,
                                  const arma::vec &duration, int start, int stop)
{

  // Slice the edgelist according to "start" and "stop"
  arma::mat slice = edgelist.rows(start, stop);

  // Initialize saving space
  arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);
  arma::mat adjC(actors.n_elem, actors.n_elem);

  // For loop over events in the sliced edgelist
  for (arma::uword m = 0; m < slice.n_rows; ++m)
  {
    // Current time
    double time = slice(m, 0);

    // Active events
    arma::uvec ind = arma::find(edgelist.col(0) < time &&
                                edgelist.col(0) + duration >= time);
    arma::mat active = edgelist.rows(ind);

    // For loop over active events to set adjC
    for (arma::uword i = 0; i < active.n_rows; ++i)
    {
      arma::rowvec event = riskset.row(active(i, 1));
      adjC(event(0), event(1)) += 1;
    }

    // Select actors with more than one active event
    arma::rowvec colSumsR = sum(adjC, 0);
    arma::vec colSums = colSumsR.as_col();
    arma::vec rowSums = sum(adjC, 1);
    arma::vec actorSums = colSums + rowSums;
    arma::uvec ind2 = arma::find(actorSums > 1);
    arma::vec middle = actors(ind2);

    if (middle.n_elem > 0)
    {
      // For loop over these "middle" actors
      for (arma::uword i = 0; i < middle.n_elem; ++i)
      {
        // Find the actors they were communicating with
        arma::uvec ind3 = arma::find(adjC.row(middle[i]) > 0);
        arma::uvec ind4 = arma::find(adjC.col(middle[i]) > 0);
        arma::uvec ind5 = join_cols(ind3, ind4);
        arma::vec ends = actors(ind5);

        // Actor combinations (for loop over pairs)
        for (arma::uword j = 0; j < ends.n_elem; ++j)
        {
          for (arma::uword h = 0; h < ends.n_elem; ++h)
          {
            if (h > j)
            {
              int dyad = getDyadIDs(riskset, ends(h), ends(j), 0, false)[0];
              stat(m, dyad) += 1;
            }
          }
        }
      }
    }

    // Reset adjC
    adjC.fill(0);
  }

  // Return
  return stat;
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
  effectsMap["ccp"] = 121;

  effectsMap["otp"] = 131;
  effectsMap["itp"] = 132;
  effectsMap["osp"] = 133;
  effectsMap["isp"] = 134;
  effectsMap["sp"] = 135;
  effectsMap["spUnique"] = 136;

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
    std::cout << "Effect not found in the map." << std::endl;
  }

  return numericValue;
}

//[[Rcpp::export]]
arma::cube compute_stats_tie(Rcpp::CharacterVector &effects,
                             const arma::mat &edgelist,
                             const arma::mat &adjmat,
                             const arma::vec &actors,
                             const arma::vec &types,
                             const arma::mat &riskset,
                             Rcpp::CharacterVector &scaling,
                             Rcpp::LogicalVector &consider_type,
                             const Rcpp::List &covariates,
                             const Rcpp::List &interactions,
                             int start, int stop, bool directed)
{

  // Initialize saving space
  int M = stop - start + 1;
  arma::cube stats(M, riskset.n_rows, effects.size());

  // For loop over effects
  for (int i = 0; i < effects.size(); ++i)
  {
    // Get case number
    Rcpp::String effectName = effects(i);
    int effect = getEffectNumber(effectName);

    // Initialize saving space
    arma::mat stat(stats.n_rows, stats.n_cols, arma::fill::zeros);

    // Compute effect
    switch (effect)
    {

    // baseline
    case 1:
      stat.fill(1);
      break;

    // FEtype
    case 2:
      // Compute statistic
      stat = calc_FEtype(covariates[i], edgelist, riskset, start, stop);
      break;

    // send
    case 11:
      // Compute statistic
      stat = calc_actor_stats_exo(1, covariates[i], edgelist, actors, types,
                                  riskset, start, stop);
      break;

    // receive
    case 12:
      // Compute statistic
      stat = calc_actor_stats_exo(2, covariates[i], edgelist, actors, types,
                                  riskset, start, stop);
      break;

    // tie
    case 13:
      // Compute statistic
      stat = calc_tie_stats_exo(covariates[i], edgelist, riskset, start, stop);
      break;

    // same
    case 14:
      // Compute statistic
      stat = calc_dyad_stats_exo(1, covariates[i], edgelist, riskset, start,
                                 stop);
      break;

    // difference
    case 15:
      // Compute statistic
      stat = calc_dyad_stats_exo(2, covariates[i], edgelist, riskset, start,
                                 stop);
      // Absolute values
      if ((scaling(i) == "none_abs") || (scaling(i) == "std_abs"))
      {
        stat = abs(stat);
      }
      // Standardize (note: if scaling == "std" the stat will be scaled at the end of the switch statement)
      if (scaling(i) == "std_abs")
      {
        stat = standardize(stat);
      }
      break;

    // average
    case 16:
      // Compute statistic
      stat = calc_dyad_stats_exo(3, covariates[i], edgelist, riskset, start,
                                 stop);
      break;

    // minimum
    case 17:
      // Compute statistic
      stat = calc_dyad_stats_exo(4, covariates[i], edgelist, riskset, start,
                                 stop);
      break;

    // maximum
    case 18:
      // Compute statistic
      stat = calc_dyad_stats_exo(5, covariates[i], edgelist, riskset, start,
                                 stop);
      break;

    // event
    case 19:
      // Compute statistic
      stat = calc_event_stats_exo(covariates[i], edgelist, riskset, start,
                                  stop);
      break;

    // inertia
    case 101:
      // Compute statistic
      stat = calc_inertia(edgelist, adjmat, riskset, actors, types, directed,
                          start, stop, consider_type(i), scaling(i));
      break;

    // reciprocity
    case 102:
      // Compute statistic
      stat = calc_reciprocity(edgelist, adjmat, riskset, actors, types, start,
                              stop, consider_type(i), scaling(i));
      break;

    // indegreeSender
    case 111:
      // Compute statistic
      stat = calc_degree_directed(1, edgelist, riskset, adjmat, actors, types, start, stop, consider_type(i), true);
      // Divide by the number/weight of past events >> the fraction of messages
      // received by the sender. If no messages have been exchanged yet, then
      // all actors are equally likely to send a message.
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / sum(adjmat.row(t));
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

    // indegreeReceiver
    case 112:
      // Compute statistic
      stat = calc_degree_directed(2, edgelist, riskset, adjmat, actors, types, start, stop, consider_type(i), true);
      // Divide by the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / sum(adjmat.row(t));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols,
                                          arma::fill::value(1.0 / actors.n_elem));
          stat.row(0) = rep;
        }
      }
      break;

    // outdegreeSender
    case 113:
      // Compute statistic
      stat = calc_degree_directed(3, edgelist, riskset, adjmat, actors, types, start, stop, consider_type(i), true);
      // Divide by the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / sum(adjmat.row(t));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols,
                                          arma::fill::value(1.0 / actors.n_elem));
          stat.row(0) = rep;
        }
      }
      break;

    // outdegreeReceiver
    case 114:
      // Compute statistic
      stat = calc_degree_directed(4, edgelist, riskset, adjmat, actors, types, start, stop, consider_type(i), true);
      // Divide by the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / sum(adjmat.row(t));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols,
                                          arma::fill::value(1.0 / actors.n_elem));
          stat.row(0) = rep;
        }
      }
      break;

    // totaldegreeSender
    case 115:
      // Compute statistic
      stat = calc_degree_directed(5, edgelist, riskset, adjmat, actors, types, start, stop, consider_type(i), true);
      // Divide by two times the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / (2 * sum(adjmat.row(t)));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols,
                                          arma::fill::value(1.0 / actors.n_elem));
          stat.row(0) = rep;
        }
      }
      break;

    // totaldegreeReceiver
    case 116:
      // Compute statistic
      stat = calc_degree_directed(6, edgelist, riskset, adjmat, actors, types, start, stop, consider_type(i), true);
      // Divide by two times the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / (2 * sum(adjmat.row(t)));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols,
                                          arma::fill::value(1.0 / actors.n_elem));
          stat.row(0) = rep;
        }
      }
      break;

    // totaldegreeDyad
    case 117:
      // Compute statistic
      stat = calc_degree_directed(7, edgelist, riskset, adjmat, actors, types, start, stop, consider_type(i), directed);
      // Divide by two times the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / (2 * sum(adjmat.row(t)));
        }
        stat.replace(arma::datum::nan, 0);
        // First row
        if (start == 0)
        {
          arma::rowvec rep = arma::rowvec(stat.n_cols,
                                          arma::fill::value(2.0 / actors.n_elem));
          stat.row(0) = rep;
        }
      }
      break;

    // degreeMin
    case 118:
      // Compute statistic
      stat = calc_degree_undirected(1, edgelist, riskset, adjmat, actors,
                                    types, start, stop, consider_type(i));
      // Divide by the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / (sum(adjmat.row(t)));
        }
        stat.replace(arma::datum::nan, 0);
      }
      break;

    // degreeMax
    case 119:
      // Compute statistic
      stat = calc_degree_undirected(2, edgelist, riskset, adjmat, actors,
                                    types, start, stop, consider_type(i));
      // Divide by the number/weight of past events
      if (scaling(i) == "prop")
      {
        for (arma::uword t = 0; t < stat.n_rows; ++t)
        {
          stat.row(t) = stat.row(t) / (sum(adjmat.row(t)));
        }
        stat.replace(arma::datum::nan, 0);
      }
      break;

    // degreeDiff
    case 120:
      // Compute statistic
      stat = calc_degree_undirected(3, edgelist, riskset, adjmat, actors,
                                    types, start, stop, consider_type(i));
      break;

    // ccp
    case 121:
      // Compute statistic
      stat = current_common_partners(edgelist, riskset,
                                     actors, covariates[i], start, stop);
      break;

    // otp
    case 131:
      // Compute statistic
      if (types.n_elem == 1)
      {
        stat = computeTriadStatsNoTypes(1, adjmat, actors, riskset);
      }
      else
      {
        if (consider_type(i)) {
          stat = computeTriadStatsTypesConsidered(1, adjmat, actors, types, riskset);
        } else {
          stat = computeTriadStatsTypesNotConsidered(1, adjmat, actors, types, riskset);
        }
      }
      break;

    // itp
    case 132:
      // Compute statistic
      if (types.n_elem == 1)
      {
        stat = computeTriadStatsNoTypes(2, adjmat, actors, riskset);
      }
      else
      {
        if (consider_type(i)) {
          stat = computeTriadStatsTypesConsidered(2, adjmat, actors, types, riskset);
        } else {
          stat = computeTriadStatsTypesNotConsidered(2, adjmat, actors, types, riskset);
        }
      }
      break;

    // osp
    case 133:
      // Compute statistic
      if (types.n_elem == 1)
      {
        stat = computeTriadStatsNoTypes(3, adjmat, actors, riskset);
      }
      else
      {
        if (consider_type(i)) {
          stat = computeTriadStatsTypesConsidered(3, adjmat, actors, types, riskset);
        } else {
          stat = computeTriadStatsTypesNotConsidered(3, adjmat, actors, types, riskset);
        }
      }
      break;

    // isp
    case 134:
      // Compute statistic
      if (types.n_elem == 1)
      {
        stat = computeTriadStatsNoTypes(4, adjmat, actors, riskset);
      }
      else
      {
        if (consider_type(i)) {
          stat = computeTriadStatsTypesConsidered(4, adjmat, actors, types, riskset);
        } else {
          stat = computeTriadStatsTypesNotConsidered(4, adjmat, actors, types, riskset);
        }
      }
      break;

    // sp
    case 135:
      // Compute statistic
      if (types.n_elem == 1)
      {
        stat = computeTriadStatsNoTypes(5, adjmat, actors, riskset);
      }
      else
      {
        if (consider_type(i)) {
          stat = computeTriadStatsTypesConsidered(5, adjmat, actors, types, riskset);
        } else {
          stat = computeTriadStatsTypesNotConsidered(5, adjmat, actors, types, riskset);
        }
      }
      break;

    // spUnique
    case 136:
      // Compute statistic
      if (types.n_elem == 1)
      {
        stat = computeTriadStatsNoTypes(6, adjmat, actors, riskset);
      }
      else
      {
        if (consider_type(i)) {
          stat = computeTriadStatsTypesConsidered(6, adjmat, actors, types, riskset);
        } else {
          stat = computeTriadStatsTypesNotConsidered(6, adjmat, actors, types, riskset);
        }
      }
      break;

    // psABBA
    case 141:
      // Compute statistic
      stat = calc_pshift(1, edgelist, riskset, actors, types, directed, start, stop, consider_type(i));
      break;

    // psABBY
    case 142:
      // Compute statistic
      stat = calc_pshift(2, edgelist, riskset, actors, types, directed, start, stop, consider_type(i));
      break;

    // psABXA
    case 143:
      // Compute statistic
      stat = calc_pshift(3, edgelist, riskset, actors, types, directed, start, stop, consider_type(i));
      break;

    // psABXB
    case 144:
      // Compute statistic
      stat = calc_pshift(4, edgelist, riskset, actors, types, directed, start, stop, consider_type(i));
      break;

    // psABXY
    case 145:
      // Compute statistic
      stat = calc_pshift(5, edgelist, riskset, actors, types, directed, start, stop, consider_type(i));
      break;

    // psABAY
    case 146:
      // Compute statistic
      stat = calc_pshift(6, edgelist, riskset, actors, types, directed, start, stop, consider_type(i));
      break;

    // 74 psABAB
    case 147:
      // Compute statistic
      stat = calc_pshift(7, edgelist, riskset, actors, types, directed, start, stop, consider_type(i));
      break;

    // rrankSend
    case 151:
      // Compute statistic
      stat = calc_rrank(1, edgelist, riskset, actors.n_elem,
                        types.n_elem, start, stop, consider_type(i));
      break;

    // rrankReceive
    case 152:
      // Compute statistic
      stat = calc_rrank(2, edgelist, riskset, actors.n_elem,
                        types.n_elem, start, stop, consider_type(i));
      break;

    // recencyContinue
    case 161:
      // Compute statistic
      stat = calc_recency(1, edgelist, riskset, actors.n_elem,
                          types.n_elem, start, stop, consider_type(i), directed);
      break;

    // recencySendSender
    case 162:
      // Compute statistic
      stat = calc_recency(2, edgelist, riskset, actors.n_elem,
                          types.n_elem, start, stop, consider_type(i), directed);
      break;

    // recencySendReceiver
    case 163:
      // Compute statistic
      stat = calc_recency(3, edgelist, riskset, actors.n_elem,
                          types.n_elem, start, stop, consider_type(i), directed);
      break;

    // recencyReceiveSender
    case 164:
      // Compute statistic
      stat = calc_recency(4, edgelist, riskset, actors.n_elem,
                          types.n_elem, start, stop, consider_type(i), directed);
      break;

    // recencyReceiveReceiver
    case 165:
      // Compute statistic
      stat = calc_recency(5, edgelist, riskset, actors.n_elem,
                          types.n_elem, start, stop, consider_type(i), directed);
      break;

    // userStat
    case 888:
      stat = get_user_stat(covariates[i], start, stop);
      break;

    // interact
    case 999:
      // Get the indices of the statistics slices (+1) with the
      // statistics for which an interaction needs to be computed.
      arma::vec x = interactions[i];
      int main1 = x(0);
      int main2 = x(1);
      // Element-wise multiplication
      stat = stats.slice(main1 - 1) % stats.slice(main2 - 1);
      break;
    }

    // Standardize
    if (scaling(i) == "std")
    {
      stat = standardize(stat);
    }

    // Save statistic
    stats.slice(i) = stat;
  }

  return stats;
}