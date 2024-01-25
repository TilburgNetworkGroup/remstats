#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
#include <stdexcept> // std::runtime_error
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include <map>
#include <string>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]

// =============================================================================
// Utility functions
// =============================================================================

// Find all event IDs where the respective actor is the sender
// [[Rcpp::export]]
arma::vec actor_is_sender(int actor, int event_type,
                          const arma::mat &risksetMatrix, bool consider_type)
{
    // Find all event IDs where the respective actor is the sender
    arma::vec event_IDs = risksetMatrix.row(actor).t();

    // If consider_type, only select those events with the respective event type
    if (consider_type)
    {
        // Number of actors
        int N = risksetMatrix.n_rows;

        // Select those events with the respective event type
        arma::uvec indices = arma::regspace<arma::uvec>(0 + (event_type * N), (N - 1) * (event_type + 1));
        event_IDs = event_IDs.elem(indices);
    }

    // Remove all non-existing dyads (below zero)
    event_IDs = event_IDs.elem(find(event_IDs >= 0));

    return event_IDs;
}

// Find all event IDs where the respective actor is the receiver
// [[Rcpp::export]]
arma::vec actor_is_receiver(int actor, int event_type,
                            const arma::mat &risksetMatrix, bool consider_type)
{
    // Number of actors
    int N = risksetMatrix.n_rows;
    // Number of event types
    int C = risksetMatrix.n_cols / N;
    // Initialize event_IDs
    arma::vec event_IDs;

    if (consider_type)
    {
        // Find all event IDs where the respective actor is the receiver and
        // the event is of the respective type
        event_IDs = risksetMatrix.col(actor + (event_type * N));
    }
    else
    {
        // Loop over event types
        for (int k = 0; k < C; ++k)
        {
            event_IDs = arma::join_cols(event_IDs, risksetMatrix.col(actor + (k * N)));
        }
    }

    // Remove all non-existing dyads (below zero)
    event_IDs = event_IDs.elem(find(event_IDs >= 0));

    return event_IDs;
}

// =============================================================================
// Degree statistics
// =============================================================================

// Calculate the in- or out-degree for an actor and event type combination, over
// the entire sequence.
arma::vec actor_degree(std::string degree_type,
                       int actor, int event_type,
                       const arma::mat &inertia,
                       const arma::mat &risksetMatrix)
{
    // Collect the ids of all relevant events with the respective actor
    arma::vec event_IDs;
    if (degree_type == "out")
    {
        // Find all event IDs where the respective actor is the sender
        event_IDs = actor_is_sender(actor, event_type, risksetMatrix, true);
    }
    else if (degree_type == "in")
    {
        // Find all event IDs where the respective actor is the sender
        event_IDs = actor_is_receiver(actor, event_type, risksetMatrix, true);
    }

    // Calculate the degree by summing over all relevant events
    arma::vec degree(inertia.n_rows, arma::fill::zeros);
    for (arma::uword i = 0; i < event_IDs.n_elem; i++)
    {
        degree += inertia.col(event_IDs(i));
    }

    // Return the 'degree' vector
    return degree;
}

// Map the integer degree type to the name of a degree statistic.
std::string degree_name(int type)
{
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

    return type_name;
}

// MAIN 1: Calculate a selection of individual degree statistics.
// [[Rcpp::export]]
arma::mat degree_sample_stat(int type, const arma::mat &inertia,
                             const arma::mat &caseControls,
                             const arma::mat &risksetMatrix,
                             const arma::mat &riskset,
                             bool consider_type, bool display_progress)
{
    // Type of degree statistic
    std::string type_name = degree_name(type);

    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating " << type_name << " statistic" << std::endl;
    }

    // Initialize the 'stat' matrix
    arma::mat stat(inertia.n_rows, caseControls.n_cols, arma::fill::zeros);

    // Number of actors (N) and event types (C)
    int N = risksetMatrix.n_rows;
    int C = risksetMatrix.n_cols / N;

    // Initialize a helper 'degree' matrix
    arma::mat degree(inertia.n_rows, N * C, arma::fill::zeros);

    // Progress bar
    Progress p(N + inertia.n_rows, display_progress);

    // Step 1: Fill the helper 'degree' matrix
    // Loop over actors and types
    for (int actor = 0; actor < N; actor++)
    {
        for (int event_type = 0; event_type < C; event_type++)
        {
            if (type_name == "outdegreeSender" || type_name == "outdegreeReceiver")
            {
                // Calculate the out-degree for the respective actor and event type
                degree.col(actor + (N * event_type)) = actor_degree("out", actor, event_type, inertia, risksetMatrix);
            }
            else if (type_name == "indegreeSender" || type_name == "indegreeReceiver")
            {
                // Calculate the in-degree for the respective actor and event type
                degree.col(actor + (N * event_type)) = actor_degree("in", actor, event_type, inertia, risksetMatrix);
            }
            else if (type_name == "totaldegreeSender" || type_name == "totaldegreeReceiver")
            {
                // Calculate the out-degree for the respective actor and event type
                degree.col(actor + (N * event_type)) += actor_degree("out", actor, event_type, inertia, risksetMatrix);
                // Calculate the in-degree for the respective actor and event type
                degree.col(actor + (N * event_type)) += actor_degree("in", actor, event_type, inertia, risksetMatrix);
            }
        }

        // Progress update
        p.increment();
    }

    // Step 2: Fill the 'stat' matrix
    // Loop over timepoints
    for (arma::uword m = 0; m < inertia.n_rows; m++)
    {
        // Loop over caseControls
        for (arma::uword i = 0; i < caseControls.n_cols; i++)
        {
            // caseControl dyad
            int dyad = caseControls(m, i);
            // extract relevant actor from the dyad
            int actor;
            if (type_name == "outdegreeSender" || type_name == "indegreeSender" || type_name == "totaldegreeSender")
            {
                actor = riskset(dyad, 0); // sender
            }
            if (type_name == "outdegreeReceiver" || type_name == "indegreeReceiver" || type_name == "totaldegreeReceiver")
            {
                actor = riskset(dyad, 1); // receiver
            }
            // event type
            if (consider_type)
            {
                int event_type = riskset(dyad, 2);
                stat(m, i) = degree(m, actor + (N * event_type));
            }
            else
            {
                // Loop over event types
                for (int event_type = 0; event_type < C; event_type++)
                {
                    stat(m, i) += degree(m, actor + (N * event_type));
                }
            }
        }

        // Progress update
        p.increment();
    }

    // Return the 'stat' matrix
    return stat;
}

// MAIN 2: Calculate a selection of dyadic degree statistics
arma::mat dyad_degree_sample_stat(int type, const arma::mat &inertia,
                                  const arma::mat &caseControls,
                                  const arma::mat &risksetMatrix,
                                  const arma::mat &riskset,
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
    arma::mat totaldegreeSender = degree_sample_stat(5, inertia, caseControls, risksetMatrix, riskset, consider_type, false);
    // Degree second actor
    arma::mat totaldegreeReceiver = degree_sample_stat(6, inertia, caseControls, risksetMatrix, riskset, consider_type, false);

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

// =============================================================================
// Inertia
// =============================================================================
// [[Rcpp::export]]
arma::mat inertia_sample_stat(const arma::mat &inertia,
                              const arma::mat &caseControls,
                              const arma::mat &riskset, const arma::mat &risksetMatrix,
                              bool consider_type, bool display_progress)
{
    if (display_progress)
    {
        Rcpp::Rcout << "Transforming inertia statistic" << std::endl;
    }

    // Initialize the 'stat' matrix
    arma::mat stat(inertia.n_rows, caseControls.n_cols, arma::fill::zeros);

    // Number of actors (N) and event types (C)
    int N = risksetMatrix.n_rows;
    int C = risksetMatrix.n_cols / N;

    // Progress bar
    Progress p(inertia.n_rows, display_progress);

    // Loop over timepoints
    for (arma::uword m = 0; m < inertia.n_rows; m++)
    {
        // Loop over caseControls
        for (arma::uword i = 0; i < caseControls.n_cols; i++)
        {
            // caseControl dyad
            int dyad = caseControls(m, i);

            // event type
            if (consider_type)
            {
                // Statistic
                stat(m, i) = inertia(m, dyad);
            }
            else
            {
                // sender and receiver
                int sender = riskset(dyad, 0);
                int receiver = riskset(dyad, 1);

                // Loop over event types
                for (int event_type = 0; event_type < C; event_type++)
                {
                    // Reverse dyad
                    int type_dyad = risksetMatrix(sender, (receiver + N * event_type));

                    // Add to statistic
                    if (type_dyad >= 0)
                    {
                        stat(m, i) += inertia(m, type_dyad);
                    }
                }
            }
        }

        // Progress update
        p.increment();
    }

    // Return the 'stat' matrix
    return stat;
}

// =============================================================================
// Reciprocity
// =============================================================================

// [[Rcpp::export]]
arma::mat reciprocity_sample_stat(const arma::mat &inertia,
                                  const arma::mat &caseControls,
                                  const arma::mat &riskset, const arma::mat &risksetMatrix,
                                  bool consider_type, bool display_progress)
{
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating reciprocity statistic" << std::endl;
    }

    // Initialize the 'stat' matrix
    arma::mat stat(inertia.n_rows, caseControls.n_cols, arma::fill::zeros);

    // Number of actors (N) and event types (C)
    int N = risksetMatrix.n_rows;
    int C = risksetMatrix.n_cols / N;

    // Progress bar
    Progress p(inertia.n_rows, display_progress);

    // Loop over timepoints
    for (arma::uword m = 0; m < inertia.n_rows; m++)
    {
        // Loop over caseControls
        for (arma::uword i = 0; i < caseControls.n_cols; i++)
        {
            // caseControl dyad
            int dyad = caseControls(m, i);
            // sender and receiver
            int sender = riskset(dyad, 0);
            int receiver = riskset(dyad, 1);
            // event type
            if (consider_type)
            {
                int event_type = riskset(dyad, 2);

                // Reverse dyad
                int rev_dyad = risksetMatrix(receiver, (sender + N * event_type));

                // Statistic
                if (rev_dyad >= 0)
                {
                    stat(m, i) = inertia(m, rev_dyad);
                }
            }
            else
            {
                // Loop over event types
                for (int event_type = 0; event_type < C; event_type++)
                {
                    // Reverse dyad
                    int rev_dyad = risksetMatrix(receiver, (sender + N * event_type));

                    // Add to statistic
                    if (rev_dyad >= 0)
                    {
                        stat(m, i) += inertia(m, rev_dyad);
                    }
                }
            }
        }

        // Progress update
        p.increment();
    }

    // Return the 'stat' matrix
    return stat;
}

// =============================================================================
// Triad
// =============================================================================
// HELPER: Calculate a triad statistic for a given time point, sender, receiver,
// and event type
int dyad_triad(int type, const arma::mat &inertia, arma::uword m,
               int sender, int receiver, int event_type,
               const arma::mat &risksetMatrix, int N, int C,
               bool consider_type)
{
    // Triad value
    int triad_val = 0;

    // Loop over all third actors
    for (int h = 0; h < N; h++)
    {
        // Continue to next actor if actor h is sender or receiver
        if ((h == sender) || (h == receiver))
        {
            continue;
        }

        // Depending on the triad stat type, get the two relevant dyads
        int dyad1, dyad2;
        if (type == 1) // otp
        {
            dyad1 = risksetMatrix(sender, h);
            dyad2 = risksetMatrix(h, receiver);
        }
        if (type == 2) // itp
        {
            dyad1 = risksetMatrix(h, sender);
            dyad2 = risksetMatrix(receiver, h);
        }
        if (type == 3) // osp
        {
            dyad1 = risksetMatrix(sender, h);
            dyad2 = risksetMatrix(receiver, h);
        }
        if (type == 4) // isp
        {
            dyad1 = risksetMatrix(h, sender);
            dyad2 = risksetMatrix(h, receiver);
        }

        // Continue to next actor if one of the dyads does not exist
        if ((dyad1 < 0) || (dyad2 < 0))
        {
            continue;
        }

        // Otherwise, get their inertia value // TODO: scaling (unique)
        int val1 = inertia(m, dyad1);
        int val2 = inertia(m, dyad2);

        // Add the minimum to the triad value
        triad_val += std::min(val1, val2);
    }

    // Return the triad value
    return triad_val;
}

// MAIN: Calculate a selection of triadic statistics.
arma::mat triad_sample_stat(int type, const arma::mat &inertia,
                            const arma::mat &caseControls,
                            const arma::mat &riskset,
                            const arma::mat &risksetMatrix,
                            bool consider_type, bool display_progress)
{
    // Progress update
    Rcpp::CharacterVector types = Rcpp::CharacterVector::create(
        "otp", "itp", "osp", "isp");

    if (display_progress)
    {
        Rcpp::Rcout << "Calculating " << types(type - 1) << " statistic" << std::endl;
    }

    // Initialize the 'stat' matrix
    arma::mat stat(inertia.n_rows, caseControls.n_cols, arma::fill::zeros);

    // Number of actors (N) and event types (C)
    int N = risksetMatrix.n_rows;
    int C = risksetMatrix.n_cols / N;

    // Progress bar
    Progress p(inertia.n_rows, display_progress);

    // Loop over timepoints
    for (arma::uword m = 0; m < inertia.n_rows; m++)
    {
        // Loop over caseControls
        for (arma::uword i = 0; i < caseControls.n_cols; i++)
        {
            // caseControl dyad id
            int dyad = caseControls(m, i);
            // sender, receiver, event_type
            int sender = riskset(dyad, 0);
            int receiver = riskset(dyad, 1);
            int event_type = riskset(dyad, 2);
            // calculate the triad value for this dyad
            stat(m, i) = dyad_triad(type, inertia,
                                    m, sender, receiver, event_type,
                                    risksetMatrix, N, C, consider_type);
        }

        p.increment();
    }

    // Output
    return stat;
}

// =============================================================================
// Participation shifts
// =============================================================================

// [[Rcpp::export]]
bool creates_pshift(std::string type, int event, int actorA, int actorB,
                    int prev_event_type, arma::mat riskset, bool consider_type)
{
    bool result = false;

    int sender = riskset(event, 0);
    int receiver = riskset(event, 1);
    int event_type = riskset(event, 2);

    if (type == "AB-BA")
    {
        if (consider_type)
        {
            result = (sender == actorB) && (receiver == actorA) && (event_type == prev_event_type);
        }
        else
        {
            result = (sender == actorB) && (receiver == actorA);
        }
    }
    else if (type == "AB-AB")
    {
        if (consider_type)
        {
            result = (sender == actorA) && (receiver == actorB) && (event_type == prev_event_type);
        }
        else
        {
            result = (sender == actorA) && (receiver == actorB);
        }
    }
    else if (type == "AB-AY")
    {
        if (consider_type)
        {
            result = (sender == actorA) && (receiver != actorB) && (event_type == prev_event_type);
        }
        else
        {
            result = (sender == actorA) && (receiver != actorB);
        }
    }
    else if (type == "AB-BY")
    {
        if (consider_type)
        {
            result = (sender == actorB) && (receiver != actorA) && (event_type == prev_event_type);
        }
        else
        {
            result = (sender == actorB) && (receiver != actorA);
        }
    }
    else if (type == "AB-XA")
    {
        if (consider_type)
        {
            result = (sender != actorB) && (receiver == actorA) && (event_type == prev_event_type);
        }
        else
        {
            result = (sender != actorB) && (receiver == actorA);
        }
    }
    else if (type == "AB-XB")
    {
        if (consider_type)
        {
            result = (sender != actorA) && (receiver == actorB) && (event_type == prev_event_type);
        }
        else
        {
            result = (sender != actorA) && (receiver == actorB);
        }
    }
    else if (type == "AB-XY")
    {
        if (consider_type)
        {
            result = (sender != actorA) && (receiver != actorB) && (event_type == prev_event_type);
        }
        else
        {
            result = (sender != actorA) && (receiver != actorB);
        }
    }

    return result;
}

// NOTE: to deal with subsetting of the edgelist (with the 'start' and 'stop')
// arguments in remstats, the Rcpp::List events starts one event earlier than
// the caseControls. Thus, at iteration m, we have the previous event in the
// corresponding events vector, and the current event in the caseControls.
// When no previous events exist, this event is given the -1 id in Rcpp::List
// events. Further, the events in Rcpp::List events are using R indexing and
// should thus be transformed to cpp indexing.
// [[Rcpp::export]]
arma::mat pshift_sample_stat(std::string type,
                             const arma::mat &caseControls, Rcpp::List events,
                             const arma::mat &riskset,
                             const arma::mat &risksetMatrix,
                             bool consider_type, bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating pshift " << type << " statistic" << std::endl;
    }

    // Initialize the 'stat' matrix
    arma::mat stat(caseControls.n_rows, caseControls.n_cols, arma::fill::zeros);

    // Progress bar
    Progress p(caseControls.n_rows, display_progress);

    // Loop over events
    for (arma::uword m = 0; m < caseControls.n_rows; m++)
    {
        // ids of the previous dyads
        arma::vec prev_dyads = events(m);
        // subtract 1 to transform to cpp indexing
        prev_dyads = prev_dyads - 1;

        // case controls for the current time point
        arma::vec timepoint_caseControls = caseControls.row(m).t();

        // Loop over previous dyads
        for (arma::uword i = 0; i < prev_dyads.n_elem; i++)
        {
            // Previous dyad
            int prev_dyad = prev_dyads(i);
            if (prev_dyad < 0)
            {
                continue;
            }
            int actorA = riskset(prev_dyad, 0);
            int actorB = riskset(prev_dyad, 1);
            int event_type = riskset(prev_dyad, 2);

            // Loop over caseControls to check if the p-shift is created
            for (arma::uword j = 0; j < timepoint_caseControls.n_elem; ++j)
            {
                int event = timepoint_caseControls(j);
                bool pshift = creates_pshift(type, event, actorA, actorB,
                                             event_type, riskset, consider_type);
                if (pshift)
                {
                    stat(m, j) = 1;
                }
            }
        }

        p.increment();
    }

    // Output
    return stat;
}

// =============================================================================
// Recency statistics
// =============================================================================
// From a given index of an event in the edgelist, update lastActive
void update_lastActive(int event, std::string type, const arma::mat &edgelist,
                       int C, int N, arma::mat risksetMatrix,
                       bool consider_type, arma::vec &lastActive)
{
    // Extract information about the event
    double time = edgelist(event, 0);
    int sender = edgelist(event, 1);
    int receiver = edgelist(event, 2);
    int event_type = 0;
    if (C > 1)
    {
        event_type = edgelist(event, 3);
    }

    // Find the dyads for which lastActive needs to be updated
    arma::vec dyads;

    if (type == "recencyContinue")
    {
        if (consider_type)
        {
            dyads.set_size(1);
            dyads(0) = risksetMatrix(sender, receiver + event_type * N);
        }
        else
        {
            // Loop over event types
            for (int k = 0; k < C; ++k)
            {
                arma::vec temp(1);
                temp(0) = risksetMatrix(sender, receiver + k * N);
                dyads = arma::join_cols(dyads, temp);
            }
        }
    }
    else if (type == "recencySendSender")
    {
        if (consider_type)
        {
            arma::vec temp = risksetMatrix.row(sender).t();
            dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
        }
        else
        {
            dyads = risksetMatrix.row(sender).t();
        }
    }
    else if (type == "recencySendReceiver")
    {
        if (consider_type)
        {
            dyads = risksetMatrix.col(sender + event_type * N);
        }
        else
        {
            for (int k = 0; k < C; ++k)
            {
                arma::vec temp = risksetMatrix.col(sender + k * N);
                dyads = arma::join_cols(dyads, temp);
            }
        }
    }
    else if (type == "recencyReceiveSender")
    {
        if (consider_type)
        {
            arma::vec temp = risksetMatrix.row(receiver).t();
            dyads = temp.subvec(event_type * N, ((event_type + 1) * N) - 1);
        }
        else
        {
            dyads = risksetMatrix.row(receiver).t();
        }
    }
    else if (type == "recencyReceiveReceiver")
    {
        if (consider_type)
        {
            dyads = risksetMatrix.col(receiver + event_type * N);
        }
        else
        {
            for (int k = 0; k < C; ++k)
            {
                arma::vec temp = risksetMatrix.col(receiver + k * N);
                dyads = arma::join_cols(dyads, temp);
            }
        }
    }

    // Update lastActive
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
arma::mat recency_sample_stat(std::string type, const arma::mat &edgelist,
                              const arma::mat &risksetMatrix, const arma::mat &caseControls,
                              int start, int stop, bool consider_type,
                              Rcpp::String method, bool display_progress)
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
    arma::mat recency(caseControls.n_rows, caseControls.n_cols, arma::fill::zeros);

    // Initialize vector with times the dyads were last active
    arma::vec lastActive(risksetMatrix.max() + 1);
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
        // Get the index of the event in the edgelist
        arma::uword event = event_indices(m);
        // Update lastActive
        update_lastActive(event, type, edgelist, C, N, risksetMatrix, consider_type, lastActive);
    }

    // Loop over new time points
    for (arma::uword m = 0; m < time_points.n_elem; ++m)
    {
        // Event_time
        double current_time = time_points(m);

        // Compute stat
        arma::vec fr = 1 / ((current_time - lastActive) + 1);

        // Iterate over caseControls at this time
        for (arma::uword i = 0; i < caseControls.n_cols; ++i)
        {
            // Dyad id
            int dyad_id = caseControls(m, i);
            // Save the stat for this dyad
            recency(m, i) = fr(dyad_id);
        }

        // Get the indices of the events for which lastActive needs to be updated
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

        // Loop over event indices
        // For loop over event_indices
        for (arma::uword i = 0; i < event_indices.n_elem; ++i)
        {
            // Get the index of the event in the edgelist
            arma::uword event = event_indices(i);
            // Update lastActive
            update_lastActive(event, type, edgelist, C, N, risksetMatrix, consider_type, lastActive);
        }

        p.increment();
    }

    // Output
    return recency;
}

// =============================================================================
// Recency rank statistics
// =============================================================================

// For the recency rank statistic, update the 'lastTime' array that indicates
// to whom the sender has most recently send/received events based on the index
// of one event in the edgelist
void update_lastTime(int event, int type, const arma::mat &edgelist, int C,
                     bool consider_type, arma::cube &lastTime)
{
    // Extract information about the event
    double time = edgelist(event, 0);
    int sender = edgelist(event, 1);
    int receiver = edgelist(event, 2);
    int event_type = 0;
    if (C > 1 && consider_type)
    {
        event_type = edgelist(event, 3);
    }

    // update lastTime
    // rrankSend: to whom the sender most recently has send event
    // (most recent times)
    if (type == 1)
    {
        lastTime(sender, receiver, event_type) = time;
    }
    // rrankReceive: from whom the sender has most recently received events
    // (most recent times)
    if (type == 2)
    {
        lastTime(receiver, sender, event_type) = time;
    }
}

arma::rowvec sample_rankR(arma::rowvec x, int N)
{
    arma::uvec ranksU = N - sort_index(sort_index(x));
    arma::rowvec ranks = arma::conv_to<arma::rowvec>::from(ranksU);
    arma::uvec indices = arma::find(x == 0);
    arma::rowvec reps(indices.n_elem, arma::fill::zeros);
    ranks(indices) = reps;
    return ranks;
}

// [[Rcpp::export]]
arma::mat rrank_sample_stat(int type, const arma::mat &edgelist,
                            const arma::mat &caseControls,
                            const arma::mat &riskset, int N, int C,
                            int start, int stop, bool consider_type,
                            Rcpp::String method, bool display_progress)
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

    // Initialize rrank matrix
    arma::mat rrank(caseControls.n_rows, caseControls.n_cols, arma::fill::zeros);

    // Progress bar
    Progress p(time_points.n_elem, display_progress);

    // Select the events in the past to initialize 'lastTime'
    double first_time = edgelist(start, 0);
    arma::uvec event_indices = arma::find(edgelist.col(0) < first_time);

    // Initialize lastTime array
    arma::cube lastTime(N, N, C);

    // For loop over the past
    for (arma::uword m = 0; m < event_indices.n_elem; ++m)
    {
        // Get the index of the event in the edgelist
        int event = event_indices(m);
        // Update the lastTime array
        update_lastTime(event, type, edgelist, C, consider_type, lastTime);
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
                ranks.slice(c).row(j) = sample_rankR(lastTime.slice(c).row(j), N);
            }
        }

        // Statistic values
        arma::cube values = 1 / ranks;
        values.replace(arma::datum::inf, 0);

        // Iterate over case Controls
        for (arma::uword j = 0; j < caseControls.n_cols; ++j)
        {
            int dyad = caseControls(i, j);
            if (consider_type)
            {
                rrank(i, j) = values(riskset(dyad, 0), riskset(dyad, 1), riskset(dyad, 2));
            }
            else
            {
                rrank(i, j) = values(riskset(dyad, 0), riskset(dyad, 1), 0);
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
            // Get the index of the event in the edgelist
            int event = event_indices(m);
            // Update the lastTime array
            update_lastTime(event, type, edgelist, C, consider_type, lastTime);
        }
    }

    return rrank;
}

int sample_stats_effects_number(Rcpp::String effect)
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

// =============================================================================
// Array with statistics
// =============================================================================
//[[Rcpp::export]]
arma::cube sample_stats(Rcpp::CharacterVector effects,
                        const arma::mat &edgelist,
                        const arma::mat &caseControls,
                        const arma::mat &riskset,
                        const arma::mat &risksetMatrix,
                        const arma::mat &inertia,
                        const Rcpp::List &covariates,
                        const Rcpp::List &interactions,
                        const Rcpp::List &events,
                        Rcpp::String memory,
                        const arma::vec &memory_value,
                        Rcpp::CharacterVector &scaling,
                        Rcpp::LogicalVector &consider_type,
                        int start, int stop,
                        bool directed,
                        bool display_progress,
                        Rcpp::String method)
{
    // Get number of actors (N) and types (C) in the network
    int N = risksetMatrix.n_rows;
    int C = risksetMatrix.n_cols / N;

    // Initialize saving space
    arma::cube stats(caseControls.n_rows, caseControls.n_cols, effects.size());

    // Loop over effects
    for (int i = 0; i < effects.size(); ++i)
    {
        // Get case number
        Rcpp::String effectName = effects(i);
        int effect = sample_stats_effects_number(effectName);

        // Compute effect
        switch (effect)
        {

        // baseline
        case 1:
            stats.slice(i).fill(1);
            break;

        // FEtype
        /*case 2:
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
        */

        // inertia
        case 101:
            // Compute statistic
            stats.slice(i) = inertia_sample_stat(inertia, caseControls, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // reciprocity
        case 102:
            // Compute statistic
            stats.slice(i) = reciprocity_sample_stat(inertia, caseControls, riskset, risksetMatrix, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_reciprocity(stats.slice(i), inertia, risksetMatrix, N, consider_type(i));
            }*/
            break;

        // indegreeSender
        case 111:
            // Compute statistic
            stats.slice(i) = degree_sample_stat(1, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }
            break;*/

        // indegreeReceiver
        case 112:
            // Compute statistic
            stats.slice(i) = degree_sample_stat(2, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // outdegreeSender
        case 113:
            // Compute statistic
            stats.slice(i) = degree_sample_stat(3, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /* if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // outdegreeReceiver
        case 114:
            // Compute statistic
            stats.slice(i) = degree_sample_stat(4, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /* if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // totaldegreeSender
        case 115:
            // Compute statistic
            stats.slice(i) = degree_sample_stat(5, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // totaldegreeReceiver
        case 116:
            // Compute statistic
            stats.slice(i) = degree_sample_stat(6, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // totaldegreeDyad
        case 117:
            // Compute statistic
            stats.slice(i) = dyad_degree_sample_stat(4, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // degreeMin
        case 118:
            // Compute statistic
            stats.slice(i) = dyad_degree_sample_stat(1, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // degreeMax
        case 119:
            // Compute statistic
            stats.slice(i) = dyad_degree_sample_stat(2, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // degreeDiff
        case 120:
            // Compute statistic
            stats.slice(i) = dyad_degree_sample_stat(3, inertia, caseControls, risksetMatrix, riskset, consider_type(i), display_progress);
            // Proportional scaling
            /*if (scaling(i) == "prop")
            {
                stats.slice(i) = normalize_degree(stats.slice(i), inertia, N, effect, start);
            }*/
            break;

        // otp
        case 131:
            // Compute statistic
            stats.slice(i) = triad_sample_stat(1, inertia, caseControls, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // itp
        case 132:
            // Compute statistic
            stats.slice(i) = triad_sample_stat(2, inertia, caseControls, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // osp
        case 133:
            // Compute statistic
            stats.slice(i) = triad_sample_stat(3, inertia, caseControls, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // isp
        case 134:
            // Compute statistic
            stats.slice(i) = triad_sample_stat(4, inertia, caseControls, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // sp
        /*
        case 135:
            // Compute statistic
            stats.slice(i) = calculate_triad(5, inertia, risksetMatrix, scaling(i), consider_type(i), display_progress);
            break;*/

        // psABBA
        case 141:
            // Compute statistic
            stats.slice(i) = pshift_sample_stat("AB-BA", caseControls, events, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // psABBY
        case 142:
            // Compute statistic
            stats.slice(i) = pshift_sample_stat("AB-BY", caseControls, events, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // psABXA
        case 143:
            // Compute statistic
            stats.slice(i) = pshift_sample_stat("AB-XA", caseControls, events, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // psABXB
        case 144:
            // Compute statistic
            stats.slice(i) = pshift_sample_stat("AB-XB", caseControls, events, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // psABXY
        case 145:
            // Compute statistic
            stats.slice(i) = pshift_sample_stat("AB-XY", caseControls, events, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // psABAY
        case 146:
            // Compute statistic
            stats.slice(i) = pshift_sample_stat("AB-AY", caseControls, events, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // psABAB
        case 147:
            // Compute statistic
            stats.slice(i) = pshift_sample_stat("AB-AB", caseControls, events, riskset, risksetMatrix, consider_type(i), display_progress);
            break;

        // rrankSend
        case 151:
            // Compute statistic
            stats.slice(i) = rrank_sample_stat(1, edgelist, caseControls, riskset, N, C, start, stop, consider_type(i), method, display_progress);
            break;

        // rrankReceive
        case 152:
            // Compute statistic
            stats.slice(i) = rrank_sample_stat(2, edgelist, caseControls, riskset, N, C, start, stop, consider_type(i), method, display_progress);
            break;

        // recencyContinue
        case 161:
            // Compute statistic
            stats.slice(i) = recency_sample_stat("recencyContinue", edgelist, risksetMatrix, caseControls, start, stop, consider_type(i), method, display_progress);
            break;

        // recencySendSender
        case 162:
            // Compute statistic
            stats.slice(i) = recency_sample_stat("recencySendSender", edgelist, risksetMatrix, caseControls, start, stop, consider_type(i), method, display_progress);
            break;

        // recencySendReceiver
        case 163:
            // Compute statistic
            stats.slice(i) = recency_sample_stat("recencySendReceiver", edgelist, risksetMatrix, caseControls, start, stop, consider_type(i), method, display_progress);
            break;

        // recencyReceiveSender
        case 164:
            // Compute statistic
            stats.slice(i) = recency_sample_stat("recencyReceiveSender", edgelist, risksetMatrix, caseControls, start, stop, consider_type(i), method, display_progress);
            break;

        // recencyReceiveReceiver
        case 165:
            // Compute statistic
            stats.slice(i) = recency_sample_stat("recencyReceiveReceiver", edgelist, risksetMatrix, caseControls, start, stop, consider_type(i), method, display_progress);
            break;

        // userStat
        /*case 888:
            stats.slice(i) = get_userstat(covariates[i], edgelist, start, stop, display_progress, method);
            break;*/

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
        /*if (scaling(i) == "std" || scaling(i) == "std_abs" || scaling(i) == "std_unique")
        {
            stats.slice(i) = standardize(stats.slice(i));
        }*/
    }

    return stats;
}