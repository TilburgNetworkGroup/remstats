#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include <map>

using namespace Rcpp;

// Helper function to get the timepoints
arma::vec get_timepoints_sender(const arma::mat &edgelist,
                                std::string method)
{
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

    return time_points;
}

// Helper function to get the indices of the events on which the statistics computation at a specific timepoint is based
arma::uvec event_indices_sender(const arma::mat &edgelist,
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

// Helper function to update decay weights
arma::vec update_decay_weights(double previous_time,
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

// Helper functions to update the degrees at a given timepoint
void update_indegree(arma::mat &indegree,
                     arma::uvec event_indices, int i,
                     const arma::mat &edgelist,
                     const arma::vec &weights)
{
    for (arma::uword j = 0; j < event_indices.n_elem; ++j)
    {
        arma::uword event = event_indices(j);
        int actor2 = edgelist(event, 2);
        indegree(i, actor2) += weights(event);
    }
}

void update_outdegree(arma::mat &oudegree,
                      arma::uvec event_indices, int i,
                      const arma::mat &edgelist,
                      const arma::vec &weights)
{
    for (arma::uword j = 0; j < event_indices.n_elem; ++j)
    {
        arma::uword event = event_indices(j);
        int actor1 = edgelist(event, 1);
        oudegree(i, actor1) += weights(event);
    }
}

arma::mat degree_sender(std::string type,
                        const arma::mat &edgelist,
                        const arma::vec &actors,
                        const arma::vec &weights,
                        std::string memory,
                        arma::vec memory_value,
                        int start,
                        int stop,
                        std::string method,
                        bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating " << type << "degree statistic" << std::endl;
    }

    // Event times
    arma::vec time_points = get_timepoints_sender(edgelist, method);

    // Subset the time points based on the provided start and stop indices
    time_points = time_points.subvec(start, stop);

    // Initialize the statistic
    arma::mat degree(time_points.n_elem, actors.n_elem, arma::fill::zeros);

    // Initialize the helper objects
    arma::mat indegree;
    arma::mat outdegree;
    if ((type == "in") || (type == "total"))
    {
        indegree.set_size(time_points.n_elem, actors.n_elem);
        indegree.zeros();
    }

    if ((type == "out") || (type == "total"))
    {
        outdegree.set_size(time_points.n_elem, actors.n_elem);
        outdegree.zeros();
    }

    // Progress bar
    Progress p(time_points.n_elem, display_progress);

    // Loop over time points to calculate degree
    for (arma::uword i = 0; i < time_points.n_elem; ++i)
    {
        // Indices of the events with which the degrees are updated
        arma::uvec event_indices = event_indices_sender(edgelist, time_points, start, i, memory, memory_value, method);

        if (memory == "full")
        {
            // Set the current row equal to the previous
            if (i > 0)
            {
                if ((type == "in") || (type == "total"))
                {
                    indegree.row(i) = indegree.row(i - 1);
                }

                if ((type == "out") || (type == "total"))
                {
                    outdegree.row(i) = outdegree.row(i - 1);
                }
            }
        }

        // Update the degree
        if ((memory == "full") || (memory == "interval"))
        {
            if ((type == "in") || (type == "total"))
            {
                update_indegree(indegree, event_indices, i, edgelist, weights);
            }

            if ((type == "out") || (type == "total"))
            {
                update_outdegree(outdegree, event_indices, i, edgelist, weights);
            }
        }
        else if (memory == "decay")
        {
            // Declare the previous event time variable
            double previous_time = 0;
            if (i == 0 && start > 0)
            {
                // Get the previous event time if start is larger than 0
                arma::vec event_times; // renew because time_points is subsetted
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
            arma::vec decay_weights = update_decay_weights(previous_time, event_indices, weights, edgelist, memory_value(0));

            // Update the degree
            if ((type == "in") || (type == "total"))
            {
                update_indegree(indegree, event_indices, i, edgelist, decay_weights);
            }

            if ((type == "out") || (type == "total"))
            {
                update_outdegree(outdegree, event_indices, i, edgelist, decay_weights);
            }
        }

        p.increment();
    }

    // The statistic
    if (type == "in")
    {
        degree = indegree;
    }
    if (type == "out")
    {
        degree = outdegree;
    }
    if (type == "total")
    {
        degree = indegree + outdegree;
    }

    return degree;
}

Rcpp::List getEventIndices(const arma::mat &edgelist,
                           int start, int stop,
                           std::string method,
                           std::string model)
{
    Rcpp::List eventIndices;

    if (method == "pt")
    {
        arma::vec eventTimes;
        if (model == "sender")
        {
            arma::vec uniqueTimes = arma::unique(edgelist.col(0));
            eventTimes = uniqueTimes.subvec(start, stop);
        }
        else if (model == "receiver")
        {
            eventTimes = arma::unique(edgelist.col(0).subvec(start, stop));
        }

        eventIndices = Rcpp::List(eventTimes.n_elem);

        for (arma::uword i = 0; i < eventTimes.n_elem; ++i)
        {
            arma::uvec indices = arma::find(edgelist.col(0) == eventTimes(i));
            eventIndices[i] = Rcpp::wrap(indices);
        }
    }
    else if (method == "pe")
    {
        int numEvents = stop - start + 1;
        eventIndices = Rcpp::List(numEvents);

        for (int i = 0; i < numEvents; ++i)
        {
            eventIndices[i] = i;
        }
    }
    else
    {
        Rcpp::stop("Invalid method specified");
    }

    return eventIndices;
}

arma::mat inertia_receiver(const arma::mat &edgelist,
                           const arma::vec &actors,
                           const arma::vec &weights,
                           int start, int stop,
                           std::string memory,
                           arma::vec memory_value,
                           Rcpp::String scaling,
                           std::string method,
                           bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating inertia statistic" << std::endl;
    }

    // Declare the statistics matrix (m x n)
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);
    // Declare the inertia matrix (n x n)
    arma::mat inertia(actors.n_elem, actors.n_elem, arma::fill::zeros);

    if (memory == "full")
    {
        // Get the past_events
        arma::uvec past_events = arma::find(edgelist.col(0) < edgelist(start, 0));

        // Loop over past_events to initialize inertia
        for (arma::uword j = 0; j < past_events.n_elem; ++j)
        {
            int event = past_events(j);
            int sender = edgelist(event, 1);
            int receiver = edgelist(event, 2);
            inertia(sender, receiver) += weights(event);
        }

        // Get the eventIndices based on the method
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            arma::vec events = eventIndices[i];

            // Loop over events to calculate stat
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                int sender = edgelist(event, 1);
                stat.row(event - start) = inertia.row(sender);
            }

            // Loop over events to update inertia
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                int sender = edgelist(event, 1);
                int receiver = edgelist(event, 2);
                inertia(sender, receiver) += weights(event);
            }

            p.increment();
        }
    }
    else if (memory == "interval")
    {
        // Progress bar
        Progress p((stop - start + 1), display_progress);

        // Loop over events
        for (int i = 0; i < (stop - start + 1); ++i)
        {
            // Reset inertia
            inertia.zeros();

            // Event indicator
            arma::uword event = start + i;

            // Get the indices of events that fall within the interval
            double current_time = edgelist(event, 0);
            double lb_interval = current_time - memory_value(1);
            double ub_interval = current_time - memory_value(0);
            arma::uvec events = arma::find(edgelist.col(0) >= lb_interval && edgelist.col(0) < ub_interval);

            // Loop over events to calculate inertia
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int past_event = events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                inertia(past_sender, past_receiver) += weights(past_event);
            }

            // Calculate stat
            int sender = edgelist(event, 1);
            stat.row(i) = inertia.row(sender);

            p.increment();
        }
    }
    else if (memory == "decay")
    {
        // Get the eventIndices with method is 'pt'
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, "pt", "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            // Reset inertia
            inertia.zeros();

            // Events at this time
            arma::vec simultaneous_events = eventIndices[i];

            // Get the indices of events that fall within the past
            double current_time = edgelist(simultaneous_events(0), 0);
            arma::uvec past_events = arma::find(edgelist.col(0) < current_time);

            // Previous event time
            double previous_time = 0;
            if (i == 0 && start > 0)
            {
                previous_time = edgelist(arma::max(past_events), 0);
            }
            else if (i > 0)
            {
                arma::vec previous_events = eventIndices[i - 1];
                previous_time = edgelist(previous_events(0), 0);
            }

            // Update decay weights
            arma::vec decay_weights = update_decay_weights(previous_time, past_events, weights, edgelist, memory_value(0));

            // Loop over events to calculate inertia
            for (arma::uword j = 0; j < past_events.n_elem; ++j)
            {
                int past_event = past_events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                inertia(past_sender, past_receiver) += decay_weights(past_event);
            }

            // Loop over simultaneous_events to get stat
            for (arma::uword j = 0; j < simultaneous_events.n_elem; ++j)
            {
                int event = simultaneous_events(j);
                int sender = edgelist(event, 1);
                stat.row(event - start) = inertia.row(sender);
            }

            p.increment();
        }
    }

    if (scaling == "prop")
    {
        // Outdegree of the sender
        arma::mat degree = degree_sender("out", edgelist, actors, weights, memory, memory_value, start, stop, method, display_progress);

        for (arma::uword i = 0; i < stat.n_rows; ++i)
        {
            arma::uword event = start + i;
            int sender = edgelist(event, 1);
            stat.row(i) /= degree(i, sender);
        }

        // Replace NaN values
        stat.replace(arma::datum::nan, 1.0 / (actors.n_elem - 1.0));
    }

    return stat;
}

arma::mat reciprocity_receiver(const arma::mat &edgelist,
                               const arma::vec &actors,
                               const arma::vec &weights,
                               int start, int stop,
                               std::string memory,
                               arma::vec memory_value,
                               Rcpp::String scaling,
                               std::string method,
                               bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating reciprocity statistic" << std::endl;
    }

    // Declare the statistics matrix (m x n)
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);
    // Declare the reciprocity matrix (n x n)
    arma::mat reciprocity(actors.n_elem, actors.n_elem, arma::fill::zeros);

    if (memory == "full")
    {
        // Get the past_events
        arma::uvec past_events = arma::find(edgelist.col(0) < edgelist(start, 0));

        // Loop over past_events to initialize reciprocity
        for (arma::uword j = 0; j < past_events.n_elem; ++j)
        {
            int event = past_events(j);
            int sender = edgelist(event, 1);
            int receiver = edgelist(event, 2);
            reciprocity(receiver, sender) += weights(event);
        }

        // Get the eventIndices based on the method
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            arma::vec events = eventIndices[i];

            // Loop over events to calculate stat
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                int sender = edgelist(event, 1);
                stat.row(event - start) = reciprocity.row(sender);
            }

            // Loop over events to update reciprocity
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                int sender = edgelist(event, 1);
                int receiver = edgelist(event, 2);
                reciprocity(receiver, sender) += weights(event);
            }

            p.increment();
        }
    }
    else if (memory == "interval")
    {
        // Progress bar
        Progress p((stop - start + 1), display_progress);

        // Loop over events
        for (int i = 0; i < (stop - start + 1); ++i)
        {
            // Reset reciprocity
            reciprocity.zeros();

            // Event indicator
            arma::uword event = start + i;

            // Get the indices of events that fall within the interval
            double current_time = edgelist(event, 0);
            double lb_interval = current_time - memory_value(1);
            double ub_interval = current_time - memory_value(0);
            arma::uvec events = arma::find(edgelist.col(0) >= lb_interval && edgelist.col(0) < ub_interval);

            // Loop over events to calculate reciprocity
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int past_event = events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                reciprocity(past_receiver, past_sender) += weights(past_event);
            }

            // Calculate stat
            int sender = edgelist(event, 1);
            stat.row(i) = reciprocity.row(sender);

            p.increment();
        }
    }
    else if (memory == "decay")
    {
        // Get the eventIndices with method is 'pt'
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, "pt", "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            // Reset reciprocity
            reciprocity.zeros();

            // Events at this time
            arma::vec simultaneous_events = eventIndices[i];

            // Get the indices of events that fall within the past
            double current_time = edgelist(simultaneous_events(0), 0);
            arma::uvec past_events = arma::find(edgelist.col(0) < current_time);

            // Previous event time
            double previous_time = 0;
            if (i == 0 && start > 0)
            {
                previous_time = edgelist(arma::max(past_events), 0);
            }
            else if (i > 0)
            {
                arma::vec previous_events = eventIndices[i - 1];
                previous_time = edgelist(previous_events(0), 0);
            }

            // Update decay weights
            arma::vec decay_weights = update_decay_weights(previous_time, past_events, weights, edgelist, memory_value(0));

            // Loop over events to calculate reciprocity
            for (arma::uword j = 0; j < past_events.n_elem; ++j)
            {
                int past_event = past_events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                reciprocity(past_receiver, past_sender) += decay_weights(past_event);
            }

            // Loop over simultaneous_events to get stat
            for (arma::uword j = 0; j < simultaneous_events.n_elem; ++j)
            {
                int event = simultaneous_events(j);
                int sender = edgelist(event, 1);
                stat.row(event - start) = reciprocity.row(sender);
            }

            p.increment();
        }
    }

    if (scaling == "prop")
    {
        // Indegree of the sender
        arma::mat degree = degree_sender("in", edgelist, actors, weights, memory, memory_value, start, stop, method, display_progress);

        for (arma::uword i = 0; i < stat.n_rows; ++i)
        {
            arma::uword event = start + i;
            int sender = edgelist(event, 1);
            stat.row(i) /= degree(i, sender);
        }

        // Replace NaN values
        stat.replace(arma::datum::nan, 1.0 / (actors.n_elem - 1.0));
    }

    return stat;
}

arma::mat degree_receiver(std::string type,
                          const arma::mat &edgelist,
                          const arma::vec &actors,
                          const arma::vec &weights,
                          int start, int stop,
                          std::string memory,
                          arma::vec memory_value,
                          std::string method,
                          bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating " << type << "degree statistic" << std::endl;
    }

    // Declare the statistics matrix (m x n)
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);
    // Declare the indegree and outdegree vector (n)
    arma::vec indegree(actors.n_elem, arma::fill::zeros);
    arma::vec outdegree(actors.n_elem, arma::fill::zeros);

    if (memory == "full")
    {
        // Get the past_events
        arma::uvec past_events = arma::find(edgelist.col(0) < edgelist(start, 0));

        // Loop over past_events to initialize indegree and outdegree
        for (arma::uword j = 0; j < past_events.n_elem; ++j)
        {
            int event = past_events(j);
            int sender = edgelist(event, 1);
            int receiver = edgelist(event, 2);
            if ((type == "out") || (type == "total"))
            {
                outdegree(sender) += weights(event);
            }
            if ((type == "in") || (type == "total"))
            {
                indegree(receiver) += weights(event);
            }
        }

        // Get the eventIndices based on the method
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            arma::vec events = eventIndices[i];

            // Loop over events to calculate stat
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                if ((type == "out") || (type == "total"))
                {
                    stat.row(event - start) += outdegree.t();
                }
                if ((type == "in") || (type == "total"))
                {
                    stat.row(event - start) += indegree.t();
                }
            }

            // Loop over events to update inertia
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                int sender = edgelist(event, 1);
                int receiver = edgelist(event, 2);
                if ((type == "out") || (type == "total"))
                {
                    outdegree(sender) += weights(event);
                }
                if ((type == "in") || (type == "total"))
                {
                    indegree(receiver) += weights(event);
                }
            }

            p.increment();
        }
    }
    else if (memory == "interval")
    {
        // Progress bar
        Progress p((stop - start + 1), display_progress);

        // Loop over events
        for (int i = 0; i < (stop - start + 1); ++i)
        {
            // Reset in- and outdegree
            indegree.zeros();
            outdegree.zeros();

            // Event indicator
            arma::uword event = start + i;

            // Get the indices of events that fall within the interval
            double current_time = edgelist(event, 0);
            double lb_interval = current_time - memory_value(1);
            double ub_interval = current_time - memory_value(0);
            arma::uvec events = arma::find(edgelist.col(0) >= lb_interval && edgelist.col(0) < ub_interval);

            // Loop over events to calculate inertia
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int past_event = events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                if ((type == "out") || (type == "total"))
                {
                    outdegree(past_sender) += weights(past_event);
                }
                if ((type == "in") || (type == "total"))
                {
                    indegree(past_receiver) += weights(past_event);
                }
            }

            // Calculate stat
            if ((type == "out") || (type == "total"))
            {
                stat.row(i) += outdegree.t();
            }
            if ((type == "in") || (type == "total"))
            {
                stat.row(i) += indegree.t();
            }

            p.increment();
        }
    }
    else if (memory == "decay")
    {
        // Get the eventIndices with method is 'pt'
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, "pt", "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            // Reset in- and outdegree
            indegree.zeros();
            outdegree.zeros();

            // Events at this time
            arma::vec simultaneous_events = eventIndices[i];

            // Get the indices of events that fall within the past
            double current_time = edgelist(simultaneous_events(0), 0);
            arma::uvec past_events = arma::find(edgelist.col(0) < current_time);

            // Previous event time
            double previous_time = 0;
            if (i == 0 && start > 0)
            {
                previous_time = edgelist(arma::max(past_events), 0);
            }
            else if (i > 0)
            {
                arma::vec previous_events = eventIndices[i - 1];
                previous_time = edgelist(previous_events(0), 0);
            }

            // Update decay weights
            arma::vec decay_weights = update_decay_weights(previous_time, past_events, weights, edgelist, memory_value(0));

            // Loop over events to calculate inertia
            for (arma::uword j = 0; j < past_events.n_elem; ++j)
            {
                int past_event = past_events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                if ((type == "out") || (type == "total"))
                {
                    outdegree(past_sender) += decay_weights(past_event);
                }
                if ((type == "in") || (type == "total"))
                {
                    indegree(past_receiver) += decay_weights(past_event);
                }
            }

            // Loop over simultaneous_events to get stat
            for (arma::uword j = 0; j < simultaneous_events.n_elem; ++j)
            {
                int event = simultaneous_events(j);
                if ((type == "out") || (type == "total"))
                {
                    stat.row(i) += outdegree.t();
                }
                if ((type == "in") || (type == "total"))
                {
                    stat.row(i) += indegree.t();
                }
            }

            p.increment();
        }
    }

    return stat;
}

// Helper function to calculate the triadic statistics for a given timepoint based on a given inertia matrix (nxn)
arma::rowvec calculate_triad(std::string type,
                             arma::uword sender,
                             arma::mat inertia)
{
    // Initialize statrow (n)
    arma::rowvec statrow(inertia.n_cols, arma::fill::zeros);

    // Declare objects
    arma::vec count1, count2;
    arma::mat counts;
    arma::vec min_counts;

    // Loop over receivers
    for (arma::uword r = 0; r < inertia.n_cols; r++)
    {
        if (r == sender)
        {
            continue;
        }

        // otp
        if (type == "otp")
        {
            count1 = inertia.row(sender).t();
            count2 = inertia.col(r);
        }

        // itp
        if (type == "itp")
        {
            count1 = inertia.col(sender);
            count2 = inertia.row(r).t();
        }

        // osp
        if (type == "osp")
        {
            count1 = inertia.row(sender).t();
            count2 = inertia.row(r).t();
        }

        // isp
        if (type == "isp")
        {
            count1 = inertia.col(sender);
            count2 = inertia.col(r);
        }

        counts = arma::join_rows(count1, count2);
        min_counts = min(counts, 1);
        statrow(r) = sum(min_counts);
    }

    return statrow;
}

arma::mat triad_receiver(std::string type,
                         const arma::mat &edgelist,
                         const arma::vec &actors,
                         const arma::vec &weights,
                         int start, int stop,
                         std::string memory,
                         arma::vec memory_value,
                         Rcpp::String scaling,
                         std::string method,
                         bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating " << type << " statistic" << std::endl;
    }

    // Decare the statistics matrix (m x n)
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);
    // Declare the inertia matrix (n x n)
    arma::mat inertia(actors.n_elem, actors.n_elem, arma::fill::zeros);

    if (memory == "full")
    {
        // Get the past events
        arma::uvec past_events = arma::find(edgelist.col(0) < edgelist(start, 0));

        // Loop over past_events to initialize inertia
        for (arma::uword j = 0; j < past_events.n_elem; ++j)
        {
            int event = past_events(j);
            int sender = edgelist(event, 1);
            int receiver = edgelist(event, 2);
            inertia(sender, receiver) += weights(event);
        }

        // Get the eventIndices based on the method
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            arma::vec events = eventIndices[i];

            // Loop over events to calculate stat
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                arma::uword sender = edgelist(event, 1);
                stat.row(event - start) = calculate_triad(type, sender, inertia);
            }

            // Loop over events to update inertia
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                arma::uword sender = edgelist(event, 1);
                arma::uword receiver = edgelist(event, 2);
                inertia(sender, receiver) += weights(event);
            }

            // Convert to binary in the case of 'unique' partners
            if ((scaling == "none_unique") || (scaling == "std_unique"))
            {
                inertia = arma::conv_to<arma::mat>::from(inertia > 0);
            }

            p.increment();
        }
    }
    else if (memory == "interval")
    {
        // Progress bar
        Progress p((stop - start + 1), display_progress);

        // Loop over events
        for (int i = 0; i < (stop - start + 1); ++i)
        {
            // Reset inertia
            inertia.zeros();

            // Event indicator
            arma::uword event = start + i;

            // Get the indices of events that fall within the interval
            double current_time = edgelist(event, 0);
            double lb_interval = current_time - memory_value(1);
            double ub_interval = current_time - memory_value(0);
            arma::uvec events = arma::find(edgelist.col(0) >= lb_interval && edgelist.col(0) < ub_interval);

            // Loop over events to calculate inertia
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int past_event = events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                inertia(past_sender, past_receiver) += weights(past_event);
            }

            // Convert to binary in the case of 'unique' partners
            if ((scaling == "none_unique") || (scaling == "std_unique"))
            {
                inertia = arma::conv_to<arma::mat>::from(inertia > 0);
            }

            // Calculate stat
            int sender = edgelist(event, 1);
            stat.row(i) = calculate_triad(type, sender, inertia);

            p.increment();
        }
    }
    else if (memory == "decay")
    {
        // Get the eventIndices with method is 'pt'
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, "pt", "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            // Reset inertia
            inertia.zeros();

            // Events at this time
            arma::vec simultaneous_events = eventIndices[i];

            // Get the indices of events that fall within the past
            double current_time = edgelist(simultaneous_events(0), 0);
            arma::uvec past_events = arma::find(edgelist.col(0) < current_time);

            // Previous event time
            double previous_time = 0;
            if (i == 0 && start > 0)
            {
                previous_time = edgelist(arma::max(past_events), 0);
            }
            else if (i > 0)
            {
                arma::vec previous_events = eventIndices[i - 1];
                previous_time = edgelist(previous_events(0), 0);
            }

            // Update decay weights
            arma::vec decay_weights = update_decay_weights(previous_time, past_events, weights, edgelist, memory_value(0));

            // Loop over events to calculate inertia
            for (arma::uword j = 0; j < past_events.n_elem; ++j)
            {
                int past_event = past_events(j);
                int past_sender = edgelist(past_event, 1);
                int past_receiver = edgelist(past_event, 2);
                inertia(past_sender, past_receiver) += decay_weights(past_event);
            }

            // Convert to binary in the case of 'unique' partners
            if ((scaling == "none_unique") || (scaling == "std_unique"))
            {
                inertia = arma::conv_to<arma::mat>::from(inertia > 0);
            }

            // Loop over simultaneous_events to get stat
            for (arma::uword j = 0; j < simultaneous_events.n_elem; ++j)
            {
                int event = simultaneous_events(j);
                int sender = edgelist(event, 1);
                stat.row(event - start) = calculate_triad(type, sender, inertia);
            }

            p.increment();
        }
    }

    return stat;
}

void update_ranks(arma::mat &ranks,
                  std::string type,
                  arma::uvec events,
                  const arma::mat &edgelist)
{
    for (arma::uword j = 0; j < events.n_elem; ++j)
    {
        // Sender and receiver of the event
        int event = events(j);
        int sender = edgelist(event, 1);
        int receiver = edgelist(event, 2);

        // rrankSend
        if (type == "send")
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
        if (type == "receive")
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
    }
}

// Memory effects: decay doesn't make sense, interval does?
arma::mat rrank_receiver(std::string type,
                         const arma::mat &edgelist,
                         const arma::vec &actors,
                         int start,
                         int stop,
                         std::string method,
                         bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating rrank-" << type << " statistic" << std::endl;
    }

    // Decare the statistics matrix (m x n)
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);
    // Declare the ranks matrix (n x n)
    arma::mat ranks(actors.n_elem, actors.n_elem, arma::fill::zeros);

    // Determine the ranks before 'start'
    double time = edgelist(start, 0);
    arma::uvec past_events = arma::find(edgelist.col(0) < time);
    update_ranks(ranks, type, past_events, edgelist);

    // Get the eventIndices based on the method
    Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

    // Progress bar
    Progress p(eventIndices.size(), display_progress);

    // Loop over unique event times
    for (int i = 0; i < eventIndices.size(); ++i)
    {
        arma::uvec events = eventIndices[i];

        // Compute the statistic based on the current ranks
        for (arma::uword j = 0; j < events.n_elem; ++j)
        {
            int event = events(j);
            int sender = edgelist(event, 1);
            stat.row(event - start) = 1 / ranks.row(sender);
            stat.replace(arma::datum::inf, 0);
        }

        // Update the ranks
        update_ranks(ranks, type, events, edgelist);

        p.increment();
    }

    return stat;
}

void update_lastActiveDyad(arma::mat &lastActive,
                           arma::uvec events,
                           const arma::mat &edgelist)
{
    // For loop over the events
    for (arma::uword m = 0; m < events.n_elem; ++m)
    {
        // Event indicator
        int event = events(m);
        // Sender and receiver of the event
        int s = edgelist(event, 1);
        int r = edgelist(event, 2);
        // Event time
        double time = edgelist(event, 0);
        // Last time dyad was active
        lastActive(s, r) = time;
    }
}

void update_lastActiveActor(arma::rowvec &lastActive,
                            std::string type,
                            arma::uvec events,
                            const arma::mat &edgelist)
{
    // For loop over the events
    for (arma::uword m = 0; m < events.n_elem; ++m)
    {
        // Event indicator
        int event = events(m);
        // Sender and receiver of the event
        int s = edgelist(event, 1);
        int r = edgelist(event, 2);
        // Event time
        double time = edgelist(event, 0);
        if (type == "send")
        {
            // Last time sender was active
            lastActive(s) = time;
        }
        else if (type == "receive")
        {
            // Last time receiver was active
            lastActive(r) = time;
        }
    }
}

arma::mat recency_receiver(std::string type,
                           const arma::mat &edgelist,
                           const arma::vec &actors,
                           int start,
                           int stop,
                           std::string method,
                           bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating recency-" << type << " statistic" << std::endl;
    }

    // Declare the statistics matrix (m x n)
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);

    if (type == "continue")
    {
        // Initialize a matrix with the times the dyads were last active
        arma::mat lastActive(actors.n_elem, actors.n_elem);
        lastActive.fill(arma::datum::inf);

        // Determine the time dyads were last active before 'start'
        double start_time = edgelist(start, 0);
        arma::uvec past_events = arma::find(edgelist.col(0) < start_time);
        update_lastActiveDyad(lastActive, past_events, edgelist);

        // Get the eventIndices based on the method
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            arma::uvec events = eventIndices[i];

            // Compute the statistic based on lastActive
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                double time = edgelist(event, 0);
                int sender = edgelist(event, 1);
                stat.row(event - start) = 1 / ((time - lastActive.row(sender)) + 1);
            }

            // Update lastActive
            update_lastActiveDyad(lastActive, events, edgelist);

            p.increment();
        }
    }
    else
    {
        // Initialize a vector with the times the actors were last active
        arma::rowvec lastActive(actors.n_elem);
        lastActive.fill(arma::datum::inf);

        // Determine the time actors were last active before 'start'
        double start_time = edgelist(start, 0);
        arma::uvec past_events = arma::find(edgelist.col(0) < start_time);
        update_lastActiveActor(lastActive, type, past_events, edgelist);

        // Get the eventIndices based on the method
        Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

        // Progress bar
        Progress p(eventIndices.size(), display_progress);

        // Loop over unique event times
        for (int i = 0; i < eventIndices.size(); ++i)
        {
            arma::uvec events = eventIndices[i];

            // Compute the statistic based on lastActive
            for (arma::uword j = 0; j < events.n_elem; ++j)
            {
                int event = events(j);
                double time = edgelist(event, 0);
                int sender = edgelist(event, 1);
                stat.row(event - start) = 1 / ((time - lastActive) + 1);
            }

            // Update lastActive
            update_lastActiveActor(lastActive, type, events, edgelist);

            p.increment();
        }
    }

    return stat;
}

void get_pshift(arma::mat &pshift,
                int type,
                arma::uvec previous_events,
                arma::uvec current_events,
                int start, const arma::mat &edgelist)
{
    // Iterate over current events
    for (arma::uword c = 0; c < current_events.n_elem; ++c)
    {
        arma::uword current_event = current_events(c);
        arma::uword next_sender = edgelist(current_event, 1);
        int j = start + current_event;

        // Iterate over previous_events
        for (arma::uword p = 0; p < previous_events.n_elem; ++p)
        {
            arma::uword prev_event = previous_events(p);
            arma::uword prev_sender = edgelist(prev_event, 1);
            arma::uword prev_receiver = edgelist(prev_event, 2);

            if (type == 1) // AB-AB
            {
                if (prev_sender == next_sender)
                {
                    pshift(j, prev_receiver) = 1;
                }
            }
            else if (type == 2) // AB-BA
            {
                if (prev_receiver == next_sender)
                {
                    pshift(j, prev_sender) = 1;
                }
            }
            else if (type == 3) // AB-XB
            {
                if (prev_receiver != next_sender && prev_sender != next_sender)
                {
                    pshift(j, prev_receiver) = 1;
                }
            }
            else if (type == 4) // AB-XA
            {
                if (prev_receiver != next_sender && prev_sender != next_sender)
                {
                    pshift(j, prev_sender) = 1;
                }
            }
            else if (type == 5) // AB-AY
            {
                if (prev_sender == next_sender)
                {
                    pshift.row(j).ones();
                    pshift(j, prev_sender) = 0;
                    pshift(j, prev_receiver) = 0;
                }
            }
            else if (type == 6) // AB-BY
            {
                if (prev_receiver == next_sender)
                {
                    pshift.row(j).ones();
                    pshift(j, prev_sender) = 0;
                    pshift(j, prev_receiver) = 0;
                }
            }
            else if (type == 7) // AB-XY
            {
                if (prev_sender != next_sender && prev_receiver != next_sender)
                {
                    pshift.row(j).ones();
                    pshift(j, prev_sender) = 0;
                    pshift(j, prev_receiver) = 0;
                }
            }
        }
    }
}

arma::mat pshift_receiver(int type, const arma::mat &edgelist,
                          const arma::vec &actors,
                          std::string memory, arma::vec memory_value,
                          int start, int stop,
                          std::string method,
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
            Rcpp::Rcout << "Calculating " << statisticNames[type - 1] << " statistic" << std::endl;
        }
        else
        {
            Rcpp::Rcerr << "Invalid type value. Please provide a valid type." << std::endl;
        }
    }

    // Decare the pshift statistic matrix (m x n)
    arma::mat pshift(stop - start + 1, actors.n_elem, arma::fill::zeros);

    // Initialize pshift
    if (start > 0)
    {
        bool update = true;

        double current_time = edgelist(start, 0);
        double previous_time = edgelist(start - 1, 0); // time of last event
        if (method == "pt")                            // vs. last event time
        {
            previous_time = arma::max(arma::find(edgelist.col(0) < current_time));
        }

        if (memory == "interval")
        {
            // Did the last event occur within the interval?
            double lb_interval = current_time - memory_value(1);
            double ub_interval = current_time - memory_value(0);
            update = (previous_time >= lb_interval && previous_time < ub_interval);
        }

        if (update)
        {
            // All events at the last and current timepoint
            arma::uvec previous_events;
            arma::uvec current_events;
            if (method == "pt")
            {
                previous_events = arma::find(edgelist.col(0) == previous_time);
                current_events = arma::find(edgelist.col(0) == current_time);
            }
            else if (method == "pe")
            {
                previous_events.set_size(1);
                previous_events(0) = start - 1;
                current_events.set_size(1);
                current_events(0) = start;
            }

            // Use this information to compute pshifts
            get_pshift(pshift, type, previous_events, current_events, start, edgelist);
        }
    }
    else if (type == 7)
    {
        arma::rowvec one_row(pshift.n_cols, arma::fill::ones);
        pshift.row(0) = one_row;
    }

    // Get the eventIndices based on the method
    Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "receiver");

    // Progress bar
    Progress p(eventIndices.size(), display_progress);

    // Loop over unique event times
    for (int i = 0; i < (eventIndices.size() - 1); ++i)
    {
        arma::uvec current_events = eventIndices[i];
        arma::uvec next_events = eventIndices[i + 1];

        bool update = true;

        if (memory == "interval")
        {
            double current_time = edgelist(current_events(0), 0);
            double next_time = edgelist(next_events(0), 0);
            // Did the last event occur within the interval?
            double lb_interval = next_time - memory_value(1);
            double ub_interval = next_time - memory_value(0);
            update = (current_time >= lb_interval && current_time < ub_interval);
        }

        if (update)
        {
            // Use this information to compute pshifts
            get_pshift(pshift, type, current_events, next_events, start, edgelist);
        }

        p.increment();
    }

    return pshift;
}

arma::mat exoActor_receiver(const arma::mat &covariates,
                            const arma::mat &edgelist,
                            const arma::vec &actors,
                            int start, int stop,
                            bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating receive statistic" << std::endl;
    }

    // Initialize matrix
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);

    // Statistic for the first timepoint
    double time = edgelist(start, 0);

    for (arma::uword k = 0; k < actors.n_elem; ++k)
    {
        // Find all the attribute values for actor k before the first timepoint
        arma::uvec indices = find(covariates.col(0) == k &&
                                  covariates.col(1) <= time);
        arma::mat actorCovar = covariates.rows(indices);

        // Find the last attribute value for actor k before the first timepoint
        double value = actorCovar.col(1).max();

        // Add the value to the correct place in the stat
        stat(0, k) = value;
    }

    // Find the times when the covariates change
    arma::vec changetimes = unique(covariates.col(1));

    // Start counter
    arma::uword counter = 0;

    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
        if (m > 0)
        {
            // Copy the previous row
            stat.row(m) = stat.row(m - 1);
        }

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
                        arma::uvec index = find(covariates.col(0) == j && covariates.col(1) == changetimes(counter));
                        // Update if a new value exists
                        if (index.n_elem > 0)
                        {
                            double value = covariates(index(0), 2);
                            stat(m, j) = value;
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

arma::mat exoActor_sender(const arma::mat &covariates,
                          const arma::mat &edgelist,
                          const arma::vec &actors,
                          int start, int stop,
                          std::string method,
                          bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating send statistic" << std::endl;
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

    // Initialize matrix
    arma::mat stat(time_points.n_elem, actors.n_elem, arma::fill::zeros);

    // Statistic for the first timepoint
    double time = edgelist(start, 0);

    for (arma::uword k = 0; k < actors.n_elem; ++k)
    {
        // Find all the attribute values for actor k before the first timepoint
        arma::uvec indices = find(covariates.col(0) == k &&
                                  covariates.col(1) <= time);
        arma::mat actorCovar = covariates.rows(indices);

        // Find the last attribute value for actor k before the first timepoint
        double value = actorCovar.col(1).max();

        // Add the value to the correct place in the stat
        stat(0, k) = value;
    }

    // Find the times when the covariates change
    arma::vec changetimes = unique(covariates.col(1));

    // Start counter
    arma::uword counter = 0;

    // Iterate over timepoints
    for (arma::uword m = 0; m < time_points.n_elem; ++m)
    {
        if (m > 0)
        {
            // Copy the previous row
            stat.row(m) = stat.row(m - 1);
        }

        // Update the statistic if required
        // Do not update after the last changetime
        if (counter < changetimes.n_elem)
        {
            // Update if the time of the event is larger than the current
            // changetime
            if (time_points(m) >= changetimes(counter))
            {
                // Update all changes in between
                while (counter < changetimes.n_elem &&
                       time_points(m) >= changetimes(counter))
                {
                    // For loop over actors
                    for (arma::uword j = 0; j < actors.n_elem; ++j)
                    {
                        arma::uvec index = find(covariates.col(0) == j && covariates.col(1) == changetimes(counter));
                        // Update if a new value exists
                        if (index.n_elem > 0)
                        {
                            double value = covariates(index(0), 2);
                            stat(m, j) = value;
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

arma::mat exoDyad_receiver(std::string type,
                           const arma::mat &covariates,
                           const arma::mat &edgelist,
                           const arma::vec &actors,
                           int start, int stop,
                           bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating " << type << " statistic" << std::endl;
    }

    // Initialize matrix
    arma::mat stat(stop - start + 1, actors.n_elem, arma::fill::zeros);

    // Get all actor values
    arma::mat exoActor = exoActor_receiver(covariates, edgelist, actors, start, stop, false);

    // Iterate over events
    for (int m = 0; m < (stop - start + 1); ++m)
    {
        // Event indicator
        int event = m + start;

        // Sender of the event
        int sender = edgelist(event, 1);
        double sender_value = exoActor(m, sender);

        // Iterate over receivers
        for (arma::uword r = 0; r < actors.n_elem; ++r)
        {
            // Receiver value
            double receiver_value = exoActor(m, r);

            if (type == "same")
            {
                stat(m, r) = (receiver_value == sender_value);
            }

            if (type == "difference")
            {
                stat(m, r) = (sender_value - receiver_value);
            }

            arma::vec both_values = {sender_value, receiver_value};

            if (type == "average")
            {
                stat(m, r) = mean(both_values);
            }

            if (type == "minimum")
            {
                stat(m, r) = arma::min(both_values);
            }

            if (type == "maximum")
            {
                stat(m, r) = arma::max(both_values);
            }
        }
    }

    return stat;
}

arma::mat userStat_sender(const arma::mat &covariates,
                          const arma::mat &edgelist,
                          int start, int stop, std::string method,
                          bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Adding user statistic" << std::endl;
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

arma::mat userStat_receiver(const arma::mat &covariates,
                            const arma::mat &edgelist,
                            int start, int stop,
                            bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Adding user statistic" << std::endl;
    }

    // Get all event_times
    arma::vec event_times = edgelist.col(0);
    event_times = event_times.subvec(start, stop);

    // Get the stat
    arma::mat stat = covariates.rows(start, stop);

    // Check dimensions of input matrices
    if (stat.n_rows != event_times.n_elem)
    {
        throw std::invalid_argument("Invalid dimensions: mismatch between 'userStat' covariate object and number of event times.");
    }

    return (stat);
}

arma::mat exoTie_receiver(
    const arma::mat &covariates,
    const arma::mat &edgelist,
    const arma::vec &actors,
    int start, int stop,
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
    arma::uword sender, receiver;
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
                        receiver_indices(0) = receiver;
                        stat(edgelist_indices, receiver_indices).fill(value);
                    }
                }
            }

            p.increment();
        }
    }

    return stat;
}

arma::mat recency_sender(std::string type,
                         const arma::mat &edgelist,
                         const arma::vec &actors,
                         int start, int stop,
                         std::string method,
                         bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating recency" << type << " statistic" << std::endl;
    }

    // Get the eventIndices per timepoint based on the method
    Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "sender");

    // Declare the statistics matrix (m x n)
    arma::mat stat(eventIndices.size(), actors.n_elem, arma::fill::zeros);

    // Initialize a vector with the times the actors were last active
    arma::rowvec lastActive(actors.n_elem);
    lastActive.fill(arma::datum::inf);

    // Determine the time actors were last active before 'start'
    double start_time = edgelist(start, 0);
    arma::uvec past_events = arma::find(edgelist.col(0) < start_time);
    update_lastActiveActor(lastActive, type, past_events, edgelist);

    // Progress bar
    Progress p(eventIndices.size(), display_progress);

    // Loop over unique event times
    for (int i = 0; i < eventIndices.size(); ++i)
    {
        arma::uvec events = eventIndices[i];
        double time = edgelist(events(0), 0);

        // Compute the statistic based on lastActive
        stat.row(i) = 1 / ((time - lastActive) + 1);

        // Update lastActive
        update_lastActiveActor(lastActive, type, events, edgelist);

        p.increment();
    }

    return stat;
}

void get_pshift_sender(arma::mat &pshift,
                       int type, double value,
                       arma::uvec previous_events,
                       const arma::mat &edgelist,
                       int i)
{
    // Iterate over previous_events
    for (arma::uword p = 0; p < previous_events.n_elem; ++p)
    {
        arma::uword prev_event = previous_events(p);
        int prev_sender = edgelist(prev_event, 1);
        int prev_receiver = edgelist(prev_event, 2);

        if ((type == 1) || (type == 3))
        {
            pshift(i, prev_sender) = value;
        }
        if ((type == 2) || (type == 3))
        {
            pshift(i, prev_receiver) = value;
        }
    }
}

arma::mat pshift_sender(int type,
                        const arma::mat &edgelist,
                        const arma::vec &actors,
                        std::string memory, arma::vec memory_value,
                        int start, int stop,
                        std::string method,
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

    // Initialize the pshift statistic matrix (m x n)
    arma::mat pshift(stop - start + 1, actors.n_elem, arma::fill::zeros);
    if ((type == 1) || (type == 2)) // ABA or ABB
    {
        pshift.zeros();
    }
    else if (type == 3) // ABX
    {
        pshift.ones();
    }

    // Declare helper variables
    int actor = 0;
    int value = 1;
    if (type == 3)
    {
        value = 0;
    }

    if (start > 0)
    {

        bool update = true;

        double current_time = edgelist(start, 0);
        double previous_time = edgelist(start - 1, 0); // time of last event
        if (method == "pt")                            // vs. last event time
        {
            previous_time = arma::max(arma::find(edgelist.col(0) < current_time));
        }

        if (memory == "interval")
        {
            // Did the last event occur within the interval?
            double lb_interval = current_time - memory_value(1);
            double ub_interval = current_time - memory_value(0);
            update = (previous_time >= lb_interval && previous_time < ub_interval);
        }

        if (update)
        {
            // All events at the timepoint
            arma::uvec previous_events;
            if (method == "pt")
            {
                previous_events = arma::find(edgelist.col(0) == previous_time);
            }
            else if (method == "pe")
            {
                previous_events.set_size(1);
                previous_events(0) = start - 1;
            }

            // Update pshifts based on previous_events
            get_pshift_sender(pshift, type, value, previous_events, edgelist, 0);
        }
    }

    // Get the eventIndices based on the method
    Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method, "sender");

    // Progress bar
    Progress p(eventIndices.size(), display_progress);

    // Loop over unique event times
    for (int i = 1; i < eventIndices.size(); ++i)
    {
        arma::uvec previous_events = eventIndices[i - 1];

        bool update = true;

        if (memory == "interval")
        {
            arma::uvec current_events = eventIndices[i];
            double current_time = edgelist(current_events(0), 0);
            double previous_time = edgelist(previous_events(0), 0);
            // Did the last event occur within the interval?
            double lb_interval = current_time - memory_value(1);
            double ub_interval = current_time - memory_value(0);
            update = (previous_time >= lb_interval && previous_time < ub_interval);
        }

        if (update)
        {
            // Use this information to compute pshifts
            get_pshift_sender(pshift, type, value, previous_events, edgelist, i);
        }

        p.increment();
    }

    return pshift;
}

int getSenderEffectNumber(std::string effect)
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

arma::mat standardize_sender(arma::mat stat)
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

arma::mat normalize_degree_aom(arma::mat degree, int start, double value)
{
    // Calculate row sums of degree
    arma::vec rowSum = arma::sum(degree, 1);
    // Element-wise division
    degree.each_col() /= rowSum;
    // Replace NaN values with 0
    degree.replace(arma::datum::nan, 0);

    // First row
    if (start == 0)
    {
        degree.row(0).fill(value / degree.n_cols);
    }

    return degree;
}

// [[Rcpp::export]]
arma::cube compute_stats_sender(Rcpp::CharacterVector &effects,
                                const arma::mat &edgelist,
                                const arma::vec &actors,
                                const arma::vec &weights,
                                const Rcpp::List &covariates,
                                const Rcpp::List &interactions,
                                std::string memory,
                                const arma::vec memory_value,
                                Rcpp::CharacterVector &scaling,
                                int start, int stop,
                                std::string method,
                                bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating statistics sender model:" << std::endl;
    }

    // Get the eventIndices per timepoint based on the method
    Rcpp::List eventIndices = getEventIndices(edgelist, start, stop, method,  "sender");

    // Initialize saving space
    arma::cube senderStats(eventIndices.size(), actors.n_elem, effects.size());

    // For loop over effects
    for (int i = 0; i < effects.size(); ++i)
    {
        // Get case number
        Rcpp::String effectName = effects(i);
        int effect = getSenderEffectNumber(effectName);

        // Compute effect
        switch (effect)
        {

        // 1 baseline
        case 1:
            senderStats.slice(i).fill(1);
            break;

        // 2 send
        case 2:
            senderStats.slice(i) = exoActor_sender(covariates[i], edgelist, actors, start, stop, method, display_progress);
            break;

        // 3 in-degree
        case 3:
            senderStats.slice(i) = degree_sender("in", edgelist, actors, weights, memory, memory_value, start, stop, method, display_progress);

            if (scaling(i) == "prop")
            {
                senderStats.slice(i) = normalize_degree_aom(senderStats.slice(i), start, 1.0);
            }

            break;

        // 4 out-degree
        case 4:
            senderStats.slice(i) = degree_sender("out", edgelist, actors, weights, memory, memory_value, start, stop, method, display_progress);

            if (scaling(i) == "prop")
            {
                senderStats.slice(i) = normalize_degree_aom(senderStats.slice(i), start, 1.0);
            }

            break;

        // 5 total-degree
        case 5:
            senderStats.slice(i) = degree_sender("total", edgelist, actors, weights, memory, memory_value, start, stop, method, display_progress);

            if (scaling(i) == "prop")
            {
                senderStats.slice(i) = normalize_degree_aom(senderStats.slice(i), start, 1.0);
            }

            break;

        // 6 recencySendSender
        case 6:
            senderStats.slice(i) = recency_sender("send", edgelist, actors, start, stop, method, display_progress);
            break;

        // 7 recencyReceiveSender
        case 7:
            senderStats.slice(i) = recency_sender("receive", edgelist, actors, start, stop, method, display_progress);
            break;

        // 8 psABA
        case 8:
            senderStats.slice(i) = pshift_sender(1, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 9 psABB
        case 9:
            senderStats.slice(i) = pshift_sender(2, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 10 psABX
        case 10:
            senderStats.slice(i) = pshift_sender(3, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // userStat
        case 888:
            senderStats.slice(i) = userStat_sender(covariates[i], edgelist, start, stop, method, display_progress);
            break;

        // 999 interact
        case 999:
            // Get the indices of the statistics slices (+1) with the
            // statistics for which an interaction needs to be computed.
            arma::vec x = interactions[i];
            int main1 = x(0);
            int main2 = x(1);
            // Element-wise multiplication
            senderStats.slice(i) = senderStats.slice(main1 - 1) % senderStats.slice(main2 - 1);
            break;
        }

        // Standardize
        if (scaling(i) == "std")
        {
            senderStats.slice(i) = standardize_sender(senderStats.slice(i));
        }
    }

    return senderStats;
}

int getReceiverEffectNumber(std::string effect)
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

arma::mat standardize_receiver(arma::mat stat, int start,
                               const arma::mat &edgelist,
                               const arma::vec &actors)
{

    // For loop over timepoints, i.e., rows
    for (arma::uword i = 0; i < stat.n_rows; ++i)
    {
        int event = start + i;
        arma::uword sender = edgelist(event, 1);

        // Extract non-sender values from the row
        arma::rowvec statrow = stat.row(i);
        arma::vec statrowMin = statrow(arma::find(actors != sender));

        // Calculate mean and standard deviation once for the row
        double meanVal = mean(statrowMin);
        double stddevVal = stddev(statrowMin);

        // For loop over receivers
        for (arma::uword r = 0; r < actors.n_elem; ++r)
        {
            if (sender == r)
            {
                stat(i, r) = 0;
            }
            else
            {
                stat(i, r) = (stat(i, r) - meanVal) / stddevVal;
            }
        }
    }

    // If the standard deviation is 0, the resulting values are NaN, replace
    // these values with 0
    stat.replace(arma::datum::nan, 0);

    // Return standardized statistic matrix
    return stat;
}

//[[Rcpp::export]]
arma::cube compute_stats_receiver(Rcpp::CharacterVector &effects,
                                  const arma::mat &edgelist,
                                  const arma::vec &actors,
                                  const arma::vec &weights,
                                  const Rcpp::List &covariates,
                                  const Rcpp::List &interactions,
                                  std::string memory,
                                  const arma::vec memory_value,
                                  Rcpp::CharacterVector &scaling,
                                  int start, int stop,
                                  std::string method,
                                  bool display_progress)
{
    // Progress update
    if (display_progress)
    {
        Rcpp::Rcout << "Calculating statistics receiver model:" << std::endl;
    }

    // Initialize saving space
    arma::cube receiverStats((stop - start + 1), actors.n_elem, effects.size());

    // For loop over effects
    for (int i = 0; i < effects.size(); ++i)
    {
        // Get case number
        Rcpp::String effectName = effects(i);
        int effect = getReceiverEffectNumber(effectName);

        // Compute effect
        switch (effect)
        {

        // 1 receive
        case 1:
            receiverStats.slice(i) = exoActor_receiver(covariates[i], edgelist, actors, start, stop, display_progress);
            break;

        // 2 same
        case 2:
            receiverStats.slice(i) = exoDyad_receiver("same", covariates[i], edgelist, actors, start, stop, display_progress);
            break;

        // 3 difference
        case 3:
            receiverStats.slice(i) = exoDyad_receiver("difference", covariates[i], edgelist, actors, start, stop, display_progress);

            if ((scaling[i] == "none_abs") || (scaling[i] == "std_abs"))
            {
                receiverStats.slice(i) = abs(receiverStats.slice(i));
            }

            break;

        // 4 average
        case 4:
            receiverStats.slice(i) = exoDyad_receiver("average", covariates[i], edgelist, actors, start, stop, display_progress);
            break;

        // 5 tie
        case 5:
            receiverStats.slice(i) = exoTie_receiver(covariates[i], edgelist, actors, start, stop, display_progress);
            break;

        // 6 inertia
        case 6:
            receiverStats.slice(i) = inertia_receiver(edgelist, actors, weights, start, stop, memory, memory_value, scaling(i), method, display_progress);
            break;

        // 7 reciprocity
        case 7:
            receiverStats.slice(i) = reciprocity_receiver(edgelist, actors, weights, start, stop, memory, memory_value, scaling(i), method, display_progress);
            break;

        // 8 in-degree
        case 8:
            receiverStats.slice(i) = degree_receiver("in", edgelist, actors, weights, start, stop, memory, memory_value, method, display_progress);

            if (scaling(i) == "prop")
            {
                receiverStats.slice(i) = normalize_degree_aom(receiverStats.slice(i), start, 1.0);
            }

            break;

        // 9 out-degree
        case 9:
            receiverStats.slice(i) = degree_receiver("out", edgelist, actors, weights, start, stop, memory, memory_value, method, display_progress);

            if (scaling(i) == "prop")
            {
                receiverStats.slice(i) = normalize_degree_aom(receiverStats.slice(i), start, 1.0);
            }

            break;

        // 10 total-degree
        case 10:
            receiverStats.slice(i) = degree_receiver("total", edgelist, actors, weights, start, stop, memory, memory_value, method, display_progress);

            if (scaling(i) == "prop")
            {
                receiverStats.slice(i) = normalize_degree_aom(receiverStats.slice(i), start, 1.0);
            }

            break;

        // 11 otp
        case 11:
            receiverStats.slice(i) = triad_receiver("otp", edgelist, actors, weights, start, stop, memory, memory_value, scaling[i], method, display_progress);
            break;

        // 12 itp
        case 12:
            receiverStats.slice(i) = triad_receiver("itp", edgelist, actors, weights, start, stop, memory, memory_value, scaling[i], method, display_progress);
            break;

        // 13 osp
        case 13:
            receiverStats.slice(i) = triad_receiver("osp", edgelist, actors, weights, start, stop, memory, memory_value, scaling[i], method, display_progress);
            break;

        // 14 isp
        case 14:
            receiverStats.slice(i) = triad_receiver("isp", edgelist, actors, weights, start, stop, memory, memory_value, scaling[i], method, display_progress);
            break;

        // 15 rrankSend
        case 15:
            receiverStats.slice(i) = rrank_receiver("send", edgelist, actors, start, stop, method, display_progress);
            break;

        // 16 rrankReceive
        case 16:
            receiverStats.slice(i) = rrank_receiver("receive", edgelist, actors, start, stop, method, display_progress);
            break;

        // 17 recencySendReceiver
        case 17:
            receiverStats.slice(i) = recency_receiver("send", edgelist, actors, start, stop, method, display_progress);
            break;

        // 18 recencyReceiveReceiver
        case 18:
            receiverStats.slice(i) = recency_receiver("receive", edgelist, actors, start, stop, method, display_progress);
            break;

        // 19 recencyContinue
        case 19:
            receiverStats.slice(i) = recency_receiver("continue", edgelist, actors, start, stop, method, display_progress);
            break;

        // 20 psABAB
        case 20:
            receiverStats.slice(i) = pshift_receiver(1, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 21 psABBA
        case 21:
            receiverStats.slice(i) = pshift_receiver(2, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 22 psABXB
        case 22:
            receiverStats.slice(i) = pshift_receiver(3, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 23 psABXA
        case 23:
            receiverStats.slice(i) = pshift_receiver(4, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 24 psABAY
        case 24:
            receiverStats.slice(i) = pshift_receiver(5, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 25 psABBY
        case 25:
            receiverStats.slice(i) = pshift_receiver(6, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // 26 psABXY
        case 26:
            receiverStats.slice(i) = pshift_receiver(7, edgelist, actors, memory, memory_value, start, stop, method, display_progress);
            break;

        // userStat
        case 888:
            receiverStats.slice(i) = userStat_receiver(covariates[i], edgelist, start, stop, display_progress);
            break;

        // 99 interact
        case 999:
            // Get the indices of the statistics slices (+1) with the
            // statistics for which an interaction needs to be computed.
            arma::vec x = interactions[i];
            int main1 = x(0);
            int main2 = x(1);
            // Element-wise multiplication
            receiverStats.slice(i) = receiverStats.slice(main1 - 1) % receiverStats.slice(main2 - 1);
            break;
        }

        // Standardize
        if ((scaling[i] == "std") || (scaling[i] == "std_abs") || (scaling[i] == "std_unique"))
        {
            receiverStats.slice(i) = standardize_receiver(receiverStats.slice(i), start, edgelist, actors);
        }
    }

    return receiverStats;
}
