#' Printing Relational Event Network Statistics
#'
#' Print a \code{\link{remstats}} object in a user-friendly format.
#'
#' @param x object of class \code{\link{remstats}}.
#'
#' @examples
#' rehObject <- remify::reh(edgelist = history, model = "tie")
#' remstatsObject <- remstats::remstats(reh = rehObject, tie_effects = ~ remstats::inertia())
#' print(remstatsObject)
#'
#' rehObject <- remify::reh(edgelist = history, model = "actor")
#' remstatsObject <- remstats::remstats(reh = rehObject, receiver_effects = ~ inertia())
#' print(remstatsObject)
#'
#' @export
print.remstats <- function(x, ...) {
  if (!any(class(x) == "remstats")) {
    stop("Expected object of class 'remstats'")
  }

  title <- "Relational Event Network Statistics"
  model <- ifelse(any(class(x) == "tomstats"), "tie-oriented", "actor-oriented")
  model.title <- paste("> Model:", model, sep = " ")

  if (model == "tie-oriented") {
    dim.stats <- dim(x$statistics)
    dim.long <- paste("> Dimensions:",
      dim.stats[1], "time points x",
      dim.stats[2], "dyads x",
      dim.stats[3], "statistics",
      sep = " "
    )
    stats.title <- paste("> Statistics:")
    stats.names <- dimnames(x$statistics)[[3]]
    stats.names2 <- lapply(1:length(stats.names), function(i) {
      paste0("\t >> ", i, ": ", stats.names[i])
    })
    stats.names3 <- paste(stats.names2, collapse = "\n")
    cat(paste(title, model.title, dim.long, stats.title, stats.names3,
      sep = "\n"
    ))
  } else if (model == "actor-oriented") {
    if (is.null(x$statistics$sender_stats)) {
      sender.title <- "> Sender model: empty"
    } else {
      sender.title <- "> Sender model:"
      dim.sender.stats <- dim(x$statistics$sender_stats)
      dim.sender.long <- paste("\t >> Dimensions:",
        dim.sender.stats[1], "time points x",
        dim.sender.stats[2], "actors x",
        dim.sender.stats[3], "statistics",
        sep = " "
      )
      stats.sender.title <- paste("\t >> Statistics:")
      stats.sender.names <- dimnames(x$statistics$sender_stats)[[3]]
      stats.sender.names2 <- lapply(1:length(stats.sender.names), function(i) {
        paste0("\t \t >>> ", i, ": ", stats.sender.names[i])
      })
      stats.sender.names3 <- paste(stats.sender.names2, collapse = "\n")
    }

    if (is.null(x$statistics$receiver_stats)) {
      receiver.title <- "> Receiver model: empty"
    } else {
      receiver.title <- "> Receiver model:"
      dim.receiver.stats <- dim(x$statistics$receiver_stats)
      dim.receiver.long <- paste("\t >> Dimensions:",
        dim.receiver.stats[1], "time points x",
        dim.receiver.stats[2], "actors x",
        dim.receiver.stats[3], "statistics",
        sep = " "
      )
      stats.receiver.title <- paste("\t >> Statistics:")
      stats.receiver.names <- dimnames(x$statistics$receiver_stats)[[3]]
      stats.receiver.names2 <- lapply(
        1:length(stats.receiver.names),
        function(i) {
          paste0("\t \t >>> ", i, ": ", stats.receiver.names[i])
        }
      )
      stats.receiver.names3 <- paste(stats.receiver.names2, collapse = "\n")
    }

    if (is.null(x$statistics$sender_stats)) {
      cat(paste(title, model.title,
        sender.title, receiver.title, dim.receiver.long, stats.receiver.title,
        stats.receiver.names3,
        sep = "\n"
      ))
    } else if (is.null(x$statistics$receiver_stats)) {
      cat(paste(title, model.title,
        sender.title, dim.sender.long, stats.sender.title, stats.sender.names3,
        sep = "\n"
      ))
    } else {
      cat(paste(title, model.title,
        sender.title, dim.sender.long, stats.sender.title, stats.sender.names3,
        receiver.title, dim.receiver.long, stats.receiver.title,
        stats.receiver.names3,
        sep = "\n"
      ))
    }
  }
}

#' Relational Event Network Statistics Summaries
#'
#' Produce summaries of each statistic from a \code{\link{remstats}} object.
#'
#' @param x object of class \code{\link{remstats}}.
#'
#' @examples
#' rehObject <- remify::reh(edgelist = history, model = "tie")
#' remstatsObject <- remstats::remstats(reh = rehObject, tie_effects = ~ remstats::inertia())
#' summary(remstatsObject)
#'
#' rehObject <- remify::reh(edgelist = history, model = "actor")
#' remstatsObject <- remstats::remstats(reh = rehObject, receiver_effects = ~ inertia())
#' summary(remstatsObject)
#'
#' @export

summary.remstats <- function(x, ...) {
  if (!any(class(x) == "remstats")) {
    stop("Expected object of class 'remstats'")
  }

  model <- ifelse(any(class(x) == "tomstats"), "tie-oriented", "actor-oriented")

  if (model == "tie-oriented") {
    out <- apply(x$statistics, 3, function(y) {
      summary(as.vector(y))
    })
  }

  if (model == "actor-oriented") {
    out <- lapply(x$statistics, function(y) {
      if (!is.null(y)) {
        apply(y, 3, function(z) {
          summary(as.vector(z))
        })
      } else {
        NULL
      }
    })
  }

  out
}
