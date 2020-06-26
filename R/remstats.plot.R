# plot.remstats
# 
# A function to plot statistics outputted from remstats(). The distribution of 
# the statistics over timepoints or dyads is plotted by means of boxplots. 
# (Note: the baseline statistic is not plotted)
# 
#  x: statistics object of class "remstats" 
#  along: along which dimension should the statistics be plotted: one of 
# "timepoints" or "dyads"
#  panels: whether plots for different statistics should be plotted in 
# different panels (panels = TRUE) or windows (panels = FALSE)
#
#' @method plot remstats
#' @export
plot.remstats <- function(x, along = c("timepoints", "dyads"), 
    panels = TRUE, ...) {

    stats <- x
    
    p <- seq_len(dim(stats)[3])
    if(any(dimnames(stats)[[3]] == "baseline")) {
        p <- p[-which(dimnames(stats)[[3]] == "baseline")]
    }

    if(panels) {
        graphics::par(mfrow=c(ceiling(length(p)/2),2))
    } else {
        graphics::par(mfrow=c(1,1))
    }
    
    along <- along[1]
    if(!(along %in% c("timepoints", "dyads"))) {
        stop("along should be one of timepoints or dyads")
    }
    if(along == "timepoints") {
        for(i in p) {
            if(all(stats[,,i] %in% c(0,1))) {
                graphics::plot(
                    apply(stats[,,i], 1, function(x) sum(x==1)/length(x))*100, 
                    type = "s", main = dimnames(stats)[[3]][i],
                    xlab = "timepoint", ylab = "% equal to 1")
            } else {
                graphics::boxplot(stats[,,i], use.cols = FALSE, 
                    main = dimnames(stats)[[3]][i],
                    xlab = "timepoint", ylab = "values")
            }            
        }
    }
    if(along == "dyads") {
        for(i in p) {
            if(all(stats[,,i] %in% c(0,1))) {
                graphics::plot(
                    apply(stats[,,i], 2, function(x) sum(x==1)/length(x))*100, 
                    type = "s", main = dimnames(stats)[[3]][i],
                    xlab = "dyad", ylab = "% equal to 1")
            } else {
                graphics::boxplot(stats[,,i], main = dimnames(stats)[[3]][i], 
                    xlab = "dyad", ylab = "values")
            }
        }
    }
}
