#' Phenogram
#'
#' @param .x integer vector of ydays, pentades or months
#' @param .method character: one of "days", "pentades" or "months"
#' @param .x.months integer vector of the corresponding months
#' @param ... arguments passed to [grid::gpar]
#'
#' @export
#' @import ggplot2 data.table
#' @examples
#' set.seed(213)
#' x <- round(rnorm(n = 100, mean = 180, sd = 50))
#' phenogram(x)
#'
#' x <- round(rnorm(n = 100, mean = 40, sd = 10))
#' phenogram(x, method = "pentades")
#'
#' x <- round(rnorm(n = 100, mean = 5, sd = 1))
#' phenogram(x, method = "months")
#'
#' # with the sum of each month under the plot
#' x <- round(rnorm(n = 1000, mean = 5, sd = 1))
#' phenogram(x, .x.months = sample(1:12, 1000, replace = TRUE), method = "months")
#'
phenogram <- function(.x, .x.months = NULL, .method = "days", ...) {

  # cutoff = 200
  # if more than 10% of the data falls on single day: cut off the diagram
  # cutoff <- min(max(table(data_long$yday)), 0.1 * length(data_long$yday))

  data <- data.table(x = .x)
  if (.method == "days") {
    lines <- c(0.5, 31.5, 59.5, 90.5, 120.5, 151.5, 181.5,
      212.5, 243.5, 273.5, 304.5, 334.5, 365.5)
    # minor_lines <- filter(lines, rep(0.5, 2))[1:12]
    label_positions <- c(15, 45, 75, 106, 136, 167, 197, 228, 259, 290, 320, 350)
    bins <- 365
  }
  if (.method == "pentades") {
    label_positions <- seq(3.5, 69.5, length.out = 12)
    lines <- seq(0.5, 72.5, length.out = 13)
    bins <- 72
  }
  if (.method == "months") {
    label_positions <- seq(1, 12, length.out = 12)
    lines <- seq(0.5, 12.5, length.out = 13)
    bins <- 12
  }
  labels <- month.abb

  if (!is.null(.x.months)) {
    month_sums <- rep(0, 12)
    names(month_sums) <- 1:12
    numbers_per_month <- table(.x.months)
    for (i in seq_along(numbers_per_month)) {
      month_sums[names(month_sums) == names(numbers_per_month)[i]] <- numbers_per_month[i]
    }

    text <- vector("list", 12)
    for (i in 1:12) {
      text[[i]] <- grid::textGrob(as.character(month_sums[i]), gp = grid::gpar(...))#, fontface = "bold"))
    }
  }

  # ylim_upper = ..count.. / sum(..count..)

  p <- ggplot(data, aes(x = x)) + geom_bar() +
    ylab("Anzahl") + xlab("") + theme_bw() +
    # coord_cartesian(ylim = c(0, ylim_upper)) +
    scale_x_continuous(limits = c(0, bins + 1), breaks = label_positions, labels = labels) +
    theme(axis.title = element_blank(), axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines")) +
    geom_vline(xintercept = lines, colour = "grey", alpha = 0.75)

  if (!is.null(.x.months)) {

    if (!is.null(ggplot_build(p)$data[[1]]$count)) {
      max_count_bars <- max(ggplot_build(p)$data[[1]]$count)

      p <- p +
        annotation_custom(text[[1]], xmin = label_positions[1], xmax = label_positions[1], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[2]], xmin = label_positions[2], xmax = label_positions[2], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[3]], xmin = label_positions[3], xmax = label_positions[3], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[4]], xmin = label_positions[4], xmax = label_positions[4], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[5]], xmin = label_positions[5], xmax = label_positions[5], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[6]], xmin = label_positions[6], xmax = label_positions[6], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[7]], xmin = label_positions[7], xmax = label_positions[7], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[8]], xmin = label_positions[8], xmax = label_positions[8], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[9]], xmin = label_positions[9], xmax = label_positions[9], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[10]], xmin = label_positions[10], xmax = label_positions[10], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[11]], xmin = label_positions[11], xmax = label_positions[11], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        annotation_custom(text[[12]], xmin = label_positions[12], xmax = label_positions[12], ymin = -max_count_bars / 7, ymax = -max_count_bars / 7) +
        theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))
    }
  }

  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.draw(gt)
  # vertical grid lines will be removed!! -> fix this
}
