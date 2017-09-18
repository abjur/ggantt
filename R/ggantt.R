ggant_theme <- ggplot2::theme_minimal() + 
ggplot2::theme(legend.position = 'bottom',
      axis.ticks.y = ggplot2::element_blank())

#' @title Construct gantt charts with ggplot2
#' @export
#' @examples
#' tasks <- c("Review literature", "Mung data", "Stats analysis", "Write Report")
#' example <- data.frame(
#' nome        = factor(tasks, levels = tasks),
#' start_date  = as.Date(c("2010-08-24", "2010-10-01", "2010-11-01", "2011-02-14")),
#' end_date    = as.Date(c("2010-10-31", "2010-12-14", "2011-02-28", "2011-04-30")),
#' is.critical = c(TRUE, FALSE, FALSE, TRUE)
#' )
#' ggantt(tasks)
ggantt <- function(dataset, label = 'nome', start = 'start_date', end = 'end_date', cor = 'royalblue'){
ggplot2::ggplot(dataset,ggplot2::aes_string(x = start, xend = end, y = label, yend = label, color = cor)) +
  ggplot2::geom_segment(lineend = 'round', size = 8) +
  ggplot2::labs(y = '') + 
  ggant_theme
}