#' @title Add feedback  lines to a gantt table
#' @export
#' @examples
#' tasks <- c("Review literature", "Mung data", "Stats analysis", "Write Report")
#' example <- data.frame(
#' nome        = factor(tasks, levels = tasks),
#' start_date  = as.Date(c("2010-08-24", "2010-10-01", "2010-11-01", "2011-02-14")),
#' end_date    = as.Date(c("2010-10-31", "2010-12-14", "2011-02-28", "2011-04-30")),
#' is.critical = c(TRUE, FALSE, FALSE, TRUE)
#' )
#' ggantt(add_feedback(tasks))
add_feedback <- function(data_set, bar_type, type2extend = 'delivery', start_date = 'start_date', end_date = 'end_date', duration = 'duration', feed_time = 15){
  
  filter_variable <- rlang::sym(bar_type)
  start_date <- rlang::sym(start_date)
  end_date <- rlang::sym(end_date)
  duration <- rlang::sym(duration)
  
  feedback_lines <- data_set %>% 
    dplyr::filter(rlang::UQ(filter_variable) == type2extend) %>% 
    dplyr::mutate(!!start_date := rlang::UQ(end_date)+15,
                  !!end_date := rlang::UQ(start_date)+feed_time,
                  !!bar_type := "Feedback",
                  !!duration := feed_time)
  
  data_set %>% 
    bind_rows(feedback_lines)
    
}