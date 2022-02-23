#' Find Largest Table
#'
#' Finds the largest table among the ones input from Medinet, as this is the
#' most likely to contain the schedule data.
#'
#' @param tables HTML tables imported by *extract_table*.
#'
#' @return An integer indicating which table has the largest area.
#' @export
#' @seealso [medinetparser::extract_table()]
#' @md
#'
#' @examples
#' largest_table_index <- find_largest_table(tables)
find_largest_table <- function(tables) {
  j <- 1
  for (i in 1:length(tables)) {
    if (tables[[i]] %>% dim() %>% prod() > tables[[j]] %>% dim() %>% prod()) {
      j <- i
    }
  }

  return(j)
}
