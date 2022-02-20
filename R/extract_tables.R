#' Extract Tables
#'
#' Loads the HTML file downloaded from Medinet and returns the HTML tables for
#' further analysis. NB! This takes a while to run. Be patient.
#'
#' @param file_path The hath to the HTML file downloaded from Medinet.
#'
#' @return A list of HTML tables for use with rvest.
#' @export
#'
#' @examples
#' tables <- extract_tables('data.html')
extract_tables <- function(file_path) {
  rvest::read_html(file_path) %>%
    html_table() %>%
    return()
}
