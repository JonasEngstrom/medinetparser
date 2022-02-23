#' Extract Table
#'
#' Loads the HTML file downloaded from Medinet and returns the HTML tables for
#' further analysis. NB! This takes a while to run. Be patient.
#'
#' If you run into memory problems using this function, exporting fewer weeks
#' at a time from Medinet is suggested. Tallies can easily be added if you are
#' interested in analyzing a longer period of time. Tables showing distribution
#' of shifts over time can be bound using dplyr’s `bind_cols()`.
#'
#' @param file_path The hath to the HTML file downloaded from Medinet.
#'
#' @return A list of HTML tables for use with rvest.
#' @export
#' @md
#'
#' @examples
#' tables <- extract_tables('data.html')
extract_table <- function(file_path) {
  rvest::read_html(file_path) %>%
    html_element('table.sw') %>%
    html_table() %>%
    return()
}
