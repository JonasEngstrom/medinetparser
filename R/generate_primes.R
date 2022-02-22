#' Generate Primes
#'
#' Generates a list of prime numbers starting at 2, to be used for encoding.
#'
#' @param number_of_primes The number of prime numbers needed.
#'
#' @return A list of prime numbers.
#' @export
#'
#' @examples
#' list_of_prime_numbers <- generate_primes(15)
generate_primes <- function(number_of_primes) {
  list_of_primes <- c(2L)
  i <- 3L

  while (length(list_of_primes) < number_of_primes) {
    if (sum(i %% list_of_primes < 1) == 0) {
      list_of_primes <- c(list_of_primes, i)
    }
    i <- i + 2
  }

  return(list_of_primes)
}
