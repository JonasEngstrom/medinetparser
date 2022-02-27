#' Make Prime Keys
#'
#' Generates keys that can be used for encoding several different categorical
#' values in the same field of a tibble.
#'
#' @param categories A list of categories for which to generate keys.
#'
#' @return A list of prime numbers, named after the input categories.
#' @export
#'
#' @examples
#' encoding_key <- make_prime_keys(list_of_categories)
make_prime_keys <- function(categories) {
  prime_keys <- categories %>%
    length() %>%
    medinetparser::generate_primes()

  names(prime_keys) <- categories

  return(prime_keys)
}
