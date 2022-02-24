#' Encode Categories
#'
#' Encodes a list of categorical values for storage in a single field using prime number multiplication.
#'
#' @param categorical_variable The values to be encoded.
#' @param prime_keys An encoding key, as returned by `make_prime_keys()`.
#'
#' @return A list of encoded values.
#' @export
#' @seealso [medinetparser::make_prime_keys()]
#' @md
#'
#' @examples
#' encoded_values <- encode_categories(shifts, prime_keys)
encode_categories <- function(categorical_variable, prime_keys) {
  categorical_variable %>%
    array() %>%
    apply(1, function(x) prime_keys[str_detect(x, names(prime_keys))] %>%
            prod(na.rm = TRUE))
}
