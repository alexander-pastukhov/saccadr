#' Extract value for a named list or use default if key is missing
#'
#' @param options Named list
#' @param key String key
#' @param default Default value to be returned, if key is missing.
#'
#' @return Value from a list or default value
#' @export
#'
#' @examples
#' option_or_default(list("A" = 25), "A", 20)
#' option_or_default(list("A" = 25), "B", 20)
option_or_default <- function(options, key, default){
  if (is.null(options)) return(default)
  ifelse(key %in% names(options), options[[key]], default)
}
