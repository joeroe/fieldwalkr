# Sampling functions -----------------------------------------------------------

#' Randomly sample survey units
#'
#' `sample_random()` generates a simple random sample of a set of survey units.
#'
#' @param units     An `sf` object of survey units.
#' @param n         Number of units to sample. Either `n` or `fraction` must be
#'                  set.
#' @param fraction  Fraction of units to sample.
#' @param column    Name of the column to be added. Default: "sample".
#'
#' @return
#' `units` with an additional boolean column describing the sample.
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' library(ggplot2)
#'
#' rpolygon() %>%
#'   quadrats(n = c(10, 10)) %>%
#'   sample_random(n = 50) %>%
#'   ggplot(aes(fill = sample)) + geom_sf()
sample_random <- function(units, n = NULL, fraction = NULL, column = "sample") {
  checkmate::assert(checkmate::checkInt(n), checkmate::checkNull(n))
  checkmate::assert(checkmate::checkNumber(fraction), checkmate::checkNull(fraction))

  if (!missing(n) && !missing(fraction)) {
    warning("Ignoring fraction as n is set.")
  }

  if (!missing(n)) {
    s <- sample(1:nrow(units), n)
  }
  else if (!missing(fraction)) {
    s <- sample(1:nrow(units), round(nrow(units) * fraction))
  }
  else {
    stop("One of the parameters n or fraction must be set.")
  }

  units[,column] <- 1:nrow(units) %in% s
  return(units)
}

#' Systematically sample survey units
#'
#' `sample_systematic()` generates a systematic sample of survey units.
#'
#' @param units   An sf object of survey units.
#' @param n       Step size, i.e. selects every `n`th unit.
#' @param start   Index of the unit to start sampling from.
#' @param column  Name of the column to be added. Default: "sample".
#'
#' @return
#' `units` with an additional boolean column describing the sample.
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' library(ggplot2)
#'
#' rpolygon() %>%
#'   quadrats(n = c(10, 10)) %>%
#'   sample_systematic(n = 10) %>%
#'   ggplot(aes(fill = sample)) + geom_sf()
sample_systematic <- function(units, n, start = 1, column = "sample") {
  checkmate::assert_number(n)
  checkmate::assert_number(start)

  s <- seq(from = start, to = nrow(units), by = n)

  units[,column] <- 1:nrow(units) %in% s
  return(units)
}


# TODO:
# Transect
# Cluster
# Adaptive cluster
