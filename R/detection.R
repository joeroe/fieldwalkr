# DETECTION FUNCTIONS ----------------------------------------------------------

# Detect all points in a unit
#' @export
detect_perfect <- function(unit, points) {
  unit <- sf::st_sfc(unit, crs = sf::st_crs(points))
  points %>%
    sf::st_intersection(unit) %>%
    # Workaround for https://github.com/tidyverse/dplyr/issues/2457
    dplyr::as_tibble() %>%
    return()
}

# Detect points by applying a simple detection rate
#' @export
detect_rate <- function(unit, points, rate = 1) {
  detect_perfect(unit, points) %>%
    dplyr::sample_frac(rate) %>%
    return()
}

# Detect points by applying a uniform stochastic detection rate
#' @export
detect_random <- function(unit, points, min = 0, max = 1) {
  return(detect_rate(unit, points, rate = runif(1,min,max)))
}

# Detect points by applying a (truncated) normal stochastic detection rate
#' @export
detect_normal <- function(unit, points, mean = 0.5, sd = 0.2) {
  repeat {
    rate <- rnorm(1, mean, sd)
    if(rate > 0 && rate < 1) break
  }

  return(detect_rate(unit, points, rate = rate))
}

#' Detection Function: Law of Random Search
#'
#' @param unit
#' @param points
#' @param time
#' @param g
#'
#' @return
#' @export
#'
#' @examples
#'
#' @references
#'
#' * \insertRef{Banning2002-uv}{fieldwalkr}
#' * \insertRef{Banning2011-wo}{fieldwalkr}
detect_random_search <- function(unit, points, time, g) {
  rate <- 1 - exp(-g * time)
  return(detect_rate(unit, points, rate))
}

#' Detection Function: Sweep Width
#'
#' @param unit
#' @param points
#'
#' @return
#' @export
#'
#' @examples
#'
#' @references
#'
#' * \insertRef{Banning2011-wo}{fieldwalkr}
detect_sweep <- function(unit, points, b, k, r) {
  rate <- b - exp(-k * r^2)
  return(detect_rate(unit, points, rate))
}

# TODO:
# Definite or 'clean sweep' detection (Koopman 1980:83; Banning 2002: pp. 57-58)
#  Perfect detection within an area defined by the visual sweep of n observers
#  (assumed to be walking equally spaced transects).

