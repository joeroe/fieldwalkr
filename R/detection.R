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
detect_simple <- function(unit, points, rate = 1) {
  detect_perfect(unit, points) %>%
    dplyr::sample_frac(rate) %>%
    return()
}

# Detect points by applying a uniform stochastic detection rate
#' @export
detect_random <- function(unit, points, min = 0, max = 1) {
  detect_simple(unit, points, rate = runif(1,min,max)) %>%
    return()
}

# Detect points by applying a (truncated) normal stochastic detection rate
#' @export
detect_normal <- function(unit, points, mean = 0.5, sd = 0.2) {
  repeat {
    rate <- rnorm(1, mean, sd)
    if(rate > 0 && rate < 1) break
  }

  detect_simple(unit, points, rate = rate) %>%
    return()
}

# TODO:
# Definite or 'clean sweep' detection (Koopman 1980:83; Banning 2002: pp. 57-58)
#  Perfect detection within an area defined by the visual sweep of n observers
#  (assumed to be walking equally spaced transects).

