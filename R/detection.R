# DETECTION FUNCTIONS ----------------------------------------------------------

#' @export
detect_perfect <- function(f) {
  return(f)
}

# Detect points by applying a simple detection rate
#' @export
detect_simple <- function(f, rate=1) {
  f <- round(f*rate)
  return(f)
}

# Detect points by applying a uniform stochastic detection rate
#' @export
detect_random <- function(f, min=0, max=1) {
  rate <- runif(1,min,max)
  f <- round(f*rate)
  return(f)
}

# Detect points by applying a (truncated) normal stochastic detection rate
#' @export
detect_normal <- function(f, mean, sd) {
  repeat {
    rate <- rnorm(1, mean, sd)
    if(rate > 0 && rate < 1) break
  }
  f <- round(f*rate)
  return(f)
}

# Definite or 'clean sweep' detection (Koopman 1980:83; Banning 2002: pp. 57-58)
#  Perfect detection within an area defined by the visual sweep of n observers
#  (assumed to be walking equally spaced transects).
#' @export
detect_definite <- function(f, unit, n.obs, radius) {
  # TODO
}
