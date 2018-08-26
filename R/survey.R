# SURVEY SIMULATION ------------------------------------------------------------

# Simulate a survey
# TODO: If sample is a function:
# * Call it... or something? Recursively?
survey <- function(surface, sample, detection.function) {
  obs.f <- quadratcount(surface, tess=sample)
  obs.f[!sample$marks] <- NA
  obs.f <- do.call(detection.function, args=list(obs.f))
  class(obs.f) <- c("survey", class(obs.f))
  return(obs.f)
}
