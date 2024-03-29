% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey.R
\name{survey}
\alias{survey}
\title{Simulate a survey}
\usage{
survey(points, units, sample_column = sample,
  detection_function = detect_constant, ...)
}
\arguments{
\item{points}{An \code{sf} of points to be surveyed}

\item{units}{An \code{sf} of sample units}

\item{sample_column}{Name of the column in \code{sf} that specifies whether
the unit is included in the sample. Default:
\code{sample}.}

\item{detection_function}{A detection function.}

\item{...}{Additional arguments to the detection function.}
}
\value{
An \code{sf} of points representing the result of the survey.
}
\description{
\code{survey()} models a spatial survey as a function with three components: a
point pattern representing the population to be surveyed; a sampling frame;
and a detection function.
}
