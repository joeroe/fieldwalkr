% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_utils.R
\name{clip_edges}
\alias{clip_edges}
\title{Clip edge geometries}
\usage{
clip_edges(geoms)
}
\arguments{
\item{geoms}{An `sf`` object.}
}
\value{
An \code{sf} object.
}
\description{
\code{clip_edges()} takes an sf object and removes geometriess that touch the
edge of its bounding box.
}
