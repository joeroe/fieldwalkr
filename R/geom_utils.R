# Utility functions for working with geometric data -----------------------

#' Grab geometries
#'
#' Recursively selects adjacent (etc.) geometries from a geometric predicate
#' sparse matrix until the target number of polygons is reached.
#'
#' @param adjacency  A geometric predicate list, see [sf::geos_binary_pred()]
#' @param target     Target number of geometries
#' @param set        Set of geometries already selected
#'
#' @return
#'
#' A list of indexes.
#'
grab_geoms <- function(mosaic, adjacency, target,
                       set = sample(1:length(adjacency), 1)) {

  if(target > length(adjacency)) {
    stop("Can't grab more polygons than there are, you silly sausage.")
  }

  if(length(set) == 1) adj <- adjacency[[set]] # Avoid the "sample surprise" (see ?sample)
  else adj <- adjacency[[sample(set, 1)]]
  set <- c(set, sample(adj, target - length(set), replace = TRUE))
  set <- unique(set)


  if (length(set) < target) {
    return(grab_geoms(mosaic, adjacency, target, set))
  }
  else {
    return(set)
  }
}

#' Clip edge geometries
#'
#' `clip_edges()` takes an sf object and removes geometriess that touch the
#' edge of its bounding box.
#'
#' @param geoms  An `sf`` object.
#'
#' @return
#'
#' An `sf` object.
#'
#' @export
#'
clip_edges <- function(geoms) {
  geoms %>%
    sf::st_bbox() %>%
    bbox_as_line() %>%
    sf::st_touches(geoms) ->
    edges

  geoms %>%
    dplyr::slice(-edges[[1]]) %>%
    return()
}

rotation <- function(degrees) {
  degrees %>%
    degrees_to_radians() %>%
    {matrix(c(cos(.), sin(.), -sin(.), cos(.)), 2, 2)} %>%
    return()
}

degrees_to_radians <- function(d) (d * pi) / 180
