#' @importFrom sf st_as_sf
#' @export
st_as_sf.deldir <- function(dd, extract = c("tiles", "triangles")) {
  extract <- match.arg(extract)

  if (extract == "tiles") {
    ddlist <- deldir::tile.list(dd)
  }
  else if (extract == "triangles") {
    ddlist <- deldir::triang.list(dd)
  }

  ddlist %>%
    purrr::map(~{cbind(x = .$x, y = .$y)} %>%
                 rbind(.[1,]) %>%
                 list() %>%
                 sf::st_polygon()) %>%
    sf::st_sfc() %>%
    sf::st_sf() %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    return()
}

rotation <- function(degrees) {
  degrees %>%
    degrees_to_radians() %>%
    {matrix(c(cos(.), sin(.), -sin(.), cos(.)), 2, 2)} %>%
    return()
}

degrees_to_radians <- function(d) (d * pi) / 180

# Slight modification of sf::st_as_sfc.bbox()
bbox_as_line <- function(x) {
  box = sf::st_multilinestring(list(matrix(x[c(1, 2, 3, 2, 3, 4, 1, 4, 1, 2)],
                                           ncol = 2, byrow = TRUE)))
  sf::st_sfc(box, crs = sf::st_crs(x))
}


#' Clip edge polygons
#'
#' `clip_edges()` takes an sf object and removes polygons that touch its bounding
#' box.
#'
#' @param polygons  Am `sf`` object.
#'
#' @return
#'
#' An `sf` object.
#'
#' @export
#'
clip_edges <- function(polygons) {
  polygons %>%
    sf::st_bbox() %>%
    bbox_as_line() %>%
    sf::st_touches(polygons) ->
    edges

  polygons %>%
    dplyr::slice(-edges[[1]]) %>%
    return()
}
