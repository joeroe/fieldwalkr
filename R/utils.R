# Other utility functions (mostly type conversion) ------------------------

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

# Slight modification of sf::st_as_sfc.bbox()
bbox_as_line <- function(x) {
  box = sf::st_multilinestring(list(matrix(x[c(1, 2, 3, 2, 3, 4, 1, 4, 1, 2)],
                                           ncol = 2, byrow = TRUE)))
  sf::st_sfc(box, crs = sf::st_crs(x))
}
