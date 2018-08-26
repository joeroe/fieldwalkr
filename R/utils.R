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
