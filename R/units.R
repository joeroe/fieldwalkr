#' Generate a random mosaic
#'
#' @description
#' `mosaic` generates polygon units within a sample frame using a stochastic
#' tessellation algorithm. The desired number of units can be specified as an
#' exact `density` or an average unit `area`.
#'
#' @param frame     An `sf` object, or an object which can be converted to `sf`,
#'                  containing the sample frame as a spatial polygon
#' @param density   An integer specifying the desired number of units to be
#'                  generated within the sample frame. Not required if `area` is
#'                  set.
#' @param area      An integer or double specifying the desired average
#'                  area of each unit. The units are assumed to match those
#'                  given by [sf::st_area()] for `frame`. Not required if
#'                   `density` is set.
#' @param method    The tessellation model to be used to generate the mosaic;
#'                  see details.
#'
#' @details
#'
#' `method` accepts a function describing the tessalation model to be used (see
#' \insertCite{Van_Lieshout2012-hb}{fieldwalkr} for an overview). Currently the
#' only method implemented in the package is [voronoi()], which generates a
#' Voronoi tessellation using a set of uniformly distributed random points and
#' Delaunay triangulation.
#'
#' @return
#' An `sf` object containing the units as polygons with an integer `id`.
#'
#' @export
#' @import rlang
#'
#' @examples
#' library(sf)
#' ashe <- read_sf(system.file("shape/nc.shp", package="sf"))[1,]
#'
#' # Fixed number of units using Voronoi tesselation
#' units <- mosaic(ashe, density = 50, method = voronoi)
#' plot(units)
#'
#' # Average unit area
#' library(units)
#' units <- mosaic(ashe, area = 25000000, method = voronoi)
#' plot(units)
#'
#' @references
#'
#' \insertRef{Van_Lieshout2012-hb}{fieldwalkr}
mosaic <- function(frame, density = NULL, area = NULL, method = voronoi) {
  checkmate::assert(checkmate::checkIntegerish(density), checkmate::checkNull(density))
  checkmate::assert(checkmate::checkNumber(area), checkmate::checkNull(area))
  checkmate::assert_function(method)

  if (missing(density) && missing(area)) {
    stop("One of the parameters density or area must be set.")
  }
  if (!missing(density) && !missing(area)) {
    warning("Only density or area need to be set, not both; ignoring area.")
    density_specified <- TRUE
  }
  else {
    density_specified <- !missing(density)
  }

  if (missing(density)) {
    density <- round(as.numeric(sf::st_area(frame)) / area)
    if (density < 1) {
      stop("area parameter is larger than the area of frame.")
    }
  }

  do.call(method, list(poly = frame, n = density,
                         warn_multipart = density_specified)) %>%
    return()
}

#' Random tessellation using Voronoi tiles
#'
#' @description
#' `voronoi` generates `n` Voronoi tiles (also known as Thiessen polygons)
#' within a polygon using Delaunay triangulation. It is essentially a wrapper
#' for [deldir::deldir()] which outputs a tidy `sf` object.
#'
#' @param poly            An `sf` object, or an object that can be converted to
#'                        `sf`, within which the tessellation will be performed.
#' @param n               Desired number of Voronoi tiles; as long as `poly` is
#'                        contiguous (see `warn_multipart`), the function will
#'                        always return exactly this number of tiles.
#' @param warn_multipart  Logical. Set `FALSE` to suppress the warning about
#'                        multipart geometries producing an inconsistent number
#'                        of tiles.
#'
#' @return
#' An `sf` object containing polygon geometries and an integer unique `id`.
#'
#' @export
#'
#' @examples
#' library(sf)
#' ashe <- read_sf(system.file("shape/nc.shp", package="sf"))[1,]
#' tiles <- voronoi(ashe, 50)
#' plot(tiles)
#'
voronoi <- function(poly, n, warn_multipart = TRUE) {
  poly <- sf::st_as_sf(poly)
  if (warn_multipart &&
      "MULTIPOLYGON" %in% class(sf::st_geometry(poly))) {
    warning("Voronoi tessellation cannot reliably generate a fixed number of ",
            "tiles within multipart polygons.")
  }

  # Generate points
  # st_sample does not return an exact number of points, so iterate and thin
  #   until we have exactly n
  points <- sf::st_sample(poly, n)
  while (length(points) < n) {
    points <- c(points, sf::st_sample(poly, ceiling(n/10)))
  }
  points <- sample(points, n)

  # Perform tessellation
  tess <- deldir::deldir(sf::st_coordinates(points)[,"X"],
                         sf::st_coordinates(points)[,"Y"],
                         rw = sf::st_bbox(poly)[c("xmin", "xmax", "ymin", "ymax")],
                         suppressMsge = TRUE)

  # Convert deldir's gruesome format to sf, clip & return
  tess %>%
    st_as_sf() %>%
    sf::st_set_crs(sf::st_crs(poly)) %>%
    sf::st_intersection(poly) %>%
    dplyr::select(id) %>%
    return()
}

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
