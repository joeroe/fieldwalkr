# Functions for generating survey units -----------------------------------

#' Random polygon
#'
#' Generates a single, random polygon.
#'
#' @param crs    An integer EPSG code specifying the coordinate reference system
#'               of the polygon to be generated.
#'               Default: 3395 (World Mercator, https://epsg.io/3395)
#' @param area   Integer. The approximate area of the polygon to be generated,
#'               in map units (see `crs`).
#' @param origin Numeric. The approximate origin coordinates of the polygon to
#'               be generated (x,y).
#' @param composite Integer. Higher values take longer, but generate more
#'                  complex polygons with more sides. See details. Default: 64.
#' @param square Logical. If `TRUE`, the algorithm tends to create squared
#'               corners and edges. If this is undesirable, set to `FALSE` (the
#'               default).
#'
#' @details
#'
#' `rpolygon()` generates a random tessellation using [mosaic()] and returns a
#'  random polygon sampled for it. If `composite` > 1, several contiguious tiles
#'  are dissolved, creating more complex polygons with more sides. The polygon
#'  may contain holes.
#'
#' @return
#'
#' An `sf` object containing a single polygon.
#'
#' @export
#'
#' @examples
#'
#' # Simple polygon
#' polygon <- rpolygon()
#' plot(polygon)
#'
#' # More complex polygon
#' polygon <- rpolygon(composite = 8)
#'
rpolygon <- function(crs = 3395, origin = c(0,0), area = 100000,
                     composite = 64, square = FALSE) {
  xmin <- origin[1]
  xmax <- origin[1] + sqrt(area*composite*4)
  ymin <- origin[2]
  ymax <- origin[2] + sqrt(area*composite*4)
  cbind(c(xmin, xmax, xmax, xmin, xmin),
        c(ymin, ymin, ymax, ymax, ymin)) %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = crs) %>%
    sf::st_sf() %>%
    mosaic(density = 16 * composite) ->
    mosaic

  if (!square) mosaic <- clip_edges(mosaic)

  if (composite == 1) {
    mosaic %>%
      dplyr::sample_n(1) %>%
      return()
  }
  else {
    mosaic %>%
      dplyr::slice(grab_geoms(., sf::st_intersects(.), target = composite)) %>%
      sf::st_union() %>%
      return()
  }
}

#' Gridded survey units
#'
#' `quadrats()` generates a regular, rectangular grid of survey units over a
#' frame.
#' `transects()` is a convenience function for when only a single row of units
#' is desired.
#'
#' @param frame       An `sf` survey frame.
#' @param n           Integer of length 1 or 2; number of units to be generated
#'                    in the x and y directions. See [sf::st_make_grid()].
#' @param size        Target size of the grid (in one dimension). See
#'                    [sf::st_make_grid()].
#' @param orientation Integer; the orientation of the grid in degrees. Default: 0.
#'
#' @return
#'
#' An `sf` grid of survey units.
#'
#' @export
#'
#' @examples
#' frame <- rpolygon()
#'
#' # North-south grid
#' grid <- quadrats(frame, n = c(10, 10))
#' plot(grid)
#'
#' # NE-SW grid
#' grid <- quadrats(frame, n = c(10, 10), orientation = 45)
#' plot(grid)
#'
#' # Fixed size grid
#' grid <- quadrats(frame, size = 100)
#' plot(grid)
#'
#' # Transects
#' trans <- transects(frame, n = 10)
#' plot(trans)
#'
#' # East-west transects
#' trans <- transects(frame, n = 10, orientation = 90)
#' plot(trans)
#'
#' # Fixed size transects
#' trans <- transects(frame, size = 100)
#' plot(trans)
#'
#' @importFrom magrittr %>%
quadrats <- function(frame, n = NULL, size = NULL, orientation = 0) {
  checkmate::assert(checkmate::checkNumber(n),
                    checkmate::checkNumeric(n),
                    checkmate::checkNull(n))
  checkmate::assert(checkmate::checkNumber(size),
                    checkmate::checkNumeric(size),
                    checkmate::checkNull(size))
  checkmate::assert_number(orientation)

  crs <- sf::st_crs(frame)

  # Rotate frame
  if(orientation != 0) {
    centroid <- sf::st_centroid(frame)
    rframe <- (frame - centroid) * rotation(-orientation) + centroid
    sf::st_crs(rframe) <- crs
  }
  else rframe <- frame

  # Generate grid
  if (!missing(n)) {
    grid <- sf::st_make_grid(rframe, n = n)
  }
  else if (!missing(size)) {
    grid <- sf::st_make_grid(rframe, cellsize = size)
  }
  else {
    stop("n or size must be specified.")
  }

  # Un-rotate grid
  if(orientation != 0) {
    grid <- (grid - centroid) * rotation(orientation) + centroid
    sf::st_crs(grid) <- crs
  }

  # Return intersection of grid and frame
  grid %>%
    sf::st_sf() %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::slice(sf::st_intersects(frame, .) %>%
                   unlist() %>%
                   unique()) %>%
    return()
}

#' @rdname quadrats
#' @export
transects <- function(frame, n = NULL, size = NULL, orientation = 0) {
  checkmate::assert(checkmate::checkNumber(n), checkmate::checkNull(n))
  checkmate::assert(checkmate::checkNumber(size), checkmate::checkNull(size))
  checkmate::assert_number(orientation)

  if(!missing(n)) {
    n <- n
  }
  else if(!missing(size)) {
    centroid <- sf::st_centroid(frame)
    rframe <- (frame - centroid) * rotation(-orientation) + centroid
    n <- round(diff(sf::st_bbox(rframe)[c(1, 3)]) / size)
  }
  else {
    stop("One of n or size must be specified.")
  }

  fieldwalkr::quadrats(frame, n = c(n, 1), orientation = orientation) %>%
    sf::st_intersection(frame) %>%
    return()
}

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
#' frame <- rpolygon()
#'
#' # Fixed number of units using Voronoi tesselation
#' units <- mosaic(frame, density = 50, method = voronoi)
#' plot(units)
#'
#' # Average unit area
#' units <- mosaic(frame, area = 20000, method = voronoi)
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

  do.call(method, list(frame = frame, n = density,
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
#' @param frame           An `sf` object, or an object that can be converted to
#'                        `sf`, within which the tessellation will be performed.
#' @param n               Desired number of Voronoi tiles; as long as `frame` is
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
#' frame <- rpolygon()
#' tiles <- voronoi(frame, 50)
#' plot(tiles)
#'
voronoi <- function(frame, n, warn_multipart = TRUE) {
  #frame <- sf::st_as_sf(frame)
  if (warn_multipart &&
      "MULTIPOLYGON" %in% class(sf::st_geometry(frame))) {
    warning("Voronoi tessellation cannot reliably generate a fixed number of ",
            "tiles within multipart polygons.")
  }

  # Generate points
  # st_sample does not return an exact number of points, so iterate and thin
  #   until we have exactly n
  points <- sf::st_sample(frame, n)
  while (length(points) < n) {
    points <- c(points, sf::st_sample(frame, ceiling(n/10)))
  }
  points <- sample(points, n)

  # Perform tessellation
  tess <- deldir::deldir(sf::st_coordinates(points)[,"X"],
                         sf::st_coordinates(points)[,"Y"],
                         rw = sf::st_bbox(frame)[c("xmin", "xmax", "ymin", "ymax")],
                         suppressMsge = TRUE)

  # Convert deldir's gruesome format to sf, clip & return
  tess %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(sf::st_crs(frame)) %>%
    sf::st_intersection(frame) %>%
    dplyr::select(id) %>%
    return()
}
