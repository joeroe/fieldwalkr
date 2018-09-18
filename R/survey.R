# SURVEY SIMULATION ------------------------------------------------------------

#' Simulate a survey
#'
#' `survey()` models a spatial survey as a function with three components: a
#' point pattern representing the population to be surveyed; a sampling frame;
#' and a detection function.
#'
#'
#' @param points              An `sf` of points to be surveyed
#' @param units               An `sf` of sample units
#' @param sample_column       Name of the column in `sf` that specifies whether
#'                            the unit is included in the sample. Default:
#'                            `sample`.
#' @param detection_function  A detection function.
#' @param ...                 Additional arguments to the detection function.
#'
#' @return
#' An `sf` of points representing the result of the survey.
#'
#' @export
#'
#' @examples
survey <- function(points, units, sample_column = sample,
                   detection_function = detect_perfect, ...) {
  sample <- rlang::quo(sample_column)

  units %>%
    dplyr::filter(sample == TRUE) %>%
    sf::st_geometry() %>%
    # Workaround for https://github.com/tidyverse/dplyr/issues/2457
    {purrr::quietly(purrr::map_dfr)}(detection_function, points = points, ...) %>%
    magrittr::extract2("result") %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(sf::st_crs(points)) %>%
    return()
}
