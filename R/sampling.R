# SAMPLING FUNCTIONS -----------------------------------------------------------

# Grid out a window with a specified grid size (in map units)
# Returns a spatstat::tess object with the grid (tessalation)
# TODO: non-rectangular windows
grid <- function(win, size.x, size.y) {
  if(win$type == "rectangle") {
    win.xdim <- (max(win$xrange) - min(win$xrange)) * win$units$multiplier
    win.ydim <- (max(win$yrange) - min(win$yrange)) * win$units$multiplier

    nx <- ceiling(win.xdim / size.x)
    ny <- ceiling(win.ydim / size.y)

    grid <- quadrats(win,
                     xbreaks = seq(from=min(win$xrange), by=size.x, length.out=nx+1),
                     ybreaks = seq(from=min(win$yrange), by=size.y, length.out=ny+1))

    return(grid)
  }
  else {
    stop("win must be a rectangular spatstat::owin object")
  }
}

# Sample all the squares of a rectangular grid
sample.all <- function(window, grid.size) {
  # Grid out the window
  tess <- grid(window, grid.size, grid.size)

  # Mark all the grid squares as part of the sample
  marks <- rep(TRUE, (length(tess$ygrid)-1)*(length(tess$xgrid)-1))
  marks(tess) <- marks

  # Return the sample as a marked spatstat::tess object
  class(tess) <- c("sample", class(tess))
  return(tess)
}

# Sample n random squares of a rectangular grid
sample.random <- function(window, n, grid.size) {
  # Grid out the window
  tess <- grid(window, grid.size, grid.size)

  # Mark n random grids as part of the sample
  marks <- rep(FALSE, (length(tess$ygrid)-1)*(length(tess$xgrid)-1))
  marks[sample(length(marks), n)] <- TRUE
  marks(tess) <- marks

  # Return the sample as a marked spatstat::tess object
  class(tess) <- c("sample", class(tess))
  return(tess)
}

# Sample n evenly spaced transects on a rectangular grid
sample.transect <- function(window, n, grid.size, orientation=0) {
  # Grid out the window
  tess <- grid(window, grid.size, grid.size)

  # Mark transects as part of the sample
  marks <- matrix(FALSE, length(tess$ygrid)-1, length(tess$xgrid)-1)
  # NS transects
  if(orientation==0 || orientation==180 || orientation==360) {
    ncols <- length(tess$xgrid) - 1
    interval <- ncols / n
    scols <- round(seq(from=interval/2, by=interval, length.out=n))
    marks[,scols] <- TRUE
  }
  # EW transects
  else if(orientation==90 || orientation==270) {
    nrows <- length(tess$ygrid) - 1
    interval <- nrows / n
    srows <- round(seq(from=interval/2, by=interval, length.out=n))
    marks[srows,] <- TRUE
  }
  else {
    stop("orientation (in degrees) must be EW (90 or 270) or NS (0, 180 or 360)")
  }
  marks(tess) <- as.vector(marks)

  # Return the sample as a marked spatstat::tess object
  class(tess) <- c("sample", class(tess))
  return(tess)
}

# Generate an adaptive cluster sample
sample.adaptive.cluster <- function(window, sample.func, criterion.func) {
  # TODO
}
