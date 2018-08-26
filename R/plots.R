# PLOTTING ---------------------------------------------------------------------
# TODO:
# * Finds and samples together
# * Heatmaps
# * Kernel densitites directly from matrices

# Plot a sample
#   (Have to do it the long way because plot.tess draws lines rather than
#   polygons. Like a dick.)
plot.sample <- function(sample, main, cols=c("white", "lightgreen"),
                        ltys=c("dashed", "solid"), labels=NA) {
  if (missing(main) || is.null(main)) {
    main <- short.deparse(substitute(sample))
  }

  # Establish coordinate system
  plot(sample$window, main=main, type="n")

  switch(sample$type,
         # Plot rectangular (grid tesselations)
         rect = {
           # Create matrices of intersection coordinates
           xs <- sapply(sample$xgrid, rep, length(sample$xgrid))
           ys <- matrix(rev(rep(sample$ygrid, length(sample$ygrid))), length(sample$xgrid), length(sample$ygrid))
           # ...and rectangle origins (i.e. all but the last row and column of intersectionc coordinates)
           oxs <- xs[1:(ncol(xs)-1),1:(nrow(xs)-1)]
           oys <- ys[1:(ncol(ys)-1),1:(nrow(ys)-1)]

           # Plot rectangles (by column)
           # TODO: Can this be rewritten a single call to rect to speed things up a bit?
           mapply(function(n, mark, label, xs, ys, oxs, oys) {
             # Calculate 2D coordinates of nth element in the origin matrices
             xn <- ((n-1) %/% ncol(oxs)) + 1
             yn <- ((n-1) %% nrow(oys)) + 1
             # Draw a rectangle bounded by this and its adjacent intersections
             rect(oxs[yn,xn], ys[yn+1,xn], xs[yn,xn+1], oys[yn,xn],
                  col=cols[as.integer(mark)+1],
                  lty=ltys[as.integer(mark)+1])
             # Add label
             if(!is.na(label)) {
               text(oxs[yn,xn]+(xs[yn,xn+1]-oxs[yn,xn])/2,
                    oys[yn,xn]+(oys[yn,xn]-ys[yn+1,xn])/2,
                    label)
             }
           }, 1:sample$n, unlist(sample$marks), labels, MoreArgs=list(xs=xs, ys=ys, oxs=oxs, oys=oys))
         },
         # TODO: Plot tiled tesselations
         tiled = {
           plot <- plot.tess(sample)
         },
         # TODO: Plot raster image tesselations
         image = {
           plot <- plot.tess(sample)
         }
  )

  # Plot a border around the window
  # TODO: axis, labels, scale, etc.?
  plot(sample$window, main=main, col="transparent", add=TRUE)

  return(invisible(plot))
}

# Plot a survey (as the sample with labels, like plot.quadratcount)
# TODO: rewrite properly, (flipping should be fixed)
# TODO: short.deparse is an internal spatstat function. Very bad idea.
plot.survey <- function(survey, main) {
  if (missing(main) || is.null(main)) {
    main <- spatstat.utils::short.deparse(substitute(survey))
  }
  #   sample <- attr(survey, "tess")
  #   plot(sample, main=main, cols=NA, labels=as.vector(t(survey)))
  class(attr(survey, "tess")) <- c("tess", list)
  plot.quadratcount(survey, main=main)
}
