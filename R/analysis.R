# ANALYSIS ---------------------------------------------------------------------
# TODO:
# * Matrix interpolation (akima?)
# * KDE
# * Model fitting
# * Sensitivity analysis
# * Parameter optimisation (including survey effort)

# Convert quadratcount to point patterns by randomly distributing points within
#   quadrats
# TODO: Rename. This is more than a type conversion really.
as.ppp.quadratcount <- function(from) {
  tess <- attr(from, "tess")
  switch(tess$type,
         rect = {
           # Create matrices of intersection coordinates
           xs <- sapply(tess$xgrid, rep, length(tess$xgrid))
           ys <- matrix(rev(rep(tess$ygrid, length(tess$ygrid))), length(tess$xgrid), length(tess$ygrid))
           # ...and rectangle origins (i.e. all but the last row and column of intersectionc coordinates)
           oxs <- xs[1:(ncol(xs)-1),1:(nrow(xs)-1)]
           oys <- ys[1:(ncol(ys)-1),1:(nrow(ys)-1)]

           # Generate f random coordinates within each rectangle where f is the observed count in that rectangle
           coords <- mapply(function(n, f, xs, ys, oxs, oys) {
             # Calculate 2D coordinates of nth element in the origin matrices
             xn <- ((n-1) %% ncol(oxs)) + 1
             yn <- ((n-1) %/% nrow(oys)) + 1

             # Generate random coordinates within the bounds of this an its adjacent intersections
             if(!is.na(f)) {
               rxs <- runif(f, oxs[yn,xn], xs[yn,xn+1])
               rys <- runif(f, ys[yn+1,xn], oys[yn,xn])
               return(cbind(rxs,rys))
             }
             else {
               return()
             }
           }, 1:tess$n, as.vector(t(from)), MoreArgs=list(xs=xs, ys=ys, oxs=oxs, oys=oys))
           coords <- do.call(rbind, coords)

           # Make ppp
           ppp <- ppp(coords[,1], coords[,2], window=as.owin(tess))
         },
         tiled = {
           # TODO
         },
         image = {
           # TODO
         }
  )

  return(ppp)
}

# Interpolate quadratcount values using simple bilinear interpolation (Akima 1978)
interp.quadratcount <- function(qc) {
  # Extract x, y coordinates and values (z) from quadratcount object
  qc <- t(qc)
  x <- as.vector(col(qc))
  y <- as.vector(row(qc))
  z <- as.vector(qc)

  # Bind and throw away NA values
  i <- as.data.frame(cbind(x, y, z))
  i <- i[complete.cases(i),]

  # Interpolate back to the full grid
  interp <- interp(x=i$x, y=i$y, z=i$z, xo=1:ncol(qc), yo=1:nrow(qc),
                   linear=TRUE, extrap=FALSE)

  # Update the quadratcount object
  classes <- class(qc)
  attrs <- attributes(qc)
  qc <- round(interp$z)
  class(qc) <- classes
  attributes(qc) <- attrs

  return(qc)
}
