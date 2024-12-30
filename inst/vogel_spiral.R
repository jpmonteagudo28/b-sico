# Create a Vogel spiral
# n: number of points
# degrees: angle between points
# scale: scaling factor
# Returns a data frame with x and y coordinates
# Create a data frame to store the x and y coordinates

create_spiral <- function(n,
                          degrees = 137.507,
                          scale = 1,
                          symbol = "☯",
                          color = "black"
                          ){

  #Check if n is numeric
  stopifnot(is.numeric(n))

  sequence <- 1:n

  spiral <- data.frame(x = numeric(n),
                       y = numeric(n))
  # Loop through the number of points
  for(i in seq_along(sequence)){
    # Calculate the angle
    theta <- i * deg2rad(degrees)
    # Calculate the radius
    r <- scale * sqrt(i)
    # Calculate the x and y coordinates
    spiral$x[i] <- r * cos(theta)
    spiral$y[i] <- r * sin(theta)
  }

  # Start a new plot
  plot.new()                         # Create a new plotting canvas
  plot.window(range(spiral$x),       # Set up plot window with x range
              range(spiral$y))       # Set up plot window with y range

  # Plot points for the spiral
  points(spiral$x, spiral$y, pch = symbol, col = color, cex = 1.2)
  points(spiral$x, spiral$y, pch = symbol, col = color, cex = 0.8)
  return(spiral)     # Set point size

  # Return the spiral data
  return(spiral)
}

# This function converts radians to degrees
rad2deg <- function(rad){
  stopifnot(is.numeric(rad))
  degrees <- (rad*180)/(pi)
  return(degrees)
}

#This function converts degrees to radians
deg2rad <- function(deg){
  stopifnot(is.numeric(deg))
  radians <- (deg*pi)/180
  return(radians)
}
