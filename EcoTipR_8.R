#' Make a barplot with a broken Y axis
#'
#' @param height Typical \code{height} parameter from \link{barplot}.
#' @param ylim1 Limits Y axis of the top plot.
#' @param yAxis1 Arguments passed to \link{axis} (Y axis) for the top plot.
#' @param ylim2 Limits Y axis of the bottom plot.
#' @param yAxis2 Arguments passed to \link{axis} (Y axis) for the bottom plot.
#' @param names.arg Typical \code{names.arg} parameter from \link{barplot}.
#' @param ... Extra arguments passed to \link{barplot}.
#'
#' @export
#'
#' @examples
#' n <- 10
#' myData <- data.frame(x = letters[1:n], y = runif(n = n, min = 0, max = 50))
#' index <- c(3, 4, 8)
#' myData$y[index] <- runif(n = 3, min = 500, max = 1000)
#'
#' brokenBarplot(height = myData$y, names.arg = myData$x,
#'               ylim1 = c(0, 50),
#'               yAxis1 = list(at = seq(0, 50, 10), las = 2),
#'               ylim2 = c(500, 1000),
#'               yAxis2 = list(at = seq(500, 1000, 100), las = 2))
brokenBarplot <- function(height, ylim1, yAxis1 = list(), ylim2, yAxis2 = list(),
                          names.arg = NULL, ...){
  # Preserves the original par arguments
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # Set object for display Y axis
  yAxis1$side <- yAxis2$side <- 2
  heightUp <- heightDown <- height

  # Set 2 sub plots
  par(mfrow = c(2, 1), mar = c(0, 0, 0, 0), oma = par()$mar)

  # Make the top plot
  extra <- diff(range(ylim2))*0.025
  index <- heightUp < ylim2[1]
  heightUp[index] <- NA
  barplot(height = heightUp, ylim = ylim2 - c(extra, 0), border = FALSE, names.arg = NA, axes = FALSE, ...)

  # Add axis for the top plot
  do.call(axis, yAxis2)
  axis(side = 2, at = c(ylim2[1] - extra, ylim2[1]), labels = NA, tcl = -0.25)
  abline(h = ylim2[1] - extra, lty = "dashed")
  box(bty = "7")

  # Add a space between sub plots
  par(mar = c(0, 0, 0.5, 0))

  # Make the bottom plot
  extra <- diff(range(ylim1))*0.025
  index <- heightDown < ylim1[1] | heightDown > ylim1[2]
  heightDown[index] <- ylim1[2] + extra
  barplot(heightDown, ylim = ylim1 + c(0, extra), names.arg = names.arg, border = FALSE, axes = FALSE, ...)

  # Add axis for the bottom plot
  do.call(axis, yAxis1)
  axis(side = 2, at = c(ylim1[2], ylim1[2] + extra), labels = NA, tcl = -0.25)
  abline(h = ylim1[2] + extra, lty = "dashed")
  box(bty = "u")

  return(invisible())
}

# Example

# Creating a new data set
n <- 10
myData <- data.frame(x = letters[1:n], y = runif(n = n, min = 0, max = 50))

# Introducing some extreme values
index <- c(3, 4, 8)
myData$y[index] <- runif(n = 3, min = 500, max = 1000)

# Typical barplot
barplot(myData$y, names.arg = myData$x,
        las = 1, ylim = c(0, 1000),
        ylab = "Alguna variable",
        col = "dodgerblue3", border = NA)

box()


# Barplot using brokenBarplot function
brokenBarplot(height = myData$y, names.arg = myData$x,
              ylim1 = c(0, 50),
              yAxis1 = list(at = seq(0, 50, 10), las = 2),
              ylim2 = c(500, 1000),
              yAxis2 = list(at = seq(500, 1000, 100), las = 2),
              col = "dodgerblue3")
              
