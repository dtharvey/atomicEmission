#' Plot Atomic Emission Spectrum
#'
#' Plots a simulated atomic emission spectrum with the absolute or
#' relative emission intensity on the y-axis and wavelength on
#' the x-axis. The plot is constructed using data stored in an object
#' created using \code{simulateAES}.
#'
#' @param file The name of an object, created by
#'   \code{simulateAES}, that contains the results of a
#'   simulation.
#'
#' @param lambda.min The minimum wavelength (in nm) for the
#'   x-axis; defaults to 390 nm, which is the lower limit.
#'
#' @param lambda.max The maximum wavelength (in nm) for tye
#'   x-axis; defaults to 700 nm, which is the upper limit.
#'
#' @param signal_type How the emission is scaled on the y-axis;
#'   one of \code{"relative"}, which scales the y-axis to a
#'   maximum value of 1, or \code{"absolute"}, which scales the
#'   y-axis using the actual emission intensities. Defaults to
#'   \code{"relative"}.
#'
#' @param main_title An optional main title. If \code{NULL}, then
#'   a default title gives the element's atomic symbol, the
#'   temperature, and the partition model.
#'
#' @return Returns a plot of the element's simulated atomic
#'   emission spectrum.
#'
#' @importFrom graphics plot grid
#'
#' @export
#'
#' @examples
#'
#' oxygen = simulateAES(atom = "oxygen", temperature = 4000, width = 0.2)
#' plotAES(file = oxygen, signal_type = "relative",
#'     main_title = "Element: Oxygen, Temperature: 4000")

plotAES = function(file,
                   lambda.min = 390,
                   lambda.max = 700,
                   signal_type = c("relative", "absolute"),
                   main_title = NULL){

# determine the type of signal to plot on the y-axis

   signal_type = match.arg(signal_type)

# set the default main title, if needed

   if (is.null(main_title) == TRUE) {
     main_title = paste("Element:", file$symbol,
                        " Temperature:", file$temperature,
                        "K"," Model:", file$model)
   }

# throw out an error message if wavelengths fall outside of
# limits and, if okay, set the lower and upper limits for the
# plot

   if (lambda.min < 390 | lambda.max > 700){
     stop("The wavelengths must fall within
          the limits of 390 nm and 700 nm")
   }

   xl.index = which(file$wavelength == lambda.min)
   xh.index = which(file$wavelength == lambda.max)

# create the plot if the y-axis scale is absolute intensity

  if (signal_type == "absolute") {
    plot(x = file$wavelength, y = file$absolute_intensity, type = "l",
         ylab = "absolute intensity (arb. units)", xlab = "wavelength (nm)",
         main = main_title, col = "blue", lwd = 2,
         xlim = c(lambda.min, lambda.max),
         ylim = c(0, max(file$absolute_intensity[xl.index:xh.index])))
    grid()

  } else {

# create the plot if the y-axis scale is relative intensity

    plot(x = file$wavelength, y = file$relative_intensity,
         type = "l", ylab = "relative intensity",
         xlab = "wavelength (nm)", main = main_title,
         col = "blue", lwd = 2, xlim = c(lambda.min, lambda.max),
         ylim = c(0, max(file$relative_intensity[xl.index:xh.index])))
    grid()

  }
}
