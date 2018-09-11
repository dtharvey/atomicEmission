#' Plot Atomic Emission Lines
#'
#' Plots an element's simulated atomic emission or atomic
#' absorption line spectrum. The lines are taken from an object
#' created using \code{simulateAES}. Atomic absorption spectra
#' are displayed as a set of black lines superimposed on a
#' background that shows the spectrum of visible electromagnetic
#' radiation. Atomic emission spectra are displayed as set of
#' lines, each of which has a color that matches its wavelength,
#' superimosed on either a white or a black background.
#'
#' @param file The name of an object, created by
#'   \code{simulateAES}, that contains the results of a
#'   simulation.
#'
#' @param lambda.min The minimum wavelength (in nm) for scaling
#'   the x-axis; defaults to 390 nm, which is the lower limit.
#'
#' @param lambda.max The maximum wavelength (in nm) for scaling
#'   the x-axis; defaults to 700 nm, which is the upper limit.
#'
#' @param spectrum_type Type of line spectrum, either
#'   \code{emission} or \code{absorbance}. Defaults to
#'   \code{emission}.
#'
#' @param emission_bg For an emission spectrum, choice of
#'   background color as \code{white} or \code{black}. Defaults
#'   to \code{white}.
#'
#' @param main_title An optional main title. If \code{NULL}, then
#'   a default title gives the element's atomic symbol, the
#'   temperature, and the partition model.
#'
#' @param scale_emission Logical value that determines whether
#'   the transparency of an emission line is scaled to the
#'   emission line's relative intensity. Defaults to
#'   \code{FALSE}, which shows all emission lines regardless of
#'   intensity; emission lines of low intensity are too faint to
#'   see when \code{scale_emission} is set to \code{TRUE}. The
#'   value for \code{scale_emission} is ignored when plotting the
#'   element's absorbance spectrum.
#'
#' @return Returns a plot of the element's atomic emission line
#'   spectrum or the element's atomic absorption line spectrum.
#'
#' @importFrom graphics plot rect abline par
#'
#' @export
#'
#' @examples
#'
#' hydrogen = simulateAES(atom = "H")
#' plotLines(file = hydrogen, spectrum_type = "emission",
#'    emission_bg = "white", scale_emission = TRUE)
#' plotLines(file = hydrogen, spectrum_type = "emission",
#'    emission_bg = "white", scale_emission = FALSE)
#' plotLines(file = hydrogen, spectrum_type = "emission",
#'    emission_bg = "black", scale_emission = FALSE)
#' plotLines(file = hydrogen, spectrum_type = "absorbance",
#'    emission_bg = "white", scale_emission = TRUE)

plotLines = function(file,
                     lambda.min = 390,
                     lambda.max = 700,
                     spectrum_type = c("emission", "absorbance"),
                     emission_bg = c("white", "black"),
                     scale_emission = TRUE,
                     main_title = NULL
                     ){

# determine the type of spectrum to plot

  spectrum_type = match.arg(spectrum_type)

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

# create plot if spectrum is emission

  if(spectrum_type == "emission"){
    old.par = par(yaxt = "n")
    plot(x = -1, y = -1, xlim = c(lambda.min, lambda.max),
         xlab = "wavelength (nm)", ylim = c(0, 1), ylab = "",
         main = main_title)
    rect(300, -1, 800, 2, col = emission_bg)
    peak_color = createVis(wavelength = file$peaks,
                            intensity = file$peak_max,
                            adj_intensity = scale_emission,
                            gamma = 1)
    abline(v = file$peaks, col = peak_color, lwd = 1)
    par(old.par)

  } else {

# create plot if spectrum is absorbance

    old.par = par(yaxt = "n")
    plot(x = -1,y = -1, xlim = c(lambda.min, lambda.max),
         xlab = "wavelength (nm)", ylim = c(0, 1), ylab = "",
         main = main_title)
    abline(v = visSpec$wavelength, col = visSpec$color)
    abline(v = file$peaks, col = "black", lwd = 1)
    par(old.par)

  }

}