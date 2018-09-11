#' Identify Peaks in AES Spectrum
#'
#' Creates a table of the most intense peaks in an element's
#' simulated atomic emission spectrum. The table is constructed
#' using data stored in an object created using
#' \code{simulateAES} and is ordered from more intense peaks to
#' less intense peaks.
#'
#' @param file The name of the object, created using
#'   \code{simulateAES}, that contains the results of a
#'   simulation.
#'
#' @param signal_type How to report the emission intensity; one
#'   of \code{"relative"}, which scales the intensity to a
#'   maximum value of 1, or \code{"absolute"}, which uses the
#'   actual emission intensities. Defaults to \code{"relative"}.
#'
#' @param percent_max The minimum intensity for which a peak is
#'   identified, given as a percentage of the element's maximum
#'   absolute or relative emission intensity. Defaults to 1\% of
#'   the element's maximum absolute or relative emission
#'   intensity.
#'
#' @param num_peaks The maximum number of peaks to return.
#'   Defaults to a table with a maximum of 5 peaks.
#'
#' @param sortby How to sort the peaks; one of
#'   \code{"intensity"}, which sorts the list of peaks from
#'   highest to lowest intensity, or \code{"wavelength"}, which
#'   sorts the list of peaks from shortest to longest wavelength.
#'
#' @return Returns a data frame with two columns: the wavelengths
#'   (in nm) of the peaks and the absolute or relative
#'   intensities of the peaks.
#'
#' @export
#'
#' @examples
#' hydrogen = simulateAES(atom = "H")
#' hydrogen_peaks = identifyPeaks(file = hydrogen)
#' hydrogen_peaks


identifyPeaks = function(file,
                         signal_type = c("relative", "absolute"),
                         percent_max = 1,
                         num_peaks = 5,
                         sortby = c("intensity", "wavelength")) {

# identify whether to use absolute or relative emission intensities

  signal_type = match.arg(signal_type)

# identify how to sort peaks

  sortby = match.arg(sortby)
  if (sortby == "intensity"){
    sortby = TRUE

  } else {

    sortby = FALSE

  }

# check to verify that the pracma package is installed

  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("You need to install the pracma package.")
  }

# create table if signal is relative emission intensity

  if (signal_type == "relative") {
  pk_data = pracma::findpeaks(file$relative_intensity,
                              minpeakheight = max(file$relative_intensity) *
                                percent_max/100,
                              npeaks = num_peaks,
                              sortstr = sortby)

# the function findpeaks returns a matrix with four columns; the
# first column gives the peak intensities, the second column
# gives the index for the peaks, and the last two columns, which
# are not of interest, give the indecies for where the peak
# begins and ends; the first two columns are extracted, saved as
# a data frame and returned

  pk_intensity = formatC(pk_data[ , 1], format = "f", digits = 3)
  pk_wavelength = file$wavelength[pk_data[ , 2]]
  pk_table = data.frame(pk_wavelength, pk_intensity)
  colnames(pk_table) = c("Wavelength (nm)", "Relative Intensity")
  output = pk_table

  } else {

# create table if signal is absolute emission intensity

    pk_data = pracma::findpeaks(file$absolute_intensity,
                                minpeakheight = max(file$absolute_intensity) *
                                  percent_max/100,
                                npeaks = num_peaks,
                                sortstr = sortby)

    pk_intensity = formatC(pk_data[ , 1], format = "e", digits = 3)
    pk_wavelength = file$wavelength[pk_data[ , 2]]
    pk_table = data.frame(pk_wavelength, pk_intensity)
    colnames(pk_table) = c("Wavelength (nm)", "Absolute Intensity")
    output = pk_table
  }

}