#' Simulate Atomic Emission
#'
#' Simulates an element's atomic emission spectrum.
#'
#' The simulation of an element's atomic emission spectrum relies on published characteristic values for each of the element's atomic emission lines, which are available from NIST: the wavelength, in nm, the energy of the upper energy level, in eV, the statistical weight of the upper energy level, and the transition probability,in s^-1. The simulation also requires a value for the element's temperature-dependent partition function, which is estimated using one of three models: the Irwin model, for temperatures between 1000 K and 1500 K, the deGalan model for temperatures between 1500 K and 7000 K, and the Tamaki model for temperatures between 7000 K and 12000 K. The Irwin model is used in place of the Tamaki model for those elements (Be, B, Sc, and Ge) for which Tamaki does not report data. See the package's vignette for additional details.
#'
#' @param atom The element of interest, entered using one of three options: the element's atomic symbol (\code{atom = "H"}), the element's name (\code{atom = "hydrogen"}), or the element's atomic number (\code{atom = 1}). Elements are limited to atomic numbers 1-32, 36-38, 47-50, 54-56, 74, 79, 80, and 82.
#'
#' @param temperature The temperature in Kelvin. The temperature must have a value between 1000 K and 12000 K.
#'
#' @param width The total line width of an emission line, in nm.
#'
#' @return Returns a list with the following components \item{element}{name of the element} \item{symbol}{the element's atomic symbol} \item{temperature}{the temperature, in Kelvin} \item{wavelength}{a vector of wavelengths, in nm, for which emission intensities are calculated} \item{absolute_intensity}{a vector of calculated absolute emission intensities} \item{relative_intensity}{a vector of calculated relative emission intensities} \item{qt}{the value of the partition function calculate for the specified temperature using the specified model} \item{model}{the model used to estimate the element's partition function} \item{width}{the total line width of an emission line, in nm} \item{peaks}{the wavelength, in nm, for each of the element's emission lines from the NIST database} \item{peak_max}{the calculated intensity for each of the element's emission lines from the NIST database}
#'
#' @source Coefficients for the partition coefficient models are from the following sources: (a) For the deGalan model, deGalan, L.; Smith, R.; Winefordner, J. D. "The Electronic Partition Functions of Atoms and Ions Between 1500 K and 7000 K" \emph{Spectrochemica Acta}, \strong{1968}, \emph{23B}, 521-525; (b) For the Tamaki model Tamaki, S.; Kuroda, T. "The Electronic Partition Functions of Atoms and Ions Between 7000 K and 12000 K" \emph{Spectrochimca Acta} \strong{1987}, \emph{42B}, 1105-1111; and (c) For the Irwin model, Irwin, A. W. "Polynomial Partition Function Approximations of 344 Atomic and Molecular Species" \emph{The Astrophysical Journal Supplement Series}, \strong{1981}, \emph{45}, 621-633. Data for the emission lines (wavelengths, transition strengths, energy levels, and statistical weights) are from Kramida, A., Ralchenko, Yu., Reader, J., and NIST ASD Team (2018). NIST Atomic Spectra Database (ver. 5.5.6), <https://physics.nist.gov/asd>; National Institute of Standards and Technology, Gaithersburg, MD.
#'
#' @export
#'
#' @examples
#'
#' hydrogen = simulateAES(atom = "hydrogen",
#'     temperature = 4000, width = 0.2)
#' str(hydrogen)

simulateAES = function(atom = "H",
                       temperature = 7000,
                       width = 0.3){

# identify the element to use based on the type of entry: atomic
# number, atomic symbol, name

  if (is.numeric(atom) == TRUE) {
    row.index = which(aesCoeff$at_no == atom)
  } else if (nchar(atom) < 3) {
    row.index = which(aesCoeff$symbol == atom)
  } else {
    row.index = which(aesCoeff$element == atom)
  }

# throw out an error message if element is not in the database

  if (length(row.index) == 0) {
    stop("The selected element is not included in this database.")
  }

# throw out an error message if temperature is not within limits

  if (temperature < 1000 | temperature > 12000){
    stop("The temperature must be between 1000 K and 12000 K")
  }

# extract the element's name, the characteristic values for the
# element's emission lines, provide Boltzmann constant in eV, and
# create a vector of wavelengths

  elementName = aesCoeff[row.index, 1]
  element = aesLines[aesLines$element == elementName, ]
  kb = 8.6173303e-5
  wavelength = seq(390,700,0.01)

# determine model to use, read in the model's coefficients, and
# calculate the partition coefficient, qt, based on the model and
# the temperature

  if (temperature < 1500){

# use the Irwin model for T < 1500 K

    partition = "Irwin"

    irwin_a0 = aesCoeff$irwin_a0[row.index]
    irwin_a1 = aesCoeff$irwin_a1[row.index]
    irwin_a2 = aesCoeff$irwin_a2[row.index]
    irwin_a3 = aesCoeff$irwin_a3[row.index]
    irwin_a4 = aesCoeff$irwin_a4[row.index]
    irwin_a5 = aesCoeff$irwin_a5[row.index]

    ln.qt = irwin_a0*log(temperature)^0 +
      irwin_a1*log(temperature)^1 +
      irwin_a2*log(temperature)^2 +
      irwin_a3*log(temperature)^3 +
      irwin_a4*log(temperature)^4 +
      irwin_a5*log(temperature)^5

    qt = exp(ln.qt)

  } else if (temperature >= 1500 & temperature < 7000) {

# use the deGalan model for T between 1500 K and 7000 K

    partition = "deGalan"

    deGalan_a = aesCoeff$deGalan_a[row.index]
    deGalan_b = aesCoeff$deGalan_b[row.index]
    deGalan_c = aesCoeff$deGalan_c[row.index]
    deGalan_d = aesCoeff$deGalan_d[row.index]
    deGalan_e = aesCoeff$deGalan_e[row.index]
    deGalan_f = aesCoeff$deGalan_f[row.index]

    qt = deGalan_a +
      deGalan_b*(temperature/1000) +
      deGalan_c*(temperature/1000)^2 +
      deGalan_d*(temperature/1000)^3 +
      deGalan_e*(temperature/1000)^4 +
      deGalan_f*(temperature/1000)^5

  } else {

# use the Tamaki model, but...

    tamaki_a = aesCoeff$tamaki_a[row.index]
    tamaki_b = aesCoeff$tamaki_b[row.index]
    tamaki_c = aesCoeff$tamaki_c[row.index]

# ...if coefficients for Tamaki model are missing, use Irwin model

    if (is.na(tamaki_a) == TRUE){

      partition = "Irwin"

      irwin_a0 = aesCoeff$irwin_a0[row.index]
      irwin_a1 = aesCoeff$irwin_a1[row.index]
      irwin_a2 = aesCoeff$irwin_a2[row.index]
      irwin_a3 = aesCoeff$irwin_a3[row.index]
      irwin_a4 = aesCoeff$irwin_a4[row.index]
      irwin_a5 = aesCoeff$irwin_a5[row.index]

      ln.qt = irwin_a0*log(temperature)^0 +
        irwin_a1*log(temperature)^1 +
        irwin_a2*log(temperature)^2 +
        irwin_a3*log(temperature)^3 +
        irwin_a4*log(temperature)^4 +
        irwin_a5*log(temperature)^5

      qt = exp(ln.qt)

    } else {

    partition = "Tamaki"

    qt = tamaki_a * (temperature/1000)^2 +
      tamaki_b * (temperature/1000) +
      tamaki_c
    }
  }

# calculate peak amplitude, S, for each emission line

  S = element$gk*element$aki/(qt*element$lc) *
    exp(-element$ek/(kb*temperature))

# create matrix to store wavelength-dependent intensities, I, for
# each emission line

  I = matrix(data = 0, ncol = length(element$lc),
             nrow = length(wavelength))

# calculate the wavelength-dependent intensities for each
# emission line

  for (i in 1:length(element$lc)) {
    I[ , i] = (2 * S[i]/pi) *
      (width/(4 * (wavelength - element$lc[i])^2 + width^2))
  }

# calculate and store the emission spectrum giving the total
# intensity at each wavelength and the maximum intensity for each
# emission line

  intensity = apply(I, 1, sum)
  max_intensity = apply(I, 2, max)

# output list of values to return

  output = list("element" = elementName,
                "symbol" = aesCoeff[row.index, 2],
                "temperature" = temperature,
                "wavelength" = wavelength,
                "absolute_intensity" = intensity,
                "relative_intensity" = intensity/max(intensity),
                "qt" = qt,
                "model" = partition,
                "width" = width,
                "peaks" = element$lc,
                "peak_max" = max_intensity)
  invisible(output)

}
