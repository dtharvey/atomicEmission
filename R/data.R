#' Coefficients for Partition Function Models.
#'
#' Coefficients for the Irwin, deGalan, and Tamaki models for predicting an element's temperature-dependent partition function.
#'
#' @docType data
#'
#' @format A data frame with 46 observations of 18 variables:
#'
#' \describe{
#' \item{element}{name of element}
#' \item{symbol}{element's atomic symbol}
#' \item{at_no}{element's atomic number}
#' \item{deGalan_a}{first coefficient of deGalan model}
#' \item{deGalan_b}{second coefficient of deGalan model}
#' \item{deGalan_c}{third coefficient of deGalan model}
#' \item{deGalan_d}{fourth coefficient of deGalan model}
#' \item{deGalan_e}{fifth coefficient of deGalan model}
#' \item{deGalan_f}{sixth coefficient of deGalan model}
#' \item{tamaki_a}{first coefficient of Tamaki model}
#' \item{tamaki_b}{second coefficient of Tamaki model}
#' \item{tamaki_c}{third coefficient of Tamaki model}
#' \item{irwin_a0}{first coefficient of Irwin model}
#' \item{irwin_a1}{second coefficient of Irwin model}
#' \item{irwin_a2}{third coefficient of Irwin model}
#' \item{irwin_a3}{fourth coefficient of Irwin model}
#' \item{irwin_a4}{fifth coefficient of Irwin model}
#' \item{irwin_a5}{sixth coefficient of Irwin model}
#' }
#'
#' @source Coefficients are adapted from the following sources: deGalan, L.; Smith, R.; Winefordner, J. D. "The Electronic Partition Functions of Atoms and Ions Between 1500 K and 7000 K" \emph{Spectrochemica Acta}, \strong{1968}, \emph{23B}, 521-525; Tamaki, S.; Kuroda, T. "The Electronic Partition Functions of Atoms and Ions Between 7000 K and 12000 K" \emph{Spectrochimca Acta} \strong{1987}, \emph{42B}, 1105-1111; Irwin, A. W. "Polynomial Partition Function Approximations of 344 Atomic and Molecular Species" \emph{The Astrophysical Journal Supplement Series}, \strong{1981}, \emph{45}, 621-633.

"aesCoeff"

#' Characteristic Values for Atomic Emission Lines.
#'
#' The characteristic values for an element's atomic emission.
#'
#' @docType data
#'
#' @format A data frame with 3857 observations of 5 variables:
#'
#' \describe{
#' \item{element}{name of element}
#' \item{lc}{wavelength, in nm, at center of emission line}
#' \item{aki}{transition probability, in s^-1, from the upper energy level to the lower energy level}
#' \item{ek}{energy of the upper energy level, in eV}
#' \item{gk}{the statistical weight of the upper energy level}
#' }
#'
#' @source Data are from Kramida, A., Ralchenko, Yu., Reader, J., and NIST ASD Team (2018). NIST Atomic Spectra Database (ver. 5.5.6), <https://physics.nist.gov/asd>; National Institute of Standards and Technology, Gaithersburg, MD and are used with permission.

"aesLines"

#' Visible Spectrum.
#'
#' Gives the rgb color for visible light with wavelengths between
#' 300 nm and 800 nm in increments of 0.01 nm. Used to provide
#' the background when displaying atomic absorption line spectra.
#'
#' @docType data
#'
#' @format A data frame with 50001 observations of two variables:
#'   \describe{
#'   \item{wavelength}{wavelength, in nm, for visible
#'   light}
#'   \item{color}{rgb value for each wavelength}
#'   }
#'
#' @source Created locally using an internal script (createVis.R)
#'   and called by the function plotLines.

"visSpec"