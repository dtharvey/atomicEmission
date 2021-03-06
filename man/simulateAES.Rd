% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulateAES.R
\name{simulateAES}
\alias{simulateAES}
\title{Simulate Atomic Emission}
\source{
Coefficients for the partition coefficient models are from the following sources: (a) For the deGalan model, deGalan, L.; Smith, R.; Winefordner, J. D. "The Electronic Partition Functions of Atoms and Ions Between 1500 K and 7000 K" \emph{Spectrochemica Acta}, \strong{1968}, \emph{23B}, 521-525; (b) For the Tamaki model Tamaki, S.; Kuroda, T. "The Electronic Partition Functions of Atoms and Ions Between 7000 K and 12000 K" \emph{Spectrochimca Acta} \strong{1987}, \emph{42B}, 1105-1111; and (c) For the Irwin model, Irwin, A. W. "Polynomial Partition Function Approximations of 344 Atomic and Molecular Species" \emph{The Astrophysical Journal Supplement Series}, \strong{1981}, \emph{45}, 621-633. Data for the emission lines (wavelengths, transition strengths, energy levels, and statistical weights) are from Kramida, A., Ralchenko, Yu., Reader, J., and NIST ASD Team (2018). NIST Atomic Spectra Database (ver. 5.5.6), <https://physics.nist.gov/asd>; National Institute of Standards and Technology, Gaithersburg, MD.
}
\usage{
simulateAES(atom = "H", temperature = 7000, width = 0.3)
}
\arguments{
\item{atom}{The element of interest, entered using one of three options: the element's atomic symbol (\code{atom = "H"}), the element's name (\code{atom = "hydrogen"}), or the element's atomic number (\code{atom = 1}). Elements are limited to atomic numbers 1-32, 36-38, 47-50, 54-56, 74, 79, 80, and 82.}

\item{temperature}{The temperature in Kelvin. The temperature must have a value between 1000 K and 12000 K.}

\item{width}{The total line width of an emission line, in nm.}
}
\value{
Returns a list with the following components \item{element}{name of the element} \item{symbol}{the element's atomic symbol} \item{temperature}{the temperature, in Kelvin} \item{wavelength}{a vector of wavelengths, in nm, for which emission intensities are calculated} \item{absolute_intensity}{a vector of calculated absolute emission intensities} \item{relative_intensity}{a vector of calculated relative emission intensities} \item{qt}{the value of the partition function calculate for the specified temperature using the specified model} \item{model}{the model used to estimate the element's partition function} \item{width}{the total line width of an emission line, in nm} \item{peaks}{the wavelength, in nm, for each of the element's emission lines from the NIST database} \item{peak_max}{the calculated intensity for each of the element's emission lines from the NIST database}
}
\description{
Simulates an element's atomic emission spectrum.
}
\details{
The simulation of an element's atomic emission spectrum relies on published characteristic values for each of the element's atomic emission lines, which are available from NIST: the wavelength, in nm, the energy of the upper energy level, in eV, the statistical weight of the upper energy level, and the transition probability,in s^-1. The simulation also requires a value for the element's temperature-dependent partition function, which is estimated using one of three models: the Irwin model, for temperatures between 1000 K and 1500 K, the deGalan model for temperatures between 1500 K and 7000 K, and the Tamaki model for temperatures between 7000 K and 12000 K. The Irwin model is used in place of the Tamaki model for those elements (Be, B, Sc, and Ge) for which Tamaki does not report data. See the package's vignette for additional details.
}
\examples{

hydrogen = simulateAES(atom = "hydrogen",
    temperature = 4000, width = 0.2)
str(hydrogen)
}
