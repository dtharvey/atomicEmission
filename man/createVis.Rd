% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createVis.R
\name{createVis}
\alias{createVis}
\title{Create Visible Spectrum}
\usage{
createVis(wavelength, intensity, adj_intensity, gamma = 1)
}
\arguments{
\item{wavelength}{Vector of visible wavelengths, in nm, to convert to RGB values. Limited to wavelengths between 380 nm and 750 nm. Wavelengths outside of this range have an RGB value of \code{#000000}, which corresponds to the color black.}

\item{intensity}{Vector of intensities of wavelengths.}

\item{adj_intensity}{Logical value that determines whether the transparency of an emission line is scaled to the emission line's relative intensity. Defaults to \code{TRUE}.}

\item{gamma}{Correction for a non-linear response to RGB values.}
}
\value{
vector of RGB values
}
\description{
Convets a vector of visible wavelengths to their approximate
RGB values with an option to adjust the transparency of the colors to the relative intensity of the emission at each wavelength. The code is modified from
https://gist.github.com/friendly/67a7df339aa999e2bcfcfec88311abfc.
}
