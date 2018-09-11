#' Atomic Emission Spectroscopy
#'
#' A collection of functions for simulating and visualizing
#' atomic emission spectra between 390 nm and 700 nm. Atomic
#' emission spectra are simulated using one of three models for
#' estimating an element's temperature-dependent partition
#' function. Emission spectra are displayed as either peaks or as
#' lines, and the corresponding atomic absorption line spectrum
#' may be viewed.
#'
#' @name atomicEmission-package
#'
#' @docType package
#'
#' @author David T. Harvey
#'
#' Maintainer: David T. Harvey \email{harvey@depauw.edu}
#'
NULL

if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("aesCoeff", "aesLines", "visSpec"))
}