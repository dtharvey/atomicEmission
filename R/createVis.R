#' Create Visible Spectrum
#'
#' Convets a vector of visible wavelengths to their approximate
#' RGB values with an option to adjust the transparency of the colors to the relative intensity of the emission at each wavelength. The code is modified from
#' https://gist.github.com/friendly/67a7df339aa999e2bcfcfec88311abfc.
#'
#' @param wavelength Vector of visible wavelengths, in nm, to convert to RGB values. Limited to wavelengths between 380 nm and 750 nm. Wavelengths outside of this range have an RGB value of \code{#000000}, which corresponds to the color black.
#'
#' @param gamma Correction for a non-linear response to RGB values.
#'
#' @param intensity Vector of intensities of wavelengths.
#'
#' @param adj_intensity Logical value that determines whether the transparency of an emission line is scaled to the emission line's relative intensity. Defaults to \code{TRUE}.
#'
#' @return vector of RGB values
#'
#' @importFrom grDevices rgb
#'
#' @export

createVis = function(wavelength, intensity,
                     adj_intensity, gamma = 1){

  R = rep(0, length(wavelength))
  G = rep(0, length(wavelength))
  B = rep(0, length(wavelength))

  for (i in 1:length(wavelength)){

    if (wavelength[i] >= 380 & wavelength[i] <= 440) {
      attenuation = 0.3 + 0.7 * (wavelength[i] - 380) / (440 - 380)
      r = ((-(wavelength[i] - 440) / (440 - 380)) * attenuation) ^ gamma
      g = 0.0
      b = (1.0 * attenuation) ^ gamma
    }
    else if (wavelength[i] >= 440 & wavelength[i] <= 490) {
      r = 0.0
      g = ((wavelength[i] - 440) / (490 - 440)) ^ gamma
      b = 1.0
    }
    else if (wavelength[i] >= 490 & wavelength[i] <= 510) {
      r = 0.0
      g = 1.0
      b = (-(wavelength[i] - 510) / (510 - 490)) ^ gamma
    }
    else if (wavelength[i] >= 510 & wavelength[i] <= 580) {
      r = ((wavelength[i] - 510) / (580 - 510)) ^ gamma
      g = 1.0
      b = 0.0
    }
    else if (wavelength[i] >= 580 & wavelength[i] <= 645) {
      r = 1.0
      g = (-(wavelength[i] - 645) / (645 - 580)) ^ gamma
      b = 0.0
    }
    else if (wavelength[i] >= 645 & wavelength[i] <= 750) {
      attenuation = 0.3 + 0.7 * (750 - wavelength[i]) / (750 - 645)
      r = (1.0 * attenuation) ^ gamma
      g = 0.0
      b = 0.0
    }
    else {
      r = 0.0
      g = 0.0
      b = 0.0
    }
    R[i] = r * 255
    G[i] = g * 255
    B[i] = b * 255
  }

  if (adj_intensity == TRUE){
  return (rgb(floor(R), floor(G), floor(B), maxColorValue = 255,
              alpha = 255 * (intensity/max(intensity))))
  } else {
    return (rgb(floor(R), floor(G), floor(B), maxColorValue = 255))
  }
}
