# script to create visible spectrum of colors for use as
# background when plotting atomic absorption line spectra

wavelength = seq(300, 800, 0.01)
intensity = rep(1, length(wavelength))
color = createVis(wavelength, intensity, adj_intensity = TRUE)
visSpec = data.frame(wavelength, color, stringsAsFactors = FALSE)
devtools::use_data(visSpec, overwrite = TRUE)
