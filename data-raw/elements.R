# script to convert the file aesLines.csv into an .rda files
# stored in the data folder; whenever a change is made to the
# .csv files, running the code below will update the
# corresponding .rda files

aesLines = read.csv("data-raw/aesLines.csv", stringsAsFactors = FALSE)
aesLines[, 4] = as.numeric(aesLines[, 4])
devtools::use_data(aesLines, overwrite = TRUE)

