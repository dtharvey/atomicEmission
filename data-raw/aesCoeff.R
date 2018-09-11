# script to convert aesCoeff.csv file into an .rds file; whenever
# a change is made to the aesCoeff.csv file, running the code
# below will update the corresponding aesCoeff.rda file

# read in .csv file with stringsAs
aesCoeff = read.csv("data-raw/aesCoeff.csv",
                    stringsAsFactors = FALSE)
devtools::use_data(aesCoeff, overwrite = TRUE)