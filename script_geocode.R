# Geocoding a csv column of "addresses" in R

#load ggmap
library(ggmap)

# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <- read.csv(fileToLoad, stringsAsFactors = FALSE, encoding = "UTF-8")

#Banco com long e lat
a <- geocode(origAddress$ENDERECO, source = "google")
names(a) <- c("END_LON", "END_LAT")

#Compondo banco final
origAddress <- cbind(origAddress, a)


# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE, fileEncoding = "UTF-8")
