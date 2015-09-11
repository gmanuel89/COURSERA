################ ASSIGNMENT 1: POLLUTANT MEAN
pollutantmean <- function (directory, pollutant, id=1:332) {
# Files in the data folder
csvPaths <- list.files (directory, full.names=TRUE)
# List containing all the csv files
csvFiles <- list()
for (f in 1:length(csvPaths)) {
	csvFiles [[f]] <- read.csv (csvPaths[f])
}
############### Generate the dataframe for the whole dataset
data <- csvFiles[[1]]
# For each csvFile (after the first):
for (f in 2:length(csvFiles)) {
	# Merge dataframes by rows
	data <- merge (data, csvFiles[[f]], all=TRUE)
}
############# Return the mean only if the pollutant inserted is actually in the dataset
if (pollutant %in% names(data)) {
	# Take a subset containing only the pollutant of interest and the ID number of interest
	pollutantVector <- data [data$ID %in% id, pollutant]
	# Calculate the mean
	pollutantMean <- round (mean (pollutantVector, na.rm=TRUE), 3)
	return (pollutantMean)
} else {print ("The pollutant of interest is not in the dataset!!")}
}
