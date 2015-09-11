########### ASSIGNMENT 1: COMPLETE
complete <- function (directory, id=1:332) {
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
	data <- merge (data, csvFiles[[f]], all=TRUE, sort=FALSE)
}
######## Generate the dataframe for the result
results <- data.frame (id=numeric(), nobs=numeric())
# For each of the selected ID...
for (i in id) {
	# Select the rows which ID is equal to i
	idSubset <- data [data$ID %in% i,]
	# Compute the number of complete cases
	completeCases <- complete.cases(idSubset)
	ccNumber <- length(completeCases [completeCases == TRUE])
	# Fill in the dataframe
	resultsRow <- data.frame (id=i, nobs=ccNumber)
	results <- rbind (results, resultsRow)
}
return (results)
}
