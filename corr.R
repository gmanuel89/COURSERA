########### ASSIGNMENT 1: CORRELATION
corr <- function (directory, threshold=0) {
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
# Generate the result Dataframe
correlationVector <- numeric()
# For every ID
for (i in unique(data$ID)) {
	# Select the rows which ID is equal to i
	idSubset <- data [data$ID == i,]
	# Compute the number of complete cases
	completeCases <- complete.cases(idSubset)
	ccNumber <- length(completeCases [completeCases == TRUE])
	# Caculate the correlation only if the number of complete cases is above the threshold
	if (ccNumber >= threshold && ccNumber !=0) {
		# Isolate the complete cases only
		idSubsetCC <- idSubset [which(completeCases ==TRUE),]
		# Compute the correlation
		corPollutants <- cor (idSubsetCC$sulfate, idSubsetCC$nitrate)
		# Fill in the result vector
		correlationVector <- append (correlationVector, round(corPollutants,5))
	}
}
return (correlationVector)
}
