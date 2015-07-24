## This script does the following. 
##
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##

## create directory for combined data
if(!file.exists("combined")) {
        dir.create("combined")
}
        
## 1. Merges the training and the test sets to create one data set.
testSet <- readLines("UCI HAR Dataset\\test\\X_test.txt")
trainSet <- readLines("UCI HAR Dataset\\train\\X_train.txt")
combinedSet <- c(testSet, trainSet)
writeLines(combinedSet, "combined\\X_combined.txt")

# read labels and combine
testLable <- read.table("UCI HAR Dataset\\test\\Y_test.txt")
trainLabel <- read.table("UCI HAR Dataset\\train\\Y_train.txt")
combinedLabel <- c(testLable$V1, trainLabel$V1)

# read subjects and combine
testSubject <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
trainSubject <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
combinedSubject <- c(testSubject$V1, trainSubject$V1)

# read variable names for columns in the combinedSet
features <- read.table("UCI HAR Dataset\\features.txt", sep = " ", stringsAsFactors = F)
variables <- features$V2
numVariables <- length(variables)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# each column in combin
# initilize the data frame to save result
res <- as.data.frame(matrix(nrow = 0, ncol = numVariables))

numRows = length(combinedSet)
print(paste("Total number of rows are ", numRows))
for(i in 1:length(combinedSet)) {
#for(i in 1:10) { # this is for test
        aLine <- combinedSet[i]
        x <- unlist(strsplit(aLine, " "))
        x1 <- x[nchar(x) > 0]
        x2 <- as.numeric(x1)
        # print out the progress
        print(paste("Percent completed", i / numRows * 100)) 
        res <- rbind(res, as.data.frame(matrix(x2, nrow = 1)))
}

## 4. Appropriately labels the data set with descriptive variable names. 
names(res) <- variables
idMeanSd <- c(grep("mean()", variables, fixed = T), grep("std()", variables, fixed = T))
resMeanSd <- res[idMeanSd]

write.table(resMeanSd, file = "combined\\combinedSet.txt", sep = "\t", row.names = T, col.names = F)

## 3. Uses descriptive activity names to name the activities in the data set
activityLables <- read.table("UCI HAR Dataset\\activity_labels.txt", stringsAsFactors = F)
activityNames <- activityLables$V2
resRowNames <- activityNames[combinedLabel]
#row.names(resMeanSd) <- resRowNames[1:10] # this line won't work.
        
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# the line below is for test purpose.
# resActSub <- cbind(activityName = resRowNames[1:10], subject = combinedSubject[1:10], resMeanSd)
resActSub <- cbind(activityName = resRowNames, subject = combinedSubject, resMeanSd)
library(dplyr)
resActSub.tbl <- tbl_df(resActSub)

resAvgByActSub <- resActSub.tbl %>%
        group_by(activityName, subject) %>%
        summarise_each(funs(mean))

# write the final tidy data set to a file
write.table(resAvgByActSub, file = "MeasurementAverageByActivityAndSubject.txt", sep = "\t", row.names = F, col.names = T)
