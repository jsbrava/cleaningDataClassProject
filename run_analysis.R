#run_analysis.R
#Getting and Cleaning Data script
#James Smith May 2014

# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# download the data file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(url, "accel.zip")
list.files()
dateDownloaded <- date()
dateDownloaded
write(dateDownloaded, "dateDownloaded.txt")

# By looking at the readme.txt file, the files I need are:
#   'train/X_train.txt' & 'train/y_train.txt' : Training set with activity
#   'test/X_test.txt': Test set.
#   'features.txt' used for column headers
#### next section gets the data files
train <- read.table(unz("accel.zip", "UCI HAR Dataset/train/X_train.txt"))
# activity numbers associated with training set data
activityTrain <- read.table(unz("accel.zip", "UCI HAR Dataset/train/y_train.txt"))
subjectTrain <- read.table(unz("accel.zip", "UCI HAR Dataset/train/subject_train.txt"))
test <- read.table(unz("accel.zip", "UCI HAR Dataset/test/X_test.txt"))
# activity numbers associated with test set data
activityTest <- read.table(unz("accel.zip", "UCI HAR Dataset/test/y_test.txt"))
subjectTest <- read.table(unz("accel.zip", "UCI HAR Dataset/test/subject_test.txt"))
names <- read.table(unz("accel.zip", "UCI HAR Dataset/features.txt"))  #names and indices of tests
activityLabels <- read.table(unz("accel.zip", "UCI HAR Dataset/activity_labels.txt"))  #names and indices of activities

####
# use rbind instead of merge when the variables are the same
#### combine the training and test data sets together
total <- rbind(train, test)

#### clean up the names and attach to the total dataset
cleanNames <- as.data.frame(sapply(names,gsub,pattern="-mean",replacement="Mean"))
cleanNames <- as.data.frame(sapply(cleanNames,gsub,pattern="-std",replacement="Std"))
cleanNames <- as.data.frame(sapply(cleanNames,gsub,pattern="\\(",replacement=""))
cleanNames <- as.data.frame(sapply(cleanNames,gsub,pattern="\\)",replacement=""))
cleanNames <- as.data.frame(sapply(cleanNames,gsub,pattern="-",replacement=""))
cleanNames <- as.data.frame(sapply(cleanNames,gsub,pattern=",gravity",replacement="Gravity"))
names(total) <- cleanNames[,2]

#### get the column names that contain Mean or Std and keep just those columns
#  use gsub() and lower() to clean up names
rowsMS <- grepl("Mean", cleanNames[,2]) | grepl("Std", cleanNames[,2])    # vector TRUE for Std or Mean
namesMS <- cleanNames[rowsMS,]  #names and index of Mean or Std
totalMS <- total[rowsMS]   #reduces total to data columns containing Mean or Std

### add the activity and subject columns
totalActivity <- rbind(activityTrain, activityTest)  # combine train and test activity sets
names(totalActivity) <- "activity"                   # give it a name
activityLabels <- as.data.frame(sapply(activityLabels,tolower))
totalActivity[,1] <- activityLabels[totalActivity[,1],2] # replace activity numbers with labels

totalSubjects <- rbind(subjectTrain, subjectTest)    # combine train and test subject sets
names(totalSubjects) <- "subject"                    # give it a name
totalActivitySubjects <- cbind(totalActivity, totalSubjects) # combine activity and subjects
finalDF <- cbind(totalMS, totalActivitySubjects)     # finally, combine reduced training & test with activity & subjects

### write the final data frame to a text file
write.table(finalDF, "watchdata.txt", sep="\t")      # write it to disk

### second tidy data set
meanActivitySubject <- ddply(finalDF, c("activity","subject"), colwise(mean))
write.table(meanActivitySubject, "tidyData.txt", sep="\t")      # write it to disk
