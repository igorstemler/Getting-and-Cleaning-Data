# Getting-and-Cleaning-Data

# This repo explains how all of the scripts work and how they are connected:

# First downloaded and unziped the Human Activity Recognition Using Smartphones Data Set.
file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")){
    download.file(file_url,"UCI_HAR.zip")
}
unzip("UCI_HAR.zip")

# List the files.
list.files("./UCI HAR Dataset/test")

# Join the test data sets. 
test <- cbind(read.table("./UCI HAR Dataset/test/subject_test.txt",header=F),
              read.table("./UCI HAR Dataset/test/y_test.txt",header=F),
              read.table("./UCI HAR Dataset/test/X_test.txt",header=F))
test$Data <- "test"

# Join the train data sets. 
train <- cbind(read.table("./UCI HAR Dataset/train/subject_train.txt",header=F),
              read.table("./UCI HAR Dataset/train/y_train.txt",header=F),
              read.table("./UCI HAR Dataset/train/X_train.txt",header=F))
train$Data <- "train"

# Read the names of the columns.
names <- read.table("./UCI HAR Dataset/features.txt",
                    header=F, stringsAsFactor=F)[,2]

# Put the names of the columns in the test and train data sets. 
colnames(test) <- c("subject","activity",names,"Data")
colnames(train) <- c("subject","activity",names,"Data")

# Merges the training and the test sets to create one data set.
HAR <- rbind(test,train)
rm(test,train)

# Extracts only the measurements on the mean and standard deviation for each measurement.
HAR<-HAR[,c("Data","subject","activity",
            grep("mean()",names(HAR),value=T,fixed=T), 
            grep("std",names(HAR),value=T,fixed=T))] 

# Uses descriptive activity names to name the activities in the Data set
lab <- read.table("./UCI HAR Dataset/activity_labels.txt",
                  header=F, stringsAsFactor=F)
                  
# Transform the variable activity as a factor.
HAR$activity <- factor(HAR$activity, levels=lab$V1, labels = lab$V2)

# Appropriately labels the data set with descriptive variable names.
names(HAR)

# Create a data frame of the name and description of the variables.
descriptive<-data.frame(variable=names(HAR), description = names(HAR),stringsAsFactors=F)
descriptive$description[4:36]<-paste("Mean of ",gsub("-mean\\()","",
                                                     descriptive$description[4:36]),sep="")
descriptive$description[37:69]<-paste("Standard Deviation ",gsub("-std\\()","",
                                                     descriptive$description[37:69]),sep="")
descriptive$description[1]<-"Data set is from train or test"
descriptive$description[2]<-"Number of the volunteer"
descriptive$description[3]<-"Activity performed"

# Data set with the descriptions.
descriptive$Class <- sapply(HAR,class)

# Save the code book.
write.table(descriptive,"Code_book.txt")

# Data set with the average of each variable for each activity and each subject.
mean_subj_activ <- aggregate(HAR[,4:ncol(HAR)],
                             by=list(HAR$Data,HAR$subject,HAR$activity),
                             FUN=mean)
colnames(mean_subj_activ)[1:3] <- c("Data","Subject","Activity")

# Save the aggregated data set.
write.table(mean_subj_activ,"mean_subj_activ.txt",row.names=F)
