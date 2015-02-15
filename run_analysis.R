######################################################################################
##
## Johns Hopkins Coursera course Data Science
## Part 3: Getting and Cleaning Data
##         Course Project
## Collect data, prepare clean and tidy dataset for analysis 
##
######################################################################################
##
## Wearable computing experiment: Human Activity Recognition with Smartphone
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
##
## Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. 
## Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass 
## Hardware-Friendly Support Vector Machine. International Workshop of Ambient 
## Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
##
####################################################################################

run_analysis <- function() {
    ## Function that merges the data, adds essential annotation from separate files,
    ## selects elements and turns out a tidy dataset
    
    ##Load necessary packages
    library(dplyr)
    library(reshape2)

    ## Load & merge Training + Test data with subjects, activities and labels
    
    #Measurement list
    Features <- read.table("./UCI HAR Dataset/features.txt")
    Feats <- Features[,2]
    
    #Training dataset
    Subject.train <- read.table("./UCI HAR Dataset/train/subject_train.txt", 
                    col.names = "Subject ID")    
    Labels.Train <- read.table("./UCI HAR Dataset/train/y_train.txt", 
                    col.names = "Activity")
    XTrain <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                    col.names = Feats, check.names = FALSE)
    X.Train <-cbind(Subject.train, Labels.Train, XTrain)
    
    #Test dataset
    Subject.test <-read.table("./UCI HAR Dataset/test/subject_test.txt", 
                    col.names = "Subject ID")
    Labels.Test <- read.table("./UCI HAR Dataset/test/y_test.txt", 
                    col.names = "Activity")
    XTest <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                     col.names = Feats, check.names = FALSE)
    X.Test <-cbind(Subject.test, Labels.Test, XTest)
    
    #Dataset merge
    XData.merge <- rbind(X.Train, X.Test)

    ## Extract mean and SD for each measurement
    mean.list <- grep("mean()", Feats)
    std.list <- grep("std", Feats)
    sel <- c(1,2, (c(mean.list, std.list) + 2))
    XData <- XData.merge[,sel]    
    
    # Average over Subject.ID and ACtivity
    dataMelt <- melt(XData,id=c("Subject.ID","Activity"), 
                     measure.vars=names(XData)[3:81])
    xData <- dcast(dataMelt, Subject.ID + Activity ~ variable,mean)
    
    # Factor activity labels, appropriate variable names
        # Subject ID, Activity, Measurement Mean, Measurement SD, 
    ActLbl <- read.table("./UCI HAR Dataset/activity_labels.txt")
    class(xData$Activity) <- "factor"
    levels(xData$Activity) <- ActLbl[,2]
    
    # Make tidy new dataset into a txt file
    write.table(xData, file = "tidydata.txt")
}