tidydataset <- function (working_directory) {
     setwd(working_directory) #go to the correct working directory: UCI HAR Dataset
     activity_labels <- read.table("activity_labels.txt")
     feature_list <- read.table("features.txt")
     desired_features <- grep(("mean|std"),feature_list[,2]) #returns a vector corresponding to the features that corresponds to either a mean or a standard deviation
     desired_feature_names <- tolower(feature_list[desired_features,2]) #returns the corresponding names for the desired features
     ### Extract all the relevant files to compile ###
     setwd("test")
     subject_test <- read.table("subject_test.txt")
     x_test <- read.table("X_test.txt")
     y_test <- read.table("y_test.txt")
     setwd(working_directory)
     setwd("train")
     subject_train <- read.table("subject_train.txt")
     x_train <- read.table("X_train.txt")
     y_train <- read.table("y_train.txt")
     ### Complile the x files and reduce the number of features ###
     x_dataset <- rbind(x_train, x_test)[,desired_features]
     ### Add the activity labels and the subject number for each measurement ###
     full_dataset <- cbind(x_dataset, as.vector(rbind(y_train,y_test)),as.vector(rbind(subject_train,subject_test)))
     ### Replace the activity labels with descritive labels ###
     for (i in 1:6) {full_dataset[,length(full_dataset)-1] <- gsub(i,activity_labels[i,2],full_dataset[,length(full_dataset)-1])}
     ### Label the dataset with descriptive names ###
     extra_names <- c("activity","subject")
     names(full_dataset) <- c(desired_feature_names,extra_names)
     ### Create the tiny dataset ###
     library(reshape2)
     temp <- melt(full_dataset, id = extra_names, measure.vars = desired_feature_names) #create a narrow dataset
     tidy_dataset <- aggregate(value ~ activity + subject, data = temp, mean) #returns the tidy dataset
     write.table(tidy_dataset, file = "tidy dataset.txt")
     return(tidy_dataset)
}