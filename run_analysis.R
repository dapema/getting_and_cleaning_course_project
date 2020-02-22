#########################################################################################
# This file runs the analysis for the 'Getting and Cleaning Data Course Project'
#
#
#######################################################################################


#empty Workspace
rm(list = ls())

#Set working directory
setwd('C:/Users/maasd/Desktop/R_exos/getting_and_cleaning_data')


#Load packages ####
###################
library(dplyr)


#Load dataset ####
###################

#Load activitiy labels
activity_labels          <- read.table('./data/UCI_HAR_Dataset/activity_labels.txt')
names(activity_labels)   <- c('id', 'activity_names') 

#Load features
features                 <- read.table('./data/UCI_HAR_Dataset/features.txt')
names(features)          <- c('id', 'feature_names')


#Load and name training subject identifiers
training_subjects        <- read.table('./data/UCI_HAR_Dataset/train/subject_train.txt')
names(training_subjects) <- 'subject_ident'

#Load training set 
training_set             <- read.table('./data/UCI_HAR_Dataset/train/X_train.txt')

#Load training labels 
training_labels          <- read.table('./data/UCI_HAR_Dataset/train/y_train.txt')
names(training_labels)   <- 'training_labels'


#Load and name training subject identifiers
test_subjects            <- read.table('./data/UCI_HAR_Dataset/test/subject_test.txt')
names(test_subjects)     <- 'subject_ident'

#Load training set 
test_set                 <- read.table('./data/UCI_HAR_Dataset/test/X_test.txt')

#Load training labels 
test_labels              <- read.table('./data/UCI_HAR_Dataset/test/y_test.txt')
names(test_labels)       <- 'test_labels'


#########################################################################################################
#########################################################################################################

#Merging datasets  ####
#######################

# PREPARING TRAINING DATASET 
#label the columns of the training set 
names(training_set) <- features$feature_names

#label the rows of the training set 
training_set2 <- cbind(training_labels, training_set)

#add activity labels to triaing set 
training_set3 <- merge(activity_labels, training_set2, by.x = 'id', by.y = 'training_labels')

#delete uninformative id variable
training_set4 <- select(training_set3, -id)

#add information on subjects 
training_set5 <- cbind(training_subjects, training_set4)

#delete temporary datasets and relabel dataset 
training_set <- training_set5
rm(training_set2, training_set3, training_set4, training_set5)



# PREPARING TEST DATASET 
#label the columns of the test set 
names(test_set) <- features$feature_names

#label the rows of the test set 
test_set2 <- cbind(test_labels, test_set)

#add activity labels to test set 
test_set3 <- merge(activity_labels, test_set2, by.x = 'id', by.y = 'test_labels')

#delete uninformative id variable
test_set4 <- select(test_set3, -id)

#add information on subjects 
test_set5 <- cbind(test_subjects, test_set4)

#delete temporary datasets and relabel dataset 
test_set <- test_set5
rm(test_set2, test_set3, test_set4, test_set5)
rm(activity_labels, features, test_labels, training_labels, test_subjects, training_subjects)

# JOIN TO FULL DATASET ####
merged_dataset <- rbind(test_set, training_set)
rm(test_set, training_set)

write.table(merged_dataset, 'merged_dataset.txt', row.names = F) 

########################################################################################
########################################################################################

# EXTRACT MEAN AND STANDARD DEVIATION OBSERVATIONS ####
# Create index that indicates the variables with 'mean' and 'std'
index <- as.logical(grepl('mean', names(merged_dataset)) + grepl('std', names(merged_dataset)))

final_dataset <- cbind(merged_dataset[, c(1,2)], merged_dataset[, index])

write.table(final_dataset, 'final_dataset.txt', row.names = F) 

##########################################################################################
########################################################################################

# MEAN DATASET FOR EACH SUBJECT AND ACTIVITY ####
avg_dataset <- final_dataset               %>%
  group_by(subject_ident, activity_names)  %>%
  summarize_all(list(mean))               


write.table(avg_dataset, "avg_dataset.txt", row.names = F)














