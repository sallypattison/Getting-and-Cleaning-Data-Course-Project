## tidy data for training cohort to include the column "subject" which records subject ID number ##
## and column "activitylabels" recording activity ID number for which each set of measurements was taken ##

train1 <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_labels <- read.table("./UCI HAR Dataset/train/Y_train.txt")
train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train2 <- cbind(train_subjects,train_labels, train1) 

## tidy data for test cohort to include the column "subject" which records subject ID number ##
## and column "activitylabels" recording activity ID number for which each set of measurements was taken ##

test1 <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/Y_test.txt")
test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test2 <- cbind(test_subjects, test_labels, test1) 

## combine training and test data sets and check for repeated rows ##

comb <- rbind(train2, test2)
comb <- unique(comb)

## all column names to complete data set and change replicate names by adding x, y and z to distinguish them ##

features <- read.table("./UCI HAR Dataset/features.txt")
features2 <- as.character(t(features[2]))
cols <- c("subject", "activitylabels", features2)
cols[305:318] <- gsub("fBody", "xfBody", cols[305:318], fixed = FALSE)
cols[319:332] <- gsub("fBody", "yfBody", cols[319:332], fixed = FALSE)
cols[333:346] <- gsub("fBody", "zfBody", cols[333:346], fixed = FALSE)
cols[384:397] <- gsub("fBody", "xfBody", cols[384:397], fixed = FALSE)
cols[398:411] <- gsub("fBody", "yfBody", cols[398:411], fixed = FALSE)
cols[412:425] <- gsub("fBody", "zfBody", cols[412:425], fixed = FALSE)
cols[463:476] <- gsub("fBody", "xfBody", cols[463:476], fixed = FALSE)
cols[477:490] <- gsub("fBody", "yfBody", cols[477:490], fixed = FALSE)
cols[491:505] <- gsub("fBody", "zfBody", cols[491:505], fixed = FALSE)
colnames(comb) <- tolower(cols)

## separate data for columns containing mean or std into dataframe called mean_std ##

search <- "mean|std"
mean_std <- comb[, grep(search, colnames(comb))]

## convert activity ID numbers in activitylabels column into activity names ##

activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
for(i in comb$activitylabels) {
        comb$activitylabels[comb$activitylabels == 1] <- "walking"
        comb$activitylabels[comb$activitylabels == 2] <- "walking_upstairs"
        comb$activitylabels[comb$activitylabels == 3] <- "walking_downstairs"
        comb$activitylabels[comb$activitylabels == 4] <- "sitting"
        comb$activitylabels[comb$activitylabels == 5] <- "standing"
        comb$activitylabels[comb$activitylabels == 6] <- "laying"      
}

## create dataframe to store results ##

results <- matrix(1, nrow = 180, ncol = 563)
results <- data.frame(results)
colnames(results) <- tolower(cols)

## set up factors in dataframe ##

library(dplyr)
comb <- arrange(comb, subject, activitylabels)
comb$subject <- as.factor(comb$subject)
comb$activitylabels <- as.factor(comb$activitylabels)

## calculate variable averages for all subjects within dataset ##

x <- 1
for(s in 1:30){
        
        ## calculate variable averages for all activities for one subject ##
             
        positions <- c("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")       
        for(i in positions){
                
                ## calculate averages for all variables for one activity for one subject ##
                
                sub <- filter(comb, subject == s & activitylabels == i)
                for(v in 3:563){
                        results[x,v] <- mean(sub[,v])
                        results[x,2] <- i
                        results[x,1] <- s
                        }              
        x <- x+1       
        }
}
