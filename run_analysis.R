# This R script reads data downloaded from the UCI Machine Learning Repository
# containing data on human activity recognition using smartphones.  Specifically,
# the 30 volunteers performed 6 activities and the data from the accelerometer
# and gyroscope were collected.  See following webpage for details:
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# The script merges the test and training data and creates two tidy data sets.
# The first data set removes all data except variables measuring mean and 
# standard deviation.  The second data set gives the average of each variable in 
# the first data set by activity and subject.

# load some libraries
library("dplyr")
library("stringr")

# read in the features - these are the column variabls
# need to clean out the () and - to make variable names cleaner
features <- read.table("./features.txt",as.is=TRUE,col.names = c("featureNum","feature"))
vars <- gsub("\\(\\)","",features$feature)
vars <- gsub("-", ".",vars)

# read the training data in
Xtrain <- read.table("./train/X_train.txt",col.names = vars)
ytrain <- read.table("./train/y_train.txt",col.names = c("activityNum"))
subtrain <- read.table("./train/subject_train.txt",col.names = c("subject"),colClasses = "character")
subtrain$subject <- str_pad(subtrain$subject,width=2, side="left", pad="0")

# add the subtrain, Xtrain, and ytrain to the train dataframe
train <- cbind(subtrain,Xtrain,ytrain)

# read the test data in
Xtest <- read.table("./test/X_test.txt",col.names = vars)
ytest <- read.table("./test/y_test.txt",col.names = c("activityNum"))
subtest <- read.table("./test/subject_test.txt",col.names = c("subject"),colClasses = "factor")
subtest$subject <- str_pad(subtest$subject,width=2, side="left", pad="0")

# add the subtest, Xtest, and ytest to the test dataframe
test <- cbind(subtest,Xtest,ytest)

# now join the train and test dataframes by adding  all the rows of
# train and test into a dataframe called df
df <- rbind(train,test)

# read in the activity list and add a column activity to df based on activityNum
activity_table <- read.table("./activity_labels.txt",as.is=TRUE,col.names = c("activityNum","activityName"))
activity <- factor(df$activityNum,labels = activity_table$activityName)
df <- cbind(df,activity)

# Subset the dataframe to only get the mean and std for each measurement
# use a regular expression to get a vector of the correct variable names
# and then select with the varnames
varnames <- grep("mean($|[^A-Z])|std",names(df),value=TRUE)
varnames <- c("subject",varnames,"activity")
df1 <- select(df, one_of(varnames))

# write the dataframe to file
write.table(df1,"dataframe.txt",row.name=FALSE)

#create the second dataframe with average for each variable by subject and by activity
df2 <- df1 %>% group_by(activity,subject) %>% summarise_each(funs(mean))

