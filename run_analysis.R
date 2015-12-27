# course project for Getting and Cleaning Data
# This R script labels and loads UCi data into R and cleans, merges, and renames data to become a tidy dataset that can be used for further analysis
require(plyr)

# Referencing all zip files
UCI_files <- "UCI\ HAR\ Dataset"
main_file <- paste(UCI_files, "/features.txt", sep = "")
act_labels_info <- paste(UCI_files, "/activity_labels.txt", sep = "")
x_train_file <- paste(UCI_files, "/train/X_train.txt", sep = "")
y_train_file <- paste(UCI_files, "/train/y_train.txt", sep = "")
subject_train_file <- paste(UCI_files, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(UCI_files, "/test/X_test.txt", sep = "")
y_test_file  <- paste(UCI_files, "/test/y_test.txt", sep = "")
subject_test_file <- paste(UCI_files, "/test/subject_test.txt", sep = "")

# Loading raw data files into R
features <- read.table(main_file, colClasses = c("character"))
activity_labels <- read.table(act_labels_info, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

# create one dataset from the train and test files
# sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels


# Extracts the mean and standard deviation for each measurement.

sensordata_stddev_mean <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]


# Joins descriptive activity names to correcly name the activities


sensordata_stddev_mean <- join(sensordata_stddev_mean, activity_labels, by = "ActivityId", match = "first")
sensordata_stddev_mean <- sensordata_stddev_mean[,-1]


# Appropriately labels the data set with descriptive names.


# Remove parentheses
names(sensordata_stddev_mean) <- gsub('\\(|\\)',"",names(sensordata_stddev_mean), perl = TRUE)
# make names valid 
names(sensordata_stddev_mean) <- make.names(names(sensordata_stddev_mean))
# Make the names readable
names(sensordata_stddev_mean) <- gsub('Acc',"Acceleration",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('GyroJerk',"AngularAcceleration",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('Gyro',"AngularSpeed",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('Mag',"Magnitude",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('^t',"TimeDomain.",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('^f',"FrequencyDomain.",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('\\.mean',".Mean",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('\\.std',".StandardDeviation",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('Freq\\.',"Frequency.",names(sensordata_stddev_mean))
names(sensordata_stddev_mean) <- gsub('Freq$',"Frequency",names(sensordata_stddev_mean))


#  Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


sensor_avg_by_act_sub = ddply(sensordata_stddev_mean, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "sensor_avg_by_act_sub.txt")

