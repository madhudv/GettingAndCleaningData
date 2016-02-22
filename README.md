## Content
1. README.md (this file)
2. run_analysis.R (R code for the project)
3. codebook.md (codebook for tidy data generated by run_analysis.R)

## run_analysis.R
### Goal
Create one R script called run_analysis.R that does the following.
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#### Step 1
1. Read data from the files provided
2. Merge test and train data together
3. Rename the colume names (variables) to readable names provided in features.txt
4. Merge all subjects, activities and data together 

#### Step 2
1. Parse out mean and std deviation from the features read
2. Pull the corresponding data into a new data frame

#### Step 3
1. Read and label activities

#### Step 4
1. Make the column names tidy and readable

#### Step 5
1. Create a new data set as per the problem description