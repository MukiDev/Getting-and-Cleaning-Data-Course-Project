******************************************************************
# Human Activity Recognition (HAR) Using Smartphones Dataset
## Means of Standard Deviations and of Mean Values, single data set ( training and test data ).
******************************************************************
Data comes from experiments carried out with a group of 30 volunteers. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS,
SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial
angular velocity signals were captured. The obtained dataset has been randomly partitioned into two sets, 
where 70% of the volunteers was selected for generating the training data and 30% the test data. See 'features_info.txt' for more details.

The objective of the present data tyding project is to join back together the training and test data sets into a single data set.
Only the columns related to mean values or standard deviations calculated over the measurements should be considered.
The final data set must be grouped by activity name and experiment subject (participant) and mean values must be calculated for each of these groups.


## Input files:
The following are the files provided with the original HAR dataset ( https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip )

*******************************************************************************************************************************
## VERY IMPORTANT 
In order to execute successfully the included script ( run_analysis.R ), the following files MUST BE PREVIOUSLY EXTRACTED locally from the
aforementioned zip file into a local UCI HAR DATASET folder and its TRAIN AND TEST subfolders.
Furthermore, the script assumes the WORKING DIRECTORY in R has been set up to point to this UCI HAR DATASET folder.
*******************************************************************************************************************************

- 'features.txt': List of all features (variable names), which are the same for the training and test sets.
- 'activity_labels.txt': Lists each of the activity labels ( activity codes from 1 to 6 )with their respective activity name (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS,
SITTING, STANDING, LAYING). This list is the same for the training and test sets.
- 'train/subject_train.txt': Subject codes ( integers from 1 to 30 ) for each measurement of the training set. Each subject code identifies a particular individual participating in the experiment.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Activity labels (activity codes) for each measurement of the training set.
- 'test/subject_test.txt': Subject codes ( integers from 1 to 30 ) for each measurement of the test set. Each subject code identifies a particular individual participating in the experiment.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Activity labels (activity codes) for each measurement of the test set.

Optional files ( for information only ):
- 'README.txt' : Additional detailed information about the original Human Activity Recognition (HAR) dataset.
- 'features_info.txt': Shows information about the variables used on the feature vector.

## Output file: 
The result of this data tyding project is a single txt file ( UCI_HAR_Full_Dataset_Means_Of_StdDeviations_and_MeanValues_By_Activity_and_Subject.txt),
which will be exported to the R working directory at the last step of the run_analysis.R script.
This txt file will contain the requested single data set, with 130 rows and 68 columns.

## Script execution:
The successful execution of the included script ( run_analysis.R ) relies only on 3 pre-requisites :
### 1.-Extracting the necessary input files into a local UCI HAR DATASET folder as described above.
### 2.-Setting up the R work directory to such UCI HAR DATASET folder.
### 3.-Installing the dplyr package.
If any of these 3 conditions is not satisfied before running the script, an error will occur and the process will be stopped. 
After remediating the missing condition(s), the script can be re-run any number of times. 
Each run will override the objects in the work environment and the output file from previous executions.


