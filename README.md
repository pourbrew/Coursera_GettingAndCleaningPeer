# Coursera_GettingAndCleaningPeer Assignment

Instructions

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
Review criterialess 
The submitted data set is tidy.
The Github repo contains the required scripts.
GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
The README that explains the analysis files is clear and understandable.
The work submitted for this project is the work of the student who submitted it.
Getting and Cleaning Data Course Projectless 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

**The codebook is at the end of this document.**

Setup
-------------

Load packages.

```{r}
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
```

Set path.

```{r}
path <- getwd()
path
```


Get the data
------------

Download the file. Put it in the `Data` folder.

```{r, eval=FALSE}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f))
```

Unzip the file. 

The archive put the files in a folder named `UCIHARDataset`. Set this folder as the input path. List the files here.

```{r}
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)
```

Read the files
--------------

Read the subject files.

```{r}
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))
```

Read the activity files.

```{r}
dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(pathIn, "test" , "Y_test.txt" ))
```

Read the data files. I used a helper function, read the file with `read.table` instead, then convert the resulting data frame to a data table. Return the data table.

```{r fileToDataTable}
fileToDataTable <- function (f) {
	df <- read.table(f)
	dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))
```


Merge the training and the test sets
------------------------------------

Concatenate the data tables.

```{r}
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)
```

Merge columns.

```{r}
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
```

Set key.

```{r}
setkey(dt, subject, activityNum)
```


Extract only the mean and standard deviation
--------------------------------------------

Read the `features.txt` file. This tells which variables in `dt` are measurements for the mean and standard deviation.

```{r}
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
```

Subset only measurements for the mean and standard deviation.

```{r}
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
```

Convert the column numbers to a vector of variable names matching columns in `dt`.

```{r}
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode
```

Subset these variables using variable names.

```{r}
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]
```


Use descriptive activity names
------------------------------

Read `activity_labels.txt` file. This will be used to add descriptive names to the activities.

```{r}
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
```


Label with descriptive activity names
-----------------------------------------------------------------

Merge activity labels.

```{r}
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
```

Add `activityName` as a key.

```{r}
setkey(dt, subject, activityNum, activityName)
```

Melt the data table to reshape it from a short and wide format to a tall and narrow format.

```{r}
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
```

Merge activity name.

```{r}
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
```

Create a new variable, `activity` that is equivalent to `activityName` as a factor class.
Create a new variable, `feature` that is equivalent to `featureName` as a factor class.

```{r}
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)
```

Seperate features from `featureName` using the helper function `grepthis`.

```{r grepthis}
grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
```

Check to make sure all possible combinations of `feature` are accounted for by all possible combinations of the factor class variables.

```{r}
r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2
```

TRUE, I accounted for all possible combinations. `feature` is now redundant.



Create a tidy data set
----------------------

Create a data set with the average of each variable for each activity and each subject.

```{r}
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
```

Create tidy.txt.
----------------
```{r}
f <- file.path(path, "tidy.txt")
write.table(dtTidy, f, quote=FALSE, sep="\t", row.names=FALSE)
```

Make codebook.
--------------

```{r}
memisc::codebook(dtTidy)
```


subject

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer

          Min.:   1.000
       1st Qu.:   8.000
        Median:  15.500
          Mean:  15.500
       3rd Qu.:  23.000
          Max.:  30.000

============================================================================================================================

   activity

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 6 levels

        Values and labels    N    Percent 
                                          
   1 'LAYING'             1980   16.7     
   2 'SITTING'            1980   16.7     
   3 'STANDING'           1980   16.7     
   4 'WALKING'            1980   16.7     
   5 'WALKING_DOWNSTAIRS' 1980   16.7     
   6 'WALKING_UPSTAIRS'   1980   16.7     

============================================================================================================================

   featDomain

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 2 levels

   Values and labels    N    Percent 
                                     
            1 'Time' 7200   60.6     
            2 'Freq' 4680   39.4     

============================================================================================================================

   featAcceleration

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 3 levels

   Values and labels    N    Percent 
                                     
         1 'NA'      4680   39.4     
         2 'Body'    5760   48.5     
         3 'Gravity' 1440   12.1     

============================================================================================================================

   featInstrument

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 2 levels

   Values and labels    N    Percent 
                                     
   1 'Accelerometer' 7200   60.6     
   2 'Gyroscope'     4680   39.4     

============================================================================================================================

   featJerk

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 2 levels

   Values and labels    N    Percent 
                                     
            1 'NA'   7200   60.6     
            2 'Jerk' 4680   39.4     

============================================================================================================================

   featMagnitude

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 2 levels

   Values and labels    N    Percent 
                                     
       1 'NA'        8640   72.7     
       2 'Magnitude' 3240   27.3     

============================================================================================================================

   featVariable

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 2 levels

   Values and labels    N    Percent 
                                     
            1 'Mean' 5940   50.0     
            2 'SD'   5940   50.0     

============================================================================================================================

   featAxis

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 4 levels

   Values and labels    N    Percent 
                                     
              1 'NA' 3240   27.3     
              2 'X'  2880   24.2     
              3 'Y'  2880   24.2     
              4 'Z'  2880   24.2     

============================================================================================================================

   count

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: integer

          Min.:  36.000
       1st Qu.:  49.000
        Median:  54.500
          Mean:  57.220
       3rd Qu.:  63.250
          Max.:  95.000

============================================================================================================================

   average

----------------------------------------------------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.998
       1st Qu.:  -0.962
        Median:  -0.470
          Mean:  -0.484
       3rd Qu.:  -0.078
          Max.:   0.975
