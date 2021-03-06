## Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## List of columns names in tidydata
"activity"
"subject"
"time_body_acceleration_mean_x"
"time_body_acceleration_mean_y"
"time_body_acceleration_mean_z"
"time_gravity_acceleration_mean_x"
"time_gravity_acceleration_mean_y"
"time_gravity_acceleration_mean_z"
"time_body_acceleration_jerk_mean_x"
"time_body_acceleration_jerk_mean_y"
"time_body_acceleration_jerk_mean_z"
"time_body_gyroscopic_mean_x"
"time_body_gyroscopic_mean_y"
"time_body_gyroscopic_mean_z"
"time_body_gyroscopic_jerk_mean_x"
"time_body_gyroscopic_jerk_mean_y"
"time_body_gyroscopic_jerk_mean_z"
"time_body_acceleration_magnitude_mean_"
"time_gravity_acceleration_magnitude_mean_"
"time_body_acceleration_jerk_magnitude_mean_"
"time_body_gyroscopic_magnitude_mean_"
"time_body_gyroscopic_jerk_magnitude_mean_"
"frequency_body_acceleration_mean_x"
"frequency_body_acceleration_mean_y"
"frequency_body_acceleration_mean_z"
"frequency_body_acceleration_jerk_mean_x"
"frequency_body_acceleration_jerk_mean_y"
"frequency_body_acceleration_jerk_mean_z"
"frequency_body_gyroscopic_mean_x"
"frequency_body_gyroscopic_mean_y"
"frequency_body_gyroscopic_mean_z"
"frequency_body_acceleration_magnitude_mean_"
"frequency_body_body_acceleration_jerk_magnitude_mean_"
"frequency_body_body_gyroscopic_magnitude_mean_"
"frequency_body_body_gyroscopic_jerk_magnitude_mean_"
"time_body_acceleration_standarddeviation_x"
"time_body_acceleration_standarddeviation_y"
"time_body_acceleration_standarddeviation_z"
"time_gravity_acceleration_standarddeviation_x"
"time_gravity_acceleration_standarddeviation_y"
"time_gravity_acceleration_standarddeviation_z"
"time_body_acceleration_jerk_standarddeviation_x"
"time_body_acceleration_jerk_standarddeviation_y"
"time_body_acceleration_jerk_standarddeviation_z"
"time_body_gyroscopic_standarddeviation_x"
"time_body_gyroscopic_standarddeviation_y"
"time_body_gyroscopic_standarddeviation_z"
"time_body_gyroscopic_jerk_standarddeviation_x"
"time_body_gyroscopic_jerk_standarddeviation_y"
"time_body_gyroscopic_jerk_standarddeviation_z"
"time_body_acceleration_magnitude_standarddeviation_"
"time_gravity_acceleration_magnitude_standarddeviation_"
"time_body_acceleration_jerk_magnitude_standarddeviation_"
"time_body_gyroscopic_magnitude_standarddeviation_"
"time_body_gyroscopic_jerk_magnitude_standarddeviation_"
"frequency_body_acceleration_standarddeviation_x"
"frequency_body_acceleration_standarddeviation_y"
"frequency_body_acceleration_standarddeviation_z"
"frequency_body_acceleration_jerk_standarddeviation_x"
"frequency_body_acceleration_jerk_standarddeviation_y"
"frequency_body_acceleration_jerk_standarddeviation_z"
"frequency_body_gyroscopic_standarddeviation_x"
"frequency_body_gyroscopic_standarddeviation_y"
"frequency_body_gyroscopic_standarddeviation_z"
"frequency_body_acceleration_magnitude_standarddeviation_"
"frequency_body_body_acceleration_jerk_magnitude_standarddeviation_"
"frequency_body_body_gyroscopic_magnitude_standarddeviation_"
"frequency_body_body_gyroscopic_jerk_magnitude_standarddeviation_"
