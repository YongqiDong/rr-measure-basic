# HyperETA

These are the program of the paper ***HyperETA: a Non-Deep-Learning Method for Estimated Time of Arrival***. 

Estimated Time of Arrival (ETA) that predicts a given GPS trajectory’s travel time has been widely used in route planning. 
We present a new machine learning algorithm, called HyperETA, for ETA prediction.
HyperETA is based on the Hypercube clustering method to measure the similarity among trajectories.
This program uses Cheng-Du taxi trajectories as a benchmark.


# Usage:

Clicking "Reproducible Run" and the Output will show MAPE and RMSE and MAE of HyperETA and HyperETA_noDTW.


# Data

## train

Raw trajectories for train.

## train_quicktest

small raw train data for quick test.

## testRemoveBeginLast

Raw trajectories for test.
Already remove staying points in the beginning and the end of a trajectory.

## testRemoveBeginLast_5
Raw trajectories for test. Lengths of trajectories are small then 5 km.


## testRemoveBeginLast_5_10
Raw trajectories for test. Lengths of trajectories are between 5 km to 10km.

## testRemoveBeginLast_10_15
Raw trajectories for test. Lengths of trajectories are between 10 km to 15km.

## testRemoveBeginLast_15_20
Raw trajectories for test. Lengths of trajectories are between 15 km to 20km.

## testRemoveBeginLast_20_25
Raw trajectories for test. Lengths of trajectories are between 20 km to 25km.

## testRemoveBeginLast_25_30
Raw trajectories for test. Lengths of trajectories are between 25 km to 30km.

## test_quicktest

small raw test data for quick test.

## trainTrajModel.pickle

The trajectories model, includes 3 tables
* Hypercube series table : Preprocessed trajectories.
* Original trajectories table: Original GPS data.
* Mapping table : It map hypercubes to original trajectories.

## testPreprocessed.pickle

Test data, includes 3 tables
* Hypercube series table : Preprocessed trajectories.
* Original trajectories table: Original GPS data.
* Mapping table : It map hypercubes to original trajectories.

