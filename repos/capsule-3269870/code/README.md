# Neural Network for Predicting Stroke Team Performance (Door to Needle Time)

Implementation of a shallow neural network using TensorFlow Low Level APIs


## Files
1. nn.py : Full neural network implementation
2. predict.py : uses the trained network for prediction on test data
3. predict.js : A vanilla JavaScript implementation of the prediction, using trained weights, which gives out put as 0,1 given the input parameters in a 4 x 1 array

~~~~
const predict = require('./predict');
var x1 = 12;  //Hour of day
var x2 = 70;  //Age
var x3 = 105; //Duration
var x4 = 13;  //Severity

var X = [[x1],[x2],[x3],[x4]];

var pred = predict(X); //pred is 1 or 0 as per the model calculates

~~~~

## Input data format
The data is read from 'train_data.csv' and 'test_data.csv' which should have the following structure, for each row:

(Hour of day),(Age),(Duration),(Severity - NIH Stroke Scale),(Door to Needle time >45 min (1) or less (0))
e.g,
12,70,105,13,1
