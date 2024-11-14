# A University Admission Prediction System using Stacked Ensemble Learning

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Usage](#Usage-Instructions)
* [Authors](#Authors)

## General info
<p>For an aspiring graduate student, shortlisting the universities to apply to is a difficult problem. Since an application is extremely dynamic, students often tend to wonder if their profile matches the requirement of a certain university. Moreover, the cost of applying to a university is extremely high making it critical that students shortlist universities based on their profile. A university admission prediction system is quite useful for students to determine their chances of acceptance to a specific university. The system could make use of data related to previous applicants to various universities and their admit or reject status. </p>
<p>Earlier models of such prediction systems suffer from several drawbacks such as not considering important parameters like GRE (Graduate Record Exam) scores or research experience. Further, the accuracy reported by earlier models is also not sufficiently high. </p>
<p>We create a stacked ensemble model that predicts the chances of admit of a student to a particular university has been proposed. The proposed model takes into consideration various factors related to the student including their research experience, industry experience etc. </p>


## Technologies
Project is created with:
* Python version: 3.7.3
* tensorflow-gpu version: 2.4.1
* keras: 2.4.3
	
## Usage
To run the project follow these steps:

```
python3 sub_models.py
```
which will save the individual models in the /ensemble folder. 
Then run,

```
python3 ensemble.py
```
to get the necessary outputs. <br/>

In sub_models.py,

```
fit_model() -> specify the Neural Network for the sub models
n_members -> specifies number of sub models

``` 
<br/>

In ensemble.py,

```
load_models -> load the sub models
define_stacked_model -> create the ensemble NN with a Level 1 model
n_members -> specifies number of sub models

```

## Authors

Contact us in case of any queries

* Sashank Sridhar - sashank.ssridhar@gmail.com
* Siddartha Mootha - siddartha.mootha20@gmail.com
* Santosh Kolagati - ksantosh1399@gmail.com 
