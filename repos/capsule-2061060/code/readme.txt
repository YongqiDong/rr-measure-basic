In this Delphi study, we surveyed the UK psychology community to understand what research methods skills they deem essential for undergraduates to learn. Based on this study we submitted recommendations to the BPS to update their 'BPS Standards for the accreditation of undergraduate, conversion and integrated Masters programmes in psychology'.

A study protocol was pre-registered on the Open Science Framework (https://osf.io/5h7bu)

The deposit includes the following folders and files:

#####################
#### environment ####
#####################

Dockeerfile contain the environment necessary to execute the manuscript.Rmd file

################
##### code #####
################

flowchart.PNG is the flowchat figure

items_cut.csv contains short-form names for the Delphi items

manuscript.Rmd contained the analysis script and outputs the results section of our study in the file manuscript.pdf

################
##### data #####
################

1.item_ratings.csv contains all raw data for the item ratings across both rounds.

2.feedback_on_questions.csv contains the raw feedback participants left regarding specific items during Round 1. This info was extracted from datafile 1 and entered into a separate spreadsheet for easier viewing. The option to provide feedback on specific questions was not available in Round 2.

3.rating_change_reasons.csv contains participants’ reasons for changing a rating from one category to another between Round 1 and Round 2 (e.g., from 'important but not essential' to 'essential').

4.participant_characteristics.csv contains the raw data regarding participant characteristics, as well as general comments that participants wrote. 

5.general_comments.csv contains only the comments from the file 4.participant_characteristics.csv. We separated this data into its own file for easier viewing.

6.additional_outcomes_suggested.csv contains the raw data for the items that participants felt the survey did not include, but that they would deem important.

Data from files 1, 2, and 3 can be linked through the variable study_id/UserID. Data from 5 is contained within file 4. Based on the format in which the data is output, it is not possible to link data among files 1/2/3, 4/5, and 6.

7.thematic_analysis.xlsx shows the process of the thematic analysis of the open-ended responses.

8.additional_qual_analyses.xlsx provides additional qualitative analyses of the open-ended responses.

Data dictionaries are provided as .csv files and outline the meaning of each variable name in the datasheets.

################
### results ####
################

This folder contains 4 sub-folders: raw, completed, final, and sensitivity. The first 3 folders follow the same structure and contain summary data from all participants who at least started Round 1 (all; n = 170), only participants who completed Round 1 and/or completed Round 2 (completed; n = 139 Round 1; n = 112 Round 2), and only participants who completed both Round 1 and Round 2 (final; n = 103). 

Each sub-folder contains...
datasheets that present summary data of all stakeholders combined (1.combined_datasheet.csv), for all stakeholders excluding students (2.noStudents_datasheet.csv), and for each stakeholder group individually (files 4-6). 
0.main_table.csv, which summarises data (i) across all stakeholders, (ii) for the two stakeholder groups with >12 participants (instructors and academics), (iii) on whether our preregistered definition of consensus was reached, and (iv) regarding differences between Round 1 and Round 2 in terms of the percentage of ratings that were essential and whether consensus was reached.

Sub-folders 'Round 1' and 'Round 2'. These contain spreadsheets that were combined to create the spreadsheets described in the previous bullet point. They also contain figures depicting the ratings for each item across stakeholder groups. The figures from the folder completed/Round1 were the figures we presented to participants during Round 2 of the Delphi study.

The folder 'sensitivity' contains comparisons for three separate sensitivity analyses. 

############################
#### additional details ####
############################

All data and data dictionary are in CSV format and can be opened using a number of software packages.

The analysis code was written in RMarkdown. The the dependencies necessary to run the code are provided in the Dockerfile. The easiest way to re-run the code is by clicking "Reproducible Run" in this manuscript's reproducible container on Code Ocean (https://doi.org/10.24433/CO.0483372.v1). Running the code will output the results section of the manuscript in pdf format and all summary datasheets in csv format. Alternatively, R can be downloaded from www.r-project.org/