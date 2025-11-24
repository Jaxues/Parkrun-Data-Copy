# Purpose
This is a personal copy of the work I did for my data wrangling course at UC. 
I anonymized all other names and most emphasis is what I contributed to the project.

# Group B - Lab 3 Assignement



# Purpose 

BAA Ltd aims to provide Puma with a repository that investigates how Puma can optimise their marketing strategy to reach future customers for Parkrun events in Christchurch. 
The repository contains documents that inform both technical and non-technical audiences and provide statistical insight for optimising marketing strategies. In summary, the repository returns 11 weeks of clean Parkrun data from sites across Christchurch, including variables for analysis by both BAA Ltd and the Data Scientists at Parkrun. 
The repository also contains a report that answers Pumas marketing questions and provides advice around effective marketing strategies.
The repository also contains a technical report that describes the entire pipeline for the purposes of reproducibility by Puma’s Data Scientists. 
Overall, these components aim to provide the Puma marketing and Data Science team with advice for how to optimise their marketing strategies for new customers at Christchurch Parkrun events. 

# Datasets

dataset: A folder of 11 weeks of Parkrun data across sites in Christchurch, provided by Puma. These 11 weeks are separated into 74 files that include variables: 
position, name, number of parkruns, gender, club(s), age category, age range, age grade, PB time and time in the given race, separated by commas and/or lines. 
Each file is named by the Parkrun location and the Parkrun number. 

parkrun_all.csv: This dataset contains all observable variables extracted from the raw dataset (position, name, number of parkruns, gender, club(s), age category, age range, age grade, PB time and time in the given race) for analysis purposes and to form the final dataset for Puma Data Scientists.
Note that additional variables, such as median age, time in seconds and runner IDs, are added for analysis by BAA Ltd. The final dataframe can be accessed locally as ‘parkrun_all’, which contains the variables mentioned above. 

final_parkrun_dataset.zip: This dataset includes a tidy and imputed version of the 11 weeks of Parkrun data across Christchurch sites, in a single, zipped CSV file. This data set is formed from the parkrun_all dataframe.  
The variables included are id, week, parkrun, sex, age, club and time, as per Puma’s Statement of Work. 

# Scripts 

00_cleaned_dataset.R: This script cleans the raw Parkrun dataset (dataset) into a CSV file (parkrun_all) with all observable variables from the raw dataset. This includes separating variables into columns, renaming variables, calculating new variables and anonymising Parkrunners. 
Note: the number of runs is used instead of run clubs (i.e. "Member of the 250 Club") to prevent redundancy.
The final dataframe is used as input for 01_tidy_data.R.    

01_tidy_data.R: This script creates a tidy and MICE-imputed version of the cleaned dataset, and returns it as a zip file. The file contains the variables listed by Puma for use by their Data Scientists. 
The final dataset will be delivered to Puma and is included in the repository.

02_analysis_code.R: This script contains the code for answering the key questions Puma has given for their marketing strategy. This includes calculations and visualisations from the Parkrun dataset with the aim of providing the Puma marketing team with statistical insight in order to reach new customers in Christchurch.  
The code will be used in the key_questions_report and requires input from the tidy data CSV file. 

key_questions_report.Rmd This report answers Puma's 6 key questions with graphs from 02_analysis code and additional interpretations, as well as providing insights necessary for Puma’s marketing team. 
This file requires input from 02_analysis_code. 

methods_document.Rmd: This document describes the project's entire pipeline and any corrections, conversions, or clarifications made around the data. 
The document considers how the project could be improved for future development and provides information on what BAA Ltd employees contributed to each part of the repository. 

# Folders 

dataset - contains all the data files provided by Puma 

intermediate - contains latitude and longitude values for question 6
