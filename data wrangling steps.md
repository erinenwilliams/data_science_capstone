# Steps to tidying data for capstone project

I will be working with three datasets:

1. 1610-1705 Demographics
	+ a collection of self-reported information including race, gender, age, and previous education.
2. All students data
	+ pulled from our enroll app, this includes name, starting cohort, a link to the student’s application, and number of cohorts completed
3. Drop out analysis - need to update and merge with recent data
	+ a subset of all students with added demographic and anecdotal information on students who dropped out of the program


Steps: 

1. Gather all datasets in excel. 
2. Tidy datasets. I cleaned up column names to be short and concise. 
3. Consolidate data sets using dplyr in R.

```{r} dropouts <- left_join(dropouts_May_2017, Drop_out_analysis2, by = "id")
demo_drop <- left_join(dropouts, demographics, by = "id")```
