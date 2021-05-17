# MisclassSurvey
R code and data to accompany manuscript titled "Survey design and analysis considerations when utilizing a misclassified sampling strata" by Aya A. Mitani, Nathaniel D. Mercaldo, Sebastien Haneuse, Jonathan S. Schildcrout

## Files

### Data file

Data used for analysis ("findata") in the manuscript is available either as a [data frame](findata.txt). To use data, 
```
analdata <- read.table("findata.txt", header = TRUE)
```

Data dictionary can be found [here](data_dictionary.md).

### R scripts

- Use [Create misclassification matrix](Create_misclassification_matrix.R) to recreate the misclassification matrix in Table 2.
- Use [Create Table 3](Create_Table_3.R) to recreate the table of patient demographics in the survey.
- Use [Create Table 4 and Table SensAnal](Create_Table_4_and_Table_SensAnal.R) to recreate the table of analysis (Table 4 in manuscript) and Table S1 from Supplementary Material.
- Use [Simulation program](Simulation_program.R) to run the simulation study described in the paper.



