# MisclassSampleStrata
R code and data to accompany manuscript titled "Survey design and analysis considerations when utilizing a misclassified sampling strata"

/***
\begin{table}[ht]
\centering
\begin{tabular}{llll}
  \hline
Variable name & Variable descriptin & Variable type & Variable values \\ 
  \hline
patient.id & Patient ID & Numeric & 1 to 604 \\ 
  svywt & Inverse survey probability weight & Numeric &  \\ 
  s.trust & Trust in healthcare system (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  s.race.eth.5 & Race/ethnicity (from survey response) & Categorical & 0 = White, 1 = Black, 2 = Asian, 3 = Other, 4 = Hispanic \\ 
  s.poverty & Poverty (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  s.agelt35 & Age less than or equal to 35 (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  s.female & Female gender (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  s.edu\_L & Less than high school diploma (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  s.edu\_M & High school diploma to some college (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  s.edu\_H & College degree or higher (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  s.rural & Rural living (from survey response) & Categorical & 0 = No, 1 = Yes \\ 
  e.race.eth.5 & Race/ethnicity (from EHR) & Categorical & 0 = White, 1 = Black, 2 = Asian, 3 = Other, 4 = Hispanic \\ 
  e.agelt35 & Age less than or equal to 35 (from EHR) & Categorical & 0 = No, 1 = Yes \\ 
  e.female & Female gender (from EHR) & Categorical & 0 = No, 1 = Yes \\ 
  e.edugrp & Level of education (from EHR) & Categorical & 0 = College or higher, 1 = High school to some college, 2 = Less than high school \\ 
  e.rural & Rural living (from EHR) & Categorical & 0 = No, 1 = Yes \\ 
   \hline
\end{tabular}
\end{table}
***/
