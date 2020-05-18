# RiskBenefit_Neuroethics
UPDATE the read me!

Here you will find the R project for preparing the behavioral task data and survey data for various modeling, statistical analysis, and visualizations.

Included are:

r_docs: contains the functions, tidy scripts, RMarkdown, Stats analysis used throughout the project

data: contains the raw datasets prior to any filtering or measure score calculations. This will also have the cleaned versions of the data moving forward. 

Images: contains the visualizations for subjects from a particular pilot cohort. To be used as reference for interpreting the findings. 


This data frame (Neuroethics_Judgement) contains the following columns:

- `user_id` --  Unique subject identifier
- `risk` -- the number indicating how much risk is associated with the treatment for that trial
- `gain` -- the number indicating how likely you are to successfully treat the dysfunction using the treatment with that trial
- `no effect` -- the number indicating how likely it is that your treatment will cause no effect for treating the dysfunction
