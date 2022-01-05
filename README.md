# Putting-a-Price-on-Popularity-Sample-Replication-Economic-Inquiry-2022
This repository contains contents for replicating certain results from the paper “Putting a Price on Popularity: Evidence from Superstars in the National Basketball Association.” In particular, it provides replication for one of the superstar players analyzed in the paper, Stephen Curry. This replication procedure applies to all remaining players, for which data is not included. This data has been pre-processed from its original form to include some additional variables needed to conduct the analysis, but the raw components of the data are included from their original state. All analysis has been done using RStudio, and necessary packages and libraries needed are included in the respective files attached and described below.

Folder Contents
1.	Filename: StephenCurryData.RData
2.	Filename: Aggregate_DID_StephenCurry.R
3.	Filename: Aggregate_DID_HomeVsAway_StephenCurry.R
4.	Filename: EventStudy_StephenCurry.R

File 1: StephenCurryData.RData
This is the data file needed to perform the replication analysis for Stephen Curry.

File 2: Aggregate_DID_StephenCurry.R
This file performs the aggregate difference-in-differences analysis found in Figure 5 of the main text. It provides replication code for the “percent change” aggregate DID analysis for Stephen Curry.

File 3: Aggregate_DID_HomeVsAway_StephenCurry.R
This file performs the aggregate difference-in-differences analysis separating between home vs. away effect found in Figure 17 of Appendix B. It provides replication code for the “percent change” home vs. away aggregate DID analysis for Stephen Curry (the “$” change effects are found in Figure 8 in the main text). 

File 4: EventStudy_StephenCurry.R
This file performs the event study analysis found in Figures 9 and 10 of the main text, which estimates equation (6) from the main text. It provides replication code for Stephen Curry.
