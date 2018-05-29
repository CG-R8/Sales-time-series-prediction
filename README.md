# Time-Series-Forecasting

Description:
- A sales data of 118 days has been provided for 100 key product ids. The goal of this project is to predict next 28 days sales of 100 key product ids. After studying the data, it was found that the data had no seasonality. So I came to the conclusion of using Seasonal Decomposition of Time Series by Loess model and Error Trend Seasonal (ETS) method. 
- This repository contains data files provided, run.R and data_trend_pred.R 
- run.R has step by step function calls for execution
- data_trend_pred.R contains the functions performing reading and writing of data and forecast function.

Requirement:
- Download R studio (https://cran.r-project.org/bin/windows/base/)
- forecast library v7.0

Installation:
- Install the R studio from the link provided.
- Install the forecast library from package installer found in R studio. 

Usage:
- Open the R studio change the working directory where all the R files and data files are present.
- Run the run.R in R studio by including the file using source("run.R") command.
- This will run the run.R file by step by step executing all the commands present in the file.
- Finally it will write the output in output.txt file and store in the working directory.
