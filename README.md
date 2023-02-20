# SMSAR_model
The SMSAR model is simulated using the R project for statistical computing, R is a language for statistical computing and graphics. R is a free software environment that compiles and runs on a wide variety of UNIX platforms, Windows and MacOS.

A detailed presentation of the model is presented in the reseach paper: Conditional Seasonal Markov-Switching Autoregressive Model to Simulate Extreme Events: Application to River Flow by Bassel Habeeb, Emilio Bastidas-Arteaga, Mauricio Sánchez-Silva, and You Dong

The input is the river flow values of the Thames, the website to download the dataset (file: UKdatabase.xlsx) was downloaded from: https://nrfa.ceh.ac.uk/

The SMSAR model code is provided in the article. A summary of main stepts is provided as follows:

1-	Define the monthly time-series dataset
2-	Stationary check
3-	Choosing a model to fit the input time-series dataset
4-	Simulate the MSAR model
5-	Compute the conditional hidden seasonal Markov component (State phase, Condition phase)
6-	Compute the HSM values
7-	Simulate SMSAR model (step 4 + step 6)
8-	Calculate the extreme events limit
9-	Forecast the time-series using Recurrent NN

# Files
- SMSAR model.R (Commented code in R)
- UKdatabase.xlsx (Database to run the Code)
