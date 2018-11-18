# The Machine learning project at NYCDSA

## About

The project uses Kaggle data on Ames (Iowa) housing prices and various machine learning techniques to build a predictive model.

## Data
Besides original Kaggle data we add several more features:

- Dow Jones US Real Estate Index
- Corn prices
- Labor force in Ames
- Unemployment rate in Ames
- Fannie Mae mortgage rates. 

All these variables are treated as lagged variables compareg to the date of house sale. 

## Folder structure
\ IndividualContributions - folders with files submitted by the team members
\ tmp - temp folder to store and move data between notebooks
\ data - datasets in .csv format
\ output - output graphs for the presentation


## Models

We use 4 linear models, a Random Forest Regressor and an XGB model to make predictions. 

Finally, we stack the models using the inverse of their error rate on a test set as weights. 

The resulting RMSLE is 0.119


