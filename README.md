# UDMTA

## Shiny App for Annual Species Temporal Abundance Models 

Users can use this app to fit temporal models to identify the significant explanatory variables and the trends of the species during a span of years. The app would use most common distributions for species such as "Poisson" and "Negative Binomial" as well as the zero inflation distributions. First the user needs to upload the data csv file with or without explanatory variables (numeric/factor) into the application and then user must select whether the numeric data should be normalized or not. Categorical predictor variables should not be included as numeric or integer variables. This is conducted using Integrated Nested Laplace Approximation (INLA). This app has the ability to fit single species temporal models for a set of species at the same time. 

INLA is popular as an approximation tool for fitting Bayesian models. INLA is an alternative robust method for the traditional Markov Chain Monte Carlo (MCMC) based Bayesian analyses (Paul et al. 2010). The key advantages of INLA are the ease with which complex models can be created and modified, without the need to write complex code, and the speed at which inference can be done even for spatial problems with hundreds of thousands of observations (Sarul, 2015).

Users can use UDMTA for single species data using R-INLA. To know if the rate at which abundance is changing over time differs according to a relevant predictor variable, we have included the facility to add interaction terms between any two predictor variables in our regression models. 

### Input File

The data file should include only:
  
  1. Species - Different species
  2. Year - Detected Year
  3. Count - Species count
with or without predictor variables (numeric/factor). The above names are case sensitive. A sample format of the data can be found in https://github.com/uwijewardhana/UDMTA.

## Installation Instructions

To build this Shiny app, we need to clone the GitHub repository from https://github.com/uwijewardhana/UDMTA and save it in our computer. This folder contains a sample Data.CSV file, the vignette and app.R file. Then, we can launch the app by clicking the Run App button at the top of the RStudio editor or by executing runApp("appdir_path")where appdir_path is the path of the directory that contains the app.R file. For this we need to install R and RStudio in our computer. User can download and install package R-INLA by  http://www.r-inla.org/download. 

### Reference

- Paul, M., Riebler, A., Bachmann, L. M., Rue, H., and Held, L. (2010). Bayesian bivariate meta-analysis of diagn ostic test studies using integrated nested Laplace approximations. Statistics in Medicine, 29: 1325-1339.
- Sarul, L. (2015). An Application Of Claim Frequency Data Using Zero Inflated And Hurdle Models In General I nsurance. Pressacademia, 4(4), pp.732-732.

```r
Udani Wijewardhana (udaniwijewardhana@gmail.com)
```