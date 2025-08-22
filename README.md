# Beat it model

![](https://upload.wikimedia.org/wikipedia/pt/8/82/Beat_It.jpg)

## Team and Contributors

Leo Bastos (Fiocruz) & Lais Freitas (Fiocruz)

## Repository Structure

The repo contains all code and data necessary to forecast dengue cases using a baseline Bayesian model by macrorregion and aggregated afterwards to UF.

The forecasts are also available.

## Libraries and Dependencies

The model is implemented in R using the tidyverse to data preparation and INLA for inference.

## Data and Variables

This model depends on case counts only.

### How was the data pre-processed?

The data is aggregated by macrorregion, and for each macrorregion forecasts were performed. We considered cases from 2015 onwards, ignoring data previous to 2015 because of dengue expansion and introduction to other arboviroses.

### How were the variables selected? Please point to the relevant part of the code.

There was no variable selection.

## Model Training

It is a negative binomial with year indpendent Gaussian random effects and a cyclic second order random walk weekly random effects. The posterior distribution for all parameters were estimated for the training dataset and, the numbe rof cases for the target dataset were estimated based on the posterior predictive distribution. Samples from this distribution were taken by macroregion and for the three target sets.

The intervals were computed based on quantiles from samples of the posterior predictive distribution using a Monte Carlo approach.

## Reference

Freitas et al. (2025) A statistical model for forecasting probabilistic epidemic bands for dengue cases in Brazil, Infectious disease modelling ([link](https://www.sciencedirect.com/science/article/pii/S2468042725000739)) ([medrxiv](https://www.medrxiv.org/content/10.1101/2025.06.12.25329525v1))
