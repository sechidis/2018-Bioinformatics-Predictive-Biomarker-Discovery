# Bioinformatics 2018 - Distinguishing prognostic and predictive biomarkers: An information theoretic approach 
 
Information theoretic predictive biomarker ranking 

**Date:** 02/02/2018

**Paper (under review):** Distinguishing prognostic and predictive biomarkers: An information theoretic approach 
**Authors:** Konstantinos Sechidis, Konstantinos Papangelou, Paul D. Metcalfe, David Svensson, James Weatherall and Gavin Brown

**Platform:** R Version 3.3.1

**Required packages:** MASS, infotheo

**Maintainer:** Konstantinos Sechidis konstantinos.sechidis@manchester.ac.uk

**Description:** Deriving rankings that capture the predictive strength biomarker strength through univariate (INFO) or higher-order (INFO+) methods

**Functions:**

```INFOplus.Output_Categorical.Covariates_Categorical(data,labels,treatment,top_k)$ranking``` 
This function returns the predictive ranking, the input arguments are

**data:** A matrix containing the covariates (biomarkers). The columns capture the different covariates, while the rows the different examples (patients). For this function the covariates are categorical (nominal).

**labels:** A vector that contains the output (target) label for each patient, in this case it takes categorical (nominal) values.

**treatment:** A vector that describes the treatment allocation (i.e. T=0 control group, T=1 experimental treatment).

**top_k:** The number of top-k predictive biomarkers to be returned.

Furthermore we provide functions that can be used for various data types:

```INFOplus.Output_Categorical.Covariates_Continuous```:  The covariates can be either all continuous or mixed (continuous and categorical). To discretise continuous covariates we follow by default Scott's rule.
```INFOplus.Output_Survival.Covariates_Categorical```: For survival (time-to-event) output targets and categorical covariates.
```INFOplus.Output_Survival.Covariates_Categorical```: For survival (time-to-event) output targets and continuous or mixed (continuous and categorical) covariates.
 
Finally, we provide the same functions for deriving the uni-variate INFO ranking.


Example

We provide a source code (```Functions-GenerateData.R```) to generate the synthetic scenarios presented in the paper. The following example shows how to derive the predictive rankings using our code.

```
## Load libraries
library(MASS) # To generate synthetic data by sampling a Multivariate Normal
library(infotheo) # Information theoretic library  
 
## Load sources
source("Functions-GenerateData.R") # Function to generate synthetic data
source("InformationTheory-PredictiveRankings.R") # Functions to derive predictive rankings


###################################
##### Generate synthetic data #####
###################################
model <- 3 ;         # Which model to use (1, 2, 3, 4, 5, 6, 7) - details on the paper
theta_pred <- 1      # Strength of predictive part
num_features <- 20   # Number of covariates
sample_size <- 2000  # Number of examples

dataset <- Generate.Data(sample_size,num_features,theta_pred,model)
    
# The methods will return the top-k biomarkers
top_k <-5

####################################################### 
# Ranking the biomarkers on their predictive strength #
#######################################################
# INFO, which captures first order interactions (returns the top_k = 5 biomarkers)
INFO.Output_Categorical.Covariates_Categorical(dataset$data,dataset$labels,dataset$treatment)$ranking[1:top_k] # this function returns the ranking

# INFO+, which captures second order interactions (returns the top_k = 5 biomarkers)
INFOplus.Output_Categorical.Covariates_Categorical(dataset$data,dataset$labels,dataset$treatment,top_k)$ranking # this function returns the ranking


```
