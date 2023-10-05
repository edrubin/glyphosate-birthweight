# Glyphosate Paper Outline

## Title
*75 characters.*  
Perinatal health effects of herbicides: Glyphosate, Roundup, and the rollout of GM crops


## Abstract/Summary
200 words. 

## Paper
**Nature/PNAS**: 4300 words, 6-8 pages, 5-6 figures/tables.  
**Science**: 2000-3000 words, 3-5 pages, 3-5 figures/tables.

### Introduction
Need to hone in on exactly what the pitch of the paper is 
* Glyphosate is widely used and considered safe
* GM improves yields GM effect on soy/corn yield (Crop Science, 2013), Lusk on Yield effect of GM (NBER 2017)
* Unclear ex-ante health effect relative to alternative herbicides (Nature Communications, 2017, Science, 2021, Ecological Economics, 2023)
* Something about EJ or equity? Related to finding largest effects in lowest BW births
* Mechanisms and evidence of exposure (Urine samples, water samples) 
    * Review of fate/transport of pesticides (Science, 2013)
    * Glyphosate can get into ground water (PNAS 2021)
* Effect of glyphosate once it is inside the body
    * Cause Autism Disorder (PNAS 2020)
    * Glyphosate and bees (Science, 2022), (PNAS 2018)
* Other studies that might be interesting 
    * Economics of GM
        * GM crop adoption and Pesticide Use (Science, 2016 by economists)
        * Welfare effects of GM seeds (RAND Journal of Economics, 2019)
        * GM and conservation tillage (AJAE, 2016)
    * Glyphosate in the courts (Science, 2019) 
    * Simulate glyphosate ban (PNAS 2021 by economists)
    * Monsanto in 1997 (from Nature)

### Local results  
* Describe data and methodology (Fig 1: Suitability map and glyphosate)
* First stage results
* Reduced form results (Fig 2)
* TSLS results (Table 3), with heterogeneity by predicted BW percentile
* Put estimates of effect into context: Significant, but not unbelievably large. Compare to other estimates from the literature on infant health.
* Robustness and caveats

### Predicting birthweights 
* Briefly describe prediction process and why it it necessary
* Effect heterogeneity by predicted BW percentile 
* Descriptive information about BW percentiles (Fig 4)
* Could be driven by a number of things: differential exposure, differential protective behavior, confluence with other choices, etc. 

### Upstream  
* Description of aggregation 
* Upstream event study (Fig 5)
* Caveats, why this may be different than Brazil paper 
*Maybe this whole section gets pushed to supplementary information

### Conclusion 
* Don’t find effects for infant mortality (explain why?)
* How this fits into general welfare effects/policy implications


## Methods
*3000 words. All elements necessary to interpret and replicate the results.*

### Natality Data

### Glyphosate Data

### GAEZ Data
* Show correlation with pre-period crops

### Empirical Strategy
* Main regression of interest
* Concern about endogeneity 
* Instruments and how they solve endogeneity

### Predicting BW
* Why do we need to predict birthweight
* Description of model (RF), predictors and outcome   
* Training, tuning, cross validation, etc. 

### Upstream Aggregation
* Disaggregating glyphosate data into watersheds 
* Other information about watersheds (slope, erodibility, rainfall)
* Finding upstream watersheds
* Distance bin aggregation
* Aggregating back to county

## Extended Data
*10 additional figures.*
* Glyphosate use maps 
* Suitability map 
* Time series trends for different groups
* Robustness checks 
* Treatment effect heterogeneity
* Correlation of watershed w/USGS measurements 

## Supplementary Information
*Details that only people in our specific field would be interested in.*
* Robustness checks
    * Different versions of the instrument (Soy, corn, acreage percentiles)
    * Shift-share instrument 
    * Different outcomes 
* Heterogeneity—Month of birth 
* Watershed stuff
    * ML version of model (?)