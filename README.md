# Association of Antenatal Dexamethasone Exposure and Neurodevelopment Among Premature Infants: A Retrospective Cohort Study

**Subtitle:** Antenatal Dexamethasone and Infant's Neurodevelopment

## Overview

This repository contains the analysis code for a retrospective cohort study investigating the independent association between antenatal dexamethasone (DXMS) exposure and early neurodevelopmental outcomes in preterm infants, assessed using the Griffiths Development Scales-Chinese (GDS-C).

## Analysis Pipeline

**Univariate analyses:** A range of candidate predictors—including maternal, perinatal, neonatal, and follow-up characteristics—were screened for association with each GDS-C subscale. Variables with *P* < 0.1 were advanced to multivariable modeling, with DXMS exposure retained regardless of significance.

**Multivariable analyses:** LASSO-penalized ordinal logistic regression models were fitted for each GDS-C subscale to identify the optimal set of confounders. DXMS exposure was forced into all models. False discovery rate (FDR) correction was applied across the subscales, with adjusted *q*-values < 0.3 retained for the final simpler model.

## Code Availability

All statistical analyses were performed using in-house scripts, which are publicly available in this repository.

## Data Availability

All statistical results supporting the findings of this study are provided within the manuscript and its supplementary materials. Additional de-identified participant data are available from the corresponding author upon reasonable request.
