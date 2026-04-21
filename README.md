---
title: "README"
author: "Defne Onur"
date: "2026-04-03"
---

# BSc (Hons) Cognitive Neuroscience and Psychology - Undergraduate Dissertation Code

This repository contains the R code (all analyses conducted R Studio) used for the statistical analysis, and visualisation for my undergraduate dissertation project.

## Project Overview
The project revolves around investigating pre-post changes in left dlPFC GABA following non-invasive brain stimulation (Sham, tDCS, tACS), and whether changes in GABA predict working memory performance in a verbal n-back task.

## Files

### Main GABA/NAA Analysis R File 
- `stats_script_final.R` — main statistical analysis (linear mixed-effects models)

### Working Memory Analysis R Files
- `corr_dprime.R` — correlation analysis using d Prime
- `corr_inverse_efficiency.R` — correlation analysis using inverse efficiency scores

## Methods
Analyses were conducted in R Studio using libraries including:
- library(car)
- library(ggplot2)
- library(dplyr)
- library(ggdist)
- library(emmeans)
- library(gridExtra)
- library(tidyr)
- library(conflicted)
- library(lme4)
- library(lmerTest)
- library(tidyverse)
- library(broom)
- library(ggpubr)

## Reproducibility
All scripts required to reproduce the analyses and figures are included in this repository.
