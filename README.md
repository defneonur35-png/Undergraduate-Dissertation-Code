---
title: "README"
author: "Defne Onur"
date: "2026-04-03"
---

# BSc (Hons) Cognitive Neuroscience and Psychology - Undergraduate Dissertation Code

This repository contains the R code (all analyses conducted in RStudio) used for the statistical analysis and visualisation of my undergraduate dissertation project.

## Project Overview
The project investigates pre-post changes in left dlPFC GABA following non-invasive brain stimulation (sham, tDCS, tACS), and whether changes in GABA predict working memory performance in a verbal n-back task.

## Files

### Main GABA/NAA Analysis R File 
- `stats_script_final.R` — main statistical analysis (linear mixed-effects models)

### Working Memory Analysis R Files
- `corr_dprime.R` — correlation analysis using d Prime
  - **Note:** For the forest plot, IE correlation values must be entered manually.
- `corr_inverse_efficiency.R` — correlation analysis using inverse efficiency scores

## Methods
Analyses were conducted in R Studio using libraries including:
- `car`
- `ggplot2`
- `dplyr`
- `ggdist`
- `emmeans`
- `gridExtra`
- `tidyr`
- `conflicted`
- `lme4`
- `lmerTest`
- `tidyverse`
- `broom`
- `ggpubr`

## Reproducibility
The raw data are not included in this repository due to participant confidentiality. Scripts are provided for transparency and to document the analysis pipeline used in the dissertation.
