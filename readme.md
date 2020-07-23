readme
================

## Description

Data and code associated with the study “An investigation of population
variation in maze exploration and its predictors in wild Trinidadian
guppies (*Poecilia reticulata*)”. *In prep*. This repository will be
permanently archived at Zenodo.

A report summary containing all the R script output, including plots,
can be found at
[analysis-summary.md](https://github.com/paulqsims/inno_pop/blob/master/reports/analysis-summary.md)
in `reports/`

R Markdown files can be rendered by sourcing
[render-Rmd.R](https://github.com/paulqsims/inno_pop/blob/master/R/render-Rmd.R)
in `R/`

## Authors

Paul Q. Sims (<paul.q.sims@gmail.com>)

Simon M. Reader (<simon.reader@mcgill.ca>)

## Directory structure

  - `data/` Data used in analyses
  - `R/` Self-contained R markdown (\*.Rmd) scripts (can be run on their
    own) and functions used for the analyses and in generating plots
  - `figs/` High quality figures generated for publication
  - `reports/` Markdown and html summaries of code output

## Prerequisites: R packages

Data clean up

  - `tidyverse`
  - `broom`, `broom.mixed`

Analyses

  - `nlme`
  - `MuMIn`

Plots

  - `ggplot2`
  - `ggsignif`
  - `scales`
  - `effects`

Misc

  - `rprojroot`
  - `rmarkdown`, `knitr`

## Metadata

Reference file:
[data\_Sims-Reader\_2020.csv](https://github.com/paulqsims/inno_pop/blob/master/data/data_Sims-Reader_2020.csv)

| Column       | Description                                                                                    |
| :----------- | :--------------------------------------------------------------------------------------------- |
| pop          | Population: Lower or upper Aripo                                                               |
| site         | Site sampled                                                                                   |
| site\_uni    | Unique site identifier                                                                         |
| trial        | Trial: 1 or 2                                                                                  |
| group        | Group id                                                                                       |
| goal\_z\_lat | Latency to reach the goal zone, zone 11, (sec)                                                 |
| tot\_z       | Total zones entered                                                                            |
| body\_length | Body length (mm)                                                                               |
| learn\_prop  | Improvement ratio learning measure = goal zone latency on trial 2/goal zone latency on trial 1 |
| LN suffix    | Natural log transformation                                                                     |
| sc suffix    | Mean centered and standardized by 1 standard deviation                                         |

## Licenses

Software and code is licensed under the MIT License, data is licensed
under CC0, and media files are under CC-BY-4.0 - see the
[license.md](https://github.com/paulqsims/inno_pop/blob/master/license.md)
file for details.

## Funding information

This work was supported by the Natural Sciences and Engineering Research
Council of Canada (NSERC; Discovery grants \#418342-2012 and
429385-2012) and the Canada Foundation for Innovation (grant \#29433).
