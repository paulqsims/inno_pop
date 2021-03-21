readme
================

## Description

Data and code associated with the study “An investigation of population
variation in maze exploration and its predictors in wild Trinidadian
guppies (*Poecilia reticulata*)”. *In prep*. Please see the publication
for further details on data collection, the experimental apparatus,
procedure, and statistical analyses.

This repository is permanently archived at Zenodo, with the latest
release found here:
[![DOI](https://zenodo.org/badge/279408355.svg)](https://zenodo.org/badge/latestdoi/279408355).

A report summary containing all the R script output, including plots,
can be found at
[analysis-summary.md](https://github.com/paulqsims/inno_pop/blob/master/reports/analysis-summary.md)
in `reports/`

R Markdown files can be rendered by sourcing
[render-Rmd.R](https://github.com/paulqsims/inno_pop/blob/master/R/render-Rmd.R)
in `R/`

**Last update**: 2021-03-21

## Authors

Paul Q. Sims (<paul.q.sims@gmail.com>)

Simon M. Reader (<simon.reader@mcgill.ca>)

## Directory structure

  - `data/` Data used in analyses
  - `R/` Self-contained R markdown (\*.Rmd) scripts (can be run on their
    own) and custom functions used for modifying the data, running
    analyses, and generating plots
  - `figs/` High quality figures generated for publication
  - `reports/` Markdown and html summaries of code output

## Prerequisites: R version and R packages

R version 4.0.4 (2021-02-15)

| Package\_Name | Version | Function      |
| :------------ | :------ | :------------ |
| tidyverse     | 1.3.0   | Data clean up |
| broom         | 0.7.5   | Data clean up |
| broom.mixed   | 0.2.6   | Data clean up |
| nlme          | 3.1.152 | Analyses      |
| MuMIn         | 1.43.17 | Analyses      |
| ggplot2       | 3.3.3   | Plots         |
| ggsignif      | 0.6.1   | Plots         |
| scales        | 1.1.1   | Plots         |
| effects       | 4.2.0   | Plots         |
| rprojroot     | 2.0.2   | Miscellaneous |
| rmarkdown     | 2.7     | Miscellaneous |
| knitr         | 1.31    | Miscellaneous |

## Metadata

Reference file:
[data\_Sims-Reader\_2020.csv](https://github.com/paulqsims/inno_pop/blob/master/data/data_Sims-Reader_2020.csv)

| Column       | Description                                    |
| :----------- | :--------------------------------------------- |
| pop          | Population: Lower or upper Aripo               |
| site         | Site sampled                                   |
| site\_uni    | Unique site identifier                         |
| trial        | Trial: 1 or 2                                  |
| group        | Group id                                       |
| goal\_z\_lat | Latency to reach the goal zone, zone 11, (sec) |
| tot\_z       | Total zones entered                            |
| body\_length | Body length (mm)                               |

  - Missing values are input as `NA` text

## Licenses

Software and code is licensed under the MIT License, data is licensed
under CC0, and media files are under CC-BY-4.0 - see the
[license.md](https://github.com/paulqsims/inno_pop/blob/master/license.md)
file for details.

## Funding information

This work was supported by the Natural Sciences and Engineering Research
Council of Canada (NSERC; Discovery grants \#418342-2012 and
429385-2012) and the Canada Foundation for Innovation (grant \#29433).
