readme
================

## Description

Data and code associated with the study “An investigation of population
variation in maze exploration and its predictors in wild Trinidadian
guppies (*Poecilia reticulata*)”. *In prep*. This repository will be
permanently archived at Zenodo.

## Authors

Paul Q. Sims (<paul.q.sims@gmail.com>)

Simon M. Reader (<simon.reader@mcgill.ca>)

## Directory structure

  - `data/` Data used in analyses
  - `R/` Self-contained R scripts (can be run on their own) and
    functions used for the analyses and in generating plots

## Prerequisites: R packages

Data clean up

  - `rprojroot`, `here`
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
| goal\_z\_lat | Latency to reach the goal zone (zone 11)                                                       |
| tot\_z       | Total zones entered                                                                            |
| body\_length | Body length (mm)                                                                               |
| learn\_prop  | Improvement ratio learning measure = goal zone latency on trial 2/goal zone latency on trial 1 |
| LN           | Natural log transformation                                                                     |
| sc           | Mean centered and standardized by 1 standard deviation                                         |

## License

This software is licensed under the MIT License and the data is licensed
under CC0 - see the
[LICENSE.md](https://github.com/paulqsims/inno_pop/blob/master/license.md)
file for details.

## Funding information

This work was supported by the Natural Sciences and Engineering Research
Council of Canada (NSERC; Discovery grants \#418342-2012 and
429385-2012) and the Canada Foundation for Innovation (grant \#29433).
