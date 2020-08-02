readme
================

## Description

Data and code associated with the study “An investigation of population
variation in maze exploration and its predictors in wild Trinidadian
guppies (*Poecilia reticulata*)”. *In prep*. Please see the publication
for further details on data collection, the experimental apparatus,
procedure, and statistical analyses.

This repository is permanently archived at Zenodo
[![DOI](https://zenodo.org/badge/279408355.svg)](https://zenodo.org/badge/latestdoi/279408355).

A report summary containing all the R script output, including plots,
can be found at
[analysis-summary.md](https://github.com/paulqsims/inno_pop/blob/master/reports/analysis-summary.md)
in `reports/`

R Markdown files can be rendered by sourcing
[render-Rmd.R](https://github.com/paulqsims/inno_pop/blob/master/R/render-Rmd.R)
in `R/`

**Last update**: 2020-08-02

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

R version 3.6.3 (2020-02-29)

<table>

<thead>

<tr>

<th style="text-align:left;">

Package\_Name

</th>

<th style="text-align:left;">

Version

</th>

<th style="text-align:left;">

Function

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

tidyverse

</td>

<td style="text-align:left;">

1.3.0

</td>

<td style="text-align:left;">

Data clean up

</td>

</tr>

<tr>

<td style="text-align:left;">

broom

</td>

<td style="text-align:left;">

0.7.0

</td>

<td style="text-align:left;">

Data clean up

</td>

</tr>

<tr>

<td style="text-align:left;">

broom.mixed

</td>

<td style="text-align:left;">

0.2.6

</td>

<td style="text-align:left;">

Data clean up

</td>

</tr>

<tr>

<td style="text-align:left;">

nlme

</td>

<td style="text-align:left;">

3.1.148

</td>

<td style="text-align:left;">

Analyses

</td>

</tr>

<tr>

<td style="text-align:left;">

MuMIn

</td>

<td style="text-align:left;">

1.43.17

</td>

<td style="text-align:left;">

Analyses

</td>

</tr>

<tr>

<td style="text-align:left;">

ggplot2

</td>

<td style="text-align:left;">

3.3.2

</td>

<td style="text-align:left;">

Plots

</td>

</tr>

<tr>

<td style="text-align:left;">

ggsignif

</td>

<td style="text-align:left;">

0.6.0

</td>

<td style="text-align:left;">

Plots

</td>

</tr>

<tr>

<td style="text-align:left;">

scales

</td>

<td style="text-align:left;">

1.1.1

</td>

<td style="text-align:left;">

Plots

</td>

</tr>

<tr>

<td style="text-align:left;">

effects

</td>

<td style="text-align:left;">

4.1.4

</td>

<td style="text-align:left;">

Plots

</td>

</tr>

<tr>

<td style="text-align:left;">

rprojroot

</td>

<td style="text-align:left;">

1.3.2

</td>

<td style="text-align:left;">

Miscellaneous

</td>

</tr>

<tr>

<td style="text-align:left;">

rmarkdown

</td>

<td style="text-align:left;">

2.3

</td>

<td style="text-align:left;">

Miscellaneous

</td>

</tr>

<tr>

<td style="text-align:left;">

knitr

</td>

<td style="text-align:left;">

1.29

</td>

<td style="text-align:left;">

Miscellaneous

</td>

</tr>

</tbody>

</table>

## Metadata

Reference file:
[data\_Sims-Reader\_2020.csv](https://github.com/paulqsims/inno_pop/blob/master/data/data_Sims-Reader_2020.csv)

<table>

<thead>

<tr>

<th style="text-align:left;">

Column

</th>

<th style="text-align:left;">

Description

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

pop

</td>

<td style="text-align:left;">

Population: Lower or upper Aripo

</td>

</tr>

<tr>

<td style="text-align:left;">

site

</td>

<td style="text-align:left;">

Site sampled

</td>

</tr>

<tr>

<td style="text-align:left;">

site\_uni

</td>

<td style="text-align:left;">

Unique site identifier

</td>

</tr>

<tr>

<td style="text-align:left;">

trial

</td>

<td style="text-align:left;">

Trial: 1 or 2

</td>

</tr>

<tr>

<td style="text-align:left;">

group

</td>

<td style="text-align:left;">

Group id

</td>

</tr>

<tr>

<td style="text-align:left;">

goal\_z\_lat

</td>

<td style="text-align:left;">

Latency to reach the goal zone, zone 11, (sec)

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z

</td>

<td style="text-align:left;">

Total zones entered

</td>

</tr>

<tr>

<td style="text-align:left;">

body\_length

</td>

<td style="text-align:left;">

Body length (mm)

</td>

</tr>

<tr>

<td style="text-align:left;">

learn\_prop

</td>

<td style="text-align:left;">

Improvement ratio learning measure = goal zone latency on trial 2/goal
zone latency on trial 1

</td>

</tr>

</tbody>

</table>

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
