readme
================
Paul Q. Sims
7/13/2020

## Description

Data and code associated with the publication “An investigation of
population variation in maze exploration and its predictors in wild
Trinidadian guppies (*Poecilia reticulata*)”. (Insert publication
information here). This repository is permanent archived at (Zenodo DOI
HERE).

## Authors

Paul Q. Sims (<paul.q.sims@gmail.com>)

Simon M. Reader (<simon.reader@mcgill.ca>)

## Directory structure

  - `data/` Data used in analysis
  - `R/` Self-contained R scripts and functions used for the analyses
    and in generating plots

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
