# Global Organ Transplantation Analysis

An R-based analysis of global organ donation and transplantation data from the
Global Observatory on Donation and Transplantation (GODT), covering 2000–2024.

The project examines regional differences in deceased donation, the disruption
and recovery surrounding COVID-19, donor-source composition, DCD adoption, and
the effect of reporting coverage on aggregate statistics.

## Project background

This project originated as a university group assignment and was subsequently
extended independently.

My contribution to the original project focused on:

- regional disparities in deceased-donor rates;
- the COVID-19 stress test;
- living versus deceased kidney donation.

I later developed additional analyses covering:

- recovery in deceased donation in 2021;
- regional adoption of DCD donation;
- country-level deceased-donor utilization;
- reporting-coverage diagnostics;
- additional data-quality checks.

## Main analytical points

### Regional comparisons

Regional deceased-donor rates differ substantially, but comparisons are also
affected by changes in which countries report data over time.

A reporting-coverage check showed, for example, that an apparent fall in the
American regional median in 2004 was largely caused by the entry of additional
reporting countries rather than a decline among the countries already present.

### COVID-19 shock and recovery

The analysis compares country-level deceased-donor rates in 2019, 2020 and
2021, distinguishing the initial pandemic shock from the subsequent recovery.

### Donor-source composition

The project compares living and deceased kidney donation and examines the
regional development of donation after circulatory death (DCD).

### Missing data and utilization

A large apparent fall in global deceased-donor utilization in 2018 was found
to depend strongly on missing and unmatched reporting.

This illustrates why aggregate trends should be checked against the countries
contributing to both the numerator and denominator.

## Data preparation

The source workbook is transformed from wide to long format and validated
before analysis.

The script checks:

- duplicate country-year-indicator observations;
- invalid population values;
- population-formatting problems;
- missing versus reported-zero observations;
- inconsistencies between reported totals and components;
- cases in which utilized donors exceed actual donors.

Population-adjusted measures are expressed per million population (pmp).

```


## Running the analysis

The analysis requires R and the following packages:

```r
install.packages(c("readxl", "tidyverse", "janitor", "ggrepel"))
```

Run `analysis.R` from the beginning and select the original GODT Excel workbook
when prompted. The script imports and validates the data, reshapes the dataset,
runs the analyses, and saves the main figures.

The source dataset is not redistributed in this repository.

## Tools

- R
- tidyverse / dplyr
- ggplot2
- readxl
- janitor
- ggrepel

## Full report

[Global Organ Transplantation Report](report/Global_Organ_Transplantation_Report.pdf)

## Repository structure

```text
.
├── analysis.R
├── figures/
├── report/
└── README.md
