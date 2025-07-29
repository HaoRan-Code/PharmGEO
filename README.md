# PharmGEO: A User-Friendly Platform for Pharmaco-Transcriptomic Analysis

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16556994.svg)](https://doi.org/10.5281/zenodo.16556994)

## Overview

PharmGEO is an interactive Shiny application that integrates and standardizes 7,931 manually curated pharmaco-transcriptomic experiments from the Gene Expression Omnibus (GEO). The platform covers 1,334 drugs with harmonized metadata including dose, treatment duration, and cell type information, providing researchers with a comprehensive resource for drug-gene association discovery and interaction analysis.

## Web Application

Access the interactive PharmGEO platform at: **http://bio-db.com/PharmGEO/**

## Data Availability

Due to GitHub's file size limitations, the complete original datasets are available at:
**https://doi.org/10.5281/zenodo.16556994**

This repository contains the source code and essential files for running the PharmGEO application.

## FAIR Principles Compliance

PharmGEO was developed following FAIR (Findable, Accessible, Interoperable, and Reproducible) data principles:
- **Findable**: Systematic curation with standardized metadata annotations
- **Accessible**: Open web interface and public data repository
- **Interoperable**: Cross-platform data integration and standardized formats
- **Reproducible**: Standardized analysis pipelines and transparent methodologies

## System Requirements

- R (>= 4.5.1)
- Shiny Server (for deployment)
- Required R packages listed in `renv.lock`

## Installation

1. Clone this repository:
```bash
git clone https://github.com/HaoRan-Code/PharmGEO.git
cd PharmGEO
```

2. Restore the R environment:
```r
renv::restore()
```

3. Download the complete dataset from Zenodo and extract to the appropriate directory

4. Run the application:
```r
shiny::runApp()
```

## Key Modules

1. **RNA-seq Datasets**: Browse and analyze curated pharmaco-transcriptomic experiments
2. **Drug Information**: Explore drug properties and associated transcriptomic profiles
3. **Gene Information**: Investigate gene expression patterns across different drug treatments
4. **Drug-Drug Interactions**: Analyze transcriptome-based drug interaction networks

## Acknowledgments

We thank the Gene Expression Omnibus (GEO) for providing the foundational transcriptomic data and the broader research community for their contributions to pharmaco-transcriptomic research.