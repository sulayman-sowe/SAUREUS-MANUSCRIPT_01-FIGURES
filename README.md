### SAUREUS-MANUSCRIPT_01-FIGURES

A fully reproducible R data visualization workflow designed to generate publication-ready figures, plots, and genomic maps for our first *Staphylococcus aureus* manuscript. 

### 🧬 Scientific & Computational Context

Data transparency and computational reproducibility are vital in genomic epidemiology. This repository serves as the definitive graphics engine for our manuscript. It contains the exact codebase used to ingest finalized analytical datasets and output the high-resolution, peer-review-ready figures found in the paper, removing the need for manual, non-reproducible graphic edits. 

### 📊 Visualizations Included

* **Pathogen Population Structures:** High-dimensional clustering plots and distribution charts detailing sequence type (ST) and clonal complex (CC) frequencies.
* **Antimicrobial Resistance (AMR) Profiles:** Heatmaps and comparative bar charts correlating phenotypic resistance profiling with genotypic determinants.
* **Phylogenetic Integrations:** Metadata-aligned tree layouts linking genetic distances to temporal or clinical source features.

### 🛠️ Tech Stack & Dependencies

* **Language:** R (v4.0+)
* **Primary Libraries:** ggplot2 (base engine), tidyverse (dplyr, tidyr, readr), patchwork (for multi-panel figure alignment), scales (for advanced color palettes).

### 📂 Repository Structure

text

├── data/
│   └── finalized_manuscript_data.csv  # Frozen, clean dataset used for final analysis
├── src/
│   ├── figure1_population_demographics.R  # Generates Figure 1
│   ├── figure2_amr_profiles.R            # Generates Figure 2
│   └── theme_manuscript.R                # Standardized global ggplot themes/colors
├── figures/                              # Output directory for publication images
│   ├── figure1_high_res.tiff
│   └── figure2_high_res.pdf
├── README.md
└── .gitignore

Use code with caution.

### 🚀 Getting Started

### Installation

Clone this repository locally to reproduce the manuscript graphics: 

bash

git clone https://github.com/sulayman-sowe/SAUREUS-MANUSCRIPT_01-FIGURES.git
cd SAUREUS-MANUSCRIPT_01-FIGURES

Use code with caution.

### Reproducing the Figures

You can generate the complete set of high-resolution figures by running the dedicated scripts sequentially or executing them via the terminal: 

bash

# Example: Generate Figure 1 (Demographics and Clonal Distribution)
Rscript src/figure1_population_demographics.R

Use code with caution.

Contact: ssulayman636@gmail.com
