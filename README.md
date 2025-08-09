# Data-Independent Separation Application (Shiny)

An R/Shiny application to split **data-independent** LC-MS full-scan files (`.mzML` / `.mzXML`) into separate **collision energy (CE) channels**, optionally removing calibrant/lock-mass scans and re-exporting to `.mzML`.

## Features

- Detects if uploaded files are **data-independent** using scan and precursor heuristics.
- **Automatic mode**: CE information is already present — splits automatically.
- **Manual mode**: CE information is missing — user specifies number of CE channels and scan indices.
- Optional removal of **calibrant/lock-mass scans**.
- Uses **ProteoWizard `msconvert`** for peak picking, noise filtering, and final `.mzML` generation.
- Outputs one `.mzML` file per CE channel for download.

## Requirements

- **R ≥ 4.2**
- R packages:  
  `shiny`, `shinyjs`, `peakTrAMS`
- **ProteoWizard** CLI (`msconvert`) installed and available in your PATH:
  - Windows: `msconvert.exe`
  - Ubuntu/Linux: `msconvert`
- Java installed (the app uses `options(java.parameters = "-Xmx16384m")`)
- All CSS/JS assets available in the `assets/` folder (see below).

## Installation

```bash
# Clone this repository
git clone https://github.com/<your-username>/<your-repo>.git
cd <your-repo>
