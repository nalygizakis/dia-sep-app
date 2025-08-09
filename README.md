# Data-Independent Separation Application

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
  `shiny`, `shinyjs`, [`peakTrAMS`](https://github.com/nalygizakis/peakTrAMS)
- **ProteoWizard** CLI (`msconvert`) installed and available in your PATH:
  - Windows: `msconvert.exe`
  - Ubuntu/Linux: `msconvert`
- Java installed (the app uses `options(java.parameters = "-Xmx16384m")`)
- All CSS/JS assets available in the `assets/` folder (see below).

## Installation

```bash
# Clone this repository
git clone https://github.com/nalygizakis/dia-sep-app.git
cd dia-sep-app
```

Install R dependencies:

```r
install.packages(c("shiny", "shinyjs"))
# Install peakTrAMS from GitHub if not already available
remotes::install_github("nalygizakis/peakTrAMS")
```

Ensure `msconvert` works from your terminal:

```bash
msconvert --version
```

## Usage

From the repository folder, run:

```bash
Rscript App.R
```

or in R:

```r
library(shiny)
runApp(".")
```

1. Upload a `.mzML` or `.mzXML` file.
2. Set **Intensity cutoff** (see in-app guidance).
3. (Optional) Enter **calibrant/lock-mass scans** (comma-separated).
4. If CE information is **present**: click **Submit Processing**.  
   If **absent**: specify CE channels and scan indices manually.
5. Download processed `.mzML` files for each CE channel.

## License

MIT License. See the [LICENSE](LICENSE) file for details.

## Citation

If you use this app in a scientific work, please cite:

- Chambers, M. C., et al. (2012). *A cross-platform toolkit for mass spectrometry and proteomics.* Nature Biotechnology, 30(10), 918–920. (ProteoWizard)
- [peakTrAMS R package](https://github.com/nalygizakis/peakTrAMS)
