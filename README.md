
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ausbushfire

## Overview

`ausbushfire` is an R package designed to explore the trends and climate
drivers associated with the Australian bushfire risk in Southeastern
Australia (1979–2019). The analysis is grounded in key meteorological
indices and major climate drivers, allowing users to investigate the
increasing risk attributed to changing climate conditions, including the
context of the devastating 2019–2020 ‘Black Summer’ fires.

The package ships a  **tidy dataset (`ausbushfire_data`)** and a **Shiny
application** to provide an interactive interface for data exploration and
trend analysis.

## Installation

You can install the development version of `ausbushfire` directly from
[GitHub](https://github.com/) using the remotes package.

``` r
# Install remotes if needed
install.packages("remotes") 

remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-Jessica19940828")
```
#### Load the package and Launch the App
You can launch the application directly after installation with the
exported function:

``` r
library(ausbushfire)

# Load dataset
data(ausbushfire_data)

# Launch the Shiny app
ausbushfire::run_app()
```

## Dataset Description

The core data used throughout the package and the Shiny application is
bundled in the `ausbushfire_data` object.

| Variable Name | Description | Source Period | Unit |
|----|----|----|----|
| year | Fire season year (aligned to the end of the season) | 1979–2019 | Year |
| fwi_sm | Fire Weather Index (FWI) seasonal maximum | 1979–2019 | Unitless |
| msr_sm | Monthly Severity Rating (MSR) seasonal maximum | 1979–2019 | Unitless |
| tmax | Maximum Temperature (Tmax) | 1979–2019 | °C |
| precip_total | Fire Season (Sep–Feb) total precipitation | 1979–2019 | mm |
| iod_mean | Indian Ocean Dipole (IOD) mean (Sep–Dec) | 1979–2019 | Unitless |
| sam_mean | Southern Annular Mode (SAM) mean (Sep–Dec) | 1979–2019 | Unitless |
| area_burned | Total burned area in the study region | 1997–2018 | km² |

## Shiny Application

The package includes a comprehensive Shiny app (located under inst/app/)
to allows users to explore trends, correlations, and risk maps interactively.

#### App Features

- **Risk Map**: Use the slider to explore the annual burned area and
  fire risk status (FWI/Burned Area) for Southeastern Australia. The
  color intensity of the study region reflects the severity of the fire
  season.
  
- **Explore Trends**: Select multiple standardized metrics (FWI, Tmax,
  IOD, Area Burned, etc.) to visualize their long-term trends and
  volatility over a customizable time range.

- **Relationship Between Metrics**: Use the scatter plot to quickly
  assess the correlation between any two climate drivers or fire impact
  metrics (e.g., Tmax vs Area Burned).

- **About**: Contains a study region description, dataset download, and session information.

#### App Screenshot
Here's a preview of the ausbushfire Shiny app interface:
![](vignettes/figures/risk_map.png)
![](vignettes/figures/time_series.png)

## Documentation & Pkgdown Site

- View functions, vignettes, and data dictionary:
  **Site Home**: <https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-Jessica19940828>

- Vignettes included:
  - Introduction to the ausbushfire package and Shiny app
  - Data dictionary with variable definitions and units

## Sources & Citation

This analysis is based on and inspired by the methodology and data
discussed in the following attribution study:

Oldenborgh, G. J. van, Krikken, F., Lewis, S., et al. (2021).
Attribution of the Australian bushfire risk to anthropogenic climate
change. *Nat. Hazards Earth Syst. Sci., 21, 941–960.*

All climate and fire data derived from KNMI Climate Explorer and related literature.

## License

This package is released under the **MIT License**.

## Authors

Heng-Hsieh Chang

## Contributing

Issues and pull requests are welcome. Please open issues on the GitHub repository for bug reports or suggestions.
