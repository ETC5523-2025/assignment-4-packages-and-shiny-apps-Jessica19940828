
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ausbushfire

## Overview

`ausbushfire` is an R package designed to explore the trends and climate
drivers associated with the Australian bushfire risk in Southeastern
Australia (1979–2019). The analysis is grounded in key meteorological
indices and major climate drivers, allowing users to investigate the
increasing risk attributed to changing climate conditions, including the
context of the devastating 2019–2020 ‘Black Summer’ fires.

The package ships a small, tidy dataset (`ausbushfire_data`) and a Shiny
application to provide an interactive interface for data exploration and
trend analysis.

#### Key Features

- **Risk Map (Tab 1)**: Visualizes the defined study region in
  Southeastern Australia and dynamically highlights the annual burned
  area intensity ($\text{km}^2$) from 1997 onwards.

- **Trend Analysis (Tab 2)**: Enables interactive comparison of
  standardized metrics over time and exploration of relationships
  between different fire risk indicators.

- **Climate Drivers**: Focuses on key factors like the Fire Weather
  Index (FWI), Monthly Severity Rating (MSR), temperature (Tmax),
  precipitation, Indian Ocean Dipole (IOD), and Southern Annular Mode
  (SAM).

## Installation

You can install the development version of `ausbushfire` directly from
[GitHub](https://github.com/) using the remotes package.

``` r
# Install remotes if needed
install.packages("remotes") 

remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-Jessica19940828")
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
to interactively visualize the data and findings.

**Launch the App**

You can launch the application directly after installation with the
exported function:

``` r
library(ausbushfire)
ausbushfire::run_app()
```

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

## Documentation

For detailed information on functions, data objects, and vignettes,
please refer to the full documentation website:

- **Site Home**:
  <https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-Jessica19940828>

## Sources & Citation

This analysis is based on and inspired by the methodology and data
discussed in the following attribution study:

Oldenborgh, G. J. van, Krikken, F., Lewis, S., et al. (2021).
Attribution of the Australian bushfire risk to anthropogenic climate
change. *Nat. Hazards Earth Syst. Sci., 21, 941–960.*

All data is derived from the KNMI Climate Explorer and corresponding
literature sources.

## License

This package is released under the **MIT License**.

## Authors

Heng-Hsieh Chang

## Contributing

Issues and pull requests are welcome.
