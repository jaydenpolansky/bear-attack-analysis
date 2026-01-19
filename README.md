# Fatal Bear Attacks in North America - Interactive Explorer

An R Shiny application for exploring patterns in fatal bear attacks across North America. Users can investigate relationships between attack timing, bear species, and victim demographics through interactive visualizations.

## Live Demo

https://jaydenp.shinyapps.io/bear-attack-analysis/

## Screenshot

![App Screenshot](screenshot.png)


## Features

- **Interactive Variable Selection**: Explore relationships between month, bear type, victim age, and gender
- **Dynamic Visualizations**: Automatically generates appropriate plots based on selected variables:
  - Single variable → Bar charts (categorical) or histograms (numeric)
  - Two variables → Grouped bar charts or boxplots
  - Three+ variables → Faceted plots or multi-dimensional scatterplots
- **Year Range Filter**: Focus analysis on specific time periods
- **Descriptive Statistics**: Displays mode, five-number summaries, or proportion tables based on selection

## Variables

| Variable | Description |
|----------|-------------|
| Month | Month when the attack occurred |
| Type of Bear | Species involved (Black, Brown/Grizzly, Polar) |
| Age | Age of the victim |
| Gender | Gender of the victim |
| Year | Year of the attack |

## Project Structure

```
bear-attack-analysis/
├── README.md
├── app.R              # Shiny application
├── data/
│   └── cleaned_bears.rds   # Cleaned dataset
└── .gitignore
```

## Requirements

```r
install.packages(c("shiny", "ggplot2", "dplyr"))
```

## Usage

### Run Locally

1. Clone this repository
2. Open `app.R` in RStudio
3. Click "Run App" or run:

```r
shiny::runApp()
```

### Deploy to shinyapps.io

1. Create a free account at [shinyapps.io](https://www.shinyapps.io/)
2. Install the rsconnect package: `install.packages("rsconnect")`
3. Configure your account and deploy:

```r
rsconnect::setAccountInfo(name='YOUR_ACCOUNT', token='YOUR_TOKEN', secret='YOUR_SECRET')
rsconnect::deployApp()
```

## Data Source

Dataset contains records of fatal bear attacks in North America. *[Add original data source/citation here if applicable]*

## Author

Jayden Polansky

## License

This project is for educational purposes.
