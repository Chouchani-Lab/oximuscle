# Oximuscle

R Shiny app from the Chouchani Lab for exploring cysteine oxidation in human skeletal muscle after endurance, sprint and resistance exercise.

## Files
- `app.R` - the Shiny app
- `*.csv` - data read at startup
- `www/Oximuscle_image.png` - image used in the app

## Run locally
```r
install.packages(c("shiny","DT","shinyjs","glue","shinydashboard","png","readr","plotly","tidyr","ggplot2","dplyr"))
shiny::runApp()
```