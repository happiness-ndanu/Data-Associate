# Health Care Workers Vaccination Dashboard

An interactive Shiny dashboard that presents vaccination coverage among health care workers. This app visualizes key metrics such as vaccine uptake, county-level distribution, and trends over time to support data-driven public health decisions.

🔗 **Live App:** [HealthCare Workers Vaccination Dashboard](https://happinessndanu1.shinyapps.io/HCWs_Vaccination/) 
Username- user1
Password- pass1

---

## 📊 Features

- 🧑‍⚕️ Vaccine coverage among health care workers
- 🌍 County-level distribution of doses
- 📈 Time trends in vaccine uptake
- 🔎 Filterable by county, vaccine type, and reporting period
- 📊 Visual summaries through bar charts, maps, and KPIs

---

## 💡 Project Goals

This dashboard was developed to:
- Communicate vaccine uptake in a visually compelling and accessible format
- Encourage transparency in vaccine distribution
- Showcase R and Shiny skills in real-world public health analysis

---

## 🛠️ Built With

- **R** & **Shiny**
- [`shinydashboard`](https://rstudio.github.io/shinydashboard/)
- [`ggplot2`](https://ggplot2.tidyverse.org/)
- [`leaflet`](https://rstudio.github.io/leaflet/)
- [`dplyr`](https://dplyr.tidyverse.org/)
- [`readr`](https://readr.tidyverse.org/)

---

## 🚀 How to Run Locally

```r
# Clone the repo
git clone https://github.com/happiness-ndanu/Health-Care-Workers-Vaccination.git

# Set working directory and run the app
setwd("Health-Care-Workers-Vaccination")
shiny::runApp()
