# book-data-web-scraping-analysis
Exploratory data analysis and predictive modeling of book data collected via web scraping. The project analyzes relationships between book price, star rating, and availability using R.

# Exploratory Data Analysis and Predictive Modeling of Book Data Using Web Scraping 📚📊

## Overview
This project focuses on collecting book data through web scraping and applying exploratory data analysis (EDA) and predictive modeling techniques to uncover insights. The analysis examines relationships between book prices, star ratings, and availability, using the R programming language.

## Objectives
- Collect book data using web scraping techniques
- Perform data cleaning and preprocessing
- Conduct exploratory data analysis (EDA)
- Visualize relationships between key variables
- Build predictive models to estimate book prices or ratings

## Dataset
- **Source:** Web-scraped data from an online book website
- **Features include:**
  - Book title
  - Price
  - Star rating
  - Availability
  - Category (if applicable)

> **Note:** The dataset was collected for academic purposes only.

## Tools & Technologies
- **Programming Language:** R
- **Environment:** RStudio
- **Key Packages:**  
  - `rvest` (web scraping)  
  - `dplyr` (data manipulation)  
  - `ggplot2` (visualization)  
  - `tidyverse`  
  - `caret` / `lm` (predictive modeling)

## Project Structure
## Key Findings
- Book prices vary significantly across rating levels
- Higher-rated books tend to have higher average prices
- Availability shows a moderate relationship with book rating
- Predictive models provide reasonable estimates of book prices using rating and availability

## How to Run the Project
1. Clone this repository: git clone https://github.com/yourusername/book-data-web-scraping-analysis.git
2. Open the project in **RStudio**
3. Run the scripts in order:
- `01_web_scraping.R`
- `02_data_cleaning.R`
- `03_exploratory_data_analysis.R`
- `04_predictive_modeling.R`

## Project Report
The final project report is available in the `report/` directory.  
It documents the web scraping process, exploratory analysis, modeling approach, and key results.

## Author
**Sadikuzzaman Rakib**  
Department of Computer Science  
American International University-Bangladesh (AIUB)

## License
This project is licensed under the MIT License.
