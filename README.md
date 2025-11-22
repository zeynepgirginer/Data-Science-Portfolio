# Data-Science-Portfolio
R portfolio: Shiny dashboard, Monte Carlo simulations, advanced business regression diagnostics, tidyverse data wrangling.

This repository brings together a set of R-based analytical projects that demonstrate practical competence in simulation, regression modeling, feature engineering, time-series analysis, and Shiny application development.  
The focus across all folders is applied reasoning: clean workflows, reproducible scripts, and interpretable outputs rather than abstract theory.

Each project is self-contained, uses publicly available or simulated data, and highlights a different aspect of data science practice.

---

## Repository Structure

```

R-Data-Science-Portfolio/
│
├── data/                           # External datasets used across projects
│
├── 1_Ecom_Shiny_Dashboard/         # Interactive Shiny app for e-commerce KPIs
│
├── 2_Advanced_Econometrics_Simulation/
│       Monte_Carlo_Simulation.R
│       Advanced_Regression_Analysis.R
│       Distribution_Sim_Analysis.R
│
├── 3_Business_Regression_Analysis/
│       Employee_Satisfac_Regression.R
│
├── 4_Financial_Time_Series/
│       Stock_Prices_Comp.R
│
└── 5_Tidyverse_Data_Wrangling/
breast_cancer_analysis.R

```

---

## Project Overview

### **1. E-commerce Shiny Dashboard**
A fully reproducible Shiny dashboard demonstrating:
- Google Sheets data ingestion  
- dynamic KPI panels  
- category-level filtering  
- interactive time-series charts  

Focus: **product analytics, dashboarding, Shiny UI/Server structure**.

---

### **2. Advanced Regression & Simulation Analysis**
A set of econometric simulation projects showcasing:
- Monte Carlo OLS validation  
- regression under correlated and non-normal predictors  
- distribution simulation using PearsonDS  
- full diagnostic workflows (BP test, VIF, residual analysis)  

Focus: **statistical modeling, simulation design, assumption testing**.

---

### **3. Business Regression Analysis**
A business-focused project modeling drivers of employee satisfaction using:
- tidyverse pipelines  
- engineered features  
- OLS with diagnostics  
- grouped summaries for insight generation  

Focus: **interpretable modeling, insight-driven analysis**.

---

### **4. Financial Time Series**
A collection of clean, independent time-series exercises:
- AMZN vs MSFT volatility comparison  
- GDP and capital stock growth using PWT  
- probability surfaces and Monte Carlo convergence examples  

Focus: **time-series manipulation, economic indicators, comparative visualization**.

---

### **5. Tidyverse Data Wrangling**
A feature-engineering and reshaping project using the BreastCancer dataset:
- factor-to-numeric conversions  
- engineered cancer-risk score  
- long/wide transformations  
- class-level summaries  

Focus: **feature engineering, tidy data restructuring, clean reporting**.

---

## Skills Demonstrated Across the Repository

- Regression modeling (OLS, diagnostics, robust inference)  
- Simulation workflows (Monte Carlo, correlated data generation)  
- Time-series analysis (volatility, growth rates, rolling metrics)  
- Tidyverse wrangling (pivoting, joins, grouped summaries)  
- Feature engineering for interpretable insights  
- Shiny app development and reactive programming  
- Visualization with ggplot2 and plotly  
- Reproducible, modular R scripting  

---

## Technical Requirements

The projects rely primarily on:

`tidyverse`, `ggplot2`, `plotly`,  
`quantmod`, `pwt8`,  
`PearsonDS`, `moments`,  
`car`, `lmtest`, `sandwich`,  
`mlbench`, `shiny`, `googlesheets4`.

All scripts use relative paths and are fully self-contained.

---

## Summary

This portfolio highlights a broad range of analytical capabilities in R, from statistical modeling to interactive dashboards.  
Each folder illustrates a different dimension of data science work—simulation, diagnostics, feature engineering, business insights, and visualization—organized in a clean, reproducible format suitable for both technical review and practical reference.

