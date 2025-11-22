# Financial Time Series & Probability Simulations

This script brings together several small but complementary analyses:
- equity price dynamics for two large tech stocks  
- GDP and capital stock growth for selected countries  
- probability surfaces for the “shared birthday” problem  
- a simple Monte Carlo experiment illustrating convergence of frequencies  

All sections are written in R and focus on clean, interpretable analysis rather than heavy formalism.

---

## 1. Equity Volatility: AMZN vs MSFT

**Libraries:** `quantmod`

The script downloads weekly closing prices for **AMZN** and **MSFT** from Yahoo Finance between 2005-05-05 and 2023-10-01 using `getSymbols()`.  

Key steps:
- extract weekly closing prices with `Cl()`  
- plot both series on the same time axis  
- compute summary statistics via a custom `info()` function:
  - max, min  
  - mean  
  - standard deviation  
  - range  
  - coefficient of variation (CV)  

Then it computes an **8-week rolling standard deviation** for each stock with `rollapply()` and plots them together to compare how short-term volatility evolves over time.

This section shows basic time-series handling, volatility inspection, and comparative visualization for financial assets.

---

## 2. GDP Growth: Sweden vs Belgium

**Libraries:** `pwt8`, `ggplot2`

Using the **Penn World Table 8.0** dataset (`pwt8.0`), the script:

- filters Sweden and Belgium for the years 1995–2005  
- computes annual real GDP growth rates using log differences of `rgdpo`  
- plots:
  - Sweden’s GDP growth as a single time series (`qplot`)  
  - Sweden vs Belgium growth on the same chart with `ggplot()` and `geom_line()`  

This section demonstrates time-series transformation (log growth), country-level filtering, and comparative macro visualization.

---

## 3. Capital Stock Growth: Nepal vs Serbia

Again using `pwt8.0`, the script:

- filters Nepal and Serbia for 1999–2005  
- computes capital stock growth rates from `ck` using log differences  
- visualizes:
  - Nepal’s capital stock level over time  
  - Nepal vs Serbia capital stock **growth rates** on the same chart  

This provides a simple macro–capital comparison using the same pattern as the GDP growth section.

---

## 4. Probability & Simulation Exercises

### 4.1 Shared Birthday Probability Surface

The script defines a function:

```r
p(n, x)
