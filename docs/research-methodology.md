# Research methodology

**Question:** How do observed rural and urban CPI measures differ, and how do published expenditure weights change the hypothetical burden of category price changes?

- Published YoY inflation is attributed to MoSPI, not recalculated from non-contiguous extracted snapshots.
- Period-on-period inflation when both index levels are present: `100 × (CPI_t / CPI_(t-k) − 1)`. For YoY, k must be 12 and the underlying index methodology comparable.
- Fixed basket with category weights `w_i` and consistent period percentage changes `r_i`: `sum(w_i × r_i)`; exact for one-period weighted growth using shares of initial basket expenditure under aligned category prices.
- Fixed-quantity cost projection uses `sum(E_i × (1 + r_i / 100)^years)`. This compounds assumed **annual** category price changes; does not model substitution.
- Rural/urban burden benchmarks use HCES 2023–24 food/non-food shares, not official CPI weights. HCES MPCE values exclude specified imputed free items.
- ARIMA forecasts are of CPI index **levels**; `evaluate_forecast()` holds out latest data chronologically and computes MAE/RMSE against seasonal naive.
- Minimum contiguous series length is 24 months; seasonality in auto-ARIMA permitted only at >=36. Historical holdout requires >=24 training points.

**Limitations:** The bundled CPI dataset contains only selected official dates; no trend interpolation and no initial usable time-series forecast. Prices and expenditure composition alone cannot identify causal consumption behaviour or welfare impacts. Results are aggregate all-India benchmarks, not household survey microdata. 2024 base CPI is methodologically distinct from base 2012. Users must verify manually imported official data.
