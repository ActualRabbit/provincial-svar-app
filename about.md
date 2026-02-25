# Provincial SVAR: Canadian Monetary Policy Transmission

## Overview

This application investigates the **differential effects of monetary policy across Canadian provinces** using Structural Vector Autoregression (SVAR).

The central finding: **monetary policy affects provinces very differently**. A 36 basis point increase in the Bank of Canada's interest rate causes Ontario's employment to drop by 0.13%, while Alberta's employment barely moves.

## Key Findings

| Province | Max Employment Response | Month of Max |
|----------|------------------------|--------------|
| Ontario | -0.13% | 12 |
| Newfoundland | -0.13% | 15 |
| Quebec | -0.10% | 15 |
| Saskatchewan | -0.10% | 13 |
| Nova Scotia | -0.10% | 14 |
| Manitoba | -0.08% | 14 |
| New Brunswick | -0.07% | 24 |
| British Columbia | -0.03% | 12 |
| Prince Edward Island | -0.02% | 10 |
| Alberta | -0.02% | 12 |

## Credit Channels

Two theories attempt to explain these differences:

### Narrow Credit Channel (Manufacturing Intensity) ✓
Provinces with more manufacturing are more sensitive to interest rates. Manufacturing firms have large fixed capital expenditures financed through banks, and face interest-rate-sensitive demand.

**This theory fits the data.** Ontario and Quebec (high manufacturing) are sensitive; Alberta (low manufacturing) is not.

### Broad Credit Channel (Firm Size) ✗
Small firms should be more affected by interest rate changes because they rely more heavily on bank loans rather than capital markets.

**This theory does NOT fit Canadian data.** British Columbia has the highest ratio of small firms but one of the lowest interest rate sensitivities.

## Policy Implications

1. **Transfer payments** help address provincial disparity when monetary policy disproportionately affects certain regions

2. **Regional information** should be incorporated into monetary policy decisions to reduce social welfare loss

3. **Higher inflation targets** may be warranted to avoid pushing interest-rate-sensitive provinces into deflation while trying to cool commodity-driven provinces

## Model Specification

The SVAR uses Cholesky decomposition with variables ordered from most to least exogenous:

1. **Commodity Price Index** — Bank of Canada BCPI (most exogenous)
2. **Interest Rate** — 1-month Treasury Bill rate
3. **Core Inflation** — CPI excluding food & energy
4. **GDP** — Real output
5. **Employment** — Provincial (least exogenous)

Data: Statistics Canada CANSIM tables + Bank of Canada BCPI
Sample: January 1992 onwards (post-inflation-targeting regime)

## References

- Carlino, G., & Defina, R. (1998). The differential regional effects of monetary policy. *Review of Economics and Statistics*, 80(4), 572-587.
- Owyang, M. & Wall, T. (2006). Regional SVARs and the channels of monetary policy. *Federal Reserve Bank of St. Louis*.
- Sims, C. (1980). Macroeconomics and reality. *Econometrica*, 48(1), 1-48.
