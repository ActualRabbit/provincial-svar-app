# Provincial SVAR Shiny App

Interactive visualization of monetary policy transmission across Canadian provinces using Structural Vector Autoregression (SVAR).

## Features

- **Live SVAR Estimation**: Estimate models for any Canadian province with configurable lag order
- **Impulse Response Functions**: Visualize how shocks propagate through the economy
- **Provincial Comparison**: Compare multiple provinces side-by-side
- **Credit Channel Analysis**: Examine why some provinces are more sensitive than others
- **Data Explorer**: Inspect underlying time series data

## Data Sources

- **Statistics Canada CANSIM**: Employment, inflation, GDP
- **Bank of Canada**: Commodity Price Index (BCPI), Treasury Bill rates

Data is cached locally to reduce API calls.

## Local Development

```bash
# Install R dependencies
R -e "install.packages(c('shiny', 'bslib', 'vars', 'cansim', 'tidyverse', 'plotly', 'markdown'))"

# Run the app
R -e "shiny::runApp('app.R', port = 3838)"
```

## Deploy to Fly.io

```bash
# First time setup
fly launch --no-deploy
fly volumes create svar_cache --region yyz --size 1

# Deploy
fly deploy

# View logs
fly logs
```

## Model Specification

The SVAR uses recursive (Cholesky) identification with variables ordered:

1. Commodity Price Index (most exogenous)
2. Interest Rate (1-month T-Bill)
3. Core Inflation
4. GDP
5. Employment (least exogenous)

Sample period: 1992-present (post-inflation-targeting regime)

## Key Findings

| Province | Interest Rate Sensitivity |
|----------|--------------------------|
| Ontario | High (-0.13%) |
| Newfoundland | High (-0.13%) |
| Quebec | Medium (-0.10%) |
| Alberta | Low (-0.02%) |
| British Columbia | Low (-0.03%) |

Manufacturing intensity predicts sensitivity; firm size does not.

## References

Based on research examining differential regional effects of monetary policy, following methodology from Carlino & Defina (1998) and Owyang & Wall (2006).
