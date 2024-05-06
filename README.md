
# InvestigateIntegrations

<!-- Badges start -->
[![Tests](https://github.com/agmcfarland/InvestigateIntegrations/actions/workflows/test-build.yml/badge.svg)](https://github.com/agmcfarland/InvestigateIntegrations/actions/workflows/test-build.yml)
[![codecov](https://codecov.io/gh/agmcfarland/InvestigateIntegrations/branch/master/graph/badge.svg)](https://codecov.io/gh/agmcfarland/InvestigateIntegrations?branch=master)
<!-- Badges end -->

A suite of workflows to analyze the distribution of integration site in different genomic features.

# Installation

## Dependencies

[hotROCs](https://rdrr.io/github/BushmanLab/hotROCs/)

```R
devtools::install_github('https://github.com/agmcfarland/hotROCs')
```

## Package

```R
devtools::install_github('agmcfarland/InvestigateIntegrations')
```

# Workflows

1. hotROCs R values, with heatmap visualization.

2. Analysis of percentage integrations counts using bootstrapped simulations, with distribution visualization