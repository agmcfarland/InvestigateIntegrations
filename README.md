
# InvestigateIntegrations

<!-- Badges start -->
[![Tests](https://github.com/agmcfarland/InvestigateIntegrations/actions/workflows/test-build.yml/badge.svg)](https://github.com/agmcfarland/InvestigateIntegrations/actions/workflows/test-build.yml)
[![codecov](https://codecov.io/gh/agmcfarland/InvestigateIntegrations/graph/badge.svg?token=NPALNGNUFJ)](https://codecov.io/gh/agmcfarland/InvestigateIntegrations)
<!-- Badges end -->

A suite of workflows to analyze the distribution of integration site in different genomic features.

# Installation

## Dependencies

[hotROCs](https://rdrr.io/github/BushmanLab/hotROCs/)

```R
devtools::install_github('agmcfarland/hotROCs')
```

## Package

```R
devtools::install_github('agmcfarland/InvestigateIntegrations')
```

# Workflows

1. hotROCs R values, with heatmap visualization.

2. Analysis of percentage integrations counts using bootstrapped simulations, with distribution visualization.

3. Genomic repeat categorization.

	- Formatting multihit clusters into repeat tables.

	- Combine with unqiue integration repeat calls.

4. Annotate integration sites with RefSeq information.