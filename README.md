# Scaling under shock — replication package

Replication materials for the manuscript **“Scaling under shock: throughput pressure and transactional reconfiguration in Ukraine's drone-related import network.”**

This upload-ready package contains the verified project files retrieved from the research folder plus documentation for public release. Raw UN Comtrade cache/download files are intentionally not redistributed here. The analysis script documents the reconstruction workflow.

## Structure
- `01_data_documentation/`: HS correspondence and macro-block dictionaries.
- `02_processed_data/`: processed analytical datasets included in this package.
- `03_code/`: full R analysis script.
- `04_results/`: selected reproducible analytical outputs.
- `05_figures/`: reserved for final publication figures.
- `06_networks/`: reserved for final network exports.
- `07_logs/`: reserved for reproducibility logs.

## Reproduction
1. Install R and the packages required at the beginning of `03_code/run_full_analysis.R`.
2. If reconstruction from UN Comtrade is required, configure the API credential locally as an environment variable. Never commit credentials.
3. Run the script from the repository root.
4. Compare generated outputs with the supplied processed data/results.

## Data boundary
The study uses international trade records and measures observable cross-border transaction architecture. These data should not be interpreted as direct measures of domestic production, operational capacity, contractual lead times, or battlefield outcomes.

## Citation
Please cite the manuscript and the archived replication package. Add the final Zenodo DOI after deposit.
