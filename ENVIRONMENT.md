# Computational environment

The analysis is implemented in R. Package dependencies are declared/loaded in `03_code/run_full_analysis.R`.

For a frozen archival release, run the final script in a clean R session and save `sessionInfo()` or `sessioninfo::session_info()` to `07_logs/session_info.txt`.

Credentials such as UN Comtrade API keys must be stored only in local environment variables and must never be committed.
