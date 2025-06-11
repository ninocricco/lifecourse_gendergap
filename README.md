# Gender Pay Gap Starting Points and Life Course Divergence

This repository contains code and data for "Trends in the Gender Pay Gap: Narrowing Starting Points and Persistent Life Course Divergence", by Alexandra A. Killewald and Nino Cricco.

**Files maintained by:** Nino Cricco

## Data Sources

The analysis uses data from the Current Population Survey, available on IPUMS at https://cps.ipums.org/
- **Annual Social and Economic Supplement (ASEC)**: Annual earnings data
- **Outgoing Rotation Group (ORG)**: Monthly earnings data with detailed wage information 

## Analysis Pipeline

### 1. Setup and Data Loading (0-*.R)
- **0-libraries.r**: Loads required R packages (tidyverse, Hmisc, gridExtra, viridis, broom, janitor, haven)
- **0-helperfunctions.R**: Contains utility functions used throughout the project

### 2. Data Processing (1-*.R)
- **1-producedata-asec.R**: Processes ASEC data to create analytic sample with variable recoding, filtering, and inflation adjustment
- **1-producedata-org.R**: Processes ORG data to create analytic sample with variable recoding, filtering, and inflation adjustment. Modifications include:
  - Hours worked imputation for "hours vary" responses (post-1994)
  - Top-coding using log-normal imputation to adjust for changes to the CPS topcode
- **2-producedata-org-selectionweights.R**: Creates inverse probability weights to adjust for selection into the ORG sample

### 3. Analysis Functions (3-*.R)
- **3-data-prep-functions.R**: Functions for preparing data for visualization
- **3-figures-functions.R**: Functions for creating standardized figures with common themes and color palettes

### 4. Bootstrap Estimation (4-*.R)
- **4-estimate-bootstrap-main.R**: Bootstrap estimation for main results
- **4-estimate-bootstrap-appendix.R**: Appendix bootstrap estimation
- **4-estimate-bootstrap-asec-appendix.R**: ASEC-specific appendix bootstrap estimation

## High Performance Computing

The project includes SLURM batch scripts for running computationally intensive bootstrap analyses on HPC clusters:
- **run_main_bootstrap_array.sbatch**: Main bootstrap analysis array job
- **run_appendix_bootstrap_array.sbatch**: Appendix bootstrap analysis array job  
- **run_asec_appendix_bootstrap_array.sbatch**: ASEC appendix bootstrap analysis array job

## Contact

For questions about the code or methodology, please contact me at ncricco@g.harvard.edu
