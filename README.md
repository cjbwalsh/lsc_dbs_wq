### Code and data for Walsh et al. (2022). "Dispersed urban-stormwater control improved stream water quality in a catchment-scale experiment"  

This paper used hierarchical linear models to assess the effects of stormwater control measures dispersed across catchments on the water quality of downstream streams.  We developed models of total phosphorus, filterable reactive phosphorus, total nitrogen, nitrate + nitrite, ammonium, total suspended solids, electrical conductivity and temperature.

`wq_manuscript.Rmd` and `wq_appendix_S1.Rmd` contain the source code (RMarkdown) for the manuscript and supplementary material, respectively. The seven `wq_methods_*.Rmd` documents contain the code for assembling the data, and compiling, selecting, sampling and interpreting the hierarchical linear models: their content is described in `wq_appendix_S1.Rmd`.

`lsc_dbs_wq.bib`, `ecol-app.csl`, and `officedown_template.docx` are used in knitting the Rmd documents.  `code/misc_functions.R` contains functions used for interpreting and plotting the data. The `data` directory contains small data files used in the analyses. Larger data files are stored in the Open Science Framework (OSF) repository [https://osf.io/4ywvq/](https://osf.io/4ywvq/).  Assuming you are working in a clone or fork of this github repository, the script `load_ld_scms_tables.R` downloads the data from the OSF repository. (This file is sourced, and the data downloaded, if you run or knit most of the Rmd files.)  

The documents require the following R packages to be installed: "readxl","rethinking","scales","flextable","RColorBrewer",
"here","knitr","ggsci","lubridate","rstan", "loo".
