# This file ensures that the data tables have been downloaded from the Open Science Framework
# repository https://osf.io/4ywvq/ (Walsh et al. 2022), into the working directory lsc_dbs_wq

# The code requires that you have cloned the associated github repository https://github.com/cjbwalsh/lsc_dbs_wq
# to your working computer.  To clone the repository with RStudio, follow the following steps. 
# Copy the following URL to your clipboard: https://github.com/cjbwalsh/lsc_dbs_wq
# Open RStudio on your local computer. Click File, New Project, Version Control, Git. Paste the repository URL 
# and enter TAB to move to the Project directory name field. Click Create Project.

# Walsh, C. J., Imberger, S. J., Burns, M. J., Fletcher, T. D. & Bos, D. G. (2022), 
# Dispersed urban-stormwater control improved stream water quality in a 
# catchment-scale experiment/data and code, Open Science Framework. https://osf.io/4ywvq

source("code/download.OSF.file.R")
# Check if all relevant spatial files have been downloaded: if not download them
files_in_datadir <- dir(here::here("data"))
all_data_files <- c("wq_data_compiled.xlsx", "prediction_set.xlsx", "oe_list_8vars.rda",
                    "figS1-2_data.rda")
guids <- c("xz8us","e7uxm","kcfrq","5c8qe")
guids <- guids[!all_data_files %in% files_in_datadir]
files <- all_data_files[!all_data_files %in% files_in_datadir]
if (length(files) > 0) {
  for (i in 1:length(files))
    download.OSF.file(GUID = guids[i],
                      Access_Token = "https://osf.io/4ywvq/?view_only=d06f349882224ae6bd123b6f45145afd",
                      file_name = files[i], subdir = "data")
}
