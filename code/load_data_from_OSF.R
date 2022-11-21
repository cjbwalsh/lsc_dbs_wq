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

library(osfr); library(dplyr)
# Check if all relevant spatial files have been downloaded: if not download them
files_in_datadir <- dir(here::here("data"))
files_on_osf <- osf_retrieve_node("4ywvq") %>% osf_ls_files(n_max = 20)
files <- files_on_osf$name[!files_on_osf$name %in% files_in_datadir]
if(length(files) > 0)
osf_download(files_on_osf[files_on_osf$name %in% files,], path = "data")

data_files <- dir("data")
if(!"fig1_data" %in% data_files ){system("mkdir data/fig1_data")}

library(osfr); library(dplyr)
gpkg_files <- dir(here::here("data/fig1_data"))[grep("gpkg",dir(here::here("data/fig1_data")))]
all_gpkgs <- c("Australia_GDA94_GCS.gpkg","catIA.gpkg","cats.gpkg","ia.gpkg",
               "lsc_dbs_streams.gpkg","parcels.gpkg","scms.gpkg","siteLabels.gpkg",
               "sites.gpkg","streams.gpkg","subcs.gpkg","Victoria_GDA94_GCS.gpkg")
gpkg_files <- all_gpkgs[!all_gpkgs %in% gpkg_files]
if (length(gpkg_files) > 0) {
  spatial_files <- osf_retrieve_node("3um4v") %>% osf_ls_files(n_max = 20)
  osf_download(spatial_files, path = "data/fig1_data")
}

# load non-spatial files
# # 1 large file on OSF
# for (i in 1:length(all_gpkgs)) {
#   temp <- sf::st_read(here::here("data/fig1_data",paste(all_gpkgs[i],sep = "")), 
#                       stringsAsFactors = FALSE, quiet = TRUE)
#   temp <- sf::st_set_geometry(sf::st_set_geometry(temp,NULL), sf::st_geometry(temp)) # set geometry, return sf
#   assign(gsub(".gpkg","",all_gpkgs[i]),temp)
# }
# small files kept on Walsh 2022 PLOS Water github repository
if(!"db_non_sf.rda" %in% dir("data"))
download.file("https://github.com/cjbwalsh/lsc_dbs_scms/raw/master/data/db_non_sf.rda","data/db_non_sf.rda")
if(!"ei_ts.rda" %in% dir("data"))
  download.file("https://github.com/cjbwalsh/lsc_dbs_scms/raw/master/data/ei_ts.rda","data/ei_ts.rda")
if(!"final_stats.rda" %in% dir("data"))
  download.file("https://github.com/cjbwalsh/lsc_dbs_scms/raw/master/data/final_stats.rda","final_stats.rda")
#download.file("https://github.com/cjbwalsh/lsc_dbs_scms/raw/master/data/scmProjects_EB.rda","scmProjects_EB.rda)
rda_files <-  dir(here::here("data"))[grep(".rda",dir(here::here("data")))]
for (i in 1:length(rda_files)) {
  load(here::here("data",rda_files[i]))
}



