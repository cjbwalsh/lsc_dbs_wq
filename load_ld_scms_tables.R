# This file ensures that all large data tables have been downloaded from the Open Science Framework
# repository https://osf.io/57azq/ (Walsh et al. 2022a), into the working directory lsc_dbs_wq/data/fig1_data
# for the preparation of Fig.1 in Walsh et al. (2022b)

# The code requires that you have cloned the associated github repository https://github.com/cjbwalsh/lsc_dbs_wq
# to your working computer.  To clone the repository with RStudio, follow the following steps. 
# Copy the following URL to your clipboard: https://github.com/cjbwalsh/lsc_dbs_wq
# Open RStudio on your local computer. Click File, New Project, Version Control, Git. Paste the repository URL 
# and enter TAB to move to the Project directory name field. Click Create Project.

# Walsh, C. J., Burns, M. J., Fletcher, T. D., Bos, D. G., Kunapo, J., Poelsma, P., & Imberger, M. (2022a), 
# Linking stormwater control performance to stream ecosystem outcomes: incorporating a performance metric into 
# effective imperviousness/Data and code, Open Science Framework. https://osf.io/57azq
#
# Walsh, C. J., Imberger, M., Burns, M. J., Fletcher, T. D., & Bos, D. G. (2022b). 
# Dispersed Urban-Stormwater Control Improved Stream Water Quality in a 
# Catchment-Scale Experiment. Water Resources Research.

source("code/BACRIfunctions.R")
# Check if all relevant spatial files have been downloaded: if not download them
# The following files (loaded above) are derived from the database tables using code in the Rmd files
# ei_ts.rda - see derivation in ei_ts chunk of WalshEtAl_wrr2021_S1-2.Rmd

#Tidy up some of the data ready for analysis and plotting
SCMs <- scms; rm(scms)
#add Little Stringybark and Dobsons impervious data to other catchments
# LIS0004 and LIS0004H are adjacent reaches monitored for different purposes
# This paper uses the downstream one, so include both as the same subc
ia$subc <- subcs$trib[match(ia$pipeID, subcs$pipeID)]
ia$subc[ia$subc == "LIS0004H"] <- "LIS0004"  
# A portion of Bailey Rd was diverted to LSS0001 late in the experiment by 
# on of our SCMs. This is accounted for in EI variant calculations. For 
# representation of impervious coverage, leave it in its original state.
ia$subc[ia$subc == "BaileyRd"] <- "LIS0004"  
#update ia polygons' connection status to that at end of the study period
db_2019 <- data_on_datex(71, "2019-12-01")
db_2019$ia <- db_2019$ia[db_2019$ia$polyID %in% ia$polyID,]
for(i in 1:length(db_2019$ia$polyID)){
  if(ia$conn[ia$polyID == db_2019$ia$polyID[i]] != db_2019$ia$conn[i])
    ia$conn[ia$polyID == db_2019$ia$polyID[i]] <- db_2019$ia$conn[i]
}
# The manually drawn subc boundaries for DBS are not an exact match to the 
# cat polygons derived from the DEM.  As a result, slight differences in 
# catchment area estimates. Make consistent for EI calculations.
subcs$scarea[subcs$pipeName == "DBS0004"] <- cats$carea_m2[cats$sitecode == "DBS0004"] - 
  sum(subcs$scarea[subcs$pipeID %in% c(102,106,107)])
subcs$scarea[subcs$pipeName == "DBS0008"] <- cats$carea_m2[cats$sitecode == "DBS0008"] - 
  sum(subcs$scarea[subcs$pipeID %in% c(101,102,104:108)])

catIA <- rbind(ia, catIA[names(catIA) != "address"])
#minor differences in sampling reaches over time and for different purposes, 
#and minor changes in catchment boundaries with some SCMs.  Select original hydrology sites for map
catMap11 <- cats[cats$hydrology == 1 & cats$origDrain == 1,]  
#Hydrology sites, pre-the Entrance, excluding 2 pipe sites
catMap11$col <- RColorBrewer::brewer.pal(3,"Dark2")[match(catMap11$treatment,c("R","E","C"))]
catMap11$sitecode <- substr(as.vector(catMap11$sitecode),1,7)
sites <- sites[!sites$sitecode %in% c("Heath","Wicks"),]
siteMap11 <- sites[sites$hydrology == 1,]
siteMap11$sitecode <- substr(siteMap11$sitecode,1,7)
siteMap11$col <- catMap11$col[match(siteMap11$sitecode, catMap11$sitecode)]
load(here::here("data","/ei_ts.rda"))

ei_ts_all <- rbind(
  data.frame(sitecode = "LSN0001", ei_53$iats[,c("date","ti","ei","eb","wq","fv","vr","ro","s")],
             stringsAsFactors = FALSE),
  data.frame(sitecode = "LSS0001", ei_36$iats[,c("date","ti","ei","eb","wq","fv","vr","ro","s")],
             stringsAsFactors = FALSE),
  data.frame(sitecode = "LIS0001", ei_74$iats[,c("date","ti","ei","eb","wq","fv","vr","ro","s")],
             stringsAsFactors = FALSE),
  data.frame(sitecode = "LIS0004", ei_71$iats[,c("date","ti","ei","eb","wq","fv","vr","ro","s")],
             stringsAsFactors = FALSE),
  data.frame(sitecode = "DBS0004", ei_101$iats[,c("date","ti","ei","eb","wq","fv","vr","ro","s")],
             stringsAsFactors = FALSE),
  data.frame(sitecode = "DBS0008", ei_103$iats[,c("date","ti","ei","eb","wq","fv","vr","ro","s")],
             stringsAsFactors = FALSE))
ei_ts <- ei_ts_all[c("sitecode","date","ti","ei")]

#### Sa aka SAS0002 ####
sasIA <- catIA[catIA$subc == "SAS0002",]
# Time between first and last nearmap images used to QA the data
ndays <- as.numeric(ymd("2019-12-31"),ymd("2009-10-12"))
# proportional growth in impervious area over that time
sasGrowthProp <- ((sum(sasIA$area_m2) - sum(sasIA$area_m2[is.na(sasIA$constructionDate)]))/sum(sasIA$area_m2))
# estimate growth rate so that it can be applied below to OLN
# assume exponential growth: P(t) = P(0)e^rt, so r = ln(Pt/Po)/t
sasTIGrowthRate <- log(sum(sasIA$area_m2) /
                         sum(sasIA$area_m2[is.na(sasIA$constructionDate)])) / ndays
# Very small (3.7e-7), and all of this growth in TI was not connected to stormwater
# Only increase in connected ia was one new carpark in 2010..so leave it as a step.
constructionDates <- sasIA[!is.na(sasIA$constructionDate),]
constructionDates <- constructionDates[order(constructionDates$constructionDate),]
# create a data frame with daily steps the same as calculated for experimental catchments
sasts <- data.frame(date = ei_101$iats$date)
sasts$ti <- tii <- sum(sasIA$area_m2[is.na(sasIA$constructionDate)]) /
  cats$carea_m2[cats$sitecode == "SAS0002"]
for (i in 1:length(constructionDates$constructionDate)) {
  sasts$ti[sasts$date >= constructionDates$constructionDate[i]] <- 
    tii <- tii + constructionDates$area_m2[i]/cats$carea_m2[cats$sitecode == "SAS0002"]
}
# and just one step increase for EI
sasts$ei <- eii <- as.numeric(sum(sasIA$area_m2[sasIA$conn == 1])/
                                cats$carea_m2[cats$sitecode== "SAS0002"])
sasts$ei[sasts$date < constructionDates$constructionDate[constructionDates$conn == 1]] <- 
  eii - as.numeric(constructionDates$area_m2[constructionDates$conn == 1])/
  as.numeric(cats$carea_m2[cats$sitecode== "SAS0002"])
ei_ts <- rbind(ei_ts, data.frame(sitecode = "SAS0002", sasts, 
                                 stringsAsFactors = FALSE))

##### Ol aka OLN0009 ####
# #  DBS catchment is similar to OLN and upper part of Ferny so assume a similar 
# #  level of error in those catchments. However, apply correction factor to 
# #  properties only.  CW redrew and corrected roads in this catchment using areal 
# #  imagery. To calculate error factor for DBS catchment, use the original area 
# #  estimates (with prefixes 'JK': not included in final data published to OSF. 
# #  Commented out lines read from original database on unimelb server)
# DBSparcels <- parcels[parcels$pipeID > 100,]
# db_dbs_parcels <- sqlQuery("SELECT * FROM parcels WHERE \"pipeID\" > 100;", "lsc_dbs_scms")
# db_dbs_parcels <- db_dbs_parcels[match(DBSparcels$parcelID,
#                                                 db_dbs_parcels$parcelID),]
# db_dbs_parcels$tiaJK <- db_dbs_parcels$JKroofArea + db_dbs_parcels$JKpaveArea
# DBSparcels$tia <- DBSparcels$roofAreaCon + DBSparcels$roofAreaUncon + 
#                     DBSparcels$paveAreaCon + DBSparcels$paveAreaUncon
# # overall IA underestimate in Db (used for Fe below_)
# jkDbUnderFactor <- sum(db_dbs_parcels$tiaJK)/sum(DBSparcels$tia)
# # property IA underestimate in Db (used for TI estimate in Ol below)
# jkPropUnderFactor <- sum(db_dbs_parcels$tiaJK[db_dbs_parcels$parcelType == "property"])/
#
jkDbUnderFactor <- 0.8593016
jkPropUnderFactor <- 0.7048386

olnIA <- catIA[catIA$subc == "OLN0009",]
olnTI2009 <- as.numeric((sum(olnIA$area_m2[is.na(olnIA$constructionDate) & 
                                             olnIA$surfType != "road"])/jkPropUnderFactor + 
                           sum(olnIA$area_m2[is.na(olnIA$constructionDate) & 
                                               olnIA$surfType == "road"]))/
                          cats$carea_m2[cats$sitecode == "OLN0009"])
# CW manually corrected the (few) connected impervious polygons using aerial imagery, 
# so no correction factor for ei
olnEI2009 <- as.numeric(sum((olnIA$area_m2*olnIA$conn)[is.na(olnIA$constructionDate)])/
                          (cats$carea_m2[cats$sitecode == "OLN0009"]))
olnts <- data.frame(date = ei_101$iats$date)
# Assume growth similar to the adjacent SAS catchment over the study period (i.e. very little)
olnts$ti <- olnTI2009*exp(sasTIGrowthRate*as.numeric(ei_101$iats$date - dmy("17-07-2009")))
#there was no ei growth over the study period in this catchment....
olnts$ei <- olnEI2009
ei_ts <- rbind(ei_ts, data.frame(sitecode = "OLN0009", olnts, 
                                 stringsAsFactors = FALSE))

##### Ly aka LYR0007 ####
lyrIA <- catIA[catIA$subc == "LYR0007",]
lyrEI2009 <- sum(lyrIA$area_m2[lyrIA$conn == 1])/(cats$carea_m2[cats$sitecode== "LYR0007"])
lyrTI2009 <- sum(lyrIA$area_m2[is.na(lyrIA$constructionDate)])/(cats$carea_m2[cats$sitecode== "LYR0007"])
constructionDates <- lyrIA[!is.na(lyrIA$constructionDate),]
lyrts <- data.frame(date = ei_101$iats$date)
# Only one new building in the catchment in the last 20 years, Oct 2013
lyrts$ti <- sum(lyrIA$area_m2)/cats$carea_m2[cats$sitecode== "LYR0007"]
lyrts$ti[lyrts$date >= constructionDates$constructionDate] <- lyrts$ti[1] + 
  constructionDates$area_m2/cats$carea_m2[cats$sitecode== "LYR0007"]
# EI consistently zero 
lyrts$ei <- as.numeric(sum(lyrIA$area_m2[lyrIA$conn == 1])/
                         cats$carea_m2[cats$sitecode== "LYR0007"])
ei_ts <- rbind(ei_ts, data.frame(sitecode = "LYR0007", lyrts, 
                                 stringsAsFactors = FALSE))

#Control sites----
##Br aka BRS0015
# # Correction factor derived from QAQC of portions of Brushy Creek catchment
# # (not included in OSF repository)
# qa_dir <- "~/uomShare/wergSpatial/Projects/LSC/foundationPaper/data/ESRI/BRS_QA/"
# brsSubCat <- sf::st_read(paste0(qa_dir, "BrushyUpperCat.shp"))
# brsTIA_JK <- sf::st_read(paste0(qa_dir, "BrushyTIA_JK.shp"))
# brsTIA <- sf::st_read(paste0(qa_dir, "BrushyTIA_AL.shp"))
# brsTIA_JK$area_m2 <- sf::st_area(brsTIA_JK)
# brsTIA$area_m2 <- sf::st_area(brsTIA)
# brsTIA$constructionDate <- lubridate::ymd(brsTIA$X.constDate)
# brsTIA$constYear <- lubridate::decimal_date(brsTIA$constructionDate)
# #calculate correction on those polygons not identified as new in QA
# brsJKCorrection <- as.numeric(sum(brsTIA_JK$area_m2)/
#                               sum(brsTIA$area_m2[is.na(brsTIA$constYear)]))
brsJKunderFactor <- 0.9327931  #For use in Fe below
# #Time between first and last nearmap images used to QA the data
# nyears <- as.numeric(as.Date(lubridate::ymd("2019-12-31")) - 
#                    as.Date(lubridate::dmy("12-10-2009")))/365.25
# #assume exponential growth: P(t) = P(0)e^rt, so r = ln(Pt/Po)/t
# brsTIGrowthRate <- as.numeric(log(sum(brsTIA$area_m2)/
#                       sum(brsTIA$area_m2[is.na(brsTIA$constructionDate)]))/nyears)
# # growth in the test area (0.0098) is likely to be ~150% higher than most parts of the catchment
brsTIGrowthRate <- 0.009751495*0.67
brs_IA_map <- catIA[catIA$subc == "BRS0015",]
#correct TI estimate based on the checked portion
brs_IA_map$area_m2_corrected <- brs_IA_map$area_m2/brsJKunderFactor
# assume those parts of the catchment serviced by stormwater drainage are 90% connected
brs_TI_2009 <- sum(brs_IA_map$area_m2_corrected) /
  cats$carea_m2[cats$sitecode == "BRS0015"]
brs_EI_2009 <- sum(brs_IA_map$area_m2_corrected*brs_IA_map$conn*0.9) /
  cats$carea_m2[cats$sitecode == "BRS0015"]
brsts <- data.frame(date = ei_101$iats$date)
brsts$ti <-  brs_TI_2009*exp(brsTIGrowthRate*(lubridate::decimal_date(brsts$date) -  
                                                lubridate::decimal_date(lubridate::dmy("17-07-2009"))))
brsts$ei <- brs_EI_2009*exp(brsTIGrowthRate*(lubridate::decimal_date(brsts$date) -  
                                               lubridate::decimal_date(lubridate::dmy("17-07-2009"))))
ei_ts <- rbind(ei_ts, data.frame(sitecode = "BRS0015", brsts, 
                                 stringsAsFactors = FALSE))

#Fe aka FER0006
ferIA <- catIA[catIA$subc == "FER0006",]
# Ferny is a mix of denser suburbs like Br and more treed suburbs like Db, 
# so take the mean of their (similar) correction factors
ferCorrFactor <- mean(brsJKunderFactor, jkDbUnderFactor)
ferTI2009 <- sum(ferIA$area_m2[is.na(ferIA$constructionDate)])/(cats$carea_m2[cats$sitecode== "FER0006"]*ferCorrFactor)
ferEI2009 <- sum(ferIA$area_m2[ferIA$conn == 1]*0.9)/(cats$carea_m2[cats$sitecode== "FER0006"]*ferCorrFactor)
ferts <- data.frame(date = ei_101$iats$date)
#growth in FER likely to be 0.5 of that in BRS
ferts$ti <-  ferTI2009*exp(0.5*brsTIGrowthRate *
                             as.numeric(lubridate::decimal_date(ferts$date) -    
                                          lubridate::decimal_date(lubridate::dmy("17-07-2009"))))
ferts$ei <-  ferEI2009*exp(0.5*brsTIGrowthRate *
                             as.numeric(lubridate::decimal_date(ferts$date) -    
                                          lubridate::decimal_date(lubridate::dmy("17-07-2009"))))
ei_ts <- rbind(ei_ts, data.frame(sitecode = "FER0006", ferts, 
                                 stringsAsFactors = FALSE))
#Remove temporary objects
rm(brs_IA_map, brsts, ferIA, ferts, lyrIA, lyrts, olnIA, olnts, sasIA, sasts, 
   brs_EI_2009,brs_TI_2009,brsJKunderFactor,brsTIGrowthRate, 
   eii,ferCorrFactor,ferEI2009,ferTI2009,jkDbUnderFactor, 
   jkPropUnderFactor,lyrEI2009,lyrTI2009,ndays,olnEI2009,olnTI2009, rda_files,
   sasGrowthProp,sasTIGrowthRate,tii)


