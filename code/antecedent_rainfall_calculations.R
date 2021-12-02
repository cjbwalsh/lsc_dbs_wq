#Dupicate samplecodes need revising in database (problem avoided here by using DISTINCT)
db_sitecodes <- c("BRS0015", "FER0006", "LIS0001", "LSS0001" ,"LSN0001",
                  "LIS0004", "DBS0004","SAS0002", "LYR0007", "OLN0009","DBS0008")  
samples <- sqlQuery(paste("SELECT DISTINCT samplecode, sitecode, dateTime6min
                          FROM wqSamples
                          WHERE sitecode IN ('",
                          paste(db_sitecodes, collapse = "', '"),
                          "');", sep = ""), db = "lsc")
samples$dateTime6min <- lubridate::ymd_hms(samples$dateTime6min)
samples$rainsite <- samples$sitecode
samples$rainsite[samples$sitecode == "DBS0008"] <- "DBS0004"
samples$rainsite[samples$sitecode %in%
                     c("LIS0001","LSN0001","LSS0001")] <- "LIS0004"
rain <- data.table::data.table(sqlQuery("SELECT sitecode, dateTime, Rain_depth FROM rainfall;", "lsc")) #12 s
rain$dateTime <- lubridate::ymd_hms(rain$dateTime, tz = "UTC")

anterain <- function(samplecodei,antePeriodDays = 1){ 
  #expw1
  #expw2
  #expw3 
samplei <- samples[samples$samplecode == samplecodei,]
x <- rain[rain$sitecode == samplei$rainsite & rain$dateTime < samplei$dateTime6min &
            rain$dateTime >= samplei$dateTime6min - lubridate::days(antePeriodDays),]
data.frame(ar_sum = sum(x$Rain_depth),
           ar_linw = sum(x$Rain_depth*(1:(dim(x)[1]))/dim(x)[1]),
           ar_expw1 = sum(x$Rain_depth*exp((dim(x)[1]:1)/-(dim(x)[1]*3))),
           ar_expw2 = sum(x$Rain_depth*exp((dim(x)[1]:1)/-(dim(x)[1]*6))),
           ar_expw3 = sum(x$Rain_depth*exp((dim(x)[1]:1)/-(dim(x)[1]*30)))
           )
}
ar <- data.table::data.table(data.frame(samplecode = samples$samplecode, anterain1 = NA,
       anterain90l = NA, anterain90e1 = NA, anterain90e2 = NA, anterain90e3 = NA,
       anterain180l = NA, anterain180e1 = NA, anterain180e2 = NA, anterain180e3 = NA,
       anterain365l = NA, anterain365e1 = NA, anterain365e2 = NA, anterain365e3 = NA))
system.time({
for(i in 1:dim(samples)[1]) {
    ar$anterain1[i] <- anterain(samples$samplecode[i], antePeriodDays = 1)$ar_sum
    ar90 <- anterain(samples$samplecode[i], antePeriodDays = 90)
    ar$anterain90 <- ar90$ar_sum
    ar$anterain90l[i] <- ar90$ar_linw
    ar$anterain90e1[i] <- ar90$ar_expw1
    ar$anterain90e2[i] <- ar90$ar_expw2
    ar$anterain90e3[i] <- ar90$ar_expw3
    ar180 <- anterain(samples$samplecode[i], antePeriodDays = 180)
    ar$anterain180 <- ar180$ar_sum
    ar$anterain180l[i] <- ar180$ar_linw
    ar$anterain180e1[i] <- ar180$ar_expw1
    ar$anterain180e2[i] <- ar180$ar_expw2
    ar$anterain180e3[i] <- ar180$ar_expw3
    ar365 <- anterain(samples$samplecode[i], antePeriodDays = 365)
    ar$anterain365 <- ar365$ar_sum
    ar$anterain365l[i] <- ar365$ar_linw
    ar$anterain365e1[i] <- ar365$ar_expw1
    ar$anterain365e2[i] <- ar365$ar_expw2
    ar$anterain365e3[i] <- ar365$ar_expw3
  }
}) #I played around with apply rather than loops. No quicker.
save(ar, file = "~/uomShare/wergStaff/ChrisW/git-data/lsc_dbs_wq/data/antecedent_rain_wq_samples.rda", compress = "xz")
