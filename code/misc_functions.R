# table_for_word() ------------------------
# Function for formatting tables when knitting to word from RMarkdown
table_for_word <- function(input_table, font = "Helvetica", pgwidth = 6.69){  
  ft <- flextable::regulartable(input_table)
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::font(ft,fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::padding(ft, padding.top = 2,  padding.bottom = 2, part = "all")
  ft <- flextable::autofit(ft)
  # fit to window for MS Word (adapted from
  # https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar)
  ft <- width(ft, width = dim(ft)$widths*pgwidth /(flextable_dim(ft)$widths))
  ft
}

# From R help(paris)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


# ct() ---------------------------------
#' Comparable to spread in tidyverse
ct <- function (rows, cols, values = NULL, FUN = sum, convertNAToZero = TRUE,...) 
{
  if(!is.vector(rows)) rows <- as.vector(rows)
  if(!is.vector(cols)) cols <- as.vector(cols)
  if(is.null(values)) values <- rep(1,length(rows))
  results <- tapply(values, list(rows, cols), FUN, ...)
  if(convertNAToZero)
    results[is.na(results)] <- 0
  as.data.frame(results)
}

#' Connect to a unimelb WERG database (on water-dev server)
#' 
#' @param db Database name, default "mwstr". Other available databases include
#'      "mwbugs", "lsc_dbs_scms", and "lsc" (the last being a MySQL database)
#' @param user User name: either "readonly", the default or "root". The latter
#'      will only work if you have encrypted root credentials in your home 
#'      directory
#' @return a connection to the specified database
#' @examples
#'      mwstr_db <- connect_to_db()

connect_to_db <- function(db, # or "mwbugs" or "lsc" or "lsc_dbs_scms", or ...  
                          user = "readonly" # or "root" - the latter will only work on water-dev for admin users
){
  # creating DB connection object with RPostgreSQL package
  hostname <- ifelse(Sys.info()["nodename"] == "water-dev.its.unimelb.edu.au",
                     "localhost", "water-dev.its.unimelb.edu.au")
  if(db %in% c("lsc_dbs_scms","mwstr","mwbugs","mwstr_dev","sunbury","wetlandia","mwb","scratch")){
    drv <- RPostgreSQL::PostgreSQL()
    if(user == "readonly")
      conn <- RPostgreSQL::dbConnect(drv, dbname = db,
                                     host = hostname, port = 5432,
                                     user = "readonly", password = "reachcode_42")  
    if(user == "root")
      conn <- RPostgreSQL::dbConnect(drv, dbname = db)
  }
  if(db %in% c("lsc")){
    if(user == "readonly")
      conn <- dbConnect(RMySQL::MySQL(), host = "127.0.0.1", user = "readonly",
                        password = "reachcode_42",dbname= db, port = 3306)
    if(user == "root")  #Doesn't work if you don't have .my.cnf in your home directory
      conn <- dbConnect(RMySQL::MySQL(), dbname = db)
  }
  conn
}

#' Apply an SQL query to a specified database
#' 
#' @param query A syntactically correct SQL query
#' @param db Database name, default "mwstr". Other available databases include
#'      "mwbugs", "lsc_dbs_scms", and "lsc" (the last being a MySQL database)
#' @param type Either "get", the default, or "send".  Send queries alter the 
#'      database and are only permitted if user = root
#' @param user User name: either "readonly", the default or "root". The latter
#'      will only work if you have encrypted root credentials in your home 
#'      directory
#' @return a connection to the specified database
#' @examples
#'    sqlQuery("SELECT * FROM stream_names WHERE str_nm = 'BASS RIVER';")

sqlQuery <- function (query,   #Syntactically correct SQL
                      db, # or "mwbugs" or "lsc" or "lsc_dbs_scms" ...  
                      type = "get", #send = dbSendQuery; get = dbGetQuery
                      user = "readonly" # or "root" - the latter will only work on water-dev for admin users
) {
  conn <- connect_to_db(db, user)
  # close db connection after function call exits
  on.exit(dbDisconnect(conn))
  # send Query to obtain result set
  if(type == "get"){
    if(db %in% c("lsc_dbs_scms","mwstr","mwbugs","mwstr_dev","sunbury","wetlandia","mwb","scratch")){
      # but first, check if query concerns a spatial table
      spatialtabs <- as.vector(t(dbGetQuery(conn, 
                                            "select f_table_name from geometry_columns")))
      spatialTRUE <- 0
      for(i in 1:length(spatialtabs)){
        if(grepl(paste("from",spatialtabs[i]), tolower(query)) & !grepl("scmsdecommissioned", tolower(query))) 
          spatialTRUE <- spatialTRUE + 1
      }
      if(spatialTRUE == 0){
        rs <- dbGetQuery(conn, query)
      }
      if(spatialTRUE > 0){
        rs <- suppressWarnings(sf::st_read(conn, query = query))
      }
    }else{
      rs <- dbGetQuery(conn, query)
    }
  }
  if(type == "send")
    rs <- dbSendQuery(conn, query)
  # return the result
  return(rs)
}

#kill the oldest n active connections to database driver drv
kill_n_connections <- function(n = 16, driver = drv){
  if(length(DBI::dbListConnections(driver)) > 0){
    for(i in 1:max(n,length(DBI::dbListConnections(driver)))){
      if(length(DBI::dbListResults(DBI::dbListConnections(driver)[[1]])) > 0)
        DBI::dbClearResult(DBI::dbListResults(DBI::dbListConnections(driver)[[1]])[[1]])
      DBI::dbDisconnect(DBI::dbListConnections(driver)[[1]])
    }
  } 
}


model_diagnostics <- function(stan_fit, nChains = 4){
  mon <- as.data.frame(monitor(stan_fit))
  out <- data.frame(diagnostic = "max_Rhat",
                    value = signif(max(mon$Rhat, na.rm = TRUE),2),
                    pass = ifelse(max(mon$Rhat, na.rm = TRUE) < 1.1,
                                  "Y","Should be <1.1"))
  out <- rbind(out, data.frame(diagnostic = "min_bulk_ESS",
                               value = signif(min(mon$Bulk_ESS/nChains),2),
                               pass = ifelse(min(mon$Bulk_ESS/nChains) >= 100,
                                             "Y",
                                             paste0(paste(row.names(mon)[which(mon$Bulk_ESS/nChains < 100)], collapse = ', '),
                                                    " have Bulk ESS < 100/chain"))))
  out <- rbind(out, data.frame(diagnostic = "min_tail_ESS",
                               value = signif(min(mon$Tail_ESS/nChains),2),
                               pass = ifelse(min(mon$Tail_ESS/nChains) >= 100,
                                             "Y",
                                             paste0(paste(row.names(mon)[which(mon$Tail_ESS/nChains < 100)], collapse = ', '),
                                                    " have Tail ESS < 100/chain"))))
  out
}

plot_mod_comp <- function(mod_comp){
  par(mar = c(4,8,1,1))
  plot(mod_comp$elpd_diff, 1:nrow(mod_comp), 
       xlab = bquote(Delta ~"looIC"),ylab = "", 
       xlim = c(min(mod_comp$elpd_diff - mod_comp$se_diff, na.rm = TRUE),
                max(mod_comp$elpd_diff + mod_comp$se_diff, na.rm = TRUE)),
       ylim = c(0.5,nrow(mod_comp) + 0.5), axes = FALSE, bty = "n")
  for(i in 1:nrow(mod_comp)){
    lines(c(mod_comp$elpd_diff[i] - mod_comp$se_diff[i], 
            mod_comp$elpd_diff[i] + mod_comp$se_diff[i]), rep(i,2))
  }
  axis(2, las = 1, at = 1:nrow(mod_comp), labels = row.names(mod_comp))
  abline(v = 0, lty = 3)
}

coef_plot <- function(input = "summary", #summary or stanfit?
                      stanfit = NA,
                      summary = NA,  #
                      parameters = "all", #Vector of parameter names to plot
                      labels = "parameters", #if not parameters, a vector of same length containing parameter labels
                      ci_prob = 0.95, 
                      ymar = 0.5,  #fraction of max(nchar(labels)) to adjust y axis margin
                        ...){
  if(input == "stanfit"){
  summ <- as.data.frame(summary(stan_fit, 
                  probs = c((1-ci_prob)/2, 0.10,0.5,0.90, 1 - (1-ci_prob)/2))$summary)
  }else{
    summ <- summary
    if("par" %in% names(summ))
      row.names(summ) <- summ$par
    if(!"X2.5." %in% names(summ))
    names(summ)[match(c("2.5%","10%","50%","90%","97.5%"),names(summ))] <- 
      c("X2.5.","X10.","X50.","X90.","X97.5.")
  }
  if(parameters[1] == "all"){
    params <- row.names(summ)
  }else{
      params <- parameters
    }
  params <- rev(params)
  if(labels[1] == "parameters"){
    labels <- params
    }else{  
      labels <- rev(labels)
          }
  opar <- par()
  summ <- summ[match(parameters,row.names(summ)),]
  par(mar = c(4,ymar*max(nchar(labels)),1,1), xaxs="r", yaxs="r")
  plot(c(min(summ$X2.5.),max(summ$X97.5.)),c(1,length(params)), 
       type = 'n', xlab = "coefficient", ylab = "", axes = FALSE) #, ...)
  axis(1)
  axis(2, at = 1:length(params), labels = labels, las= 1)
  box(bty = 'l')
  abline(v = 0, lty = 1, col = gray(0.75))
  abline(h = 1:length(params), lty = 2, , col = gray(0.75))
  for(i in 1:length(params)){
    lines(c(summ$X2.5.[row.names(summ) == params[i]],
            summ$X97.5.[row.names(summ) == params[i]]),c(i,i),
          col = "darkblue", lend = 1)
    lines(c(summ$X10.[row.names(summ) == params[i]],
            summ$X90.[row.names(summ) == params[i]]),c(i,i),
        lwd = 5, col = "darkblue", lend = 1)
  }
  points(summ$mean[match(params, row.names(summ))], 1:length(params), 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  #  suppressWarnings(par(opar))
}

#Variation on coef_plot for multi-panel plot in Fig. 1
coef_plot_fig1 <- function(summ,
                           xticklabs = TRUE,
                           xlabel = ""){
    params <- rev(summ$par)
      labels <- rev(summ$lab)
    par(mar = c(ifelse(xticklabs, 2, 1),6,1,1), xaxs="r", yaxs="r")
    plot(c(min(summ$X2.5.),max(summ$X97.5.)),c(1,length(params)), 
         xlim = c(-1.25,1.75), type = 'n', xlab = xlabel, ylab = "", 
         axes = FALSE) 
    axis(1, labels = ifelse(xticklabs,TRUE,FALSE))
    axis(2, at = 1:length(params), labels = labels, las= 1)
    box(bty = 'l')
    abline(v = 0, lty = 1, col = gray(0.75))
    abline(h = 1:length(params), lty = 2, , col = gray(0.75))
    for(i in 1:length(params)){
      lines(c(summ$X2.5.[summ$par == params[i]],
              summ$X97.5.[summ$par == params[i]]),c(i,i),
              col = "darkblue", lend = 1)
      lines(c(summ$X10.[summ$par == params[i]],
              summ$X90.[summ$par == params[i]]),c(i,i),
              lwd = 5, col = "darkblue", lend = 1)
  }
  points(summ$mean[match(params, summ$par)], 1:length(params), 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
}

counter_restr_rain <- function(response_var, #string of stan fit object name
                               new_X_set,
                               max_restr_level = unique(new_X_set$restr)[2],
                               # 1:7 -1 (a stretch target), -0.49 (achieved in Ln), -0.35 (achieved in Ls),
                               #.    -0.2 (achieved in L4), -0.165 (achieved in D8), - 0.096 (achieved in D4), 0 (no SCMs)
                               degrd_level = unique(new_X_set$degrd)[6],
                               # 1:8 = 0 EI (Ref), 0.3, 1, 2.5 (D4), 6.18 (Ln), 10.2 (L4), 13.336 (Ls), 20
                               add_vars = data.frame(par = c("channel","season","rain365"),
                                                     value = c(0,0,0)),
                               # note if third add_vars is septic the nominated value is ignored 
                               # and set to the value appropraite to the nominated max_restr_level (see below)
                               xaxis_labs = TRUE, yaxis_labs = TRUE,show_SEPP = FALSE){
  vars <- data.frame(y = c("lTP","lFRP","lTN","lNH3","sNOx","lTem","sEC","lTSS"),
                     var = c("tp","frp","tn","nh3","nox","tem","ec","tss"))
  ylab_expressions <- list(tp = expression(bold(paste("Total P (", mu, "g P/L)"))),
                           frp = expression(bold(paste("FRP (", mu, "g P/L)"))),
                           tn = expression(bold(paste("Total N (mg N/L)"))),
                           nh3 = expression(bold(paste("NH"[4]^+{}, " (", mu, "g N/L)"))),
                           nox = expression(bold(paste("NO"[x], "(mg N/L)"))),
                           tem = expression(bold(paste("Temp. (",degree,"C)"))),
                           ec = expression(bold(paste("EC (mS/cm)"))),
                           tss = expression(bold(paste("TSS (mg/L)"))))
  new_Xi <- unique(new_X_set[c(paste0(response_var,c("_mean","_025","_975")),
                            "degrd","restr","rain1","t",add_vars$par)])
  new_Xi <- new_Xi[new_Xi$t == 1.5,]
  new_Xi <- new_Xi[!is.na(new_Xi[,1]),]
  if(dim(add_vars)[1] == 3){
    # Regardless of value set for septic make it equal to the value appropriate for the max_restr_level
    # achieved at the site
    if(add_vars$par[3] == "septic"){
      add_vars$value[3] <- unique(new_Xi$septic[new_Xi$septic != 0 & new_Xi$restr == max_restr_level])
    }
    no_scms_inds <- which(new_Xi$restr == 0 & 
                              new_Xi$degrd == degrd_level  &
                              new_Xi[,add_vars$par[1]] == add_vars$value[1] & 
                              new_Xi[,add_vars$par[2]] == add_vars$value[2] & 
                              new_Xi[,add_vars$par[3]] == add_vars$value[3])
    achieved_inds <- which(new_Xi$restr == max_restr_level & 
                               new_Xi$degrd == degrd_level  &
                               new_Xi[,add_vars$par[1]] == add_vars$value[1] & 
                               new_Xi[,add_vars$par[2]] == add_vars$value[2] & 
                               new_Xi[,add_vars$par[3]] == add_vars$value[3])
  if(add_vars$par[3] == "septic"){
    add_vars$value[3] <- 0
  }
  ref_inds <- which(new_Xi$restr == 0 & new_Xi$degrd == -1  & 
                      new_Xi[,add_vars$par[1]] == add_vars$value[1] & 
                      new_Xi[,add_vars$par[2]] == add_vars$value[2] & 
                      new_Xi[,add_vars$par[3]] == add_vars$value[3])
  }
  if(dim(add_vars)[1] == 2){
    no_scms_inds <- which(new_Xi$restr == 0 & 
                            new_Xi$degrd == degrd_level  &
                            new_Xi[,add_vars$par[1]] == add_vars$value[1] & 
                            new_Xi[,add_vars$par[2]] == add_vars$value[2])
    achieved_inds <- which(new_Xi$restr == max_restr_level & 
                             new_Xi$degrd == degrd_level  &
                             new_Xi[,add_vars$par[1]] == add_vars$value[1] & 
                             new_Xi[,add_vars$par[2]] == add_vars$value[2])
    ref_inds <- which(new_Xi$restr == 0 & new_Xi$degrd == -1  & 
                        new_Xi[,add_vars$par[1]] == add_vars$value[1] & 
                        new_Xi[,add_vars$par[2]] == add_vars$value[2])
    }
if(dim(add_vars)[1] == 1){
  no_scms_inds <- which(new_Xi$restr == 0 & 
                          new_Xi$degrd == degrd_level  &
                          new_Xi[,add_vars$par[1]] == add_vars$value[1])
  achieved_inds <- which(new_Xi$restr == max_restr_level & 
                           new_Xi$degrd == degrd_level  &
                           new_Xi[,add_vars$par[1]] == add_vars$value[1])
  ref_inds <- which(new_Xi$restr == 0 & new_Xi$degrd == -1  & 
                      new_Xi[,add_vars$par[1]] == add_vars$value[1])
}
if(dim(add_vars)[1] == 0){
  no_scms_inds <- which(new_Xi$restr == 0 & 
                          new_Xi$degrd == degrd_level)
  achieved_inds <- which(new_Xi$restr == max_restr_level & 
                           new_Xi$degrd == degrd_level)
  ref_inds <- which(new_Xi$restr == 0 & new_Xi$degrd == -1)
}
  no_scms <- new_Xi[no_scms_inds,]
  achieved <- new_Xi[achieved_inds,]
  ref <- new_Xi[ref_inds,]
  # if(response_var == "tem"){
  #   y_lims <- log10(c(13,23))
  # }else{
  #   if(response_var == "ec"){
  #     y_lims <- sqrt(c(10,1000))
  #   }else{
  #     if(response_var == "nox"){
  #       y_lims <- c(0,1.25)
  #     }else{
  #       if(response_var == "tn"){
  #         y_lims <- c(-0.5,0.3)
  #       }else{
    y_lims <- c(min(no_scms[,2],achieved[,2],ref[,2]),
                max(no_scms[,3],achieved[,3],ref[,3]))
 #   }
 #   }}}
  plot(no_scms$rain1,no_scms[,1], 
       ylim = y_lims, 
       type = 'n', axes = FALSE,
       xlab = "Rain in preceding 24 h (mm)", 
       ylab = ylab_expressions[[which(vars$var == response_var)]])
  box(bty = 'l')
  axis(1, at = c(-1,log10(0:20 + 1)), labels = rep("",22))
  if(xaxis_labs)
  axis(1, at = log10(c(0:5,8,10,20) + 1), labels = c(0:5,8,10,20))
  if(response_var %in% c("ec","nox")){
    if(response_var == "ec") {
      y_labs <- c(seq(10,100,10),seq(200,1000,100))
    }else{
      y_labs <- seq(0,2,0.1)          
              }
    axis(2, at = sqrt(y_labs), labels = rep("",length(y_labs)))
    if(yaxis_labs)
      axis(2, at = sqrt(y_labs), labels = y_labs*ifelse(response_var== "ec",1e-3,1), las = 1)
  }else{
  axis(2, at = c(-4,log10(c(1:9*1e-3,0.01,0.03,0.1,0.02,0.3,1,3,5,10:25,30,50,100))), 
       labels = rep("",37))
  if(yaxis_labs)
    axis(2, at = log10(c(0.001,0.002,0.003,0.005,0.01,0.02,0.03,0.1,0.2,0.3,0.5,1,2,3,5,10:25,30,50,100)), 
       labels = c(0.001,0.002,0.003,0.005,0.01,0.02,0.03,0.1,0.2,0.3,0.5,1,2,3,5,10:25,30,50,100) *
         ifelse(response_var %in% c("lTP","lFRP","lNH3"), 1e3, 1), las = 1)
    }
  #colours from ggsci::pal_jco("default")(3)
  rethinking::shade(t(no_scms[,2:3]),
                    no_scms$rain1,col = scales::alpha("#EFC000FF", 0.7))
  lines(no_scms$rain1,no_scms[,1], col = "#EFC000FF", lwd = 2)
  rethinking::shade(t(achieved[,2:3]),
                    achieved$rain1,col = scales::alpha("#868686FF", 0.7))
  lines(achieved$rain1, achieved[,1], col = "#868686FF", lwd = 2)
  rethinking::shade(t(ref[,2:3]),
                    ref$rain1,col = scales::alpha("#0073C2FF", 0.7))
  lines(ref$rain1, ref[,1], col = "#0073C2FF", lwd = 2)
  abline(v = log10(c(2,8) + 1), lty = 3)  
  if(response_var %in% c("tn","tp","ec") & show_SEPP){
    sepp_targets <- data.frame(var = c("tn","tp","ec"), 
#                               yarra_upland = c(log10(0.9),log10(0.035),100^0.5),
                               yarra_lowland = c(log10(1.1),log10(0.055),250^0.5)) #,
#                               yarra_urban = c(log10(1.3),log10(0.11),500^0.5))
    points(log10(2 + 1),  #
           sepp_targets[sepp_targets$var == response_var,2], #2:4
           pch = 21, bg = RColorBrewer::brewer.pal(4,"PiYG")[1], cex = 1.5)  #3:1
  }
}

counter_rest_rain_4_panels <- function(response_var, new_X_set, 
                                       add_vars = data.frame(par = c("channel","season","rain365"),
                                                             value = c(0,0,0)),
                                       ...){
  vars <- data.frame(y = c("lFRP","lTP","lNH3","lTSS","lTN","sNOx","sEC","lTem"),
                     var = c("frp","tp","nh3","tss","tn","nox","ec","tem"),
                     lab = c("Filterable Reactive P (ug/L)","Total P (mg/L)",
                             "Ammonium (mg/L)","Total suspended solids (mg/L)",
                             "Total N (mg/L)","Nitrate + nitrite (mg/L)",
                             "Electrical conductivity (ms/cm)", "Temperature (C)"))
  ylabel <- vars$lab[vars$var == response_var]
  layout(matrix(c(1,2,3,1,4,5,0,6,6),3,3,byrow=TRUE),heights = c(10,10,1), widths = c(1,10,10))
  par(mar = c(0,0,0,0))
  plot.new()
  title(ylab = ylabel, line = -1.25, font.lab = 2, cex.lab = 1.25)
  par(mar = c(2,2,0,0))
  
  counter_restr_rain(response_var, new_X_set, add_vars = add_vars,
                     max_restr_level = unique(new_X_set$restr)[2],
                     # 1:7 -1 (a stretch target), -0.49 (achieved in Ln), -0.35 (achieved in Ls), 
                     #.    -0.2 (achieved in L4), -0.165 (achieved in D8), - 0.096 (achieved in D4), 0 (no SCMs)
                     degrd_level = unique(new_X_set$degrd)[6], 
                     # 1:10 0 (ref), 0.3, 1 2018 values of EIs1 for DBS0008 (2.0497), DBS0004 (2.5481), LSN (6.1793), LIS004 (10.207), 
                     #    LSS (13.3356), 20,  LIS0001 (25.529)
                     xaxis_labs = FALSE, yaxis_labs = TRUE,...)
  title(main = " A. Ln", line = -1, adj = 0)
  #Ls
  counter_restr_rain(response_var, new_X_set, add_vars = add_vars,
                     max_restr_level = unique(new_X_set$restr)[3],
                     # 1:7 -1 (a stretch target), -0.49 (achieved in Ln), -0.35 (achieved in Ls), 
                     #.    -0.2 (achieved in L4), -0.165 (achieved in D8), - 0.096 (achieved in D4), 0 (no SCMs)
                     degrd_level = unique(new_X_set$degrd)[8], 
                     # 1:10 0 (ref), 0.3, 1 2018 values of EIs1 for DBS0008 (2.0497), DBS0004 (2.5481), LSN (6.1793), LIS004 (10.207), 
                     #    LSS (13.3356), 20,  LIS0001 (25.529)
                     xaxis_labs = FALSE, yaxis_labs = FALSE,...)
  title(main = " B. Ls", line = -1, adj = 0)
  #L4
  counter_restr_rain(response_var, new_X_set, add_vars = add_vars,
                     max_restr_level = unique(new_X_set$restr)[4],
                     # restr_levels 1:8 -1 (a stretch target), -0.49 (achieved in Ln), -0.35 (achieved in Ls),
                     #                  -0.2 (achieved in L4), -0.165 (achieved in D8), - 0.096 (achieved in D4), 
                     #                  -? (achieved in L1) and 0 (no SCMs)
                     degrd_level = unique(new_X_set$degrd)[7], 
                     # 1:10 0 (ref), 0.3, 1 2018 values of EIs1 for DBS0008 (2.0497), DBS0004 (2.5481), LSN (6.1793), LIS004 (10.207), 
                     #    LSS (13.3356), 20,  LIS0001 (25.529)
                     xaxis_labs = TRUE, yaxis_labs = TRUE,...)
  title(main = " C. L4", line = -1, adj = 0)
  
  #D8
  counter_restr_rain(response_var, new_X_set, add_vars = add_vars,
                     max_restr_level = unique(new_X_set$restr)[5],
                     # restr_levels 1:8 -1 (a stretch target), -0.49 (achieved in Ln), -0.35 (achieved in Ls),
                     #                  -0.2 (achieved in L4), -0.165 (achieved in D8), - 0.096 (achieved in D4), 
                     #                  -? (achieved in L1) and 0 (no SCMs)
                     degrd_level = unique(new_X_set$degrd)[4], 
                     # 1:10 0 (ref), 0.3, 1 2018 values of EIs1 for DBS0008 (2.0497), DBS0004 (2.5481), LSN (6.1793), LIS004 (10.207), 
                     #    LSS (13.3356), 20,  LIS0001 (25.529)
                     xaxis_labs = TRUE, yaxis_labs = FALSE,...)
  title(main = " D. D8", line = -1, adj = 0)
  par(mar = c(0,0,0,0))
  plot.new()
  title(xlab = "Rain in previous 24 h (mm)", line = -1.25, font.lab = 2, cex.lab = 1.25)
}  

mean_cis_string <- function(post){
  qs <- quantile(post, probs = c(0.025,0.5,0.975))
  paste0(signif(qs[2],2), " (", signif(qs[1],2), " -- ", signif(qs[3],2),")")
}
  
contrast_pairs <- function(var = "frp",
                           restr_levels = c(2,7),  #restr levels to compare (result subtracts first from second)
                           # 1:8 -1 (a stretch target), -0.49 (achieved in Ln), -0.35 (achieved in Ls),
                           #                  -0.2 (achieved in L4), -0.165 (achieved in D8), - 0.096 (achieved in D4), 
                           #                  -? (achieved in L1) and 0 (no SCMs)
                           degrd_levels = c(5,5),  #degrd levels to compare
                           # 1:10 0 (ref), 0.3, 1 2018 values of EIs1 for DBS0008 (2.0497), DBS0004 (2.5481), LSN (6.1793), LIS004 (10.207), 
                           #    LSS (13.3356), 20,  LIS0001 (25.529)
                           rain1_levels = c(1,1),
                           # 1:5 = 0, 1, 2, 5, 8, 
                           new_Xii, # subset of predictor dataset to be used to compare scenarios
                           pred_seti #ypred posterior predictions subsetted as for new_Xii
                           #Needs to be reduced sufficiently so that 1 value of restr, degrd, and rain1 = 1 estimate
                           ){
restr <- unique(new_Xii$restr); restr <- restr[order(restr)]
degrd <- unique(new_Xii$degrd); degrd <- degrd[order(degrd)]
rain1 <- unique(new_Xii$rain1); rain1 <- rain1[order(rain1)]
if(!(length(restr) == 8 & length(degrd) == 10 & length(rain1) == 7)) stop()
scen1 <- which(new_Xii$restr == restr[restr_levels[1]] & 
                 new_Xii$degrd == degrd[degrd_levels[1]] & 
                 new_Xii$rain1 == rain1[rain1_levels[1]])
scen2 <- which(new_Xii$restr == restr[restr_levels[2]] & 
                 new_Xii$degrd == degrd[degrd_levels[2]] & 
                 new_Xii$rain1 == rain1[rain1_levels[2]])
if(var %in% c("nox","ec")){
  diffs <- pred_seti[,scen2]^2 - pred_seti[,scen1]^2
  scen1_post <- pred_seti[,scen1]^2
  scen2_post <- pred_seti[,scen2]^2
}else{
  diffs <- 10^(pred_seti[,scen2]) - 10^(pred_seti[,scen1])
  scen1_post <- 10^(pred_seti[,scen1])
  scen2_post <- 10^(pred_seti[,scen2])
}
if(var == "frp"){
  diffs <- diffs * 1e3 #convert to ug/L
  scen1_post <- scen1_post * 1e3 
  scen2_post <- scen2_post * 1e3 
}
scen1_quants <- quantile(scen1_post, probs = c(0.025,0.5,0.975))
scen2_quants <- quantile(scen2_post, probs = c(0.025,0.5,0.975))
diff_quants <- quantile(diffs, probs = c(0.025,0.5,0.975))
string1 <- paste0(signif(diff_quants[2],2), " (", 
                      signif(diff_quants[1],2), " -- ",  
                      signif(diff_quants[3],2), ")")
string2 <- paste0(signif(diff_quants[2],2), " (95% CIs ", 
                  signif(diff_quants[1],2), " -- ",  
                  signif(diff_quants[3],2), ")")
list(scen1_quants = scen1_quants, 
     scen2_quants = scen2_quants, 
     diff_quants = diff_quants, 
     str1 = string1, str2 = string2)
}

degrd_v_restr_plot1 <- function(response_var, new_X_set){ 
  vars <- data.frame(y = c("lTP","lFRP","lTN","lNH3","sNOx","lTem","sEC","lTSS"),
                     var = c("tp","frp","tn","nh3","nox","tem","ec","tss"))
  ylab_expressions <- list(tp = expression(bold(paste("Total P (", mu, "g P/L)"))),
                           frp = expression(bold(paste("FRP (", mu, "g P/L)"))),
                           tn = expression(bold(paste("Total N (mg N/L)"))),
                           nh3 = expression(bold(paste("NH"[4]^+{}, " (", mu, "g N/L)"))),
                           nox = expression(bold(paste("NO"[x], "(mg N/L)"))),
                           tem = expression(bold(paste("Temp. (",degree,"C)"))),
                           ec = expression(bold(paste("EC (mS/cm)"))),
                           tss = expression(bold(paste("TSS (mg/L)"))))
  pred_frame <- new_X_set
  names(pred_frame) <- gsub(response_var, "var", names(pred_frame))
ei_rain10 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == 0 & pred_frame$t == 1.5,]
ei_rain12 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[3] & 
                       pred_frame$t == 1.5,]
ei_rain18 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[5] & 
                       pred_frame$t == 1.5,]
ei_rain120 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[7] & 
                        pred_frame$t == 1.5,]
par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(c(-1,1.5),c(min(ei_rain10$var_025),max(ei_rain120$var_975)),type = 'n',
     axes = FALSE, xlab = "Effective Imperviousness (%)", ylab = ylab_expressions[response_var][[1]])
axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = c(0,0.1,0.3,1,3,10,30))
axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01))), 
     labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01)))))
axis(2,at = log10(c(0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.1)), 
     labels = c(0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.1),las = 1)
box(bty = 'l')
rethinking::shade(t(ei_rain10[,c("var_025","var_975")]),
                  ei_rain10$degrd,col = scales::alpha("#0073C2FF", 0.7))

rethinking::shade(t(ei_rain12[,c("var_025","var_975")]),
                  ei_rain12$degrd,col = scales::alpha("#EFC000FF", 0.7))
rethinking::shade(t(ei_rain18[,c("var_025","var_975")]),
                  ei_rain18$degrd,col = scales::alpha("#868686FF", 0.7))
rethinking::shade(t(ei_rain120[,c("var_025","var_975")]),
                  ei_rain120$degrd,col = scales::alpha("#CD534CFF", 0.7))
lines(ei_rain12$degrd, ei_rain12$var_mean, lwd = 3, col = "#EFC000FF")
lines(ei_rain10$degrd, ei_rain10$var_mean, lwd = 3, col = "#0073C2FF")
lines(ei_rain18$degrd, ei_rain18$var_mean, lwd = 3, col = "#868686FF")
lines(ei_rain120$degrd, ei_rain120$var_mean, lwd = 3, col = "#CD534CFF")

for(i in 1:dim(exp_sites)[1]){
  max_restr <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] & 
                         pred_frame$rain1 == 0 & pred_frame$season == season & pred_frame$t == 1.5,]
  ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
  points(ei_rain10$degrd[exp_sites$degrd_ind[i]], ei_rain10$var_mean[exp_sites$degrd_ind[i]], 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr$var_mean, pch = 21, bg = "lightgreen", col = "darkgreen", cex = 1.5)
  lines(c(ei_rain10$degrd[exp_sites$degrd_ind[i]],ei_s0),
        c(ei_rain10$var_mean[exp_sites$degrd_ind[i]],max_restr$var_mean),
        col = "lightgreen", lwd = 2)
  # lines(c(ei_s0,ei_s0),
  #      c(max_restr$var_025,max_restr$var_975), #ei_rain10$var_mean[exp_sites$degrd_ind[i]],
  #      col = "black", lwd = 2)
  max_restr2 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                          pred_frame$rain1 == unique(pred_frame$rain1)[3] & pred_frame$season == season & pred_frame$t == 1.5,]
  points(ei_rain12$degrd[exp_sites$degrd_ind[i]], ei_rain12$var_mean[exp_sites$degrd_ind[i]], 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr2$var_mean, pch = 21, bg = "lightgreen", col = "darkgreen", cex = 1.5)
  lines(c(ei_rain12$degrd[exp_sites$degrd_ind[i]],ei_s0),
        c(ei_rain12$var_mean[exp_sites$degrd_ind[i]],max_restr2$var_mean),
        col = "lightgreen", lwd = 2)
  max_restr8 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                          pred_frame$rain1 == unique(pred_frame$rain1)[5] & pred_frame$season == season & pred_frame$t == 1.5,]
  points(ei_rain18$degrd[exp_sites$degrd_ind[i]], ei_rain18$var_mean[exp_sites$degrd_ind[i]], 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr8$var_mean, pch = 21, bg = "lightgreen", col = "darkgreen", cex = 1.5)
  lines(c(ei_rain18$degrd[exp_sites$degrd_ind[i]],ei_s0),
        c(ei_rain18$var_mean[exp_sites$degrd_ind[i]],max_restr8$var_mean),
        col = "lightgreen", lwd = 2)
  max_restr20 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                           pred_frame$rain1 == unique(pred_frame$rain1)[7] & pred_frame$season == season & pred_frame$t == 1.5,]
  points(ei_rain120$degrd[exp_sites$degrd_ind[i]], ei_rain120$var_mean[exp_sites$degrd_ind[i]], 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr20$var_mean, pch = 21, bg = "lightgreen", col = "darkgreen", cex = 1.5)
  lines(c(ei_rain120$degrd[exp_sites$degrd_ind[i]],ei_s0),
        c(ei_rain120$var_mean[exp_sites$degrd_ind[i]],max_restr20$var_mean),
        col = "lightgreen", lwd = 2)
}
}

degrd_v_restr_plot4 <- function(response_var, new_X_set){
  sites <- data.frame(readxl::read_excel("data/wq_data_compiled.xlsx", 
                                         sheet = "sites"))
  sites$septic <- (sites$septics^0.5)/10
  sites$septic <- 3*(sites$septic - (min(sites$septic) + 
                                       diff(range(sites$septic))*0.5) )/diff(range(sites$septic))
  exp_sites <- data.frame(site = c("D8","D4","Ln","L4","Ls","L1"),
                          restr_ind = c(5,6,2,4,3,7),
                          max_restr = unique(new_X$restr)[c(5,6,2,4,3,7)],
                          degrd_ind = c(4:8,10),
                          degrd = unique(new_X$degrd)[c(4:8,10)],
                          septic = sites$septic[match(c("DBS0008","DBS0004",
                                                        "LSN0001","LIS0004",
                                                        "LSS0001","LIS0001"),sites$sitecode)])
  vars <- data.frame(y = c("lTP","lFRP","lTN","lNH3","sNOx","lTem","sEC","lTSS"),
                     var = c("tp","frp","tn","nh3","nox","tem","ec","tss"),
                     ylab_mult = c(1e3,1e3,1,1e3,1,1,1e-3,1),
                     ymin = c(log10(c(0.015,0.0015,0.5,0.003)),0.1^0.5,log10(14),30^0.5,log10(3)),
                     ymax = c(log10(c(0.25,0.05,3,0.08)),2^0.5,log10(23),900^0.5,log10(150)))
  ylab_expressions <- list(tp = expression(bold(paste("Total P (", mu, "g P/L)"))),
                           frp = expression(bold(paste("FRP (", mu, "g P/L)"))),
                           tn = expression(bold(paste("Total N (mg N/L)"))),
                           nh3 = expression(bold(paste("NH"[4]^+{}, " (", mu, "g N/L)"))),
                           nox = expression(bold(paste("NO"[x], "(mg N/L)"))),
                           tem = expression(bold(paste("Temp. (",degree,"C)"))),
                           ec = expression(bold(paste("EC (mS/cm)"))),
                           tss = expression(bold(paste("TSS (mg/L)"))))
  pred_frame <- new_X_set
  names(pred_frame) <- gsub(response_var, "var", names(pred_frame))
  ei_rain10 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == 0,]
  ei_rain12 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[3],]
  ei_rain18 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[5],]
  ei_rain120 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[7],]
  if("septic" %in% names(pred_frame)){
    ei_rain10s <- ei_rain10[ei_rain10$septic != 0,]
    ei_rain10 <- ei_rain10[ei_rain10$septic == 0,]
    ei_rain12s <- ei_rain12[ei_rain12$septic != 0,]
    ei_rain12 <- ei_rain12[ei_rain12$septic == 0,]
    ei_rain18s <- ei_rain18[ei_rain18$septic != 0,]
    ei_rain18 <- ei_rain18[ei_rain18$septic == 0,]
    ei_rain120s <- ei_rain120[ei_rain120$septic != 0,]
    ei_rain120 <- ei_rain120[ei_rain120$septic == 0,]
  }
  layout(matrix(c(5,1:4,0,6,6,6,6),2,5,byrow=TRUE),widths = c(1,14,10,10,10),heights = c(10,1))
par(mar = c(4,4,1,1))
plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
     axes = FALSE, xlab = "", ylab = "")
axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = c(0,0.1,0.3,1,3,10,30))
if(response_var == "nox"){
  axis(2, at = seq(0,4,0.5)^0.5, labels =  seq(0,4,0.5),las = 1);
  axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
}else{
  if(response_var == "ec"){
    axis(2, at = seq(0,1000,100)^0.5, labels =  seq(0,1000,100)*1e-3,las = 1);
  }else{
    if(response_var == "tem"){
      axis(2, at = log10(13:24), labels = 13:24,las = 1);
    }else{
axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
     labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
    if(response_var == "tn")  {
    axis(2, at = log10(c(seq(0.5,1,0.1),2,3)), labels = c(seq(0.5,1,0.1),2,3), las = 1)
         }else{ 
      axis(2,at = log10(c(0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.1,0.2,0.3,1,3,10,30,100,200,300,1000)), 
                labels = c(0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.1,0.2,0.3,1,3,10,30,100,200,300,
                1000)*vars$ylab_mult[vars$var == response_var], las = 1)
}}}}
box(bty = 'l')
rethinking::shade(t(ei_rain10[,c("var_025","var_975")]),
                  ei_rain10$degrd,col = scales::alpha("#0073C2FF", 0.7))
lines(ei_rain10$degrd, ei_rain10$var_mean, lwd = 2, col = "#0073C2FF")
for(i in 1:dim(exp_sites)[1]){
  max_restr <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] & 
                         pred_frame$rain1 == 0,]
  if("septic" %in% names(pred_frame)){
    max_restr <- max_restr[max_restr$septic != 0,]
    pre_restr <- ei_rain10s[round(ei_rain10s$septic,5) == round(exp_sites$septic[i],5),]  
  }else{
    pre_restr <- ei_rain10[exp_sites$degrd_ind[i],]
    }
  ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
  points(pre_restr$degrd, pre_restr$var_mean, 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
  lines(c(pre_restr$degrd,ei_s0),
        c(pre_restr$var_mean,max_restr$var_mean),
        col = "#EFC000FF", lwd = 2)
  # lines(c(ei_s0,ei_s0),
  #      c(max_restr$var_025,max_restr$var_975), #ei_rain10$var_mean[exp_sites$degrd_ind[i]],
  #      col = "black", lwd = 2)
}
title("A. rain1 = 0 mm", adj =0)

par(mar = c(4,0,1,1))
plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
     axes = FALSE, xlab = "", ylab = "")
axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = c(0,0.1,0.3,1,3,10,30))
if(response_var == c("nox")){
  axis(2, at = seq(0,4,0.5)^0.5, labels = rep("",length(seq(0,4,0.5))),las = 1);
  axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
}else{
  if(response_var == "ec"){
    axis(2, at = seq(0,1000,100)^0.5, labels = rep("",length(seq(0,1000,100))),las = 1);
  }else{
    if(response_var == "tem"){
      axis(2, at = log10(13:24), labels = rep("",length(13:24)),las = 1);
    }else{
  axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
     labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
}}}
  box(bty = 'l')
rethinking::shade(t(ei_rain12[,c("var_025","var_975")]),
                  ei_rain12$degrd,col = scales::alpha("#0073C2FF", 0.7))
lines(ei_rain12$degrd, ei_rain12$var_mean, lwd = 2, col = "#0073C2FF")
for(i in 1:dim(exp_sites)[1]){
  max_restr2 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                          pred_frame$rain1 == unique(pred_frame$rain1)[3],]
  if("septic" %in% names(pred_frame)){
    max_restr2 <- max_restr2[max_restr2$septic != 0,]
    pre_restr2 <- ei_rain12s[round(ei_rain12s$septic,5) == round(exp_sites$septic[i],5),]  
  }else{
    pre_restr2 <- ei_rain12[exp_sites$degrd_ind[i],]
  }
  ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
  points(pre_restr2$degrd, pre_restr2$var_mean, 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr2$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
  lines(c(pre_restr2$degrd,ei_s0),
        c(pre_restr2$var_mean,max_restr2$var_mean),
        col = "#EFC000FF", lwd = 2)
}
title("B. rain1 = 2 mm", adj =0)

plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
     axes = FALSE, xlab = "", ylab = "")
axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = c(0,0.1,0.3,1,3,10,30))
if(response_var %in% c("nox")){
  axis(2, at = seq(0,4,0.5)^0.5, labels = rep("",length(seq(0,4,0.5))),las = 1);
  axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
}else{
  if(response_var == "ec"){
    axis(2, at = seq(0,1000,100)^0.5, labels =  rep("",length(seq(0,1000,100))),las = 1);
  }else{
    if(response_var == "tem"){
      axis(2, at = log10(13:24), labels = rep("",length(13:24)),las = 1);
    }else{
  axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
     labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
}}}
  box(bty = 'l')
rethinking::shade(t(ei_rain18[,c("var_025","var_975")]),
                  ei_rain18$degrd,col = scales::alpha("#0073C2FF", 0.7))
lines(ei_rain18$degrd, ei_rain18$var_mean, lwd = 2, col = "#0073C2FF")
for(i in 1:dim(exp_sites)[1]){
  max_restr8 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                          pred_frame$rain1 == unique(pred_frame$rain1)[5],]
  if("septic" %in% names(pred_frame)){
    max_restr8 <- max_restr8[max_restr8$septic != 0,]
    pre_restr8 <- ei_rain18s[round(ei_rain18s$septic,5) == round(exp_sites$septic[i],5),]  
  }else{
    pre_restr8 <- ei_rain18[exp_sites$degrd_ind[i],]
  }
  ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
  points(pre_restr8$degrd, pre_restr8$var_mean, 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr8$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
  lines(c(pre_restr8$degrd,ei_s0),
        c(pre_restr8$var_mean,max_restr8$var_mean),
        col = "#EFC000FF", lwd = 2)
}
title("C. rain1 = 8 mm", adj =0)

plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
     axes = FALSE, xlab = "", ylab = "")
axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = c(0,0.1,0.3,1,3,10,30))
if(response_var %in% c("nox","ec")){
  axis(2, at = seq(0,4,0.5)^0.5, labels = rep("",length(seq(0,4,0.5))),las = 1);
  axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
}else{
  if(response_var == "ec"){
    axis(2, at = seq(0,1000,100)^0.5, labels =  rep("",length(seq(0,1000,100))),las = 1);
  }else{
    if(response_var == "tem"){
      axis(2, at = log10(13:24), labels = rep("",length(13:24)),las = 1);
    }else{
  axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
     labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
}}}
  box(bty = 'l')
rethinking::shade(t(ei_rain120[,c("var_025","var_975")]),
                  ei_rain120$degrd,col = scales::alpha("#0073C2FF", 0.7))
lines(ei_rain18$degrd, ei_rain120$var_mean, lwd = 2, col = "#0073C2FF")
for(i in 1:dim(exp_sites)[1]){
  max_restr20 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                           pred_frame$rain1 == unique(pred_frame$rain1)[7],]
  if("septic" %in% names(pred_frame)){
    max_restr20 <- max_restr20[max_restr20$septic != 0,]
    pre_restr20 <- ei_rain120s[round(ei_rain120s$septic,5) == round(exp_sites$septic[i],5),]  
  }else{
    pre_restr20 <- ei_rain120[exp_sites$degrd_ind[i],] 
  }
  ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
  points(pre_restr20$degrd, pre_restr20$var_mean, 
         pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
  points(ei_s0,max_restr20$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
  lines(c(pre_restr20$degrd,ei_s0),
        c(pre_restr20$var_mean,max_restr20$var_mean),
        col = "#EFC000FF", lwd = 2)
}
title("D. rain1 = 20 mm", adj =0)
par(mar = c(0,0,0,0))
plot.new()
title(ylab = ylab_expressions[response_var][[1]],line = -1.25, font.lab = 2, cex.lab = 1.25)
plot.new()
title(xlab = "Effective imperviousness (%)", line = -1.25, font.lab = 2, cex.lab = 1.25)
}

degrd_v_restr_plot4_in32 <- function(response_var, new_X_set, 
                                     xax_labs = FALSE,
                                     main_lab = "A."){
  sites <- data.frame(readxl::read_excel("data/wq_data_compiled.xlsx", 
                                         sheet = "sites"))
  sites$septic <- (sites$septics^0.5)/10
  sites$septic <- 3*(sites$septic - (min(sites$septic) + 
                                       diff(range(sites$septic))*0.5) )/diff(range(sites$septic))
  exp_sites <- data.frame(site = c("D8","D4","Ln","L4","Ls","L1"),
                          restr_ind = c(5,6,2,4,3,7),
                          max_restr = unique(new_X$restr)[c(5,6,2,4,3,7)],
                          degrd_ind = c(4:8,10),
                          degrd = unique(new_X$degrd)[c(4:8,10)],
                          septic = sites$septic[match(c("DBS0008","DBS0004",
                                                        "LSN0001","LIS0004",
                                                        "LSS0001","LIS0001"),sites$sitecode)])
  vars <- data.frame(y = c("lTP","lFRP","lTN","lNH3","sNOx","lTem","sEC","lTSS"),
                     var = c("tp","frp","tn","nh3","nox","tem","ec","tss"),
                     ylab_mult = c(1e3,1e3,1,1e3,1,1,1e-3,1),
                     ymin = c(log10(c(0.015,0.0015,0.5,0.003)),0.1^0.5,log10(14),30^0.5,log10(3)),
                     ymax = c(log10(c(0.25,0.05,3,0.08)),2^0.5,log10(23),900^0.5,log10(150)))
  pred_frame <- new_X_set
  names(pred_frame) <- gsub(response_var, "var", names(pred_frame))
  ei_rain10 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == 0,]
  ei_rain12 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[3],]
  ei_rain18 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[5],]
  ei_rain120 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[7],]
  if("septic" %in% names(pred_frame)){
    ei_rain10s <- ei_rain10[ei_rain10$septic != 0,]
    ei_rain10 <- ei_rain10[ei_rain10$septic == 0,]
    ei_rain12s <- ei_rain12[ei_rain12$septic != 0,]
    ei_rain12 <- ei_rain12[ei_rain12$septic == 0,]
    ei_rain18s <- ei_rain18[ei_rain18$septic != 0,]
    ei_rain18 <- ei_rain18[ei_rain18$septic == 0,]
    ei_rain120s <- ei_rain120[ei_rain120$septic != 0,]
    ei_rain120 <- ei_rain120[ei_rain120$septic == 0,]
  }
  if(xax_labs){xaxis_labs <- c(0,0.1,0.3,1,3,10,30)} else{xaxis_labs <- rep("",7)}
  par(mar = c(1,2,0,1))
  plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(response_var == "nox"){
    axis(2, at = seq(0,4,0.5)^0.5, labels =  seq(0,4,0.5),las = 1);
    axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
  }else{
    if(response_var == "ec"){
      axis(2, at = seq(0,1000,100)^0.5, labels =  seq(0,1000,100)*1e-3,las = 1);
    }else{
      if(response_var == "tem"){
        axis(2, at = log10(13:24), labels = 13:24,las = 1);
      }else{
        axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
             labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
        if(response_var == "tn")  {
          axis(2, at = log10(c(seq(0.5,1,0.1),2,3)), labels = c(seq(0.5,1,0.1),2,3), las = 1)
        }else{ 
          axis(2,at = log10(c(0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.1,0.2,0.3,1,3,10,30,100,200,300,1000)), 
               labels = c(0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.1,0.2,0.3,1,3,10,30,100,200,300,
                          1000)*vars$ylab_mult[vars$var == response_var], las = 1)
        }}}}
  title(main = paste0(" ",main_lab), adj = 0, line = -1)
  box(bty = 'l')
  rethinking::shade(t(ei_rain10[,c("var_025","var_975")]),
                    ei_rain10$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain10$degrd, ei_rain10$var_mean, lwd = 2, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] & 
                              pred_frame$rain1 == 0,]
    if("septic" %in% names(pred_frame)){
      max_restr <- max_restr[max_restr$septic != 0,]
      pre_restr <- ei_rain10s[round(ei_rain10s$septic,5) == round(exp_sites$septic[i],5),]  
    }else{
      pre_restr <- ei_rain10[exp_sites$degrd_ind[i],] 
    }
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    points(pre_restr$degrd, pre_restr$var_mean, 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
    points(ei_s0,max_restr$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
    lines(c(pre_restr$degrd,ei_s0),
          c(pre_restr$var_mean,max_restr$var_mean),
          col = "#EFC000FF", lwd = 2)
    # lines(c(ei_s0,ei_s0),
    #      c(max_restr$var_025,max_restr$var_975), #ei_rain10$var_mean[exp_sites$degrd_ind[i]],
    #      col = "black", lwd = 2)
  }
  
  par(mar = c(1,0,0,1))
  plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(response_var == c("nox")){
    axis(2, at = seq(0,4,0.5)^0.5, labels = rep("",length(seq(0,4,0.5))),las = 1);
    axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
  }else{
    if(response_var == "ec"){
      axis(2, at = seq(0,1000,100)^0.5, labels = rep("",length(seq(0,1000,100))),las = 1);
    }else{
      if(response_var == "tem"){
        axis(2, at = log10(13:24), labels = rep("",length(13:24)),las = 1);
      }else{
        axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
             labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
      }}}
  box(bty = 'l')
  rethinking::shade(t(ei_rain12[,c("var_025","var_975")]),
                    ei_rain12$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain12$degrd, ei_rain12$var_mean, lwd = 2, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr2 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                               pred_frame$rain1 == unique(pred_frame$rain1)[3],]
    if("septic" %in% names(pred_frame)){
      max_restr2 <- max_restr2[max_restr2$septic != 0,]
      pre_restr2 <- ei_rain12s[round(ei_rain12s$septic,5) == round(exp_sites$septic[i],5),]  
    }else{
      pre_restr2 <- ei_rain12[exp_sites$degrd_ind[i],] 
    }
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    points(pre_restr2$degrd, pre_restr2$var_mean, 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
    points(ei_s0,max_restr2$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
    lines(c(pre_restr2$degrd,ei_s0),
          c(pre_restr2$var_mean,max_restr2$var_mean),
          col = "#EFC000FF", lwd = 2)
  }
  plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(response_var %in% c("nox")){
    axis(2, at = seq(0,4,0.5)^0.5, labels = rep("",length(seq(0,4,0.5))),las = 1);
    axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
  }else{
    if(response_var == "ec"){
      axis(2, at = seq(0,1000,100)^0.5, labels =  rep("",length(seq(0,1000,100))),las = 1);
    }else{
      if(response_var == "tem"){
        axis(2, at = log10(13:24), labels = rep("",length(13:24)),las = 1);
      }else{
        axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
             labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
      }}}
  box(bty = 'l')
  rethinking::shade(t(ei_rain18[,c("var_025","var_975")]),
                    ei_rain18$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain18$degrd, ei_rain18$var_mean, lwd = 2, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr8 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                               pred_frame$rain1 == unique(pred_frame$rain1)[5],]
    if("septic" %in% names(pred_frame)){
      max_restr8 <- max_restr8[max_restr8$septic != 0,]
      pre_restr8 <- ei_rain18s[round(ei_rain18s$septic,5) == round(exp_sites$septic[i],5),]  
    }else{
      pre_restr8 <- ei_rain18[exp_sites$degrd_ind[i],] 
    }
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    points(pre_restr8$degrd, pre_restr8$var_mean, 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
    points(ei_s0,max_restr8$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
    lines(c(pre_restr8$degrd,ei_s0),
          c(pre_restr8$var_mean,max_restr8$var_mean),
          col = "#EFC000FF", lwd = 2)
  }
  plot(c(-1,1.5),c(vars$ymin,vars$ymax)[vars$var == response_var],type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(response_var %in% c("nox","ec")){
    axis(2, at = seq(0,4,0.5)^0.5, labels = rep("",length(seq(0,4,0.5))),las = 1);
    axis(2, at = seq(0,4,0.1)^0.5, labels = rep("",length(seq(0,4,0.1))),las = 1);
  }else{
    if(response_var == "ec"){
      axis(2, at = seq(0,1000,100)^0.5, labels =  rep("",length(seq(0,1000,100))),las = 1);
    }else{
      if(response_var == "tem"){
        axis(2, at = log10(13:24), labels = rep("",length(13:24)),las = 1);
      }else{
        axis(2,at = log10(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100))), 
             labels = rep("",length(c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)))))
      }}}
  box(bty = 'l')
  rethinking::shade(t(ei_rain120[,c("var_025","var_975")]),
                    ei_rain120$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain18$degrd, ei_rain120$var_mean, lwd = 2, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr20 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                                pred_frame$rain1 == unique(pred_frame$rain1)[7],]
    if("septic" %in% names(pred_frame)){
      max_restr20 <- max_restr20[max_restr20$septic != 0,]
      pre_restr20 <- ei_rain120s[round(ei_rain120s$septic,5) == round(exp_sites$septic[i],5),]  
    }else{
      pre_restr20 <- ei_rain120[exp_sites$degrd_ind[i],] 
    }
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    points(pre_restr20$degrd, pre_restr20$var_mean, 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1.5)
    points(ei_s0,max_restr20$var_mean, pch = 21, bg = "#EFC000FF", col = "darkgreen", cex = 1.5)
    lines(c(pre_restr20$degrd,ei_s0),
          c(pre_restr20$var_mean,max_restr20$var_mean),
          col = "#EFC000FF", lwd = 2)
  }
}

degrd_v_restr_plot4_in32_bu <- function(response_var, new_X_set, 
                                     xax_labs = FALSE,
                                     y_transform = "log",
                                     y_mult = 1, 
                                     main_lab = "A."){
  pred_frame <- new_X_set
  names(pred_frame) <- gsub(response_var, "var", names(pred_frame))
  ei_rain10 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == 0,]
  ei_rain12 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[3],]
  ei_rain18 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[5],]
  ei_rain120 <- pred_frame[pred_frame$restr == 0 & pred_frame$rain1 == unique(pred_frame$rain1)[7],]
  if(y_transform == "log"){
    if(max(ei_rain120$var_975) > log10(20) & max(ei_rain120$var_975) <  log10(35)){ #i.e. temperature
      yticks <- yticklabs <- 1:30
    }else{
      yticks <- c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),2:10,seq(20,100,10),seq(200,1000,100)) 
      yticklabs <- c(0.002,0.003,0.005,0.01,0.02,0.03,0.05,0.1,0.3,1,3,10,30,100,300,1000) * y_mult
    }
  }else{
    if(max(ei_rain10$var_975) > sqrt(500)){ # i.e. EC
      yticks <-  seq(0,1000,100)
      yticklabs <-  seq(0,1000,100) * y_mult
    }else{
      yticks <- seq(0,2,0.1) 
      yticklabs <-  seq(0,2,0.1) * y_mult
    }
  }
  par(mar = c(1,2,0,1))
  plot(c(-1,1.5),range(c(ei_rain10$var_025, ei_rain120$var_975,ei_rain10$var_975, ei_rain120$var_025)),type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  if(xax_labs){xaxis_labs <- c(0,0.1,0.3,1,3,10,30)} else{xaxis_labs <- rep("",7)}
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(y_transform == "log"){
    axis(2,at = log10(yticks), labels = rep("",length(yticks)))
    axis(2,at = log10(yticklabs/y_mult), labels = yticklabs, las = 1)
  }else{
    axis(2, at = yticks^0.5, labels = yticklabs, las = 1)
  }
  title(main = paste0(" ",main_lab), adj = 0, line = -1)
  box(bty = 'l')
  rethinking::shade(t(ei_rain10[,c("var_025","var_975")]),
                    ei_rain10$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain10$degrd, ei_rain10$var_mean, lwd = 1, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] & 
                              pred_frame$rain1 == 0,]
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    lines(c(ei_rain10$degrd[exp_sites$degrd_ind[i]],ei_s0),
          c(ei_rain10$var_mean[exp_sites$degrd_ind[i]],max_restr$var_mean),
          col = "#EFC000FF", lwd = 2)
    points(ei_rain10$degrd[exp_sites$degrd_ind[i]], ei_rain10$var_mean[exp_sites$degrd_ind[i]], 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1)
    points(ei_s0,max_restr$var_mean, pch = 21, bg = "#EFC000FF", col = "black", cex = 1)
  }
  par(mar = c(1,0,0,1))
  plot(c(-1,1.5),range(c(ei_rain10$var_025, ei_rain120$var_975,ei_rain10$var_975, ei_rain120$var_025)),type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(y_transform == "log"){
    axis(2,at = log10(yticks), labels = rep("",length(yticks)))
  }else{
    axis(2, at = yticks^0.5, labels = rep("", length(yticks)), las = 1)
  }
  box(bty = 'l')
  rethinking::shade(t(ei_rain12[,c("var_025","var_975")]),
                    ei_rain12$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain12$degrd, ei_rain12$var_mean, lwd = 1, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr2 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                               pred_frame$rain1 == unique(pred_frame$rain1)[3],]
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    lines(c(ei_rain12$degrd[exp_sites$degrd_ind[i]],ei_s0),
          c(ei_rain12$var_mean[exp_sites$degrd_ind[i]],max_restr2$var_mean),
          col = "#EFC000FF", lwd = 2)
    points(ei_rain12$degrd[exp_sites$degrd_ind[i]], ei_rain12$var_mean[exp_sites$degrd_ind[i]], 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1)
    points(ei_s0,max_restr2$var_mean, pch = 21, bg = "#EFC000FF", col = "black", cex = 1)
  }
  
  plot(c(-1,1.5),range(c(ei_rain10$var_025, ei_rain120$var_975,ei_rain10$var_975, ei_rain120$var_025)),type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(y_transform == "log"){
    axis(2,at = log10(yticks), labels = rep("",length(yticks)))
  }else{
    axis(2, at = yticks^0.5, labels = rep("", length(yticks)), las = 1)
  }
  box(bty = 'l')
  rethinking::shade(t(ei_rain18[,c("var_025","var_975")]),
                    ei_rain18$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain18$degrd, ei_rain18$var_mean, lwd = 1, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr8 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                               pred_frame$rain1 == unique(pred_frame$rain1)[5],]
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    lines(c(ei_rain18$degrd[exp_sites$degrd_ind[i]],ei_s0),
          c(ei_rain18$var_mean[exp_sites$degrd_ind[i]],max_restr8$var_mean),
          col = "#EFC000FF", lwd = 2)
    points(ei_rain18$degrd[exp_sites$degrd_ind[i]], ei_rain18$var_mean[exp_sites$degrd_ind[i]], 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1)
    points(ei_s0,max_restr8$var_mean, pch = 21, bg = "#EFC000FF", col = "black", cex = 1)
  }
  plot(c(-1,1.5),range(c(ei_rain10$var_025, ei_rain120$var_975,ei_rain10$var_975, ei_rain120$var_025)),type = 'n',
       axes = FALSE, xlab = "", ylab = "")
  axis(1,at = log10(c(0,0.1,0.3,1,3,10,30) + 0.1), labels = xaxis_labs)
  if(y_transform == "log"){
    axis(2,at = log10(yticks), labels = rep("",length(yticks)))
  }else{
    axis(2, at = yticks^0.5, labels = rep("", length(yticks)), las = 1)
  }
  box(bty = 'l')
  rethinking::shade(t(ei_rain120[,c("var_025","var_975")]),
                    ei_rain120$degrd,col = scales::alpha("#0073C2FF", 0.7))
  lines(ei_rain18$degrd, ei_rain120$var_mean, lwd = 1, col = "#0073C2FF")
  for(i in 1:dim(exp_sites)[1]){
    max_restr20 <- pred_frame[pred_frame$restr == exp_sites$max_restr[i] & pred_frame$degrd == exp_sites$degrd[i] &
                                pred_frame$rain1 == unique(pred_frame$rain1)[7],]
    ei_s0 <- exp_sites$degrd[i] + exp_sites$max_restr[i]
    lines(c(ei_rain120$degrd[exp_sites$degrd_ind[i]],ei_s0),
          c(ei_rain120$var_mean[exp_sites$degrd_ind[i]],max_restr20$var_mean),
          col = "#EFC000FF", lwd = 2)
    points(ei_rain120$degrd[exp_sites$degrd_ind[i]], ei_rain120$var_mean[exp_sites$degrd_ind[i]], 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1)
    points(ei_s0,max_restr20$var_mean, pch = 21, bg = "#EFC000FF", col = "black", cex = 1)
  }
}




coef_plot_f <- function(summary_f = NA,  summary_no_f = NA,
                        parameters = c("b_d","b_r","b_p","b_dp","b_rp","b_add1","b_add1p","b_t","b_add4"), 
                        labels = c("degrd","restr","rain1","degrd:rain1",
                                    "restr:rain1","septic","septic:rain1","time","filter"), 
                        ci_prob = 0.95, 
                        ymar = 0.5,  #fraction of max(nchar(labels)) to adjust y axis margin
                        ...){
    names(summary_f)[match(c("2.5%","10%","50%","90%","97.5%"),names(summary_f))] <- 
      c("X2.5.","X10.","X50.","X90.","X97.5.")
    names(summary_no_f)[match(c("2.5%","10%","50%","90%","97.5%"),names(summary_no_f))] <- 
      c("X2.5.","X10.","X50.","X90.","X97.5.")
    params <- rev(parameters)
    opar <- par()
    summary_f <- summary_f[match(params,row.names(summary_f)),]
    summary_no_f <- summary_no_f[match(params[-1],row.names(summary_no_f)),]
    
    par(mar = c(4,ymar*max(nchar(labels)),1,1), xaxs="r", yaxs="r")
    plot(c(min(summary_f$X2.5.),max(summary_f$X97.5.)),c(1,length(params)), 
         type = 'n', xlab = "coefficient", ylab = "", axes = FALSE) #, …)
    axis(1)
    axis(2, at = 1:length(params), labels = rev(labels), las= 1)
    box(bty = 'l')
    abline(v = 0, lty = 1, col = gray(0.75))
    abline(h = 1:length(params), lty = 2, , col = gray(0.75))
 
  for(i in 1:length(params)){
    yadj <- ifelse(i == 1, 0, 0.2)
    lines(c(summary_f$X2.5.[row.names(summary_f) == params[i]],
            summary_f$X97.5.[row.names(summary_f) == params[i]]),c(i + yadj,i + yadj),
          col = "darkblue", lend = 1)
    if(i > 1)
      lines(c(summary_no_f$X2.5.[row.names(summary_no_f) == params[i]],
              summary_no_f$X97.5.[row.names(summary_no_f) == params[i]]),c(i - yadj,i - yadj),
            col = "darkred", lend = 1)
    lines(c(summary_f$X10.[row.names(summary_f) == params[i]],
            summary_f$X90.[row.names(summary_f) == params[i]]),c(i + yadj,i + yadj),
          lwd = 5, col = "darkblue", lend = 1)
    if(i > 1)
      lines(c(summary_no_f$X10.[row.names(summary_no_f) == params[i]],
              summary_no_f$X90.[row.names(summary_no_f) == params[i]]),c(i - yadj,i - yadj),
            lwd = 5, col = "darkred", lend = 1)
    }
    points(summary_f$mean[match(params, row.names(summary_f))], c(1,(2:length(params)) + yadj), 
           pch = 21, bg = "lightblue", col = "darkblue", cex = 1.2)
    points(summary_no_f$mean[match(params[-1], row.names(summary_no_f))], (2:length(params)) - yadj, 
           pch = 21, bg = "pink", col = "darkred", cex = 1.2)
}

