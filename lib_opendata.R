# source ("opencanada-base.R")

############################################################ #
# ___ Canada related ____ #####
############################################################ #

source("lib_utils.R")
source("cOpenData.R")

library(ckanr); # open ontario
library(cansim) # statcan 
# library(RSocrata) # usa cdc - was built under R version 4.2.2  

# in0 <- list()
# in0$geo  = "Canada" # c("Ontario", "Alberta",  "British Columbia")
# # in0$geo <- dtAll$GEO %>% unique %wo% c("Canada","Yukon", "Nunavut","Northwest Territories") 
# 



#dtGEO and population -----

dtGeo <- data.table(
  GEO = c(  "Ontario", "Quebec", "British Columbia", "Alberta",
            "Manitoba", "Saskatchewan", 
            "Nova Scotia", "New Brunswick",  "Newfoundland and Labrador", "Prince Edward Island",
            "Northwest Territories", "Nunavut", "Yukon", "Canada"  ),
  population = c( 14826276, 8604495, 
                  5214805, 4442879, 
                  1383765, 1179844, 
                  992055, 789225,  520553, 164318, 
                  45504, 39403, 42986, 38246108 ),
  lat = c( 7,6, 12,11,9,10, 5,4,3,2, 15,16, 17, 0 )
)

aGeo <- dtGeo [order(lat)]$GEO  


# statcan.populations not useful see vaccaintion rates for that}


# * pandemic.periods ----

# period = c(
#   ymd("2010-01-05"), # early prepandemic, early flu waves 
#   # ymd("2016-01-01"), # late pre-pandemic, later flu  waves
#   # ymd("2020-01-01"), # four new cause categories added: 3 for covid + NO_INFO
#   # ymd("2020-03-24"), # early pre-vax pandemic, first COVID death
#   # ymd("2020-08-01"), # late pre-vax pandemic, end of first wave
#   ymd("2020-12-14"), # early vax, first vax shot 
#   ymd("2021-05-01") # late vax, vax rate >= 3%
#   # ymd("2021-12-02"), # 3rd doses (first booster)
#   # ymd("2021-10-04")     # dateToday dateMax   
#   # dateToday
# )

pandemic_milestones = c(
  # ymd("2010-01-05"), # early prepandemic, early flu waves 
  ymd("2020-01-01"), # four new cause categories added: 3 for covid + NO_INFO
  ymd("2020-12-14"), 
  ymd("2021-12-01"), # 
  # ymd("2022-01-01"), 
  ymd("2022-03-01") # # 
)
period <- pandemic_milestones

dtPandemicMilestones <- data.table(
  Date = pandemic_milestones,
  Event = ""
)



# dt.melt.metrics.by.when <- function(dt) {
#   measure <- list()
#   for (i in 1:length(aGranularity)) {
#     measure[[i]] = c(aGranularity[i], aGranularity.2019[i], aGranularity.diff[i])
#   }
#   
#   # cols <- aGranularity.paired.with.2019
#   dtAll <-  dt %>% melt(  measure = measure, value.name = aGranularity, 
#                           variable.name = "When", variable.factor = F)
#   
#   # ask Why it does not pick up names automatically
#   dtAll[, When := ifelse(When==1, "Deaths", 
#                          ifelse(When==2, "Prepandemic 5-year average", 
#                                 "Excess over prepandemic"))]
#   dtAll
# }


# Global constants -----



C_OPEN_DATA_TO_CACHE_ALL = F
C_OPEN_DATA_LIVE = F # !C_OPEN_DATA_TO_CACHE_ALL
C_OPEN_DATA_USE_LOCAL = T
C_OPEN_DATA_USE_STORED_CLASSES = F



aGranularity <- 
  c("value", "(4W SMA)", "(13W SMA)", "(26W SMA)", 
    "(52W SMA)", "(104W SMA)", "Monthly", "Quarterly", "Yearly", "Yearly To Date")
# aGranularity.2019 <- paste0(aGranularity, ".2019")
# aGranularity.paired.with.2019 <- c(rbind(aGranularity, aGranularity.2019))
# aGranularity.diff <- paste0(aGranularity, ".diff")


# Function to convert "dd-mm-yyyy-ss" to "ddmmyyyyss" format
convert_to_numeric_format <- function(input_string) {
  # Remove the hyphens from the input string
  output_string <- gsub("-", "", input_string)
  return(output_string)
}

# Function to convert "ddmmyyyyss" to "dd-mm-yyyy-ss" format
convert_to_original_format <- function(input_string) {
  # Insert hyphens at appropriate positions in the input string
  output_string <- paste0(
    substr(input_string, 1, 2), "-", 
    substr(input_string, 3, 4), "-", 
    substr(input_string, 5, 8), "-", 
    substr(input_string, 9)
  )
  return(output_string)
}


if (F){
  # Test the conversion function
  input_string <- "13-10-0783-01"
  output_string <- convert_to_numeric_format(input_string)
  print(output_string)
  # Test the conversion function
  input_string <- "1310078301"
  output_string <- convert_to_original_format(input_string)
  print(output_string)
  
}



remove_common_prefix <- function(strings_vector) {
  # Split each string into words
  words_list <- strsplit(strings_vector, "\\W+")
  
  # Find the longest common prefix
  longest_common_prefix <- Reduce(intersect, words_list)
  
  # Remove the common prefix from each string
  cleaned_strings <- sapply(words_list, function(words) {
    paste(setdiff(words, longest_common_prefix), collapse = " ")
  })
  
  return(cleaned_strings)
}

if (F) {
  # Test the function
  str <- c(
    "Canada, place of residence of mother",
    "Newfoundland and Labrador, place of residence of mother",
    "Prince Edward Island, place of residence of mother"
  )
  
  result <- remove_common_prefix(str)
  print(result)
}

str.clean.geo.canada = function(str, geo) {
  str = str_replace_all(str, ", place of occurrence", "")
  # str:=fct_inorder(str)] 
}

dt.clean.geo.canada = function(dt, geo) {
  try2(setnames(dt, geo, "GEO") ) 
  dt[, GEO := gsub(", place of occurrence", "", GEO)]
  # dt[, GEO:=fct_inorder(GEO)] 
  invisible(dt)
}

# functions(odClass) -----
# These can be made COpenData methods, or not
#   
# 



od.store.me = function(self) {
  self$dateStoredMe = dateToday
  print(paste0(self$myname, ".Rds - stored! On ", self$dateStoredMe))
  saveRDS(self, paste0(self$myname, ".Rds"))
}
od.restore.me = function(self) { 
  self <- readRDS(paste0(self$myname, ".Rds"))
  print(paste0(self$myname, ".Rds - restored. It was stored (cached) on ", self$dateStoredMe, "Last record", self$dateMax))
  self
}

od.get.dt.subset = function () {
  input <- self$input
  if (is.null(input))
    return(self$dt)
  dt <- self$dt[  Date>=input$date.start[1] & Date <=input$date.start[2] ]
  cols <- self$col.factors
  if (!is.na(cols[1]))  dt <- dt[  get(cols[1]) %in% input$factor1 ]
  if (!is.na(cols[2]))  dt <- dt[  get(cols[2]) %in% input$factor2 ]
  if (!is.na(cols[3]))  dt <- dt[  get(cols[3]) %in% input$factor3 ]
  if (!is.na(cols[4]))  dt <- dt[  get(cols[4]) %in% input$factor4 ]
  if (!is.na(cols[5]))  dt <- dt[  get(cols[5]) %in% input$factor5 ]
  if (!is.na(cols[6]))  dt <- dt[  get(cols[6]) %in% input$factor6 ]
  dt.input <- dt
  dt
}


od.fread = function (self, origin="csv.local") {
  # if (!is.na(origin))
  self$origin = origin
  print(self$origin)
  # if ( origin %in%  c("csv.live", "csv.cached", "csv.local") ) 
  if ( origin %>% str_detect("csv") ) 
    self$dt <- fread( self$data[[origin]] )
  else if (origin %>% str_detect("zip") ) 
    self$dt <- readr::read_csv(archive::archive_read(self$data[[origin]]), col_types = readr::cols()) 
  else if (origin %>% str_detect("xls") )
    # self$dt <- readxl::read_excel(self$data[[origin]], sheet = self$data[["xls.sheet"]]) # does not read from url
    self$dt <- openxlsx::read.xlsx(self$data[[origin]], sheet = self$data[["xls.sheet"]])
    
  else if (origin == "socrata" ) 
    self$dt <-RSocrata::read.socrata( self$data[[origin]] )
  else if (origin == "cansim" ) 
    self$dt <- cansim::get_cansim( self$data[[origin]] )
  else if (origin == "ckan" ) 
    self$dt <- fread( ckanr::resource_show(id = self$data[[origin]]) [["url"]] )
  # else if (origin == "json" ) {    
  #   json_string <- jsonlite::read_json( self$data[[origin]] ) 
  #   qqq <- fromJSON(( self$data[[origin]] ))
  #   qqq$fields %>% View
  # }
  
  self$dt %>% setDT
  self
  # invisible(self)
}

od.fwrite = function (self, timestamp=T, tozip=F) {
  strName <- self$data[["csv.local"]]
  strName <- str_replace(strName, ".csv", "")
  
  if (tozip) {
    if (timestamp) strName <- paste0(strName, "_", dateToday, ".zip")  
    else strName <- paste0(strName, ".zip") 
    write.csv(self$dt, file=gzfile( strName ) )
  }  else {
    if (timestamp) strName <- paste0(strName, "_", dateToday, ".csv")  
    else strName <- paste0(strName, ".csv") 
    fwrite(self$dt, strName)
  }
  
  strName
  #   
  #   url.zip <- "https://github.com/opencanada-info/statcan-13100768-deaths-by-age/raw/main/13100768-2022-12-08.zip"
  #   # dt <- readr::read_csv(archive::archive_read(url.zip), col_types = readr::cols()) 
}


od.view = function(self, what=c("info", "lte", "metrics_range", "lastentry" )){
  # "summary","str", "summary.factored"
  if ("info" %in% what) {
    cat("* info: \n" )
    self$info %>% print
    cat("-------------------------------\n" )
  }
  if ( "lte" %in% what) {
    cat("* fLte() <- ")
    self$fLte %>%  print
    cat("-------------------------------\n" )
  }
  cat("* Names: \n")
  self$dt %>% names %>% print
  
  cat("* Factors: \n")
  cat(paste(self$col.factors, collapse = ", ") )
  cat("* Metrics: \n")
  cat(paste(self$col.metrics, collapse = ", ") )
  
  if (!is.null(self$col.factors)) {
    for (i in 1:length(self$col.factors)) {
      cat(".................................\n*    Factor #", i, ": ", self$col.factors[i], " \n" ) 
      self$col.factors.values[[i]] %>%  print
      # self$col.factors.values[[i]] %>% paste(collapse = ", ") %>%  print
    }
  }
  if (!is.null(self$col.metrics)) {
    if ("metrics_range" %in% what) {
      # for (i in 1:length(self$col.metrics)) {
      #   cat("\n* Metric #", i, ": ", self$col.metrics[i], " \n" ) 
      #   self$get.dt.subset() %>% dt2summary(self$col.factors, self$col.metrics[i]) %>% print
      # }
      for (m in self$col.metrics) {
        cat("-------------------------------\n*    Metric: ", m, " \n" ) 
        self$get.dt.subset() %>% dt.metric.range.by.factor(m, self$col.factors) %>% print
      }
      cat("-------------------------------\n" )
    }
  }
  
  cat(" . . . . . . . . . . . . . . . . . \n* Data: \n")    
  self$dt %>% dim %>% cat("\n")
  self$dt[c(1, .N)] %>% print
  if (T %in% what %>% str_detect("str")) {
    cat(" . . . . . . . . . . . . . . . . . \n* Structure and Summary: \n")
    self$dt %>% str
    self$dt %>% summary %>% print()
    self$dt %>% summary.factored() %>% print
  }
  
  cat(" . . . . . . . . . . . . . . . . . \n* Input: \n")
  self$input %>%  print
  
  
  if (!is.null(self$input)) {
    cat(" . . . . . . . . . . . . . . . . . \n*     Subset: ") 
    self$get.dt.subset()  %>% dim %>% cat("dt0: ", ., "\n")
    self$get.dt.subset() %>% .[c(1, .N)] %>% print
  }
  
  if (!is.null(self$col.metrics) & !is.null(self$col.factors)) {
    if ("lastentry" %in% what) {
      cat("* LAST ENTRY: \n" ) 
      self$dt[!is.na(get(self$col.metrics[1])), dateMax:=max(Date), by=mget(self$col.factors)] [Date==dateMax] [, Date:=NULL] %>% print
      
    }
  }
  
  # cat(" . . . . . . . . . . . . . . . . . \n* ") 
  cat("* Value:", self$value, "| * Granularity: ", self$granularity, "| * Averaged over: ", self$convolution_window)
  
}



if (F) {
  dt.melted <- dt %>%  melt (col.factors, measure.vars= (length(col.factors)+1):(length(col.factors)+3), variable.name = "statistic")
  
  dtSummary
  
  # dtSummary %>%  melt (c("GEO", "variable" ), measure.vars= 3:5, variable.name = "metric")
  
  # dtSummary <- dt [, .(metric="mean", value=mean(get(col.value), na.rm = T)), by = c(col.factors)] %>%
  #   rbind ( dt[, .(metric="min", value=min(get(col.value), na.rm = T)), by = c(col.factors) ] ) %>%
  #   rbind ( dt[, .(metric="max", value=max(get(col.value), na.rm = T)), by = c(col.factors) ]  ) %>% 
  #   setorderv(col.factors)
  # dtSummary %>% dcast (GEO + variable  ~ metric)
  # dtSummary %>% dcast (GEO + variable  ~ metric) %>% melt (c("GEO", "variable" ), measure.vars= 3:5, variable.name = "metric")
  
}



# setFactors.Date.Input .Metrics setAll
od.setColumns  = function (self, date, factors, metrics){
  self$col.date = date
  try2(setnames(self$dt, date, "Date")) 
  self$dt[, Date := ymd(Date)] 
  # self$dt %>% setorder(Date)  
  self$dt %>% setorder(-Date)
  
  self$dateMax <- self$dt$Date %>% max(na.rm = T)
  self$dateMin <- self$dt$Date %>% min(na.rm = T)  
  self$dt[, Date:=ceiling_date (ymd(Date), "week", 1)]
  
  self$dt %>% setcolorder(c("Date", factors, metrics))
  
  self$col.metrics <- metrics
  
  self$col.factors <- factors
  if (length(self$col.factors) > 0)
    for (i in 1:length(self$col.factors) ) {
      cat("*" %+% self$col.factors [i] %+% ": \n")
      self$col.factors.values [[i]] <- self$dt[[ self$col.factors [i] ]] %>% unique() %T>% print
    }
  
  # to avoid problems when joining convert all factors to characters
  # but if I don't join and plot only, leave as factors!
  for (col in factors)  {
    self$dt[, (col) := as.character( self$dt[[col]])] #fastest
    self$dt[, (col) := fct_inorder( get(col))] 
  }
  
  self$input <- list()
  self$input$date.start <- c(self$dateMin, self$dateMax)
  
  self$input$metric <- self$col.metrics[1]
  if (length(self$col.factors) > 0) {
    cols <- self$col.factors
    if (!is.na(cols[1])) self$input$factor1 <- self$col.factors.values[[1]][1]
    if (!is.na(cols[2])) self$input$factor2 <- self$col.factors.values[[2]][1]
    if (!is.na(cols[3])) self$input$factor3 <- self$col.factors.values[[3]][1]
    if (!is.na(cols[4])) self$input$factor4 <- self$col.factors.values[[4]][1]
    if (!is.na(cols[5])) self$input$factor5 <- self$col.factors.values[[5]][1]
    if (!is.na(cols[6])) self$input$factor6 <- self$col.factors.values[[6]][1]
  }
  
  self$input$var.Colour = 'None'
  self$input$var.Size = 'None'
  self$input$var.linetype = 'None'
  self$input$var.facetX = '.'
  self$input$var.facetY = '.'
  
  
  self$dt$value <- self$dt[[self$input$metric]]
  self$input
  self %>% invisible()
}

# ask? It does not modify by reference.why??
# assumes Value is get(metric)
od.add.value.by.granularity<- function (self, metric, granularity=aGranularity[1] ) {
  self$value <- metric
  self$granularity <- granularity
  factors <- self$col.factors
  
  self$dt[, ':=' (ww=week(Date), mm=month(Date), qq=quarter(Date), yy=year(Date))]
  
  self$dt[, value:=get(metric)]
  
  if (granularity ==  "Monthly") {
    # /N*52.1429  <- this is needed because in some years - depending on date selection - N can be less than 52 weeks
    self$dt[, ':=' (value=sum(get(metric), na.rm = F), N=.N), by=c("mm", "yy", factors) ][
      , value:=as.integer(value/N*4.34524)][, N:=NULL]
  } else if (granularity ==   "Quarterly") {
    self$dt[, ':=' (value=sum(get(metric), na.rm = F), N=.N), by=c("qq", "yy",  factors) ][
      , value:=as.integer(value/N*4.34524*3)][, N:=NULL]
  } else if (granularity ==  "Yearly") {
    self$dt[, ':=' (value=sum(get(metric), na.rm = F), N=.N), by=c("yy",  factors) ][
      , value:=as.integer(value/N*52.1429)][, N:=NULL]
  } else if (granularity ==  "Yearly To Date") {
    self$dt[, `value`:= 52.1429*as.integer(frollmean(get(metric), 52, align = "left", fill = 0)), by = factors]
  } else if (granularity ==  "(4W SMA)") {
    self$dt[, `value`:= as.integer(frollmean(get(metric), 4, align = "left", fill = 0)), by = factors]
  }  else if (granularity ==  "(13W SMA)") {
    self$dt[, `value`:= as.integer(frollmean(get(metric), 13, align = "left", fill = 0)), by = factors]
  } else if (granularity ==  "(26W SMA)") {
    self$dt[, `value`:= as.integer(frollmean(get(metric), 26, align = "left", fill = 0)), by = factors]
  } else if (granularity ==  "(52W SMA)") {
    self$dt[, `value`:= as.integer(frollmean(get(metric), 52, align = "left", fill = 0)), by = factors]
  } else if (granularity ==  "(104W SMA)") {
    self$dt[, `value`:= as.integer(frollmean(get(metric), 104, align = "left", fill = 0)), by = factors]
  }
  # dt[, .(ww,mm,qq,yy):=NULL]
  # dt[, .(value):=NULL]
  self %>% invisible() 
}

# od.add.metric.granularity <- function(self, metric) {
#   self$dt %>% dt.add.metric.granularity.over.Date (metric, self$col.factors)
#   # self$metric <- metric #self$input$metrc
# }

od.add.value.by.convolution <- function (self, metric, convolution_window=4) {
  
  metric <- self$input$metric
  # self$value <- metric
  self$convolution_window <- convolution_window
  
  if (convolution_window <= 1) {
    self$dt[, value:= get(metric) ]
  } else {
    self$dt[, value:= as.integer(frollmean(get(metric), convolution_window, align = "left", fill = 0)), by = eval(self$col.factors)]
  }
  # dt
  # dt %>% invisible()
}





# dt.add.baseline <- function (dt, factors, from=2018, to=2019) {

dt.add.prepandemic.value <- function (dt, factors, granularity, nYearsBeforePandemic=5) {
  # nYearsBeforePandemic = 5
  # dt <- self$dt
  # factors <- self$col.factors
  # granularity <- self$granularity
  q <- quote( Date >=  ymd("2020-01-01") - years(nYearsBeforePandemic) & Date < ymd("2020-01-01") )
  
  if (granularity ==  "Monthly") {
    dt2019 <-dt [ eval(q) ,.(value.2019=as.integer(mean(value, na.rm=T))), by=c( "mm", factors )]
    dt <- dt2019[dt, on=c( "mm", factors ) ]
  }   else if (granularity ==   "Quarterly") {
    dt2019 <-dt [eval(q) ,.(value.2019=as.integer(mean(value, na.rm=T))), by=c( "qq", factors )]
    dt <- dt2019[dt, on=c( "qq", factors ) ] 
  }   else if (granularity ==  "Yearly") {
    dt2019 <- dt [eval(q), .(value.2019=as.integer(mean(value, na.rm=T))), by=c( "yy", factors )]
    dt <- dt2019[dt, on=c( "yy", factors ) ]
  } else {
    dt2019 <- dt [eval(q), .(value.2019 = as.integer(mean(value, na.rm=T)) ), by=c( "ww", factors )] 
    dt <- dt2019[dt, on=c( "ww", factors ) ]
  }
  
  rm(dt2019)
  
  dt[, difference:= value - value.2019] 
  setcolorder(dt, names(dt) %wo% c(c("value", "value.2019", "difference")))
  
  dt
}

od.add.prepandemic.value <- function (self, nYearsBeforePandemic=5) {
  dt <- self$dt
  factors <- self$col.factors
  granularity <- self$granularity
  
  self$dt <- self$dt %>% dt.add.prepandemic.value(factors, granularity, nYearsBeforePandemic)
  self
}



od.plot.layout <- function () {
  g <-
    # theme_bw() +
    theme(legend.position = "bottom")  +
    # geom_hline(yintercept=0, col="black" ) +
    #   # theme(legend.position = "none") +
    guides(x =  guide_axis(angle = 60)) +
    guides(y =  guide_axis(angle = 60))  
  #   
  #   # scale_x_date(date_labels = "%Y") +
  #   scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") 
  # scale_x_continuous(breaks=(3:9)*10, limit=c(30,90)) +
  # geom_vline(aes(xintercept=50), col="black", linetype=4) +
  
  # coord_flip() +
  #       scale_y_continuous(breaks = (0:10)*10,limits=c(0,100)) +
  # scale_fill_brewer(palette = "Greens", direction = 1) +   #scale_fill_grey(0.3, 0.9) +
  
  
  
  # coord_cartesian(ylim = c(0, 100)) +
  # ylim(0, 100) + # coord_cartesian(xlim = c(0, 100))
  # guides(fill="none", col="none") +
  
  
  
  #  # scale_y_log10(breaks = c(1, seq(0, 1, 0.1))) +
  #     scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x) ) +
  # # scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), labels = trans_format('log10', math_format(10^.x))) +
  
  
  # labs(
  #   title= self$info[["title.short"]],
  #   caption = paste0(self$info[["source"]]), 
  #   x=NULL,
  #   y=NULL
  # )
  g
}

od.plot <- function (self) {
  
  # cols <- self$col.factors
  input <- self$input
  
  dt <- self$get.dt.subset()
  
  g <- ggplot(dt, aes(x=Date, 
                      y=value
                      # y=get(input$metric)
  )
  ) + 
    
    theme(legend.position = "bottom")  +
    #   # theme(legend.position = "none") +
    
    guides(x =  guide_axis(angle = 60)) +
    # guides(y =  guide_axis(angle = 60)) +
    labs(
      # title=self$info[["title"]] [["short"]],
      y=self$input$metric, 
      color = "Colour",
      # color = self$input$var.Colour, # NULL
      x=NULL
    ) +
    
    # scale_color_manual(name=NULL) +
    
    
    # geom_point()  + 
    geom_line() 
  
  
  if (input$var.Colour != 'None')
    g <- g + aes(color=get(input$var.Colour))
  if (input$var.Size != 'None')
    g <- g + aes(size=get(input$var.Size))

  facets <- paste(input$var.facetX, '~', input$var.facetY)
  if (facets != '. ~ .')
    g <- g + facet_grid(facets, scales = ifelse(input$g.sameScale==F, "free", "fixed") )
  

  # if (input$g.markMilestones)
  #   g <- g +   g.add.milestones(dtPandemicMilestones)
  if (input$g.smooth)
    g <- g + geom_smooth(se = F)
  if (input$g.smooth.lm)
    g <- g + geom_smooth(method = "lm", na.rm=T)
  if (input$g.showXzero)
    g <- g + geom_hline(yintercept=0, col="black" )
  
  
  # renderPlot(g)
  
  if (input$interactive) {
    renderPlotly( g %>% ggplotly ) %>%  return
  } else {
    renderPlot(g) %>% return
  }
  
}





od.grid <- function (self){
  
  input <- self$input
  # req(self$input$interactive) # will not show anything ever!
  
  # dt <- self$get.dt.subset()
  # dt$dummy <- NULL
  # 
  # # try2(setnames(dt, "value", self$input$granularity))
  # # try2(setnames(dt, "value.2019", paste0(self$input$granularity,".2019")) )
  # 
  # # if ( shiny::isRunning() ) {
  # if (self$input$interactive) {
  #   addSpinner(
  #     dt %>% datatable.title()  %>% renderDataTable
  #   ) #, spin="circle", color="green")
  # } else {
  #   dt %>% dt.with.date.as.character %>% renderTable
  # }
  # # } 
  # # else {
  # #     dt
  # #   }
  
  
  
  if ( input$t.showSelectionOnly ) { 
    dt <- self$get.dt.subset()
  } else {
    dt <- copy(self$dt)
  }

  dt$dummy <- NULL
  
  if (input$t.wide)
    dt <- dt %>% dcast( Date ~ ...)
  
  switch (input$t.check.printDT,
          "nothing" = NULL,
          "str" = renderPrint( dt %>% str),
          "summary" = renderPrint( dt %>% summary),
          "summary2" = {
            
            cols = dt %>% names
            dt0 <- copy(dt)
            dt0[, (cols):=lapply(.SD, as.character), .SDcols = cols]
            dt0[, (cols):=lapply(.SD, as.ordered), .SDcols = cols]
            
            # dt <-  dt %>%
            #   mutate_if(is.character, as.factor) %>%
            #   mutate_if(is.integer, as.factor) %>% as.data.table()
            
            
            # dt <-  dt %>%
            #   mutate_if(is.numeric, as.integer) %>%
            #   mutate_if(!is.numeric, as.factor) %>% as.data.table()
            
            renderPrint( dt0 %>% summary)
          },
          "head" = renderPrint( dt[c(1:input$t.rows, max(1,(.N-input$t.rows+1) ):.N) ] ),
          "table" =   renderTable( dt[c(1:input$t.rows, max(1,(.N-input$t.rows+1) ):.N) ] , rownames = T ),
          "interactive" = renderDataTable( datatable.title( dt, "Click on column names to sort. Filter by typing in boxes under column names. Save selected data by clicking the buttons") )
          
  )
  
  
  
}   

# TESTING ################

od.renderTable <- function (self){
  fluidRow( 
    column (width=12, 
            
            
            # print result #####
            
            renderUI({
              
              # dt <- r$dt
              req(self)
              req(input$check.printDT)
              
              if ( "selected.columns" %in% input$check.show ) { # a0.search.details == T
                dt <- self$dt
              } else {
                dt <- self$dt [, input$select.columns , with=F]
              }
              # dt %>% renderPrint
              
              switch (input$check.printDT,
                      "nothing" = NULL,
                      "str" = renderPrint( dt %>% str),
                      "summary" = renderPrint( dt %>% summary),
                      "summary2" = {
                        
                        cols = dt %>% names
                        dt0 <- copy(dt)
                        dt0[, (cols):=lapply(.SD, as.character), .SDcols = cols]
                        dt0[, (cols):=lapply(.SD, as.ordered), .SDcols = cols]
                        
                        renderPrint( dt0 %>% summary)
                      },
                      "head" = renderPrint( dt[c(1:input$rows, (.N-input$rows+1):.N) ] ),
                      
                      "table" =   renderTable( 
                        dt[c(1:input$rows, (.N-input$rows+1):.N) ], 
                        rownames = T ),
                      
                      # dt0 <- r.dtExcess() [, Date:=format(Date,'%Y-%m-%d')] [order(-Date)]
                      
                      "interactive" = renderDataTable( datatable.title( dt, "Click on column names to sort. Filter by typing in boxes under column names. Save selected data by clicking the buttons") )
                      
              )
              
              
              
            })
            
    ) # column
  ) # fluidrow
  
}



if (F) { # HOW TO USE lib_opendata.R ----
  
  # source("lib_opendata.R")
  
  
  self <- COpenData$new( myname = "odStatcan.deaths.by.age" )
  
  if (input$select.data_live) {
    self %>% od.fread(origin="cansim") 
    self %>% od.fread(origin=self$data[["origin"]])
    if (F) {
      self %>% od.view()
      self %>% od.fwrite(timestamp = F);
      self %>% od.fwrite(timestamp = T,tozip=T);
      self %>% od.fwrite(timestamp = T);
    }
    self$fLte(self)
    if (F){
      self$store.me()
    }
  }  else {
    self$restore.me()
  }
  

  if (T) {
    self$dt
    self$dt.input
    self$input
    self$info
    self$info[["title.full"]]
    self$info[["title"]]
    self$info[["title.short"]]
    self$info[["title.new"]]
    
    self$info[["source"]]
    
    self %>% od.view()
    self$dt
    self$dt$GEO %>% u
  }
  
  
  if (F) {
    self$col.factors.values
    self$dateMax
    self %>% od.view()
    self$dt
  }
  
  
  self %>% od.add.value.by.granularity (metric="Weekly")
  
  
  
  self %>% od.view()
  self$dt; 
  self$fLte
  self$dt[!is.na(get(self$col.metrics[1])), .SD[.N], by=mget(self$col.factors)] 
  self$input
  self$get.dt()
  self$get.dt.subset()
  
  self$get.dt() %>% dt.metric.range.by.factor(metric = self$col.metrics[1], factors = self$col.factors)
  
  self$get.dt.subset() 
  
  self$get.dt() %>% datatable.title() 
  self$get.dt.subset() %>% datatable.title() 
  dt.with.date.as.character %>% datatable.title() 
}


