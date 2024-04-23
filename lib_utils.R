# opendata-utils.R

# Other tools for data vis-ing:
# https://chart-studio.plotly.com/create/
# https://data.cdc.gov/d/54ys-qyzm/visualization

#0. Global settings ----
if (T) {   
  library(magrittr); 
  library(ggplot2); 
  # library(dtplyr)
  library(lubridate,  quietly=T); options(lubridate.week.start =  1)
  library(data.table); options(datatable.print.class=TRUE)
  # library(dygraphs)
  library(plotly); library(DT); 
  # library(heatmaply);  
  # library(ggpubr)
  library(stringr); library(forcats) 
  library(R6)
  library(glue)
  library("readxl")
  
  options(digits = 3,
          #max.print = 100, # 1000
          scipen = 999) # remove scientific notation
  options(dstringsAsFactors = F)
  options(quit_onError=F,
          stopErr = F, warnOut = F)
  
  
  
  dateToday <- format(Sys.time(), '%d %B, %Y') %>% dmy; dateToday
  
  "%wo%" <- function(x, y) setdiff(x,y) 
  "%wo2%" <-function(x,y) x[ ( ! x %in% y )]
  "%ni%" <-  Negate(`%in%`)
  "%+%" <- function(x, y) paste(x,y)
  
  yesno <- c("Yes", "No")
  
  
  theme_set(theme_bw())
  # theme_set(theme_minimal())
  # library(ggthemes); 
  # theme_set(theme_economist())
  # theme_set(theme_economist_white())
    

  
}

u = function(...) {   unique (...)}

of <- function (dt, cols)  {
  dt[, cols, with=F]
  # Note also other ways to select column(s) in data.table
  # dt[, mget(cols), 
  # dt[, ..cols] # FOR ONE COLUMN ONLY
  # dt[,.SD, .SDcols=cols] 
}




dygraph.title <- function(dts, title=NULL, group="1st group") {
  dygraph(dts, main = title, group = group) %>%
    # dySeries(input$var1, color = input$color1, strokePattern = input$stroke1,  axis = input$axis1 )  %>% 
    dyOptions(fillGraph = F, stepPlot = F, drawGrid = T, drawPoints = TRUE, pointSize = 2) %>%
    dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    # dyAxis("y", label="Deaths / week") %>%
    # dyAnnotation("2021-5-1", text = "3%", tooltip = "Fully Vaccinated Rate 3%") %>%
    # dyAnnotation("2021-4-6", text = "2%", tooltip = "Fully Vaccinated Rate 2%") %>%
    # dyAnnotation("2021-6-10", text = "10%", tooltip = "Fully Vaccinated Rate 10%") %>%
    # dyAnnotation("2021-2-18", text = "1%", tooltip = "Fully Vaccinated Rate 1%") %>%
    dyRangeSelector() 
}




# dt0_wide <- dt0 %>% dcast(GEO    +   Date ~ `Cause of death (ICD-10)`, value.var = "value")


# renderTable.with_date <- function (dt0) {
#   # dt0_wide <- dt0 %>% dcast(GEO    +   Date ~ `Cause of death (ICD-10)`, value.var = "value")
#   # dt0_wide() [, Date:=format(Date,'%Y-%m-%d')] %>% renderTable
#   dt0  [, Date:=format(Date,'%Y-%m-%d')] %>% renderTable
# }
# 
# renderTable.melted <- function (dt0) {
#   # dt0_wide <- dt0 %>% dcast(GEO    +   Date ~ `Cause of death (ICD-10)`, value.var = "value")
#   # dt0_wide() [, Date:=format(Date,'%Y-%m-%d')] %>% renderTable
# }

# Consider also: paged_table(dt), reactable() and https://gt.rstudio.com/
# https://rstudio.github.io/DT/options.html


# Click on column names to sort. Filter by typing in boxes under column names. 
# Press Buttons above to save selected data.
datatable.title <- function(dt0, 
                            title="Tip: You can rearrange, sort, and filter data by dragging 
column names and using filter boxes."
) {
  
  dt <- copy(dt0)
  cols  <-  which(sapply (dt,is.character)); 
  dt[, (cols):=lapply(.SD, ordered), .SDcols=cols]
  
  
  dt %>% DT::datatable (
    filter = "top",  
    caption = title,
    rownames=F,   
    extensions =  c('ColReorder', 'Buttons'),
    # extensions =  c('ColReorder'),
    options = list(
      dom = 'Blfrtip',
      # paging = FALSE, 
      # pageLength=50, scrollX='400px', # horizontal scroll
      scrollX = TRUE,       scrollY = "600px",
      
      colReorder = TRUE,
      lengthMenu = list(c(10,25,100,-1), c(10,25,100,"All")),
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print') 
    ) )
}



datatable.title2 <- function(x){
  DT::datatable(x,
                extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               lengthMenu = list(c(10,25,50,-1),
                                                 c(10,25,50,"All"))))
}

datatable.fixedCols <- function(dt, l=1, r=2, title=NULL) {
  dt %>% datatable( extensions = 'FixedColumns',
                    caption = title,
                    rownames=F,   
                    options = list(
                      dom = 't',
                      rowId = 0,
                      scrollX = TRUE,
                      fixedColumns = list(leftColumns = l, rightColumns = r)
                    )
  )
}

print.trace.start <- function(label= "") {
  print("------------------------------")
  print(paste0("[TRACING STARTS] ",label))
}
print.trace.end <- function(label="") {
  print(paste0("[TRACING ENDS] ", label))
  print("----------------------------- ")
}

try2 <- function(code, silent = FALSE) {
  tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent) message(c)
    invisible(structure(msg, class = "try-error"))
  })
}
# try.fread <- function(file, ...) {
#   tryCatch(fread2(file, ...), error = function(c) {
#     c$message <- paste0(c$message, " (in ", file, ")")
#     stop(c)
#   })
# }
if (F) {
  # try2(1)
  # #> [1] 1
  # try2(stop("Hi"))
  # #> Error in doTryCatch(return(expr), name, parentenv, handler): Hi
  # try2(stop("Hi"), silent = TRUE)
  
  try2(setnames(dt, "VALUE", "val_norm"))
  cDeaths$dt <- NULL
  strError <- try2(
    cDeaths$dt <- readRDS(paste0 (cDeaths$table.short, ".Rds") ) 
  )
  
  
  strError <- try2({
    dt <- cansim::get_cansim(cDeaths$table) %>%  setDT(dt)
  })
  if (is.character(strError)) {
    showModal(modalDialog(
      title = "Cant open StatCan portal:", strError, easyClose = TRUE,
      footer = "trying to open from Cache." # NULL
    ))
  }
  strError <- try2(
    cDeaths$dt <- readRDS(paste0 (cDeaths$table.short, ".Rds") ) 
  )
  if (is.character(strError)) {
    showModal(modalDialog(
      title = "Error:", strError, easyClose = TRUE,
      footer = "Press anywhere to close this window" # NULL
    ))
  }
}


dt.findCategoricalColumns <- function (dt) {
  cols  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );cols
}

dt.convertColumnToOrdered <- function(dt, cols) {
  for (col in cols) dt[, (col) := ordered(dt[[col]])] #fastest
  # or
  # dt[, (cols):=lapply(.SD, ordered), .SDcols=cols]
  # dt[, cols] <- lapply(dt[, cols, with = FALSE], ordered)
}


# dt.findColumnsByType <- function (
    #     dt, is.what=function(x){is.ordered(x) | is.integer(x) | is.character(x)}
# ) {
#   cols <- names(dt[sapply(dt, is.what)]); cols
# }
# 
# dt.findColumnsByFunctionInNthRow <- function (dt, is.what=function (x) {x>0}, rows_to_use=1) {
#   # cols <- names(dt[sapply(dt, is.what)]); cols
#   cols  <- dt[rows_to_use, which(sapply(.SD, is.what))];cols
# }





dt.create.sample.dataset <- function(ndays = 65, nstates=3) {
  set.seed(0);
  DT = data.table(
    state = paste0(LETTERS[1:nstates],LETTERS[1:nstates]),
    date = rep(as.Date(1:ndays, origin="2023-01-01"), 40),
    age = paste0(round(rpois(ndays*nstates, 40),-1), "+"),
    sex = round(runif(ndays*nstates),0),
    births = rpois(ndays*nstates, 20),
    deaths = rpois(ndays*nstates, 20)
  )
  DT[, sex:=as.factor(ifelse(sex==1, "M", "F"))]
  DT
}

# By reference, or  USE THIS: %>% .[cols, with=F]
dt.select <- function (dt, cols)  {
  dt [, ( names(dt) %wo% cols ) := NULL]
}
dt.remove <- function (dt, cols)  {
  dt[, (cols) := NULL] 
}

dt.with.character.as.factor= function(dt, cols=NA) { 
  if (is.na(cols))
    dt2 <- copy(dt)
  else
    dt2 <- dt[ , ..cols]
  # cols  <-  which(sapply (dt,is.character)); 
  # dt[, (cols):=lapply(.SD, ordered), .SDcols=cols]
  for (col in which(sapply (dt,is.character)) )  
    # dt2[, (col) := ordered(dt[[col]])] #fastest
    dt2[, (col) := fct_inorder( get(col)) ] 
  dt2
}

dt.with.date.as.character = function(dt, cols=NA) { 
  if (is.na(cols))
    dt2 <- copy(dt)
  else
    dt2 <- dt[ , ..cols]
  # dt2 [, Date:=format(Date,'%Y-%m-%d')]
  dt2 [, Date:=as.character(Date)]
  dt2
}


# dt.metricstats.by.factors = function (dt, factors, metric) {
#     dt %>% dt.summary(factors, metric)
# }

# dt.summary

dt.metric.range.by.factor = function(dt, metric, factors,  melt=F) { # One metric only!
  dtSummary <- dt [, .(
    # ave = median(get(metric), na.rm = T), 
    ave = mean(get(metric), na.rm = T),
    min = min(get(metric), na.rm = T),
    max= max(get(metric), na.rm = T)
  ), by = c(factors) ]  %>% 
    setorderv(factors)
  if (melt) {
    dtSummary <- dtSummary %>%  melt (factors, measure.vars= (length(factors)+1):(length(col.factors)+3), variable.name = "statistic")
  }
  dtSummary
}

if (F){
  dtSummary %>%  melt (c("GEO", "variable" ), measure.vars= 3:5, variable.name = "metric")
  
  dtSummary <- dt [, .(metric="mean", value=mean(get(col.value), na.rm = T)), by = c(col.factors)] %>%
    rbind ( dt[, .(metric="min", value=min(get(col.value), na.rm = T)), by = c(col.factors) ] ) %>%
    rbind ( dt[, .(metric="max", value=max(get(col.value), na.rm = T)), by = c(col.factors) ]  ) %>%
    setorderv(col.factors)
  dtSummary %>% dcast (GEO + variable  ~ metric)
  dtSummary %>% dcast (GEO + variable  ~ metric) %>% melt (c("GEO", "variable" ), measure.vars= 3:5, variable.name = "metric")
}



# opencanada.dt.melted()???
dt.melt.by.factors = function (dt, factors) {
  dt%>% melt(1:length(factors), variable.factor = T, variable.name = "metric") 
}


############################################################# #


# https://stackoverflow.com/questions/75785642/how-to-create-functions-for-geom

g.add.milestones <- function(dtMilestones) {
  list(
    geom_vline(xintercept = dtMilestones$Date, linetype=5),
    geom_label(aes(x=Date, y = Inf, label=Event),
               label.size = 0.5, vjust = 1,
               data = dtMilestones)
  )
}


if (F) {
  DT <- data.table(
    Date = as.Date(1:100, origin="2010-01-01"),
    state = LETTERS[1:3],
    sex = c("m","f"),
    value = as.integer(runif(1:100)*100) 
  )
  dtMilestones <- data.table(
    Date = paste( "2010-15-", 2:4) %>% ydm,
    Event = paste( "Phase ", 1:3)
  )
  
  g0 <- ggplot(DT, aes(Date, value)) +
    geom_line() +
    facet_grid(state ~ sex, scales = "free")
  g0
  g0 + g.add.milestones(dtMilestones)
}



g.geom_point <- function(yy, colour = "black"){
  list(
    geom_line(mapping = aes(y= yy),  col = colour), data = data.frame(x, yy),
    geom_point(mapping = aes(y = yy), col = colour, data = data.frame(x, yy))
  )
}
if (F) {
  ggplot(mapping = aes(x))  + 
    my_geom_y(x, "red") + 
    my_geom_y(x/2, "yellow")
}


if (F) { # SANDBOX zone ----
  
  DT <- data.table(
    # today = as.Date(1:3, origin="2023-01-01"),
    # weekday = rep(TRUE,3),
    parent.name = words[101:103],
    parent.dob = as.Date(1:3, origin="2020-01-01"),
    child_boy = words[11:13],
    child_girl = words[21:23],
    child_trans = words[201:203],
    dob_boy= as.Date(1:3, origin="2010-01-01"),
    dob_girk= as.Date(1:3, origin="2012-01-01"), 
    dob_trans= as.Date(1:3, origin="2022-01-01"), 
    weight = 11:13
  )
  DT
  
  DT.final <- data.table(
    today = as.Date(1:3, origin="2023-01-01"),
    weekday = rep(TRUE,3),
    who = c("parent", "boy", "girl", "trans"),
    dob = as.Date(1:3, origin="2020-01-01"),
    name = words[11:13]
  ); DT.final
  
  DT2 <- melt(DT,  id_=c(1,2) , measure=patterns(dob="^dob_", name="^child_"), value.factor=TRUE, 
              variable.name =  "child")
  DT2 [child=="1", child:="boy"]
  DT2 [child=="2", child:="girl"]
  DT2 [child=="3", child:="trans"]
  DT2 %>% setorder("parent.dob")
  DT2
  
  
  DT3 <- melt(DT2,  id_=c(1,5) ,
              measure.vars = c("parent.dob", 'dob'),
              value.factor=TRUE, value.name = "Date",
              variable.name =  "type")
  DT3
  DT3 %>% setorder("Date") %>% setcolorder("Date")
  
  
  DT3 %>% dcast( Date ~ weight)
  DT3 %>% dcast( Date + name + child ~ weight)
  DT3 %>% dcast( Date ~ ...)
  
  data()
  
  data("EuStockMarkets")
  
  EuStockMarkets
  LakeHuron 
  JohnsonJohnson 
  
}


