

# cOpenData.R -----

COpenData <-  R6Class(   
  classname = "COpenData", # portable = F, cloneable = F, # 
  private = list(
    dateCOpenClassRelease = "2023-04-27" # "2023-04-14" 
  ),
  
  public = list(
    myname = NULL,
    myapp = "https://open-data.sinyapps.io/info",
    
    # user entered  fields ----
    
    info = c (
      source = NA, title=NA, title.short=NA, 
      url = NA,    url.cached = NA,
      comment="", comment.priv="",
      frequency = "get(metric)", updated = "monthly",
      table = NULL, # table.short = NULL,  # optional
      webpage.live  = NULL,       url = NA,    
      url.cached = NA,
      dateLatestRelease = NA, dateCached = NA
    ),
    data = c (
      origin = "csv.live",
      csv.live = NULL, csv.local = NULL, csv.cached = NULL,
      zip.live = NULL, zip.local = NULL, zip.cached = NULL, 
      xls.live = NULL, xls.local = NULL,  xls.local = NULL, xls.sheet = NULL, 
      gsheet = NULL, gsheet.sheet = NULL
    ), 
    origin = c("live", "cached", "local"), # private
    dt = NULL, 
    dt.input = NULL,
    
    dateStoredMe = NA, dateMin = NA, dateMax = NA,
    
    col.factors = NULL,
    col.factors.values = NULL, # list()
    col.date = NULL, 
    col.metrics = NULL,
    
    # Characteristics = NULL, # of the 1st metric?  is it really needed "
    
    value = NA, # which col.metrics is analyzed
    granularity = NA, #aGranularity[8],
    convolution_window= NA,
    input =  NULL,
    
    initialize = function( myname=NA,  info=NA, data=NA, create.sample.data = F  ) {
      self$myname = myname;   self$info <- info; self$data <- data
      if (create.sample.data) dt=dt.create.sample.dataset()
    },
    
    store.me = function() {
      self$dateStoredMe = dateToday
      print(paste0(self$myname, ".Rds - stored! On ", self$dateStoredMe))
      saveRDS(self, paste0(self$myname, ".Rds"))
    },
    restore.me = function() { 
      readRDS(paste0(self$myname, ".Rds")) 
      print(paste0(self$myname, ".Rds - restored! It was stored (cached) on ", self$dateStoredMe, "\n"))
    }, 
    
    get.dt = function () { # if it were private
      self$dt
    },
    get.dt.subset = function () {
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
    },
    
    fLte = NA,
    fLte.Define = function (new.fLte) { 
      self$fLte <-  new.fLte
    },
    view = NA,
    view.Define = function (new.view) {
      self$view <-  new.view
    }
  )
)