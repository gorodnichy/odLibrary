
library(shiny)
# shinybusy::add_busy_bar(color = "Red", height = "40px", centered = T)
library(shinybusy)

# https://stackoverflow.com/questions/33925430/shiny-plot-results-in-popup-window https://stackoverflow.com/questions/61667924/create-a-small-window-to-describe-the-meaning-of-filters-in-shiny
library(shinyBS) 

library(shinyWidgets) #   shinyWidgets::addSpinner()
# shinyWidgets::shinyWidgetsGallery() requires 
# library(shinydashboard);    library(webshot)


if (F) {
  # https://stackoverflow.com/questions/63011154/r-shiny-include-button-that-collapses-one-part-and-expands-another-part-of-a-pan
  library(shinyjs)
  
  ui <- fluidPage(
    useShinyjs(),
    actionButton(inputId = "go",label = "Button"),
    div(id="div_a","Hello There",style="background-color:red;"),
    div(id="div_b","General Kenobi",style="background-color:green;")
  )
  
  server <- function(input, output, session) {
    
    hide(id = "div_b")
    
    observeEvent(input$go,{
      toggle(id = "div_a",anim = T)
      toggle(id = "div_b",anim = T)
    })
  }
}



# r <- list( )-----------------------------------------------------------

# if (T) {
#   r$use.png.instead.of.ggplot <- T
#   r$date.start <- c(ymd("2018-01-01"), ymd(dateToday - months(1)) )
# }
# 
# in0$granularity  = isolate( r$granularity )





# unshiny() function  ----

if ( shiny::isRunning()) {
  r <- reactiveValues()
} else {
  r <- list()
  input <- list()
}


# if ( is.character( try2(UNSHINY_ALL_CODE) ) == F) if (UNSHINY_ALL_CODE == T ) {
#   print(" -- All shiny:: functions are 'unshiny'ied")
#   UNSHINY_ALL_CODE_DONE <<- T
# # }
if (!shiny::isRunning()) {
  
  selectInput <- function(inputId, label, choices, selected = NULL, multiple = FALSE, 
                          selectize = TRUE, width = NULL, size = NULL) {
    if (!is.null(selected)) {
      attr(selected, "names") <- inputId
      input[[inputId]] <<- selected 
    } else {
      attr(choices, "names") <- inputId
      input[[inputId]] <<- choices[1]
    }
    # if (!is.null(selected)) {
    #   attr(selected, "names") <- inputId
    #   input[[inputId]] <<-  ifelse( multiple, selected, selected[length(selected)] ) 
    # } else {
    #   attr(choices, "names") <- inputId
    #   input[[inputId]] <<- ifelse( multiple, choices, choices[length(selected)] ) 
    # }
    
    
  }
  checkboxGroupInput <- function (inputId, label, choices = NULL, selected = NULL, ...) {
    attr(choices, "names") <- attr(selected, "names") <- inputId
    if (!is.null(selected)) input[[inputId]] <<- selected else input[[inputId]] <<- choices[1]
  }
  checkboxInput <- function (inputId, label, value = FALSE, width = NULL) {
    if (!is.null(value) ) attr(value, "names") <- inputId
    input[[inputId]] <<- value; 
  }
  sliderInput <- function (inputId, label, min, max, value, ...) {
    if (!is.null(value) ) attr(value, "names") <- inputId
    input[[inputId]] <<- value 
  }
  actionButton <- updateActionButton <- function (inputId, label, ...) {
    value=0
    attr(value, "names") <- inputId
    input[[inputId]] <<- value 
  }
  tipify <- popify <- addSpinner <- function(expr, ...) {
    eval(expr)
  }
  
  hr <- function() {}
  
  observe <- req <-     
    function (expr, ...) { eval(expr) %>% invisible() }
  
  observeEvent <- 
    function (go, expr, ...) { eval(expr) %>% invisible() }
  
  
  renderPrint <- renderTable <- renderDataTable  <-   
    renderPlot  <- renderPlotly <-   renderText <-  renderUI  <- 
    function (expr, ...) { eval(expr) %>% print }
  
  splitLayout <- sidebarLayout <- verticalLayout <- flowLayout <-  
    function(...) { lapply(list(...), eval) }
  
  reactiveValuesToList <- function(input) {
    input
  }
  
} 


# 
# 
# shinyBRRRRR.input.for.DtwithDate.with.input <- function(dt, factors, metrics, input <---- TESTING THIS) {
#   req(dt)
#   
#   # from template-dashboard.Rmd
#   # if ( names(dt) %in% "Date" ) return ("Data does not contain 'Date' column")
#   
#   # cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
#   dateMax <- dt$Date %>% max(na.rm = T)
#   dateMin <- dt$Date %>% min(na.rm = T)  
#   
#   col.metrics <- metrics
#   col.factors <- cols <- factors
#   cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
#   col.factors.values <- list()
#   if (length(col.factors) > 0)
#     for (i in 1:length(col.factors) ) {
#       cat("*" %+% col.factors [i] %+% ": \n")
#       col.factors.values [[i]] <- dt[[ col.factors [i] ]] %>% unique() %T>% print
#     }
#   
#   
#   flowLayout(
#     # h5("Select options:"),
#     
#     
#     
#     tipify(
#       selectInput("granularity", "Select granularity and dates:", aGranularity, aGranularity[8] ),
#       "nW SMA signifies Simple Moving Average over 'n' weeks."
#     ),
#     tipify(
#       checkboxInput( "convolution", "Average over time",  F),
#       title = "Apply smoothing average"
#     ),
#     
#     sliderInput("date.start", NULL,
#                 min = dateMin, 
#                 max = dateMax, 
#                 # value=c(ymd("2019-01-01"), dateMax ),
#                 value=c(input$date.start[1], input$date.start[2] ),
#                 timeFormat="%Y-%m-%d", ticks=F, width = "100%"),
#     
#     
#     if (!is.na(cols[1])) selectInput("factor1", NULL, col.factors.values[[1]], col.factors.values[[1]], multiple = !input$simplified),
#     if (!is.na(cols[2])) selectInput("factor2", NULL, col.factors.values[[2]], col.factors.values[[2]], multiple = !input$simplified),
#     if (!is.na(cols[3])) selectInput("factor3", NULL, col.factors.values[[3]], col.factors.values[[3]], multiple = !input$simplified),
#     if (!is.na(cols[4])) selectInput("factor4", NULL, col.factors.values[[4]], col.factors.values[[4]], multiple = !input$simplified),
#     if (!is.na(cols[5])) selectInput("factor5", NULL, col.factors.values[[5]], col.factors.values[[5]], multiple = !input$simplified),
#     if (!is.na(cols[6])) selectInput("factor6", NULL, col.factors.values[[6]], col.factors.values[[6]], multiple = !input$simplified),
#     
#     
#     tipify(
#       checkboxInput("interactive", "Enable interactivity", F),
#       "Interactive layout provides additional options for mouse hovering and dragging over displayed plots and tables."
#     ),
#     
#     h5("* Plot options:"),
#     selectInput( "var.facetX", "Vertical facets:",  c(None='.', cols), ifelse(!is.na(cols[1]), cols[1], "None")),
#     selectInput( "var.facetY", "Horizontal facets:",  c(None='.', cols), ifelse(!is.na(cols[2]), cols[2], "None")),
#     selectInput( "var.Colour", "Colour:",  c("None", cols), ifelse(!is.na(cols[3]), cols[3], "None") ),
#     selectInput( "var.Size", "Size:",  c("None", "value", cols)),
#     
#     
#     # checkboxInput("alternative_view", "Reverse axis", F),
#     checkboxInput("g.periods", "Mark milestone dates", F),
#     checkboxInput("g.scales", "Keep the same scale", F),
#     checkboxInput("g.smooth", "Smooth", F),
#     checkboxInput("g.smooth.lm", "Show trendline", F),
#     checkboxInput("g.point", "Show points", F),
#     
#     
#     h5("* Table options:"),
#     checkboxInput("t.summary", "Show data summary", F),
#     checkboxInput("t.wide", "Use wide format (one row per date)", F)
#     
#     
#   )
# }




shiny.menu.dt.basic_as_in_diamonds_example <- function(dt) {
  req(dt)
  cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
  flowLayout(
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dt), value=min(1000, nrow(dt)), step=500, round=0),
    checkboxInput('jitter', 'Jitter', value = TRUE),
    checkboxInput('smooth', 'Smooth', value = TRUE),
    selectInput('x', 'X', names(dt)),
    selectInput('y', 'Y', names(dt), names(dt)[[2]]),
    selectInput('color', 'Color', c('None', cols.factors)),
    selectInput('facet_row', 'Facet Row', c(None='.', cols.factors)),
    selectInput('facet_col', 'Facet Column', c(None='.', cols.factors))
  )
}



shiny.menu.dt.date <- function(dt, input=NA) {
  req(dt)

  # if ( names(dt) %in% "Date" ) { print("Data does not contain 'Date' column"); NULL }
  
  dateMax <- dt$Date %>% max(na.rm = T)
  dateMin <- dt$Date %>% min(na.rm = T)  

  flowLayout(
   tipify(
      selectInput("granularity", "Select granularity and dates:", aGranularity, aGranularity[8] ),
      "nW SMA signifies Simple Moving Average over 'n' weeks."
    ),
    # tipify(
    #   checkboxInput( "convolution", "Average over time",  F),
    #   title = "Apply smoothing average"
    # ),
    sliderInput("date.start", NULL,
                min = dateMin, 
                max = dateMax, 
                value=c(ymd("2019-01-01"), dateMax ),
                # value=c(input$date.start[1], input$date.start[2] ),
                timeFormat="%Y-%m-%d", ticks=F, width = "100%")
  )
}

str.selectInput.factor <- function (n) {
  glue::glue('if (!is.na(cols[{n}])) selectInput("factor{n}", NULL, col.factors.values[[{n}]], multiple = !simplified)')
}
# eval(parse(text = r$filter) )
  

shiny.menu.dt.subset <- function(dt, factors, metrics, simplified=T) {
  req(dt)
 
  
  # cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
  
  col.metrics <- metrics
  col.factors <- cols <- factors
  cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
  col.factors.values <- list()
  if (length(col.factors) > 0)
    for (i in 1:length(col.factors) ) {
      cat("*" %+% col.factors [i] %+% ": \n")
      col.factors.values [[i]] <- dt[[ col.factors [i] ]] %>% unique() %T>% print
    }
  
  
  
  flowLayout(
    
    # tipify(
    #   checkboxInput( "simplified", "Use Simplified layout",  T),
    #   title = "Simplified layout limits selection to one item only"
    # ),
    
    selectInput("metric", " Select Output metric:", metrics),
    
    p("Subset data by factors:"),

    # if (!is.na(cols[1])) selectInput("factor1", NULL, col.factors.values[[1]],  multiple = !simplified),
    # if (!is.na(cols[2])) selectInput("factor2", NULL, col.factors.values[[2]], col.factors.values[[2]], multiple = !simplified),
    # if (!is.na(cols[3])) selectInput("factor3", NULL, col.factors.values[[3]], col.factors.values[[3]], multiple = !simplified),
    # if (!is.na(cols[4])) selectInput("factor4", NULL, col.factors.values[[4]], col.factors.values[[4]], multiple = !simplified),
    # if (!is.na(cols[5])) selectInput("factor5", NULL, col.factors.values[[5]], col.factors.values[[5]], multiple = !simplified),
    # if (!is.na(cols[6])) selectInput("factor6", NULL, col.factors.values[[6]], col.factors.values[[6]], multiple = !simplified)
    
    eval(parse(text = str.selectInput.factor(1)) ),
    eval(parse(text = str.selectInput.factor(2)) ),
    eval(parse(text = str.selectInput.factor(3)) ),
    eval(parse(text = str.selectInput.factor(4)) ),
    eval(parse(text = str.selectInput.factor(5)) ),
    eval(parse(text = str.selectInput.factor(6)) )
 
    
  )
}


shiny.menu.dt.plot <- function (dt, factors, metrics=NA) {
  req(dt)

  # cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
  dateMax <- dt$Date %>% max(na.rm = T)
  dateMin <- dt$Date %>% min(na.rm = T)  
  
  col.metrics <- metrics
  col.factors <- cols <- factors
  # if (is.na(factors)) {
  #   cols <- cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
  #   
  # }
  # cols.factors  <- c (dt[, which(sapply(.SD, is.factor))], dt[, which(sapply(.SD, is.character))] );
  # col.factors.values <- list()
  # if (length(col.factors) > 0)
  #   for (i in 1:length(col.factors) ) {
  #     cat("*" %+% col.factors [i] %+% ": \n")
  #     col.factors.values [[i]] <- dt[[ col.factors [i] ]] %>% unique() %T>% print
  #   }
  
  
  flowLayout(
    
    tipify(
      checkboxInput("interactive", "Enable interactivity", F),
      "Interactive layout provides additional options for mouse hovering and dragging over displayed plots and tables."
    ),
    
    selectInput( "var.facetX", "Vertical facets:",  c(None='.', cols), ifelse(!is.na(cols[1]), cols[1], "None")),
    selectInput( "var.facetY", "Horizontal facets:",  c(None='.', cols), ifelse(!is.na(cols[2]), cols[2], "None")),
    selectInput( "var.Colour", "Colour:",  c("None", cols), ifelse(!is.na(cols[3]), cols[3], "None") ),
    selectInput( "var.Size", "Size:",  c("None", "value", cols)),
    
    
    
    # checkboxInput("alternative_view", "Reverse axis", F),
    checkboxInput("g.markMilestones", "Mark milestone dates", F),
    checkboxInput("g.sameScale", "Keep the same scale", F),
    checkboxInput("g.smooth", "Smooth", F),
    checkboxInput("g.smooth.lm", "Show trendline", F),
    # checkboxInput("g.showPoints", "Show points", F),
    checkboxInput("g.showXzero", "Show full range", F)

  )
  
}

shiny.menu.dt.grid <- function(dt) {
  
  req(dt)
  
  flowLayout(
    checkboxInput("t.showSelectionOnly", "Show table selection only", T),
    checkboxInput("t.wide", "Use wide format (one row per date)", F),
    numericInput( "t.rows", "Number of top/bottom rows to show", width="100%", 2, 1, dt %>% nrow(), 1 ),
    
    radioButtons ("t.check.printDT", "Show:",  width="100%", inline = T,
                  choices = c(
                    "Interactive Table (Search/Save)" = "interactive",
                    "Summary" = "summary",
                    "Categorical summary" = "summary2",
                    "Structure" = "str",
                    "Table "= "table",
                    "Table (with types)" = "head"

                    
                  ), selected = "head"
    )

  )
}


shiny.renderTable.withMenu <- function(dt) {
  renderUI({
    req(r$dt);
    fluidPage(
      fluidRow(
        column(width=6, 
               renderUI({
                 req(r$dt);
                 # checkboxInput("a0.search.details", strong( "Show selected columns only" ), T )
                 
                 checkboxGroupInput("check.show", NULL,
                                    c("Show all columns" = "selected.columns",
                                      "Show interactive" = "interactive") )
               })
        ),
        column(width=6, 
               renderUI({
                 req(r$dt);
                 numericInput( "rows", "Number of top/bottom rows to show", width="100%", 2, 1, r$dt %>% nrow(),1 )
               })
        )
      ),
      fluidRow( 
        column (width=12, 
                renderUI({
                  req(r$dt, input$button.add_data);
                  if (input$button.add_data > 0)
                    
                    radioButtons ("check.printDT", "Show:",  width="100%", inline = T,
                                  choices = c(
                                    "Summary" = "summary",
                                    "Summary (Categorical)" = "summary2",
                                    "Structure" = "str",
                                    "Table "= "table",
                                    "Table (with types)" = "head",
                                    "Interactive Table (Search/Save)" = "interactive"
                                    
                                  ), selected = "head"
                    )
                })
        )
      ),
      
      fluidRow( 
        column (width=12, 
                
                
                # print result #####
                
                renderUI({
                  
                  # dt <- r$dt
                  req(r$dt)
                  req(input$check.printDT)
                  
                  if ( "selected.columns" %in% input$check.show ) { # a0.search.details == T
                    dt <- r$dt
                  } else {
                    dt <- r$dt0
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
    ) #fluidpage
    
  }) # renderUI
  
}
  
  