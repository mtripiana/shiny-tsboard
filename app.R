library(shiny)
library(shinyWidgets)
library(shinydashboard)

path <- "C:/Users/martin.tripiana/Work/projects/ShinyTest/TSBoard/"

source("TSTesterLib.R")

# Set TS datetime variable
tsvar   <- "REQUEST_DATE"

## LIST OF VARIABLES
varlist <- list("VAR1" = "VAR1",
                "VAR2" = "VAR2",
                #"VAR3" = "VAR3",
                #"VAR4" = "VAR4",
                #"VAR5" = "VAR5",
                "VAR6" = "VAR6",
                "VAR7" = "VAR7",
                "VAR8" = "VAR8",
                "VAR9" = "VAR9")

vartypes <- list("VAR1" = col_double(),
                "VAR2" = col_double(),
                #"VAR3" = "VAR3",
                #"VAR4" = "VAR4",
                #"VAR5" = "VAR5",
                "VAR6" = col_double(),
                "VAR7" = col_double(),
                "VAR8" = col_double(),
                "VAR9" = col_double(),
                "REQUEST_DATE" = col_date(format="%d/%m/%Y"))

## READ DATA
infile  <- 'data/VAR_SAMPLING.csv'
cat <- read_csv(infile, col_types = do.call(cols_only, vartypes)) #%>% select(c(names(varlist), tsvar))


#Create dummy summary
#rnd_status <- runif(length(varlist), 0, 1)

stprobs <- c(0.04,0.12,0.15,0.2,0.28,0.33,0.51,0.68,0.72,0.85,0.99)
rnd_status <- sample(x = stprobs, size = length(varlist), replace=F)

summary_tbl <- as.tibble(cbind(VARIABLE = unlist(unname(varlist)), STATUS = round(rnd_status,2))) %>% mutate_at('STATUS', funs(as.numeric))

#Anomaly color scale
colfunc <- colorRampPalette(c("forestgreen", "red"))
clrs <- colfunc(11)
brks <- seq(.05, .95, .1)



# GET TS FUNCTION
get_ts <- function(input){
  
  #decode agg function
  freq = input$freq
  if(input$freq=="none")
    freq = "point"
  
  #decode agg function
  if(input$agg_function == 'mean') 
    myfunc <- mean
  else if(input$agg_function == 'median') 
    myfunc <- median
  else 
    myfunc <- input$agg_function
  
  
  # get TS
  dst <- NULL
  if(is.character(myfunc) && myfunc == 'quantile'){ # treat quantile functions differently
    
    probs <- c(0, as.numeric(input$qvalue)/100)
    
    dst_qqs <- build_ts_qs(data=cat, var=input$var, tsvar=tsvar, freq=freq, dpvar=dpvarmap[input$var],
                           mints=as.Date(input$DateRange[1],'%Y-%m-%d'),
                           maxts=as.Date(input$DateRange[2],'%Y-%m-%d'),
                           probs=probs)
    
    dst <- dst_qqs %>% filter(QP==paste0(input$qvalue,'%')) %>% select('TSindex','VALUE')
    
  } else if(is.character(myfunc) && myfunc == 'SVfraction'){ # treat SV-fractions differently
    
    VPICK <<- input$svalue

    dst_SVALUE = build_ts(data=cat, var=input$var, tsvar=tsvar, freq=freq, dpvar=dpvarmap[input$var], 
                          mints=as.Date(input$DateRange[1],'%Y-%m-%d'),
                          maxts=as.Date(input$DateRange[2],'%Y-%m-%d'),
                          agg_func = count_equal)
    dst_ALL    = build_ts(data=cat, var=input$var, tsvar=tsvar, freq=freq, dpvar=dpvarmap[input$var], 
                          mints=as.Date(input$DateRange[1],'%Y-%m-%d'),
                          maxts=as.Date(input$DateRange[2],'%Y-%m-%d'),
                          agg_func = 'counts')
    

    dst <- ts_ratio(dst_SVALUE, dst_ALL)
    
  } else if(is.character(myfunc) && (myfunc %in% c('Qfraction','Tfraction') ) ){ # treat SV-fractions differently

    dst_point    = build_ts(data=cat, var=input$var, tsvar=tsvar, freq=freq, dpvar=dpvarmap[input$var], 
                          mints=as.Date(input$DateRange[1],'%Y-%m-%d'),
                          maxts=as.Date(input$DateRange[2],'%Y-%m-%d'))
    
    #set global threshold
    if(myfunc == 'Tfraction'){
      VTHRESHOLD <<- input$tvalue
    } else{
      qs <- quantile(dst_point$VALUE, as.numeric(input$qvalue)/100)
      VTHRESHOLD <<- qs[[1]]
    }
    
    dst_HIGH   = build_ts(data=cat, var=input$var, tsvar=tsvar, freq=freq, dpvar=dpvarmap[input$var], 
                          mints=as.Date(input$DateRange[1],'%Y-%m-%d'),
                          maxts=as.Date(input$DateRange[2],'%Y-%m-%d'),
                          agg_func = count_above)
    
    dst_ALL    = build_ts(data=cat, var=input$var, tsvar=tsvar, freq=freq, dpvar=dpvarmap[input$var], 
                          mints=as.Date(input$DateRange[1],'%Y-%m-%d'),
                          maxts=as.Date(input$DateRange[2],'%Y-%m-%d'),
                          agg_func = 'counts')

    dst <- ts_ratio(dst_HIGH, dst_ALL)
    
  } else{
    dst <- build_ts(data=cat, var=input$var, tsvar=tsvar, freq=freq, dpvar=dpvarmap[input$var],
                    mints=as.Date(input$DateRange[1],'%Y-%m-%d'),
                    maxts=as.Date(input$DateRange[2],'%Y-%m-%d'),
                    agg_func = myfunc)
  }
  
  return(dst)
}

# -----------------------------------------------------------------------------
# Dashboard UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "TS-Board"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("tasks", lib="glyphicon"), badgeColor = "green"),
      menuItem("Variable profile", tabName = "dist", icon = icon("signal", lib="glyphicon"), badgeColor = "green"),
      menuItem("Outliers", tabName = "outliers", icon = icon("record", lib="glyphicon")),
      menuItem("Change Points", tabName = "chpoints", icon = icon("sort", lib="glyphicon")),
      
      menuItem("Stats Tests", tabName = "stats", icon = icon("stats", lib="glyphicon")
      ),
 
      selectInput("var", label = "Variable", 
                choices = varlist,
                selected = "VAR7"),
      
      sliderInput("DateRange",
                  tagList(icon("calendar"), label="Dates"),
                  min = as.Date("2011-01-01","%Y-%m-%d"),
                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                  value=c(as.Date("2016-01-01"), Sys.Date()),
                  timeFormat="%Y-%m-%d"),
      
      
      # Aggregation
      shinyWidgets::sliderTextInput("freq", tagList(icon('time', lib='glyphicon'), label = "Aggregation period"), 
                                    choices = c("none","day","week","month"), 
                                    selected = "day"),
      
      selectInput("agg_function", tagList(icon('plus'), label = "Aggregation function"), 
                  choices = list("mean" = "mean", 
                                 "median" = "median", 
                                 "counts" = "counts",
                                 "quantile (%)" = "quantile", 
                                 "SV fraction" = "SVfraction",
                                 "Tail fraction" = "Tfraction",
                                 "Q-Tail fraction" = "Qfraction"),
                  selected = "mean"),
      
      conditionalPanel(
        condition = "input.agg_function == 'quantile' || input.agg_function == 'Qfraction'",
        radioButtons("qvalue", label = 'Q(%)',
                     choices = list("75" = 75, "95" = 95, "99" = 99), 
                     selected = 75)
      ),
      
      conditionalPanel(
        condition = "input.agg_function == 'SVfraction'",
        numericInput("svalue", label = "Single Value", value = 0)
      ),
      
      conditionalPanel(
        condition = "input.agg_function == 'Tfraction'",
        numericInput("tvalue", label = "Threshold", value = 0)
      )
    
    
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow( box(status='primary', DTOutput('tbl'), width=8),
              tags$hr())
      ),
      tabItem(tabName = "dist",
              fluidRow( box(status='primary', plotOutput("distPlot"), width=12)),
              fluidRow( valueBoxOutput("kpi_summary_box_1", width = 2))
      ),
      tabItem(tabName = "outliers",
              fluidRow( box(status='primary', plotOutput("outliersPlot", height="auto"), width="auto")),
              fluidRow( box(status='primary', plotOutput("stlPlot", height="auto"), width="auto"))
      ),
      tabItem(tabName = "chpoints",
              fluidRow( box(title = "Sequential Monitoring", solidHeader = TRUE, status = "primary",
	      		plotlyOutput("dcpPlot", height="auto"), width="auto"),
                        box(status='primary', collapsible=T,
			    numericInput("dcp_window", label = "Rolling Window", value = 3),
                            numericInput("dcp_start", label = "Start Period", value = 100),
                            radioButtons(inputId="dcp_test", label="Test", 
                                         choices=c("Mann-Whitney","Student"),
                                         selected = "Mann-Whitney"),
                            radioButtons(inputId="dcp_mode", label="Mode", 
                                         choices=c("Batch","Sequential"),
                                         selected = "Batch")
                        )
              )
      ),
      tabItem(tabName = "stats",
              fluidRow( box(status = 'primary', collapsible = T,			
                            sliderInput("StatRefRange",
                                    tagList(icon("calendar"), label="Ref.Dates"),
                                    min = as.Date("2011-01-01","%Y-%m-%d"),
                                    max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                    value=c(as.Date("2016-01-01"), as.Date("2017-01-01")),
                                    timeFormat="%Y-%m-%d"),
                            sliderInput("StatAltRange",
                                    tagList(icon("calendar"), label="Alt.Dates"),
                                    min = as.Date("2011-01-01","%Y-%m-%d"),
                                    max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                    value=c(as.Date("2017-01-01"), as.Date("2018-01-01")),
                                    timeFormat="%Y-%m-%d")
                        ),
                        box(title = "Statistical stability", width = 12, solidHeader = TRUE, status = "primary",
                            plotOutput("refaltPlot"), 
                            hr(),
                            plotOutput("refaltEvolPlot"))
              )
      )
    )
  )
)


# -----------------------------------------------------------------------------
# Dashboard server code
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Distribution -------------------------------------------------------

  dst <- reactive({get_ts(input)})
  
  #distPlot
  output$distPlot <- renderPlot({
    
    if (input$var == "")
      return(NULL)
    
    # dst <- get_ts(input)
    
    # decode xlabel
    xlabel <- 'DATE'
    if(input$freq == "week") xlabel = 'WEEK'
    if(input$freq == "month") xlabel = 'MONTH'
    
    # plot it!
    plot_ts(dst(), xlabel, input$var, input$var, dpvarmap[input$var])
    
  })
  
  #STL plot
  output$outliersPlot <- renderPlot({

    if (input$var == "")
      return(NULL)
    
    dst <- dst() 
    
    # convert to dates if needed    
    if(input$freq == "week")  dst <- dst %>% mutate_at(vars('TSindex'), funs(week_to_day))
    if(input$freq == "month") dst <- dst %>% mutate_at(vars('TSindex'), funs(month_to_day))
    
    ## TS decomposition (STL + IQR settings) outliers
    dst %>% 
      # Data Manipulation / Anomaly Detection
      time_decompose(VALUE, method = "stl") %>%
      anomalize(remainder, method = "iqr") %>%
      time_recompose() %>%
      # Anomaly Visualization
      plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
      labs(title = input$var, subtitle = "Anomaly detection : STL + IQR Methods") 

  }, height=function() { session$clientData$output_outliersPlot_width * 0.5 })
  
  output$stlPlot <- renderPlot({
    
      if (input$var == "")
        return(NULL)
    
      dst <- dst()
      
      # convert to dates if needed    
      if(input$freq == "week") dst <- dst %>% mutate_at(vars('TSindex'), funs(week_to_day))
      if(input$freq == "month") dst <- dst %>% mutate_at(vars('TSindex'), funs(month_to_day))
      
      ## TS decomposition (STL + IQR settings)
      dst %>% 
      time_decompose(VALUE, method = "stl") %>%
      anomalize(remainder, method='iqr') %>%
      plot_anomaly_decomposition() +
      labs(title = "Decomposition of Anomalized Signal") 
      
  }, height=function() { session$clientData$output_outliersPlot_width * 1 })
  

  output$dcpPlot <- renderPlotly({
    
    if (input$var == "")
      return(NULL)
    
    #@ -- Detect change points on rolling average (to be less sensitive to spikes)
    mav_window <- input$dcp_window # rolling window
    startupPeriod <- input$dcp_start
    testStatistic <- input$dcp_test
    cpmode        <- input$dcp_mode #  change point detection mode ('seq' or 'batch')
    
    
    data <- dst() %>%
      arrange(TSindex) %>%
      mutate(ROLLVALUE = rollmean(x = VALUE, mav_window, align = "right", fill = NA)) %>% na.exclude()
    
    resMW <- processStream(data$ROLLVALUE, cpmType=testStatistic, ARL0=50000, startup=startupPeriod)
    breaks <- diff(c(0,resMW$changePoints,length(data$ROLLVALUE)))
    trendGroup <- unlist(sapply(c(1:length(breaks)), function(i){ rep(i,breaks[i]) }))
    
    segmentMeanValues <- rep(tapply(data$ROLLVALUE, trendGroup, mean), breaks)
    detectionTimeValues <- rep(0, length(data$ROLLVALUE))
    detectionTimeValues[resMW$changePoints] <- data$ROLLVALUE[resMW$changePoints]
    
    data$detected     <- detectionTimeValues
    data$segmentMeans <- segmentMeanValues
    
    ### BATCH MODE 
    if(cpmode=='Batch'){
      resultsStudent <- detectChangePointBatch(data$ROLLVALUE, cpmType = "Student", alpha = 0.05)
      resultsMW      <- detectChangePointBatch(data$ROLLVALUE, cpmType = "Mann-Whitney", alpha = 0.05)
    } else{ # SEQ MODE
      resultsStudent <- detectChangePoint(data$ROLLVALUE, cpmType = "Student", ARL0 = 50000)
      resultsMW      <- detectChangePoint(data$ROLLVALUE, cpmType = "Mann-Whitney", ARL0 = 50000)
    }
    
    if(testStatistic == "Student"){
      xCP <- resultsStudent$changePoint + 1 # book position of change point 
    } else{
      xCP <- resultsMW$changePoint + 1 # book position of change point 
    }    
    
    ymax <- 1.5*max(data$ROLLVALUE)
    print(ymax)
    
    plot_ly(data, x = ~TSindex, y = ~ROLLVALUE, type = 'scatter', mode = 'lines', name='Original',  line = list(color = ('rgb(93, 109, 126)')),           
                 transforms = list(  list(type = 'filter',
                                          target = 'y',
                                          operation = '>',
                                          value = 0
                 ))) %>%
      add_trace(y = ~segmentMeans, name = 'Mean Segments', mode = 'lines', line = list(color = ('rgb(235, 152, 78)'))) %>%
      add_trace(y = ~detected, name = 'Change Points', mode = 'markers', marker = chpoint_style, line = list(width=0)) %>%
      add_segments(name=paste0(input$dcp_test,' (DCP)'), x = data$TSindex[xCP], xend = data$TSindex[xCP], 
                   y = 0.0001, yend = ymax, line = list(color='green', dash='dot', width=1))  %>%
      layout(title=paste0('CPM (',input$dcp_test, ' test)'),
             xaxis = list(title='Date'),
             yaxis = list(title=input$var, range = c(0,ymax)))
    
  })
  
  
  
  output$refaltPlot <- renderPlot({
    
    if (input$var == "")
      NULL
    
    ref_dates <- input$StatRefRange 
    alt_dates <- input$StatAltRange 

    dst <- dst()
    dref <- dst %>% filter(between(TSindex, as.Date(ref_dates[1]), as.Date(ref_dates[2])))
    dalt <- dst %>% filter(between(TSindex, as.Date(alt_dates[1]), as.Date(alt_dates[2])))
    
    dref$name <- 'Ref'
    dalt$name <- 'Alt'
    
    mdist <- rbind(dref, dalt)
    
    ## Compute statistical difference
    wcdist <- wasserstein1d(dref$VALUE, dalt$VALUE)
    
    ksresult <- ks.test(dref$VALUE, dalt$VALUE)
    ksdist <- suppressWarnings( ksresult$statistic[[1]] )
    
    ## PLOT DISTRIBUTION
    ytxt <- max(density(dref$VALUE)$y, density(dalt$VALUE)$y)
    
    p <- ggplot(mdist, aes(VALUE, fill = name, color = name)) + 
      geom_density(alpha = 0.2, size=1, aes(color = name))  + 
      scale_fill_manual( values = c("darkorange","skyblue")) +
      labs(color='', fill='') +
      xlab(input$var) + 
      ylab('Density') + theme_minimal() + 
      theme(legend.position= c(0.85, 0.9),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12), 
            legend.text = element_text(size=14))  
    
    
    p <- p + annotate(geom="text", x=0.75*max(dalt$VALUE,dref$VALUE), y=0.8*ytxt, hjust=0, size=5, label=paste0('W1D(R,A) = ', round(wcdist,2)))
    p <- p + annotate(geom="text", x=0.75*max(dalt$VALUE,dref$VALUE), y=0.75*ytxt, hjust=0, size=5, label=paste0('KS(R,A) = ', round(ksdist,2)))
    p
  })


  output$refaltEvolPlot <- renderPlot({
    
    if (input$var == "")
      NULL
    
    ref_dates <- input$StatRefRange 
    alt_dates <- input$StatAltRange 
    
    dst <- dst()
    dref <- dst %>% filter(between(TSindex, as.Date(ref_dates[1]), as.Date(ref_dates[2])))

    dalt <- dst %>% filter(between(TSindex, as.Date(alt_dates[1]), as.Date(alt_dates[2]))) %>% mutate(week = format(TSindex, "%Y-%V"))
    
    # get list of weeks in test period
    weeklist <- unique(dalt %>% select(week))
    
    # compute stat distance for each week w.r.t. reference
    distances <- c()
    for (wk in weeklist$week) {
      alt <- dalt %>% filter(!is.na(VALUE), week==wk) %>% select(TSindex, VALUE)
      distances <- c(distances, wasserstein1d(dref$VALUE, alt$VALUE))
    }

    tbdist <- as.tibble(cbind(weeklist$week,distances)) %>% mutate_at('distances',funs(as.numeric))

    p <- tbdist %>% na.omit() %>%
      ggplot(aes(V1, distances)) +
      geom_point(color = "#2c3e50", alpha = 0.25) +
      xlab('WEEK') + ylab('W1D(R,A)') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size=0)) +
      labs(title = 'Weekly Distance Evolution')
    p
    
       
  })



  output$kpi_summary_box_1 <- renderValueBox({
    valueBox(
      value = '0.2',
      subtitle = 'Anomaly Score', #sprintf("Anomaly Score (%.1f%%)", 0.2),
      icon = icon("ok"),
      color = "green"
    )
  })
  
  
  output$tbl <- renderDataTable({
    
    datatable(summary_tbl, selection = 'single', style = 'bootstrap', class = 'table-bordered table-condensed', 
              options = list(dom = 't', lengthChange=F, compact=T)) %>% 
      formatStyle('STATUS', backgroundColor = styleInterval(brks, clrs), fontSize = '120%')

  })
  
  
  
}


shinyApp(ui, server)
# © 2018 GitHub, Inc.
# Terms
# Privacy
# Security
# Status
# Help
# Contact GitHub
# Pricing
# API
# Training
# Blog
# About
