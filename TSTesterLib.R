library(DT)
#library(tidyverse)
library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(broom)

library(ggplot2)
library(plotly)
library(latex2exp)

# Anomaly detection : STL + IQR
library(anomalize)

#-- Change point detection
library(cpm)
library(zoo)

#-- Statistical differences
library(transport)

## FUNCTIONS

dpvarmap <-  list("VAR1" = "TABLE1.SCORE_VAR1",
                  "VAR2" = "TABLE1.SCORE_VAR2",
                  "VAR3" = "TABLE1.SCORE_VAR3",
                  "VAR4" = "TABLE1.SCORE_VAR4",
                  "VAR5" = "TABLE1.SCORE_VAR5",
                  "VAR6" = "TABLE1.SCORE_VAR6",
                  "VAR7" = "TABLE1.SCORE_VAR7",
                  "VAR8" = "TABLE1.SCORE_VAR8",
                  "VAR9" = "TABLE1.SCORE_VAR9")



# build TS (filter NAN, rebuild datetime var and order)
build_ts_base <- function(data, var, tsvar, dpvar='', freq='point', mints=NULL, maxts=NULL){
  
  # select relevant variables only  
  ts <- data %>% select_at(c(tsvar, var)) %>% filter(!is.na(var)) 
  
  # filter dates if requested
  if((is.na(mints)==F) && length(mints)>0)
    ts <- ts %>% filter_at(vars(tsvar), all_vars(. >= mints)) 
  
  if((is.na(maxts)==F) && length(maxts)>0)
    ts <- ts %>% filter_at(vars(tsvar), all_vars(. < maxts)) 
  
  # (re)build datetime variable
  if(freq=='day'){
    ts <- ts %>% mutate_at(vars(tsvar), funs(as.Date(., format='%Y-%m-%d'))) 
  }
  else if (freq=='week') {
    #ts <- ts %>% mutate_at(vars(tsvar), funs(as.Date(., format='%Y-%V'))) 
    ts <- ts %>% mutate_at(vars(tsvar), funs(format(.,'%Y-%V'))) 
  }
  else if (freq=='month') {
    ts <- ts %>% mutate_at(vars(tsvar), funs(format(.,'%Y-%m'))) 
  }
  
  ts <- ts %>% na.omit()
  
  colnames(ts) <- c('TSindex','VALUE')
  
  ts <- ts %>%  arrange_at('TSindex')
  
  return(ts)
}

# Custom functions (dependent on global variables)
VTHRESHOLD = 900
count_above = function(x){
  length(which(x >= VTHRESHOLD))
}

VPICK = 900
count_equal = function(x){
  length(which(x == VPICK))
}

# date conversion helpers
week_to_day <- function(date){
  return(as.Date(paste0(substring(date,1,4),'-01-01')) + as.numeric(substring(date,6,8))*7)
}
month_to_day <- function(date){
  return(as.Date(paste0(date,'-01'), format='%Y-%m-%d'))
}

#  build TS (filter NAN, aggregated as needed, as order by datetime var)
build_ts <- function(data, var, tsvar, dpvar='', freq='point', mints=NULL, maxts=NULL, agg_func = mean){
  
  # get basic TS
  ts <- build_ts_base(data, var, tsvar, dpvar, freq, mints, maxts)
  
  if(freq!='point'){ #aggregated if requested
    if(is.character(agg_func)){
      if(agg_func=='counts'){
        ts <- ts %>% group_by_at('TSindex') %>% summarize(counts = n()) %>% ungroup()
      }
      #else if(agg_func==''){
      #  
      #}
    }
    else
      ts <- ts %>% group_by_at('TSindex') %>% summarize_at(.vars = 'VALUE', .funs = agg_func) %>% ungroup()
  }
  ts <- ts %>% na.omit()
  
  ts <- ts %>%  arrange_at('TSindex')
  
  colnames(ts) <- c('TSindex','VALUE')
  
  return(ts)
}


# build TS with percentiles
build_ts_qs <- function(data, var, tsvar, dpvar='', freq='point', mints=NULL, maxts=NULL, probs = c(0.5, 0.75, 0.95, 0.98, 0.99)){
  
  # get basic TS
  ts <- build_ts_base(data, var, tsvar, dpvar, freq, mints, maxts)
  
  # nest and compute quantiles 
  ts <- ts %>% nest(VALUE) %>%
    mutate(Quantiles = map(data, ~ quantile(.$VALUE, probs = probs))) %>% 
    unnest(map(Quantiles, tidy))
  
  ts <- ts %>% na.omit()
  ts <- ts %>%  arrange_at('TSindex')
  
  colnames(ts) <- c('TSindex', 'QP', 'VALUE')
  
  return(ts)
}


# plot TS
plot_ts <- function(ts, xlab='', ylab='', title='', subtitle='', mints=NULL, maxts=NULL){
  
  #restrict range if needed
  if((is.na(mints)==F) && length(mints)>0)
    ts <- ts %>% filter_at(vars(tsvar), all_vars(. >= mints)) 
  
  if((is.na(maxts)==F) && length(maxts)>0)
    ts <- ts %>% filter_at(vars(tsvar), all_vars(. < maxts)) 
  
  # handle factor labels
  nlabs <- -1
  if(is.character(ts[1][[1]][1])){
    labels <- unlist(strsplit(ts$TSindex, split=' '))
    
    # sample labels if too many
    nlabs <- length(labels)
    if(nlabs>30){
      labels <- labels[seq(1, nlabs, round(nlabs/30))]
    }    
  }
  
  p <- ts %>%
    ggplot(aes(TSindex, VALUE)) +
    geom_point(color = "#2c3e50", alpha = 0.25) +
    xlab(xlab) + ylab(ylab) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(title = title,
         subtitle = subtitle)
  
  if(nlabs>0)
    p <- p + scale_x_discrete(breaks = labels) 
  
  return(p)
}

# Compute ratio of two time series
ts_ratio <- function(t1, t2){
  tb <- merge(t1, t2, by='TSindex', all.y = T) %>% replace_na(list(VALUE.x = 0)) %>% mutate(VALUE = VALUE.x/VALUE.y) %>% select(TSindex, VALUE)
  return(as.tibble(tb))
}

# Style helpers
chpoint_style = list(  size = 8,
                       color = 'rgba(255, 182, 193, .9)',
                       line = list(color = 'rgba(152, 0, 0, .8)', width = 2)  
                    )


vline <- function(x = 0, color = "red", yrange=c(0,1)) {
  list(
    type = "line", 
    y0 = yrange[1], 
    y1 = yrange[2], 
    #    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash='dot', width=1)
  )
}