############################################
##
##  
##      OHA_Performance_Measure_plot    ##
##
##
############################################

## Description
## This function takes in a dataset with counts for performance measure 
## inclusion in the numerator and denominators for all Populations. It 
## takes arguments that allow for aggregation to the right time resolution, 
## Final percentages, formats labels and constructs graph title, and outputs 
## a ggplotly graph.

## Requires
  # library(tidyverse)
  # library(scales)

## Input
## Takes a df that must contain the following columns:
  ## year             Integer
  ## quarter          Integer
  ## month            Integer     
  ## tot_denom        Integer  
  ## ped_denom        Integer 
  ## adult_denom      Integer  
  ## tot_num          Integer  
  ## ped_num          Integer   
  ## adult_num        Integer

## Output
## This function will output a ggplotly graph for inclusion in a dashboard

## Arguments
  ## df - Name of the input dataframe
  ## pop = "Total" - One of three options {"Total", "Pediatric", "Adult"}
  ## time_unit = "Month", One of three options {"Month", "Quarter", "Year"}
  ## goal - A percentage goal for the metric expressed as a decimal. 
      ## If present generates a horizontal dotted line on the graph.
  ## metric_name = "" - Name and Number of the NEMSQA metric {e.g., "NEMSQA Asthma-01"}

## Change Log
## Created 3.11.24 Peter Geissert

## Known Issues
  ## There is an issue with the Year time unit. Need to rework the script so that 
      ## when year is selected there is no grouping variable, and the color palette 
      ## changes so that a single time series can be graphed.


############################################
##
##  
##      OHA_Performance_Measure_plot        ##
##
##
############################################


OHA_Performance_Measure_plot1 <- function(df, pop = "Total", time_unit = "Month", goal, metric_name = "") {
  
  series <- ifelse(pop == "Pediatric", "ped_percent", 
                   ifelse(pop == "Adult", "adult_percent", "tot_percent"))
  
  denom_series <- ifelse(pop == "Pediatric", "ped_denom", 
                         ifelse(pop == "Adult", "adult_denom", "tot_denom"))
  
  group_vars <- if(time_unit == "Month") {rlang::syms(c("year", "quarter", "month"))
    } else if(time_unit == "Quarter") {rlang::syms(c("year", "quarter"))
      } else if(time_unit == "Year") {rlang::syms(c("year"))}
  
  analytic_df <- df %>%
    group_by(., !!!group_vars) %>%
    summarize(tot_denom = sum(tot_denom, na.rm = TRUE), 
              ped_denom = sum(ped_denom, na.rm = TRUE),
              adult_denom = sum(adult_denom, na.rm = TRUE),
              tot_num = sum(tot_num, na.rm = TRUE), 
              ped_num = sum(ped_num, na.rm = TRUE),
              adult_num = sum(adult_num, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(., tot_percent = tot_num / tot_denom,
           ped_percent = ped_num / ped_denom,
           adult_percent = adult_num / adult_denom)
  
  quarter_label_list <- c("Q1", "Q2", "Q3", "Q4")
  
  year_list <- df %>% distinct(., year) %>% arrange(., year) %>% pull(., year)
  
  time_var <- if(time_unit == "Month") {"month"
    } else if(time_unit == "Quarter") {"quarter"
      } else if(time_unit == "Year") {"year"}
  
  
  x_axis_labels <- if(time_unit == "Month") {month.abb
    } else if(time_unit == "Quarter") {quarter_label_list
      } else if(time_unit == "Year") {as.character(year_list)}
  
  
  x_axis_breaks <- if(time_unit == "Month") {c(1:12)
    } else if(time_unit == "Quarter") {c(1:4)
      } else if(time_unit == "Year") {c(year_list)}
  
  a <- ggplot(analytic_df, aes(x = get(time_var), y = get(series), group = as.factor(year),
                               text = sprintf("%s: %s, %i<br>Denominator: %s<br>Percent: %s", 
                                              time_unit, 
                                              x_axis_labels[get(time_var)], 
                                              year,
                                              comma(get(denom_series), accuracy = 1),
                                              paste(round(get(series) * 100, 1), "%")))) + ##Custom hover text feeds to plotly
    geom_point(size = 2, aes(color = as.factor(year))) +
    geom_line(linewidth = 1, aes(color = as.factor(year))) +
    geom_hline(yintercept = goal, linetype = "dashed", color = "#EC891D") +
    scale_x_continuous(labels = x_axis_labels, breaks = x_axis_breaks) + ## custom breaks and labels on the x axis for month name
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) +
    scale_color_manual(values=c("#c0c0c0", "#005595"))+
    labs(title = paste(metric_name, pop, "Population, Oregon", min(df$year), "-", max(df$year)),
         x = "", 
         y = "Percent",
         color = "Year") +
    ems_theme
  
  ggplotly(a, tooltip = "text")
}


############################################
##
##  
##          Generate Graph             ##
##
##
############################################

# OHA_Performance_Measure_plot(asthma1_table,
#                              pop = "Total",
#                              time_unit = "Year",
#                              goal = .9,
#                              metric_name = "NEMSQA Asthma-01")
