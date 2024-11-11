###_____________________________________________________________________________
### Comparative function for one numeric vector and a factor grouping variable
###_____________________________________________________________________________

compare_grouped_is_it_normal <-
  function(df,
           data_name = deparse(substitute(df)),
           x,
           group_var,
           include_plots = TRUE,
           filter1 = NULL,
           filter2 = NULL,
           operator = NULL,
           scale = FALSE,
           print_out = TRUE,
           plot_theme = theme_cleaner,
           ...) {
    
    # Ensure the required packages are installed and loaded
    
    if (!requireNamespace("tidyverse", quietly = TRUE)) {
      install.packages("tidyverse", quiet = TRUE)
    }
    
    if (!requireNamespace("viridis", quietly = TRUE)) {
      install.packages("viridis", quiet = TRUE)
    }
    
    library(tidyverse, quietly = TRUE)
    library(viridis, quietly = TRUE)
    
    # Check if x is provided
    
    if (missing(x)) {
      
      cli_h1("Missing Required Argument {.var x}")
      
      cli_abort(c(
        "The {.var x} argument is required.",
        "i" = "Please provide a value for {.var x}."
      ))
    }
    
    # Check if group_var is provided
    if (missing(group_var)) {
      
      cli_h1("Missing Required Argument {.var group_var}")
      
      cli_abort(c(
        "The {.var group_var} argument is required.",
        "i" = "Please provide a value for {.var group_var}."
      ))
    }
    
    # Set up the local environment
    
    local_env <- new.env()
    
    
    with(local_env, {
      
      # set the random number seed for the shapiro-wilks test
      set.seed(123)
      
      # set scipen for this temp environment
      options(scipen = 0, digits = 3)
      
      x <- enquo(x)
      group_var <- enquo(group_var)
      
      # Evaluate x within the context of df
      group_eval <- df %>% pull(!!group_var)
      
      if (!is.integer(df[[as_name(x)]]) && !is.numeric(df[[as_name(x)]])) {
        
        cli_h1("Problem with Input")
        
        cli_abort(
          paste0(
            "Variable '",
            col_green(style_bold(as_name(x))),
            "' must be of class {.cls numeric} or {.cls integer}."
          ))
        
      }
      
      if (!is.function(plot_theme)) {
        
        cli_h1("Problem with Input")
        
        cli_abort(
          "You must supply an object of class {.cls function} to {.var plot_theme}.  You supplied an argument of class {.cls {class(plot_theme)}}. Please supply a valid theme function to {.var plot_theme}."
        )
        
      } else {
        
        chosen_theme <- as.function(plot_theme)
        
      }
      
      cli_h1("Output Summary")
      
      # Check and mutate group_var if necessary
      if (!is.factor(df[[as_name(group_var)]]) && !is.character(df[[as_name(group_var)]])) {
        
        cli_inform(
          c("*" = "The value supplied to {.var group_var} was of class {.cls {class(group_eval)}} and will be coerced to {.cls factor}.")
        )
        
        df[[as_name(group_var)]] <- factor(df[[as_name(group_var)]], levels = unique(df[[as_name(group_var)]]))
        
        # Get unique levels for the grouping variable
        levels_group_var <- na.omit(levels(df[[as_name(group_var)]]))  # Remove NA values
        
        
      } else if(is.character(df[[as_name(group_var)]])) {
        
        # Get unique levels for the grouping variable
        levels_group_var <- na.omit(unique(df[[as_name(group_var)]]))  # Remove NA values
        
      } else if(is.factor(df[[as_name(group_var)]])) {
        
        # Get unique levels for the grouping variable
        levels_group_var <- na.omit(levels(df[[as_name(group_var)]]))  # Remove NA values
        
      }
      
      
      num_colors <- length(levels_group_var)
      
      blue_colors <-
        colors()[c(107,
                   121:125,
                   128:132,
                   435,
                   461,
                   477,
                   490:491,
                   589:598,
                   615:619,
                   26:30,
                   62,
                   73)]
      
      # Set color palette from ggplot2 package
      color_palette1 <- blue_colors
      
      color_palette2 <-
        viridis(num_colors,
                begin = 0.9,
                end = 1,
                option = "turbo")
      
      color_palette3 <-
        viridis(num_colors,
                begin = 0.75,
                end = 1,
                option = "plasma")
      
      color_palette4 <-
        viridis(num_colors,
                begin = 0.15,
                end = 0.4,
                option = "inferno")
      
      color_palette5 <-
        viridis(num_colors,
                begin = 0.8,
                end = 1,
                option = "viridis")
      
      color_palette6 <-
        viridis(num_colors,
                begin = 0.5,
                end = 1,
                option = "mako")
      
      # Initialize an empty data.frame to store results
      result_df <- data.frame(
        Group = character(),
        Mean = numeric(),
        Std_Dev = numeric(),
        Minimum = numeric(),
        Quantile_25 = numeric(),
        Median = numeric(),
        Quantile_75 = numeric(),
        Maximum = numeric(),
        Non_Missings = numeric(),
        Missings = numeric(),
        Percent_Missing = numeric(),
        Shapiro_Wilks_Result = numeric(),
        Normality_Diagnosis = character(),
        stringsAsFactors = FALSE
      )
      
      # Message to call out the variable name
      cli_inform(c(
        "*" = paste0(
          "Exploratory data analysis on the variable '",
          col_green(style_bold(as_name(x))),
          "' from the '",
          col_magenta(style_bold(data_name)),
          "' dataset."
        )
      ))
      
      
      # Optional filters applied pre-calculations
      # Create a filter object only if the 'filter' argument is not NULL
      
      if (!is.null(filter1) &
          is.null(filter2) & is.null(operator)) {
        filter_text <- paste0(as_name(x), filter1)
        
        condition <- parse_expr(filter_text)
        
        cli_inform(c("*" = paste0(
          "Filter applied to '", col_green(style_bold(as_name(x))), "'."
        )))
        
      } else if (!is.null(filter1) &
                 !is.null(filter2) & !is.null(operator)) {
        filter_text <- paste0(as_name(x), " ", filter1, operator, as_name(x), " ", filter2)
        
        condition <- parse_expr(filter_text)
        
        cli_inform(c("*" = paste0(
          "Filter applied to '", col_green(style_bold(as_name(x))), "'."
        )))
        
      } else {
        
        # Handle the case when filter is NULL
        # For example, you can print a message or proceed without filtering
        
        condition <- TRUE  # No filtering condition
        
        cli_inform(c(
          "*" = paste0(
            "No filter applied to '",
            col_green(style_bold(as_name(x))),
            "'. Proceeding with the original dataframe."
          )
        ))
        
      }
      
      # Apply the filter condition
      
      df_filtered <- df %>% dplyr::filter(!!condition)
      
      # Message calling out total number of observations in the data
      cli_inform(c("*" = paste0("Descriptive statistics given on ", pretty_number(nrow(df_filtered), n_decimal = 2), " observations for the variable '", col_green(style_bold(as_name(x))),"' from the supplied dataset.")))
      
      if (include_plots == FALSE) {
        
        cli_inform(c("*" = "Halting plot logic."))
        
      } else {
        
        cli_inform(c("*" = "Running plot logic."))
        
      }
      
      # Perform optional min-max normalization on the provided variable
      
      if (scale == TRUE) {
        df_filtered <- df_filtered %>%
          mutate(!!x := normalize(!!x))
        
        cli_inform(c("*" = paste0("Min-max normalization was performed on '",
                                  col_green(style_bold(as_name(x))),
                                  "', and will be reflected in the descriptive statistics below."
        )))
        
      } else {
        
        cli_inform(c("*" = paste0("Min-max normalization was not performed on '", col_green(style_bold(as_name(x))), "'.")))
        
      }
      
      # Plotting for each level using ggplot2
      # Loop through each level of the grouping variable
      # Loop through each level of the grouping variable
      
      for (i in seq_along(levels_group_var)) {
        level <- levels_group_var[i]
        subset_df <-
          df_filtered %>% dplyr::filter(!!group_var == level)
        
        # conduct the Shapiro Wilks test on all levels of the group_var
        
        subset_df_clean <- subset_df %>% 
          dplyr::filter(!is.na(!!x))
        
        n_subset <- nrow(subset_df_clean)
        
        if (n_subset < 3) {
          shapiro_p_value <- NA_real_
          normality_diagnosis <- "Error: n < 3"
        } else if (n_subset > 5000) {
          # Sample 5000 records for Shapiro-Wilk test
          sample_df <- subset_df_clean %>%
            sample_n(5000, replace = FALSE)
          shapiro_test_result <- shapiro.test(sample_df[[as_name(x)]])
          
          # Store p-value and normality diagnosis
          shapiro_p_value <- shapiro_test_result$p.value
          normality_diagnosis <- if_else(shapiro_p_value < 0.05, "Non-normal distribution", "Normal distribution")
          
        } else {
          # Perform Shapiro-Wilk test on the cleaned data
          shapiro_test_result <- shapiro.test(subset_df_clean[[as_name(x)]])
          
          # Store p-value and normality diagnosis
          shapiro_p_value <- shapiro_test_result$p.value
          normality_diagnosis <- if_else(shapiro_p_value < 0.05, "Non-normal distribution", "Normal distribution")
          
        }
        
        # Use summarize to calculate statistics within the context of subset_df
        summary_stats <- subset_df %>%
          summarize(
            Mean = round(mean(!!x, na.rm = TRUE), digits = 3),
            Std_Dev = round(sd(!!x, na.rm = TRUE), digits = 3),
            Minimum = min(!!x, na.rm = TRUE),
            Quantile_25 = quantile(!!x, probs = 0.25, na.rm = TRUE),
            Median = quantile(!!x, probs = 0.5, na.rm = TRUE),
            Quantile_75 = quantile(!!x, probs = 0.75, na.rm = TRUE),
            Maximum = max(!!x, na.rm = TRUE),
            Non_Missings = sum(!is.na(!!x)),
            Missings = sum(is.na(!!x)),
            Percent_Missing = round(mean(is.na(!!x)), digits = 3)
          )
        
        # Extract individual values for max, min, and median
        max_val <- summary_stats$Maximum
        min_val <- summary_stats$Minimum
        median_val <- summary_stats$Median
        
        # Add the calculated statistics to the result data.frame
        result_df <- bind_rows(
          result_df,
          tibble(
            Group = level,
            Mean = summary_stats$Mean,
            Std_Dev = summary_stats$Std_Dev,
            Minimum = summary_stats$Minimum,
            Quantile_25 = summary_stats$Quantile_25,
            Median = summary_stats$Median,
            Quantile_75 = summary_stats$Quantile_75,
            Maximum = summary_stats$Maximum,
            Non_Missings = summary_stats$Non_Missings,
            Missings = summary_stats$Missings,
            Percent_Missing = summary_stats$Percent_Missing,
            Shapiro_Wilks_Result = shapiro_p_value,
            Normality_Diagnosis = normality_diagnosis
          )
        )
      }
        
      if(print_out == T) {
        
        cli_h2("Output")
        
        print(result_df)
        
        }
        
        if (include_plots == T) {
          
        for (i in seq_along(levels_group_var)) {
          level <- levels_group_var[i]
          subset_df <-
            df_filtered %>% dplyr::filter(!!group_var == level)
            
          
          # only calculate the missings for a caption if plot logic is activated
          non_missing_n <- subset_df %>%
            dplyr::select(all_of(as_name(x))) %>%
            na.omit() %>%
            summarize(n = n()) %>%
            pull(n)
          
          # Create ggplot objects for each plot type
          
          # Normal Q-Q Plots
          qqplot <- subset_df %>% ggplot(aes(sample = !!x)) +
            stat_qq(size = 2,
                    na.rm = TRUE,
                    color = color_palette1[i]) +
            geom_qq_line(
              linewidth = 1.25,
              alpha = 0.7,
              na.rm = TRUE,
              color = color_palette2[i]
            ) +
            ggtitle(paste0("Normal Q-Q Plot for ", as_name(x), " at level '", level, "'")) +
            chosen_theme(...)
          
          # Histograms
          histogram <- subset_df %>% ggplot(aes(!!x)) +
            geom_histogram(
              binwidth = (max_val - min_val) / 15,
              color = "black",
              na.rm = TRUE,
              fill = color_palette3[i]
            ) +
            ggtitle(paste0("Histogram of ", as_name(x), " at level '", level, "'")) +
            chosen_theme(...)
          
          # Kernel density plots
          density <- subset_df %>% ggplot(aes(!!x)) +
            geom_density(
              color = "black",
              alpha = 0.7,
              na.rm = TRUE,
              fill = color_palette4[i]
            ) +
            geom_vline(
              xintercept = median_val,
              color = color_palette5[i],
              linetype = "dashed",
              linewidth = 1.5
            ) +
            ggtitle(
              paste0(
                "Kernel Density Plot of ",
                as_name(x),
                " at level '",
                level,
                "'\nwith a horizontal line at the median"
              )
            ) +
            labs(caption = paste0("n = ", non_missing_n, " non-missings")) +
            chosen_theme(...)
          
          # Boxplots
          boxplot <- subset_df %>% ggplot(aes(!!x)) +
            geom_boxplot(
              color = "black",
              na.rm = TRUE,
              fill = color_palette6[i],
              orientation = "y"
            ) +
            stat_boxplot(geom = "errorbar", width = 0.5) +
            ggtitle(paste0("Boxplot of ", as_name(x), " at level '", level, "'")) +
            chosen_theme(...)
          
          # Create dynamic captions for the patchwork
          
          custom_caption1 <- if (condition == TRUE) {
            
            paste0("No filter applied.")
            
          } else {
            
            paste0("Filter of ",
                   filter_text,
                   " applied.")
            
          }
          
          custom_caption2 <- if (scale == TRUE) {
            paste0("Min-max normalization was applied to '", as_name(x), "'.")
            
          } else {
            paste0("Min-max normalization was not applied to '", as_name(x), "'.")
            
          }
          
          # combine the plots with patchwork
          combined <- (qqplot | histogram) / (density | boxplot) +
            plot_annotation(
              title = paste0(
                "Normality Test of the '",
                data_name,
                "' dataset, regarding variable '",
                as_name(x),
                "' at level '",
                level,
                "'"
              ),
              subtitle = paste0(
                "Levels of '",
                as_name(group_var),
                "' are compared here",
                "\n",
                "Source: 'compare_grouped_is_it_normal()'"
              ),
              caption = paste0(custom_caption1, "\n", custom_caption2),
              theme = chosen_theme(...)
            )
          
          print(combined)
          
        }
        
        }
        
      if(print_out == FALSE){
        
        return(result_df)
        
      }
      
    })
    
  }
