###_____________________________________________________________________________
### Comparative function for two variables from the same df
###_____________________________________________________________________________

compare_is_it_normal <-
  function(df,
           data_name = deparse(substitute(df)),
           x1,
           x2,
           include_plots = TRUE,
           normalize_x1 = FALSE,
           normalize_x2 = FALSE,
           filter_x1_1 = NULL,
           filter_x1_2 = NULL,
           operator_x1 = NULL,
           filter_x2_1 = NULL,
           filter_x2_2 = NULL,
           operator_x2 = NULL,
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
    
    # Check if x1 is provided
    
    if (missing(x1)) {
      
      cli_h1("Missing Required Argument {.var x1}")
      
      cli_abort(c(
        "The {.var x1} argument is required.",
        "i" = "Please provide a value for {.var x1}."
      ))
    }
    
    # Check if x2 is provided
    
    if (missing(x2)) {
      
      cli_h1("Missing Required Argument {.var x2}")
      
      cli_abort(c(
        "The {.var x2} argument is required.",
        "i" = "Please provide a value for {.var x2}."
      ))
    }
    
    
    
    # create a local environment
    
    local_env <- new.env()
    with(local_env, {
      
      # set the random number seed for the shapiro-wilks test
      set.seed(123)
      
      # set this for outputs to be neat and clean
      options(scipen = 0, digits = 3)
      
      x1 <- enquo(x1)
      x2 <- enquo(x2)
      
      if (!is.data.frame(df) && !is_tibble(df)) {
        
        cli_h1("Problem with Input")
        
        cli_abort(c(
          
          "Input to argument {.strong {.var df}} was not of the correct class. {.fn compare_is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(df)}}.",
          "i" = "Please use an object of class {.cls data.frame} or {.cls tibble}."
        )
        )
        
      }
      
      if (!is.integer(df[[as_name(x1)]]) &&  !is.numeric(df[[as_name(x1)]])) {
        
        cli_h1("Problem with Input")
        
        cli_abort(c(
          
          "Input to argument {.strong {.var x1}} was not of the correct class. {.fn compare_is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(as_name(x1))}} to {.var x1}.",
          "i" = "Please use an object of class {.cls numeric} or {.cls integer}."))
        
      }
      
      if (!is.integer(df[[as_name(x2)]]) && !is.numeric(df[[as_name(x2)]])) {
        
        cli_h1("Problem with Input")
        
        cli_abort(c(
          
          "Input to argument {.strong {.var x2}} was not of the correct class. {.fn compare_is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(as_name(x2))}} to {.var x2}.",
          "i" = "Please use an object of class {.cls numeric} or {.cls integer}."))
      }
      
      if (!is.function(plot_theme)) {
        
        cli_h1("Problem with Theme")
        
        cli_abort(c(
          
          "Input to argument {.strong {.var plot_theme}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(plot_theme)}} to {.var plot_theme}.",
          "i" = "Please use an object of class {.cls function}, usually a {.fn ggplot2::theme} object."))
        
      } else {
        
        chosen_theme <- as.function(plot_theme)
        
      }
      
      # Set color palette from viridis package
      color_palette1 <- viridis(6, option = "inferno")
      color_palette2 <- viridis(6, option = "turbo")
      
      # compare the distributions of two variables from the same df
      # Let df be the symbol of a data.frame and
      # let x be df[,"x"], a character vector of length 1
      
      cli_h1("Output Summary")
      
      # Message to call out the variable name
      
      cli_inform(c("*" = paste0("Exploratory data analysis on the variables '", col_green(style_bold(as_name(x1))), " and '", col_cyan(style_bold(as_name(x2))), "' from the '", col_magenta(style_bold(data_name)), "' dataset.")))
      
      # FOR VARIABLE X1: Create a filter object only if the 'filter' argument is not NULL
      
      if (!is.null(filter_x1_1) &
          is.null(filter_x1_2) & is.null(operator_x1)) {
        filter_text_x1 <- paste0(as_name(x1), filter_x1_1)
        
        condition_x1 <- parse_expr(filter_text_x1)
        
        cli_inform(c("*" = paste0("Filter applied to variable '", col_green(style_bold(as_name(x1))), "'.")))
        
      } else if (!is.null(filter_x1_1) &
                 !is.null(filter_x1_2) & !is.null(operator_x1)) {
        filter_text_x1 <-
          paste0(as_name(x1), " ", filter_x1_1, operator_x1, col_green(style_bold(as_name(x1))), " ", filter_x1_2)
        
        condition_x1 <- parse_expr(filter_text_x1)
        
        cli_inform(c("*" = paste0("Filter applied to variable '", col_green(style_bold(as_name(x1))), "'.")))
        
        
      } else {
        # Handle the case when filter is NULL
        # For example, you can print a message or proceed without filtering
        
        
        condition_x1 <- TRUE  # No filtering condition
        
        cli_inform(
          c("*" = paste0(
            "No filter applied to variable '",
            col_green(style_bold(as_name(x1))),
            "'. Proceeding with the original data for '",
            col_green(style_bold(as_name(x1))),
            "'."
          )
          ))
        
      }
    })
    
    # FOR VARIABLE X2: Create a filter object only if the 'filter' argument is not NULL
    
    with(local_env, {
      if (!is.null(filter_x2_1) &
          is.null(filter_x2_2) & is.null(operator_x2)) {
        filter_text_x2 <- paste0(as_name(x2), filter_x2_1)
        
        condition_x2 <- parse_expr(filter_text_x2)
        
        cli_inform(c("*" = paste0("Filter applied to variable '", col_cyan(style_bold(as_name(x2))), "'.")))
        
      } else if (!is.null(filter_x2_1) &
                 !is.null(filter_x2_2) & !is.null(operator_x2)) {
        filter_text_x2 <-
          paste0(as_name(x2), " ", filter_x2_1, operator_x2, as_name(x2), " ", filter_x2_2)
        
        condition_x2 <- parse_expr(filter_text_x2)
        
        cli_inform(c("*" = paste0("Filter applied to variable '", col_cyan(style_bold(as_name(x2))), "'.")))
        
      } else {
        # Handle the case when filter is NULL
        # For example, you can print a message or proceed without filtering
        
        condition_x2 <- TRUE  # No filtering condition
        
        cli_inform(
          c("*" = paste0(
            "No filter applied to '",
            col_cyan(style_bold(as_name(x2))),
            "'.",
            "Proceeding with the original data for '",
            col_cyan(style_bold(as_name(x2))),
            "'."
          )
          ))
        
      }
    })
    
    # Apply the filter condition
    
    with(local_env, {
      df_filtered_x1 <- df %>% dplyr::filter(!!condition_x1)
      df_filtered_x2 <- df %>% dplyr::filter(!!condition_x2)
      
    })
    
    with(local_env, {
      if (normalize_x1 == FALSE & normalize_x2 == FALSE) {
        
        cli_inform(c("*" = paste0("Proceeding without normalization of any variables in '",
                                  col_magenta(style_bold(data_name)),
                                  "'.")))
        
      } else if (normalize_x1 == TRUE & normalize_x2 == FALSE) {
        df_filtered_x1 <- df_filtered_x1 %>%
          mutate(!!x1 := normalize(!!x1))
        
        cli_inform(c("*" = paste0(
          "Only variable '",
          col_green(style_bold(as_name(x1))),
          "' was normalized using min-max normalization"
        )))
        
      } else if (normalize_x1 == FALSE & normalize_x2 == TRUE) {
        df_filtered_x2 <- df_filtered_x2 %>%
          mutate(!!x2 := normalize(!!x2))
        
        cli_inform(c("*" = paste0(
          "Only variable '",
          col_cyan(style_bold(as_name(x2))),
          "' was normalized using min-max normalization"
        )))
        
      } else {
        df_filtered_x1 <- df_filtered_x1 %>%
          mutate(!!x1 := normalize(!!x1))
        
        df_filtered_x2 <- df_filtered_x2 %>%
          mutate(!!x2 := normalize(!!x2))
        
        cli_inform(
          c("*" = paste0(
            "Variables '",
            col_green(style_bold(as_name(x1))),
            "' and '",
            col_cyan(style_bold(as_name(x2))),
            "' were normalized using min-max normalization"
          )
          ))
        
      }
      
    })
    
    with(local_env, {
      
      # Message calling out total number of observations
      cli_inform(c("*" = paste0("Descriptive statistics given on ", pretty_number(nrow(df_filtered_x1), n_decimal = 2), " observations for variable '", col_green(style_bold(as_name(x1))), "', and ", pretty_number(nrow(df_filtered_x2), n_decimal = 2), " observations for variable '", col_cyan(style_bold(as_name(x2))), "'.")))
      
    })
    
    with(local_env, {
      
      # Filter out missing values in the column x1, get nrow()
      df_filtered_x1_clean <- df_filtered_x1 %>% 
        dplyr::filter(!is.na(!!x1))
      
      n_x1 <- nrow(df_filtered_x1_clean)
      
      # Filter out missing values in the column x2, get nrow()
      df_filtered_x2_clean <- df_filtered_x2 %>% 
        dplyr::filter(!is.na(!!x2))
      
      n_x2 <- nrow(df_filtered_x2_clean)
    
    # for x1
      
    if(n_x1 < 3) { # for x1 with n < 3
    
    shapiro_p_value_x1 <- NA_real_
    
    normality_diagnosis_x1 <- "Error: n < 3"
      
    } else if(n_x1 > 5000) { # for x1 with n > 5000
      
      # Sample 5000 records for Shapiro-Wilk test
      sample_df_x1 <- df_filtered_x1_clean %>%
        sample_n(5000, replace = FALSE)
      
      shapiro_test_result_x1 <- shapiro.test(sample_df_x1[[as_name(x1)]])
      
      # Store p-value and normality diagnosis
      shapiro_p_value_x1 <- shapiro_test_result_x1$p.value
      normality_diagnosis_x1 <- if_else(shapiro_p_value_x1 < 0.05, "Non-normal distribution", "Normal distribution")
      
    } else { # with 4 >= n <= 5000 for x1
      
      # Perform Shapiro-Wilk test on the cleaned data
      shapiro_test_result_x1 <- shapiro.test(df_filtered_x1_clean[[as_name(x1)]])
      
      # Store p-value and normality diagnosis
      shapiro_p_value_x1 <- shapiro_test_result_x1$p.value
      normality_diagnosis_x1 <- if_else(shapiro_p_value_x1 < 0.05, "Non-normal distribution", "Normal distribution")
      
    }
      
    # for x2
      
      if(n_x2 < 3) { # for x2 with n < 3
        
        shapiro_p_value_x2 <- NA_real_
        
        normality_diagnosis_x2 <- "Error: n < 3"
        
      } else if(n_x2 > 5000) { # for x1 with n > 5000
        
        # Sample 5000 records for Shapiro-Wilk test
        sample_df_x2 <- df_filtered_x2_clean %>%
          sample_n(5000, replace = FALSE)
        
        shapiro_test_result_x2 <- shapiro.test(sample_df_x2[[as_name(x2)]])
        
        # Store p-value and normality diagnosis
        shapiro_p_value_x2 <- shapiro_test_result_x2$p.value
        normality_diagnosis_x2 <- if_else(shapiro_p_value_x2 < 0.05, "Non-normal distribution", "Normal distribution")
        
      } else { # with 4 >= n <= 5000 for x2
        
        # Perform Shapiro-Wilk test on the cleaned data
        shapiro_test_result_x2 <- shapiro.test(df_filtered_x2_clean[[as_name(x2)]])
        
        # Store p-value and normality diagnosis
        shapiro_p_value_x2 <- shapiro_test_result_x2$p.value
        normality_diagnosis_x2 <- if_else(shapiro_p_value_x2 < 0.05, "Non-normal distribution", "Normal distribution")
        
      }
    

    })
    
    with(local_env, {
      # Create the tibble giving summary statistics
      result_tibble <- tibble(
        variable = c(as_name(x1), as_name(x2)),
        mean = c(
          mean(df_filtered_x1[[as_name(x1)]], na.rm = TRUE),
          mean(df_filtered_x2[[as_name(x2)]], na.rm = TRUE)
        ),
        std_dev = c(
          sd(df_filtered_x1[[as_name(x1)]], na.rm = TRUE),
          sd(df_filtered_x2[[as_name(x2)]], na.rm = TRUE)
        ),
        minimum = c(
          min(df_filtered_x1[[as_name(x1)]], na.rm = TRUE),
          min(df_filtered_x2[[as_name(x2)]], na.rm = TRUE)
        ),
        quantile_25 = c(
          quantile(df_filtered_x1[[as_name(x1)]], probs = 0.25, na.rm = TRUE),
          quantile(df_filtered_x2[[as_name(x2)]], probs = 0.25, na.rm = TRUE)
        ),
        median = c(
          quantile(df_filtered_x1[[as_name(x1)]], probs = 0.5, na.rm = TRUE),
          quantile(df_filtered_x2[[as_name(x2)]], probs = 0.5, na.rm = TRUE)
        ),
        quantile_75 = c(
          quantile(df_filtered_x1[[as_name(x1)]], probs = 0.75, na.rm = TRUE),
          quantile(df_filtered_x2[[as_name(x2)]], probs = 0.75, na.rm = TRUE)
        ),
        maximum = c(
          max(df_filtered_x1[[as_name(x1)]], na.rm = TRUE),
          max(df_filtered_x2[[as_name(x2)]], na.rm = TRUE)
        ),
        non_missing = c(
          sum(!is.na(df_filtered_x1[[as_name(x1)]])),
          sum(!is.na(df_filtered_x2[[as_name(x2)]]))
        ),
        missing = c(
          sum(is.na(df_filtered_x1[[as_name(x1)]])),
          sum(is.na(df_filtered_x2[[as_name(x2)]]))
        ),
        percent_missing = c(
          round(mean(is.na(df_filtered_x1[[as_name(x1)]])), digits = 5),
          round(mean(is.na(df_filtered_x2[[as_name(x2)]])), digits = 5)
        ),
        shapiro_test_result = c(
          shapiro_p_value_x1,
          shapiro_p_value_x2
        ),
        normality_diagnosis = c(
          normality_diagnosis_x1,
          normality_diagnosis_x2
        )
      )
    })
    
    # Plotting code using ggplot2 and patchwork
    
    with(local_env, {
      if (include_plots == FALSE) {
        
        cli_inform(c("*" = paste0("Plotting functionality ", col_silver(style_bold("will not")), " run.")))
        
      } else {
        
        cli_inform(c("*" = paste0("Plotting functionality ", col_silver(style_bold("will")), " run.")))
        
        qqplot_var1 <-
          ggplot(df_filtered_x1, aes(sample = !!x1)) +
          stat_qq(color = color_palette1[1],
                  size = 2,
                  na.rm = TRUE) +
          geom_qq_line(
            color = color_palette1[3],
            linewidth = 1.25,
            alpha = 0.7,
            na.rm = TRUE
          ) +
          ggtitle(paste0("Normal Q-Q Plot for ", as_name(x1))) +
          chosen_theme(...)
        
        qqplot_var2 <-
          ggplot(df_filtered_x2, aes(sample = !!x2)) +
          stat_qq(color = color_palette2[1],
                  size = 2,
                  na.rm = TRUE) +
          geom_qq_line(
            color = color_palette2[3],
            linewidth = 1.25,
            alpha = 0.7,
            na.rm = TRUE
          ) +
          ggtitle(paste0("Normal Q-Q Plot for ", as_name(x2))) +
          chosen_theme(...)
        
        non_missing_n_x1 <-
          df_filtered_x1 %>% dplyr::select(all_of(as_name(x1))) %>% na.omit() %>% summarize(n = n()) %>% pull(n)
        
        densityplot_var1 <-
          ggplot(df_filtered_x1, aes(x = !!x1)) +
          geom_density(
            fill = color_palette1[2],
            color = "black",
            alpha = 0.7,
            na.rm = TRUE
          ) +
          geom_vline(
            xintercept = median(df_filtered_x1[[as_name(x1)]], na.rm = TRUE),
            linetype = "dashed",
            color = color_palette1[4],
            linewidth = 1.5
          ) +
          ggtitle(paste0(
            "Kernel Density Plot of ",
            as_name(x1),
            " with horizontal line at the median"
          )) +
          labs(caption = paste0("n = ", non_missing_n_x1, " non-missings")) +
          chosen_theme(...)
        
        non_missing_n_x2 <-
          df_filtered_x2 %>% dplyr::select(all_of(as_name(x2))) %>% na.omit() %>% summarize(n = n()) %>% pull(n)
        
        densityplot_var2 <-
          ggplot(df_filtered_x2, aes(x = !!x2)) +
          geom_density(
            fill = color_palette2[2],
            color = "black",
            alpha = 0.7,
            na.rm = TRUE
          ) +
          geom_vline(
            xintercept = median(df_filtered_x2[[as_name(x2)]], na.rm = TRUE),
            linetype = "dashed",
            color = color_palette2[4],
            linewidth = 1.5
          ) +
          ggtitle(paste0(
            "Kernel Density Plot of ",
            as_name(x2),
            " with horizontal line at the median"
          )) +
          labs(caption = paste0("n = ", non_missing_n_x2, " non-missings")) +
          chosen_theme(...)
        
        histplot_var1 <-
          ggplot(df_filtered_x1, aes(x = !!x1)) +
          geom_histogram(
            binwidth = (
              max(df_filtered_x1[[as_name(x1)]], na.rm = TRUE) - min(df_filtered_x1[[as_name(x1)]], na.rm = TRUE)
            ) / 15,
            fill = color_palette1[5],
            color = "black",
            na.rm = TRUE
          ) +
          ggtitle(paste0("Histogram of ", as_name(x1))) +
          chosen_theme(...)
        
        histplot_var2 <-
          ggplot(df_filtered_x2, aes(x = !!x2)) +
          geom_histogram(
            binwidth = (
              max(df_filtered_x2[[as_name(x2)]], na.rm = TRUE) - min(df_filtered_x2[[as_name(x2)]], na.rm = TRUE)
            ) / 15,
            fill = color_palette2[5],
            color = "black",
            na.rm = TRUE
          ) +
          ggtitle(paste0("Histogram of ", as_name(x2))) +
          chosen_theme(...)
        
        boxplot_var1 <- ggplot(df_filtered_x1, aes(!!x1)) +
          geom_boxplot(
            fill = color_palette1[6],
            color = "black",
            na.rm = TRUE,
            orientation = "y"
          ) +
          stat_boxplot(geom = "errorbar", width = 0.5) +
          ggtitle(paste0("Boxplot of ", as_name(x1))) +
          chosen_theme(...)
        
        boxplot_var2 <- ggplot(df_filtered_x2, aes(!!x2)) +
          geom_boxplot(
            fill = color_palette2[6],
            color = "black",
            na.rm = TRUE,
            orientation = "y"
          ) +
          stat_boxplot(geom = "errorbar", width = 0.5) +
          ggtitle(paste0("Boxplot of ", as_name(x2))) +
          chosen_theme(...)
        
        # Create dynamic captions
        
        custom_caption1 <-
          if (condition_x1 == TRUE & condition_x2 == TRUE) {
            paste0("No filters applied.")
            
          } else if (condition_x1 != TRUE & condition_x2 == TRUE) {
            paste0("Filter of ",
                   filter_text_x1,
                   " applied.")
            
          } else if (condition_x1 == TRUE & condition_x2 != TRUE) {
            paste0("Filter of ",
                   filter_text_x2,
                   " applied.")
            
          } else {
            paste0(
              "Filters of ",
              filter_text_x1,
              " and ",
              filter_text_x2,
              " were applied."
            )
            
          }
        
        custom_caption2 <-
          if (normalize_x1 == FALSE & normalize_x2 == FALSE) {
            paste0("No min-max normalization applied to the data.")
            
          } else if (normalize_x1 == TRUE & normalize_x2 == FALSE) {
            paste0("Min-max normalization was applied to variable '",
                   as_name(x1),
                   "' only.")
            
          } else if (normalize_x1 == FALSE & normalize_x2 == TRUE) {
            paste0("Min-max normalization was applied to variable '",
                   as_name(x2),
                   "' only.")
            
          } else {
            paste0("Min-max normalization was applied to '",
                   as_name(x1),
                   "' and '",
                   as_name(x2),
                   "'.")
            
          }
        
        # Arrange the plots using patchwork
        combined_plots <- (
          (qqplot_var1 | qqplot_var2) /
            (densityplot_var1 | densityplot_var2) /
            (histplot_var1 | histplot_var2) /
            (boxplot_var1 | boxplot_var2) +
            plot_annotation(
              title = paste0(
                "Normality Test of the '",
                data_name,
                "' dataset, regarding variables '",
                as_name(x1),
                "' and '",
                as_name(x2),
                "'"
              ),
              subtitle = "Source: 'compare_is_it_normal()'",
              caption = paste0(custom_caption1, "\n", custom_caption2),
              theme = chosen_theme(...)
            )
        )
        
        
        
      }
      
    })
    
    
    
    with(local_env, {
      
      if(print_out == T){
        
      cli_h2("Output")
        
      print(result_tibble)
      
      }
      
      if(include_plots == T){
        
        print(combined_plots)
        
      }
      
      if(print_out == F) {
        
        return(result_tibble)
        
      }
      
    })
    
  }
