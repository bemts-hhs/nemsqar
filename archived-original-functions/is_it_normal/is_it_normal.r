###_____________________________________________________________________________
### Single variable function
### a function to check if a continuous variable is normally distributed
### or not.
###_____________________________________________________________________________

is_it_normal <- function(df,
                         data_name = deparse(substitute(df)),
                         x,
                         include_plots = TRUE,
                         filter1 = NULL,
                         filter2 = NULL,
                         operator = NULL,
                         scale = FALSE,
                         print_out = TRUE,
                         plot_theme = theme_cleaner,
                         ...) {
  # Ensure the required tidyverse package is installed
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse", quiet = TRUE)
  }
  
  # Load the tidyverse package if needed
  library(tidyverse, quietly = TRUE)
  
  # Check if x is provided
  
  if (missing(x)) {
    
    cli_h1("Missing Required Argument {.var x}")
    
    cli_abort(c(
      "The {.var x} argument is required.",
      "i" = "Please provide a value for {.var x}."
    ))
  }
  
  
  # Set up a local environment
  local_env <- new.env()
  
  
  with(local_env, {
    
    # set the random seed for the shapiro-wilks test
    set.seed(123)
    
    # set the scipen option so results come out as desired
    options(scipen = 0, digits = 3)
    
    # Set color palette from viridis package
    color_palette <- viridis(6, option = "inferno")
  })
  
  # Capture the unquoted variable name using enquo()
  x <- enquo(x)
  
  # Let df be the symbol of a data.frame and
  # let x be df[,"x"], a character vector of length 1
  if (!is.data.frame(df) && !is_tibble(df)) {
    
    cli_h1("Problem with Input")
    
    cli_abort(c(
      
      "Input to argument {.strong {.var df}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(df)}}.",
      "i" = "Please use an object of class {.cls data.frame} or {.cls tibble}."
    )
    )
    
  }
  
  if (!is.integer(df[[as_name(x)]]) && !is.numeric(df[[as_name(x)]])) {
    
    cli_h1("Problem with Input")
    
    cli_abort(c(
      
      "Input to argument {.strong {.var x}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(as_name(x))}} to {.var x}.",
      "i" = "Please use an object of class {.cls numeric} or {.cls integer}."
    )
    )
  }
  
  # Check if plot_theme is a function
  if (!is.function(plot_theme)) {
    
    cli_h1("Problem with Theme")
    
    cli_abort(c(
      
      "Input to argument {.strong {.var plot_theme}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(plot_theme)}} to {.var plot_theme}.",
      "i" = "Please use an object of class {.cls function}, usually a {.fn ggplot2::theme} object."
    )
    )
    
  } else {
    chosen_theme <- as.function(plot_theme)
  }
  
  with(local_env, {
    
    cli_h1("Output Summary")
    
    # Message to call out the variable name
    
    cli_inform(c("*" = paste0("Exploratory data analysis on the variable '", col_green(style_bold(as_name(x))), "' from the '", col_magenta(style_bold(data_name)), "' dataset.")))
    
    # Create a filter object only if the 'filter' argument is not NULL
    if (!is.null(filter1) &
        is.null(filter2) & is.null(operator)) {
      filter_text <- paste0(as_name(x), " ", filter1)
      
      condition <- parse_expr(filter_text)
      
      cli_inform(c("*" = paste0("Filter applied to '", col_green(style_bold(as_name(x))), "'.")))
      
    } else if (!is.null(filter1) &
               !is.null(filter2) & !is.null(operator)) {
      filter_text <- paste0(as_name(x), " ", filter1, operator, as_name(x), " ", filter2)
      
      condition <- parse_expr(filter_text)
      
      cli_inform(c("*" = paste0("Filter applied to '", col_green(style_bold(as_name(x))), "'.")))
      
    } else {
      # Handle the case when filter is NULL
      # For example, you can print a message or proceed without filtering
      
      condition <- TRUE  # No filtering condition
      
      cli_inform(c("*" = paste0("No filter applied to '", col_green(style_bold(as_name(x))), "'. Proceeding with the original dataframe.")))
      
    }
  })
  
  with(local_env, {
    # Apply the filter condition
    
    df_filtered <- df %>% dplyr::filter(!!condition)
    
  })
  
  with(local_env, {
    # Perform optional Min-max normalization on the provided variable
    
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
    
  })
  
  # Shapiro-Wilk test of normality
  with(local_env, {
    # Filter out missing values in the column x
    df_filtered_clean <- df_filtered %>%
      dplyr::filter(!is.na(!!x))
    
    n <- nrow(df_filtered_clean)
    
    if (n < 3) {
      shapiro_p_value <- NA
      normality_diagnosis <- "Sample size is too small for Shapiro-Wilk test."
    } else if (n > 5000) {
      # Sample 5000 records for Shapiro-Wilk test
      sample_df <- df_filtered_clean %>%
        sample_n(5000, replace = FALSE)
      shapiro_test_result <- shapiro.test(sample_df[[as_name(x)]])
      
      # Store p-value and normality diagnosis
      shapiro_p_value <- shapiro_test_result$p.value
      normality_diagnosis <- if_else(shapiro_p_value < 0.05, "Data are not normally distributed", "Data are normally distributed")
      
    } else {
      # Perform Shapiro-Wilk test on the cleaned data
      shapiro_test_result <- shapiro.test(df_filtered_clean[[as_name(x)]])
      
      # Store p-value and normality diagnosis
      shapiro_p_value <- shapiro_test_result$p.value
      normality_diagnosis <- if_else(shapiro_p_value < 0.05, "Data are not normally distributed", "Data are normally distributed")
                                     
    }
  }
)
  
  # Function to format p-value
  format_p_value <- function(p_value) {
    
    if (is.na(p_value)) {
      
      return(NA)
      
    } else if (p_value < 0.001) {
      
      return(
        
        paste0("p < 0.001 (p = ", formatC(p_value, format = "g"), ")")
        
             )
      
    } else if (p_value < 0.01) {
      
      return(
        
        paste0("p < 0.01 (p = ", formatC(p_value, format = "g"), ")")
        
             )
      
    } else if (p_value < 0.05) {
      
      return(
        
        paste0("p < 0.05 (p = ", formatC(p_value, format = "g"), ")")
        
             )
    } else {
      
      return(
        
        paste0("p > 0.05 (p = ", formatC(p_value, format = "g"), ")")
        
      )
      
    }
  }
  
  # define the output table
  with(local_env, {
    tibble_x <- tibble(
      Stat = c(
        "mean",
        "std. dev.",
        "minimum",
        "quantile 25%",
        "median",
        "quantile 75%",
        "maximum",
        "non-missings",
        "missings",
        "total observations",
        "percent missing",
        "Shapiro-Wilk p-value",
        "Normality Diagnosis"
      ),
      Value = c(
        round(mean(df_filtered[[as_name(x)]], na.rm = TRUE), digits = 3),
        round(sd(df_filtered[[as_name(x)]], na.rm = TRUE), digits = 3),
        min(df_filtered[[as_name(x)]], na.rm = TRUE),
        quantile(df_filtered[[as_name(x)]], probs = 0.25, na.rm = TRUE),
        quantile(df_filtered[[as_name(x)]], probs = 0.5, na.rm = TRUE),
        quantile(df_filtered[[as_name(x)]], probs = 0.75, na.rm = TRUE),
        max(df_filtered[[as_name(x)]], na.rm = TRUE),
        sum(!is.na(df_filtered[[as_name(x)]])),
        sum(is.na(df_filtered[[as_name(x)]])),
        nrow(df_filtered),
        round(mean(is.na(df_filtered[[as_name(x)]])), digits = 3),
        format_p_value(shapiro_p_value),
        normality_diagnosis
      )
    )
    
  })
  
  with(local_env, {
    if (include_plots == FALSE) {
      cli_inform(c("*" = paste0("Plotting functionality ", col_silver(style_bold("will not")), " run.")))
      
    } else {
      cli_inform(c("*" = paste0("Plotting functionality ", col_silver(style_bold("will")), " run.")))
      
      # Use ggplot2 and patchwork for plotting
      qqplot <- ggplot(df_filtered, aes(sample = !!x)) +
        stat_qq(color = color_palette[1],
                size = 2,
                na.rm = TRUE) +
        geom_qq_line(
          linewidth = 1.25,
          color = color_palette[3],
          alpha = 0.7,
          na.rm = TRUE
        ) +  # Add this line to fit the quantiles
        ggtitle(paste0("Normal Q-Q Plot of ", as_name(x))) +
        labs(x = as_name(x)) +
        chosen_theme(...)
      
      hist_plot <- ggplot(df_filtered, aes(x = !!x)) +
        geom_histogram(
          binwidth = (
            max(df_filtered[[as_name(x)]], na.rm = TRUE) - min(df_filtered[[as_name(x)]], na.rm = TRUE)
          ) / 15,
          fill = color_palette[5],
          color = "black",
          na.rm = TRUE
        ) +
        ggtitle(paste0("Histogram of ", as_name(x))) +
        labs(x = as_name(x)) +
        chosen_theme(...)
      
      non_missing_n <-
        df_filtered %>% dplyr::select(all_of(as_name(x))) %>% na.omit() %>% summarize(n = n()) %>% pull(n)
      
      density_plot <- ggplot(df_filtered, aes(x = !!x)) +
        geom_density(
          fill = color_palette[2],
          color = "black",
          alpha = 0.7,
          na.rm = TRUE
        ) +
        geom_vline(
          xintercept = median(df_filtered[[as_name(x)]], na.rm = TRUE),
          linetype = "dashed",
          color = color_palette[4],
          linewidth = 1.5
        ) +
        ggtitle(paste0(
          "Kernel Density Plot of ",
          as_name(x),
          "\nwith horizontal line at the median"
        )) +
        labs(x = as_name(x),
             caption = paste0("n = ", non_missing_n, " non-missings")) +
        chosen_theme(...)
      
      boxplot_plot <- ggplot(df_filtered, aes(!!x)) +
        geom_boxplot(
          fill = color_palette[6],
          color = "black",
          na.rm = TRUE,
          orientation = "y"
        ) +
        stat_boxplot(geom = "errorbar", width = 0.5) +
        ggtitle(paste0("Boxplot of ", as_name(x))) +
        labs(x = as_name(x)) +
        chosen_theme(...)
      
      # Set up a custom caption
      
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
      
      with(local_env, {
        # Arrange the plots using patchwork
        combined_plots <-
          qqplot + hist_plot + density_plot + boxplot_plot +
          plot_annotation(
            title = paste0(
              "Normality Test of the '",
              data_name,
              "' dataset, regarding variable '",
              as_name(x),
              "'"
            ),
            subtitle = paste0("Function: 'is_it_normal()'"),
            caption = paste0(custom_caption1, "\n", custom_caption2),
            theme = chosen_theme(...)
          )
        
      })
      
      
      
    }
    
  })
  
  
  
  with(local_env, {
    
    if(print_out == T){
      
    cli_h2("Output")
    print(tibble_x)
    
    }
    
    if(include_plots == T){
      
      print(combined_plots)
      
    }
    
    if(print_out == F) {
      
      return(tibble_x)
      
    }
    
  })
  
}
