
#' Is It Normal
#'
#' Assesses whether a continuous numeric variable in a dataset follows a normal distribution.
#'
#' The `is_it_normal` function is designed to assess whether a continuous numeric
#' variable in a dataset follows a normal distribution. This function provides
#' key descriptive statistics, performs a Shapiro-Wilk test for normality, and
#' offers a range of visualizations such as Q-Q plots, histograms, kernel density
#' plots, and boxplots to help you evaluate the distribution of the variable.
#'
#' @section details:
#' The is_it_normal function provides a comprehensive way to check whether a continuous variable in a dataset is normally distributed. It performs several key tasks:
#' - Filters the dataframe based on provided conditions.
#' - Computes descriptive statistics such as mean, standard deviation, quartiles, and missing data information.
#' - Conducts a Shapiro-Wilk test of normality.
#' - Optionally applies Min-Max scaling to the data.
#' - Generates several useful visualizations including:
#'   - Q-Q Plot: to visualize the quantiles against a theoretical normal distribution.
#'   - Histogram: to observe the shape of the distribution.
#'   - Kernel Density Plot: to estimate the probability density function.
#'   - Boxplot: to summarize the distribution through median and quartiles.
#'
#'
#' @param df A dataframe or tibble containing the variable to be analyzed.
#' @param data_name Optional name of dataset.
#' @param x (Required) The unquoted name of the numeric variable to test for normality.
#' @param include_plots (Logical) Whether to include visualizations such as Q-Q plot, histogram, density plot, and boxplot. Default is TRUE.
#' @param filter1 (Optional) A condition to filter the dataframe based on the variable. Used when you want to filter data before the analysis.  Must be a RHS logical argument as a string (e.g. " < 40").
#' @param filter2 (Optional) A second condition for filtering.  Must be a RHS logical argument as a string (e.g. " > 50").
#' @param operator (Optional) A logical operator (e.g., &, |) for more complex filtering conditions as a string to join `filter1` and `filter2`.
#' @param scale (Logical) Whether to apply Min-Max normalization to the variable before the analysis. Default is FALSE.
#' @param print_out (Logical) Whether to print the output tibble or return it invisibly. Default is TRUE.
#' @param plot_theme (Optional) A ggplot2 theme function name to apply custom styling to the plots. Default is theme_cleaner.
#' @param ... Additional arguments passed to ggplot2 themes or other customization.
#'
#' @return A tibble containing mean, SD, min/max/quartiles, number of missing values, Shapiro-Wilk p-value, normality diagnosis, and optional plots.
#' 
#' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.#' 
#' 
#' @export
#'
#' @examples
#' data(mtcars)
#' # Check if 'mpg' variable is normallly distributed.
#' is_it_normal(mtcars, mpg)
#'
#' # Using a filter and custom theme
#' is_it_normal(mtcars, mpg, filter1 = " > 15", filter2 = " < 100", operator = " & ", plot_theme = theme_minimal)
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
  if (missing(x)) {

    cli::cli_h1("Missing Required Argument {.var x}")

    cli::cli_abort(c(
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
    color_palette <- viridis::viridis(6, option = "inferno")
  })

  # Capture the unquoted variable name using enquo()
  x <- rlang::enquo(x)

  # Let df be the symbol of a data.frame and
  # let x be df[,"x"], a character vector of length 1
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {

    cli::cli_h1("Problem with Input")

    cli::cli_abort(c(

      "Input to argument {.strong {.var df}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(df)}}.",
      "i" = "Please use an object of class {.cls data.frame} or {.cls tibble}."
    )
    )

  }

  if (!is.integer(df[[rlang::as_name(x)]]) && !is.numeric(df[[rlang::as_name(x)]])) {

    cli::cli_h1("Problem with Input")

    cli::cli_abort(c(

      "Input to argument {.strong {.var x}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(as_name(x))}} to {.var x}.",
      "i" = "Please use an object of class {.cls numeric} or {.cls integer}."
    )
    )
  }

  # Check if plot_theme is a function
  if (!is.function(plot_theme)) {

    cli::cli_h1("Problem with Theme")

    cli::cli_abort(c(

      "Input to argument {.strong {.var plot_theme}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
      "x" = "You supplied an object of class {.cls {class(plot_theme)}} to {.var plot_theme}.",
      "i" = "Please use an object of class {.cls function}, usually a {.fn ggplot2::theme} object."
    )
    )

  } else {
    chosen_theme <- as.function(plot_theme)
  }

  with(local_env, {

    cli::cli_h1("Output Summary")

    # Message to call out the variable name

    cli::cli_inform(c("*" = paste0("Exploratory data analysis on the variable '",
                                   cli::col_green(cli::style_bold(rlang::as_name(x))),
                                   "' from the '",
                                   cli::col_magenta(cli::style_bold(data_name)), "' dataset.")))

    # Create a filter object only if the 'filter' argument is not NULL
    if (!is.null(filter1) &
        is.null(filter2) & is.null(operator)) {
      filter_text <- paste0(rlang::as_name(x), " ", filter1)

      condition <- rlang::parse_expr(filter_text)

      cli::cli_inform(c("*" = paste0("Filter applied to '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x))), "'.")))

    } else if (!is.null(filter1) &
               !is.null(filter2) & !is.null(operator)) {
      filter_text <- paste0(rlang::as_name(x), " ", filter1, operator, rlang::as_name(x), " ", filter2)

      condition <- rlang::parse_expr(filter_text)

      cli::cli_inform(c("*" = paste0("Filter applied to '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x))), "'.")))

    } else {
      # Handle the case when filter is NULL
      # For example, you can print a message or proceed without filtering

      condition <- TRUE  # No filtering condition

      cli::cli_inform(c("*" = paste0("No filter applied to '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x))),
                                     "'. Proceeding with the original dataframe.")))

    }
  })

  with(local_env, {
    # Apply the filter condition

    df_filtered <- df |>  dplyr::filter(!!condition)

  })

  with(local_env, {
    # Perform optional Min-max normalization on the provided variable

    if (scale == TRUE) {
      df_filtered <- df_filtered |>
        mutate(!!x := normalize(!!x))

      cli::cli_inform(c("*" = paste0("Min-max normalization was performed on '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x))),
                                     "', and will be reflected in the descriptive statistics below."
      )))

    } else {

      cli::cli_inform(c("*" = paste0("Min-max normalization was not performed on '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x))), "'.")))

    }

  })

  # Shapiro-Wilk test of normality
  with(local_env, {
    # Filter out missing values in the column x
    df_filtered_clean <- df_filtered |>
      dplyr::filter(!is.na(!!x))

    n <- nrow(df_filtered_clean)

    if (n < 3) {
      shapiro_p_value <- NA
      normality_diagnosis <- "Sample size is too small for Shapiro-Wilk test."
    } else if (n > 5000) {
      # Sample 5000 records for Shapiro-Wilk test
      sample_df <- df_filtered_clean |>
        sample_n(5000, replace = FALSE)
      shapiro_test_result <- shapiro.test(sample_df[[rlang::as_name(x)]])

      # Store p-value and normality diagnosis
      shapiro_p_value <- shapiro_test_result$p.value
      normality_diagnosis <- dplyr::if_else(shapiro_p_value < 0.05,
                                            "Data are not normally distributed",
                                            "Data are normally distributed")

    } else {
      # Perform Shapiro-Wilk test on the cleaned data
      shapiro_test_result <- shapiro.test(df_filtered_clean[[rlang::as_name(x)]])

      # Store p-value and normality diagnosis
      shapiro_p_value <- shapiro_test_result$p.value
      normality_diagnosis <- dplyr::if_else(shapiro_p_value < 0.05,
                                            "Data are not normally distributed",
                                            "Data are normally distributed")

    }
  }
  )

  # define the output table
  with(local_env, {
    tibble_x <- tibble::tibble(
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
        round(mean(df_filtered[[rlang::as_name(x)]], na.rm = TRUE), digits = 3),
        round(sd(df_filtered[[rlang::as_name(x)]], na.rm = TRUE), digits = 3),
        min(df_filtered[[rlang::as_name(x)]], na.rm = TRUE),
        quantile(df_filtered[[rlang::as_name(x)]], probs = 0.25, na.rm = TRUE),
        quantile(df_filtered[[rlang::as_name(x)]], probs = 0.5, na.rm = TRUE),
        quantile(df_filtered[[rlang::as_name(x)]], probs = 0.75, na.rm = TRUE),
        max(df_filtered[[rlang::as_name(x)]], na.rm = TRUE),
        sum(!is.na(df_filtered[[rlang::as_name(x)]])),
        sum(is.na(df_filtered[[rlang::as_name(x)]])),
        nrow(df_filtered),
        round(mean(is.na(df_filtered[[rlang::as_name(x)]])), digits = 3),
        format_p_value(shapiro_p_value),
        normality_diagnosis
      )
    )

  })

  with(local_env, {
    if (include_plots == FALSE) {
      cli::cli_inform(c("*" = paste0("Plotting functionality ",
                                     cli::col_silver(cli::style_bold("will not")), " run.")))

    } else {
      cli::cli_inform(c("*" = paste0("Plotting functionality ",
                                     cli::col_silver(cli::style_bold("will")), " run.")))

      # Use ggplot2 and patchwork for plotting
      qqplot <- ggplot2::ggplot(df_filtered, ggplot2::aes(sample = !!x)) +
        ggplot2::stat_qq(color = color_palette[1],
                         size = 2,
                         na.rm = TRUE) +
        ggplot2::geom_qq_line(
          linewidth = 1.25,
          color = color_palette[3],
          alpha = 0.7,
          na.rm = TRUE
        ) +  # Add this line to fit the quantiles
        ggplot2::ggtitle(paste0("Normal Q-Q Plot of ", rlang::as_name(x))) +
        ggplot2::labs(x = rlang::as_name(x)) +
        chosen_theme(...)

      hist_plot <- ggplot2::ggplot(df_filtered, ggplot2::aes(x = !!x)) +
        ggplot2::geom_histogram(
          binwidth = (
            max(df_filtered[[rlang::as_name(x)]], na.rm = TRUE) - min(df_filtered[[rlang::as_name(x)]], na.rm = TRUE)
          ) / 15,
          fill = color_palette[5],
          color = "black",
          na.rm = TRUE
        ) +
        ggplot2::ggtitle(paste0("Histogram of ", rlang::as_name(x))) +
        ggplot2::labs(x = rlang::as_name(x)) +
        chosen_theme(...)

      non_missing_n <-
        df_filtered |>  dplyr::select(all_of(rlang::as_name(x))) |>  na.omit() |>
        dplyr::summarize(n = dplyr::n()) |>  dplyr::pull(n)

      density_plot <- ggplot2::ggplot(df_filtered, ggplot2::aes(x = !!x)) +
        ggplot2::geom_density(
          fill = color_palette[2],
          color = "black",
          alpha = 0.7,
          na.rm = TRUE
        ) +
        ggplot2::geom_vline(
          xintercept = median(df_filtered[[rlang::as_name(x)]], na.rm = TRUE),
          linetype = "dashed",
          color = color_palette[4],
          linewidth = 1.5
        ) +
        ggplot2::ggtitle(paste0(
          "Kernel Density Plot of ",
          rlang::as_name(x),
          "\nwith horizontal line at the median"
        )) +
        ggplot2::labs(x = rlang::as_name(x),
                      caption = paste0("n = ", non_missing_n, " non-missings")) +
        chosen_theme(...)

      boxplot_plot <- ggplot2::ggplot(df_filtered, ggplot2::aes(!!x)) +
        ggplot2::geom_boxplot(
          fill = color_palette[6],
          color = "black",
          na.rm = TRUE,
          orientation = "y"
        ) +
        ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
        ggplot2::ggtitle(paste0("Boxplot of ", rlang::as_name(x))) +
        ggplot2::labs(x = rlang::as_name(x)) +
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
        paste0("Min-max normalization was applied to '", rlang::as_name(x), "'.")

      } else {
        paste0("Min-max normalization was not applied to '", rlang::as_name(x), "'.")

      }

      with(local_env, {
        # Arrange the plots using patchwork
        combined_plots <-
          qqplot + hist_plot + density_plot + boxplot_plot +
          patchwork::plot_annotation(
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

      cli::cli_h2("Output")
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

#' Compare Is It Normal
#'
#' Comparative function for two variables from the same df
#'
#' The `compare_is_it_normal` function is designed to assess the normality of
#' two numeric variables from the same dataset. It compares these variables using
#' the Shapiro-Wilk test, provides descriptive statistics, and generates visual
#' diagnostics such as Q-Q plots and histograms. This function is a valuable tool
#' for exploratory data analysis when investigating whether two distributions
#' follow a normal distribution.
#'
#' @section details:
#' The `compare_is_it_normal` function provides a comprehensive analysis of two
#' numeric variables within the same dataset. It supports the following steps:
#' - Checks the class of input variables and applies optional filtering.
#' - Normalizes the variables if required.
#' - Performs the Shapiro-Wilk test for normality.
#' - Computes descriptive statistics including mean, standard deviation, and
#'   percent missing data.
#'   - Generates diagnostic plots such as Q-Q plots and histograms if
#'   `include_plots = TRUE`.
#'
#' @section notes:
#' - When the sample size exceeds 5,000, the Shapiro-Wilk test is conducted on a
#'   random sample of 5,000 records to ensure computational efficiency.
#' - Diagnostic plots (Q-Q plots, histograms) are only generated if
#'   include_plots = TRUE.
#' - Filters can be applied to both variables to limit the range of the data
#'   being analyzed, but must be passed as RHS logical arguments as a string
#'   (see _Arguments_ above).  If more than one filter is applied, it must be
#'   joined by an `operator`, which is a string logical operator such as " & "
#'   or " | ".
#' - This plot uses `theme_cleaner` which is a custom ggplot2 theme created for
#'   `traumaR`, and can be accessed via this Github repo.
#'
#'
#' @param df Data frame or tibble containing the variables to compare.
#' @param data_name Optional name of the dataset (default is the df object name).
#' @param x1 The first numeric variable to assess for normality.
#' @param x2 The second numeric variable to assess for normality.
#' @param include_plots Logical flag to include diagnostic plots (TRUE by default).
#' @param normalize_x1 Logical flag to normalize x1 (min-max normalization).
#' @param normalize_x2 Logical flag to normalize x2 (min-max normalization).
#' @param filter_x1_1 First filtering condition for x1 as a RHS logical argument as a string (e.g. " < 40").
#' @param filter_x1_2 Second filtering condition for x1 as a RHS logical argument as a string (e.g. " < 50").
#' @param operator_x1 Logical operator for combining filter conditions for x1 as a string (e.g. " & ").
#' @param filter_x2_1 First filtering condition for x2 as a RHS logical argument as a string (e.g. " < 40").
#' @param filter_x2_2 Second filtering condition for x2 as a RHS logical argument as a string (e.g. " < 50").
#' @param operator_x2 Logical operator for combining filter conditions for x2 as a string (e.g. " & ").
#' @param print_out Logical flag to print results to the console.
#' @param plot_theme Custom ggplot2 theme for the diagnostic plots (theme_cleaner by default).
#' @param ... Additional arguments passed to internal functions.
#'
#' @return Combined plots or tibble
#' @export
#'
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


    # Check if x1 is provided

    if (missing(x1)) {

      cli::cli_h1("Missing Required Argument {.var x1}")

      cli::cli_abort(c(
        "The {.var x1} argument is required.",
        "i" = "Please provide a value for {.var x1}."
      ))
    }

    # Check if x2 is provided

    if (missing(x2)) {

      cli::cli_h1("Missing Required Argument {.var x2}")

      cli::cli_abort(c(
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

      if (!is.data.frame(df) && !tibble::is_tibble(df)) {

        cli::cli_h1("Problem with Input")

        cli::cli_abort(c(

          "Input to argument {.strong {.var df}} was not of the correct class. {.fn compare_is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(df)}}.",
          "i" = "Please use an object of class {.cls data.frame} or {.cls tibble}."
        )
        )

      }

      if (!is.integer(df[[rlang::as_name(x1)]]) &&  !is.numeric(df[[rlang::as_name(x1)]])) {

        cli::cli_h1("Problem with Input")

        cli::cli_abort(c(

          "Input to argument {.strong {.var x1}} was not of the correct class. {.fn compare_is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(as_name(x1))}} to {.var x1}.",
          "i" = "Please use an object of class {.cls numeric} or {.cls integer}."))

      }

      if (!is.integer(df[[rlang::as_name(x2)]]) && !is.numeric(df[[rlang::as_name(x2)]])) {

        cli::cli_h1("Problem with Input")

        cli::cli_abort(c(

          "Input to argument {.strong {.var x2}} was not of the correct class. {.fn compare_is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(as_name(x2))}} to {.var x2}.",
          "i" = "Please use an object of class {.cls numeric} or {.cls integer}."))
      }

      if (!is.function(plot_theme)) {

        cli::cli_h1("Problem with Theme")

        cli::cli_abort(c(

          "Input to argument {.strong {.var plot_theme}} was not of the correct class. {.fn is_it_normal} does not know what to do with it!",
          "x" = "You supplied an object of class {.cls {class(plot_theme)}} to {.var plot_theme}.",
          "i" = "Please use an object of class {.cls function}, usually a {.fn ggplot2::theme} object."))

      } else {

        chosen_theme <- as.function(plot_theme)

      }

      # Set color palette from viridis package
      color_palette1 <- viridis::viridis(6, option = "inferno")
      color_palette2 <- viridis::viridis(6, option = "turbo")

      # compare the distributions of two variables from the same df
      # Let df be the symbol of a data.frame and
      # let x be df[,"x"], a character vector of length 1

      cli::cli_h1("Output Summary")

      # Message to call out the variable name

      cli::cli_inform(c("*" = paste0("Exploratory data analysis on the variables '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x1))), " and '",
                                     cli::col_cyan(cli::style_bold(rlang::as_name(x2))), "' from the '",
                                     cli::col_magenta(cli::style_bold(data_name)), "' dataset.")))

      # FOR VARIABLE X1: Create a filter object only if the 'filter' argument is not NULL

      if (!is.null(filter_x1_1) &
          is.null(filter_x1_2) & is.null(operator_x1)) {
        filter_text_x1 <- paste0(rlang::as_name(x1), filter_x1_1)

        condition_x1 <- rlang::parse_expr(filter_text_x1)

        cli::cli_inform(c("*" = paste0("Filter applied to variable '",
                                       cli::col_green(cli::style_bold(rlang::as_name(x1))), "'.")))

      } else if (!is.null(filter_x1_1) &
                 !is.null(filter_x1_2) & !is.null(operator_x1)) {
        filter_text_x1 <-
          paste0(rlang::as_name(x1), " ", filter_x1_1, operator_x1,
                 cli::col_green(cli::style_bold(rlang::as_name(x1))), " ", filter_x1_2)

        condition_x1 <- rlang::parse_expr(filter_text_x1)

        cli::cli_inform(c("*" = paste0("Filter applied to variable '",
                                       cli::col_green(cli::style_bold(rlang::as_name(x1))), "'.")))


      } else {
        # Handle the case when filter is NULL
        # For example, you can print a message or proceed without filtering


        condition_x1 <- TRUE  # No filtering condition

        cli::cli_inform(
          c("*" = paste0(
            "No filter applied to variable '",
            cli::col_green(cli::style_bold(rlang::as_name(x1))),
            "'. Proceeding with the original data for '",
            cli::col_green(cli::style_bold(rlang::as_name(x1))),
            "'."
          )
          ))

      }
    })

    # FOR VARIABLE X2: Create a filter object only if the 'filter' argument is not NULL

    with(local_env, {
      if (!is.null(filter_x2_1) &
          is.null(filter_x2_2) & is.null(operator_x2)) {
        filter_text_x2 <- paste0(rlang::as_name(x2), filter_x2_1)

        condition_x2 <- rlang::parse_expr(filter_text_x2)

        cli::cli_inform(c("*" = paste0("Filter applied to variable '",
                                       cli::col_cyan(cli::style_bold(rlang::as_name(x2))), "'.")))

      } else if (!is.null(filter_x2_1) &
                 !is.null(filter_x2_2) & !is.null(operator_x2)) {
        filter_text_x2 <-
          paste0(rlang::as_name(x2), " ", filter_x2_1, operator_x2, rlang::as_name(x2), " ", filter_x2_2)

        condition_x2 <- rlang::parse_expr(filter_text_x2)

        cli::cli_inform(c("*" = paste0("Filter applied to variable '",
                                       cli::col_cyan(cli::style_bold(rlang::as_name(x2))), "'.")))

      } else {
        # Handle the case when filter is NULL
        # For example, you can print a message or proceed without filtering

        condition_x2 <- TRUE  # No filtering condition

        cli::cli_inform(
          c("*" = paste0(
            "No filter applied to '",
            cli::col_cyan(cli::style_bold(rlang::as_name(x2))),
            "'.",
            "Proceeding with the original data for '",
            cli::col_cyan(cli::style_bold(rlang::as_name(x2))),
            "'."
          )
          ))

      }
    })

    # Apply the filter condition

    with(local_env, {
      df_filtered_x1 <- df |> dplyr::filter(!!condition_x1)
      df_filtered_x2 <- df |> dplyr::filter(!!condition_x2)

    })

    with(local_env, {
      if (normalize_x1 == FALSE & normalize_x2 == FALSE) {

        cli::cli_inform(c("*" = paste0("Proceeding without normalization of any variables in '",
                                       cli::col_magenta(cli::style_bold(data_name)),
                                       "'.")))

      } else if (normalize_x1 == TRUE & normalize_x2 == FALSE) {
        df_filtered_x1 <- df_filtered_x1 %>%
          dplyr::mutate(!!x1 := normalize(!!x1))

        cli::cli_inform(c("*" = paste0(
          "Only variable '",
          cli::col_green(cli::style_bold(rlang::as_name(x1))),
          "' was normalized using min-max normalization"
        )))

      } else if (normalize_x1 == FALSE & normalize_x2 == TRUE) {
        df_filtered_x2 <- df_filtered_x2 %>%
          dplyr::mutate(!!x2 := normalize(!!x2))

        cli::cli_inform(c("*" = paste0(
          "Only variable '",
          cli::col_cyan(cli::style_bold(rlang::as_name(x2))),
          "' was normalized using min-max normalization"
        )))

      } else {
        df_filtered_x1 <- df_filtered_x1 %>%
          dplyr::mutate(!!x1 := normalize(!!x1))

        df_filtered_x2 <- df_filtered_x2 %>%
          dplyr::mutate(!!x2 := normalize(!!x2))

        cli::cli_inform(
          c("*" = paste0(
            "Variables '",
            cli::col_green(cli::style_bold(rlang::as_name(x1))),
            "' and '",
            cli::col_cyan(cli::style_bold(rlang::as_name(x2))),
            "' were normalized using min-max normalization"
          )
          ))

      }

    })

    with(local_env, {

      # Message calling out total number of observations
      cli::cli_inform(c("*" = paste0("Descriptive statistics given on ",
                                     pretty_number(nrow(df_filtered_x1), n_decimal = 2),
                                     " observations for variable '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x1))),
                                     "', and ", pretty_number(nrow(df_filtered_x2), n_decimal = 2),
                                     " observations for variable '",
                                     cli::col_cyan(cli::style_bold(rlang::as_name(x2))), "'.")))

    })

    with(local_env, {

      # Filter out missing values in the column x1, get nrow()
      df_filtered_x1_clean <- df_filtered_x1 |>
        dplyr::filter(!is.na(!!x1))

      n_x1 <- nrow(df_filtered_x1_clean)

      # Filter out missing values in the column x2, get nrow()
      df_filtered_x2_clean <- df_filtered_x2 |>
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

        shapiro_test_result_x1 <- shapiro.test(sample_df_x1[[rlang::as_name(x1)]])

        # Store p-value and normality diagnosis
        shapiro_p_value_x1 <- shapiro_test_result_x1$p.value
        normality_diagnosis_x1 <- if_else(shapiro_p_value_x1 < 0.05,
                                          "Non-normal distribution",
                                          "Normal distribution")

      } else { # with 4 >= n <= 5000 for x1

        # Perform Shapiro-Wilk test on the cleaned data
        shapiro_test_result_x1 <- shapiro.test(df_filtered_x1_clean[[rlang::as_name(x1)]])

        # Store p-value and normality diagnosis
        shapiro_p_value_x1 <- shapiro_test_result_x1$p.value
        normality_diagnosis_x1 <- dplyr::if_else(shapiro_p_value_x1 < 0.05,
                                                 "Non-normal distribution",
                                                 "Normal distribution")

      }

      # for x2

      if(n_x2 < 3) { # for x2 with n < 3

        shapiro_p_value_x2 <- NA_real_

        normality_diagnosis_x2 <- "Error: n < 3"

      } else if(n_x2 > 5000) { # for x1 with n > 5000

        # Sample 5000 records for Shapiro-Wilk test
        sample_df_x2 <- df_filtered_x2_clean %>%
          sample_n(5000, replace = FALSE)

        shapiro_test_result_x2 <- shapiro.test(sample_df_x2[[rlang::as_name(x2)]])

        # Store p-value and normality diagnosis
        shapiro_p_value_x2 <- shapiro_test_result_x2$p.value
        normality_diagnosis_x2 <- if_else(shapiro_p_value_x2 < 0.05,
                                          "Non-normal distribution",
                                          "Normal distribution")

      } else { # with 4 >= n <= 5000 for x2

        # Perform Shapiro-Wilk test on the cleaned data
        shapiro_test_result_x2 <- shapiro.test(df_filtered_x2_clean[[rlang::as_name(x2)]])

        # Store p-value and normality diagnosis
        shapiro_p_value_x2 <- shapiro_test_result_x2$p.value
        normality_diagnosis_x2 <- dplyr::if_else(shapiro_p_value_x2 < 0.05,
                                                 "Non-normal distribution",
                                                 "Normal distribution")

      }


    })

    with(local_env, {
      # Create the tibble giving summary statistics
      result_tibble <- tibble::tibble(
        variable = c(rlang::as_name(x1), rlang::as_name(x2)),
        mean = c(
          mean(df_filtered_x1[[rlang::as_name(x1)]], na.rm = TRUE),
          mean(df_filtered_x2[[rlang::as_name(x2)]], na.rm = TRUE)
        ),
        std_dev = c(
          sd(df_filtered_x1[[rlang::as_name(x1)]], na.rm = TRUE),
          sd(df_filtered_x2[[rlang::as_name(x2)]], na.rm = TRUE)
        ),
        minimum = c(
          min(df_filtered_x1[[rlang::as_name(x1)]], na.rm = TRUE),
          min(df_filtered_x2[[rlang::as_name(x2)]], na.rm = TRUE)
        ),
        quantile_25 = c(
          quantile(df_filtered_x1[[rlang::as_name(x1)]], probs = 0.25, na.rm = TRUE),
          quantile(df_filtered_x2[[rlang::as_name(x2)]], probs = 0.25, na.rm = TRUE)
        ),
        median = c(
          quantile(df_filtered_x1[[rlang::as_name(x1)]], probs = 0.5, na.rm = TRUE),
          quantile(df_filtered_x2[[rlang::as_name(x2)]], probs = 0.5, na.rm = TRUE)
        ),
        quantile_75 = c(
          quantile(df_filtered_x1[[rlang::as_name(x1)]], probs = 0.75, na.rm = TRUE),
          quantile(df_filtered_x2[[rlang::as_name(x2)]], probs = 0.75, na.rm = TRUE)
        ),
        maximum = c(
          max(df_filtered_x1[[rlang::as_name(x1)]], na.rm = TRUE),
          max(df_filtered_x2[[rlang::as_name(x2)]], na.rm = TRUE)
        ),
        non_missing = c(
          sum(!is.na(df_filtered_x1[[rlang::as_name(x1)]])),
          sum(!is.na(df_filtered_x2[[rlang::as_name(x2)]]))
        ),
        missing = c(
          sum(is.na(df_filtered_x1[[rlang::as_name(x1)]])),
          sum(is.na(df_filtered_x2[[rlang::as_name(x2)]]))
        ),
        percent_missing = c(
          round(mean(is.na(df_filtered_x1[[rlang::as_name(x1)]])), digits = 5),
          round(mean(is.na(df_filtered_x2[[rlang::as_name(x2)]])), digits = 5)
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

        cli::cli_inform(c("*" = paste0("Plotting functionality ",
                                       cli::col_silver(cli::style_bold("will not")), " run.")))

      } else {

        cli::cli_inform(c("*" = paste0("Plotting functionality ",
                                       cli::col_silver(cli::style_bold("will")), " run.")))

        qqplot_var1 <-
          ggplot2::ggplot(df_filtered_x1, ggplot2::aes(sample = !!x1)) +
          ggplot2::stat_qq(color = color_palette1[1],
                           size = 2,
                           na.rm = TRUE) +
          ggplot2::geom_qq_line(
            color = color_palette1[3],
            linewidth = 1.25,
            alpha = 0.7,
            na.rm = TRUE
          ) +
          ggplot2::ggtitle(paste0("Normal Q-Q Plot for ", rlang::as_name(x1))) +
          chosen_theme(...)

        qqplot_var2 <-
          ggplot2::ggplot(df_filtered_x2, ggplot2::aes(sample = !!x2)) +
          ggplot2::stat_qq(color = color_palette2[1],
                           size = 2,
                           na.rm = TRUE) +
          ggplot2::geom_qq_line(
            color = color_palette2[3],
            linewidth = 1.25,
            alpha = 0.7,
            na.rm = TRUE
          ) +
          ggplot2::ggtitle(paste0("Normal Q-Q Plot for ", rlang::as_name(x2))) +
          chosen_theme(...)

        non_missing_n_x1 <- df_filtered_x1 |>
          dplyr::select(all_of(as_name(x1))) |>
          na.omit() %>% dplyr::summarize(n = dplyr::n()) |>
          dplyr::pull(n)

        densityplot_var1 <-
          ggplot2::ggplot(df_filtered_x1, ggplot2::aes(x = !!x1)) +
          ggplot2::geom_density(
            fill = color_palette1[2],
            color = "black",
            alpha = 0.7,
            na.rm = TRUE
          ) +
          ggplot2::geom_vline(
            xintercept = median(df_filtered_x1[[rlang::as_name(x1)]], na.rm = TRUE),
            linetype = "dashed",
            color = color_palette1[4],
            linewidth = 1.5
          ) +
          ggplot2::ggtitle(paste0(
            "Kernel Density Plot of ",
            rlang::as_name(x1),
            " with horizontal line at the median"
          )) +
          ggplot2::labs(caption = paste0("n = ", non_missing_n_x1, " non-missings")) +
          chosen_theme(...)

        non_missing_n_x2 <-
          df_filtered_x2 |>  dplyr::select(all_of(rlang::as_name(x2))) |>
          na.omit() |>  dplyr::summarize(n = dplyr::n()) |>  dplyr::pull(n)

        densityplot_var2 <-
          ggplot2::ggplot(df_filtered_x2, ggplot2::aes(x = !!x2)) +
          ggplot2::geom_density(
            fill = color_palette2[2],
            color = "black",
            alpha = 0.7,
            na.rm = TRUE
          ) +
          ggplot2::geom_vline(
            xintercept = median(df_filtered_x2[[rlang::as_name(x2)]], na.rm = TRUE),
            linetype = "dashed",
            color = color_palette2[4],
            linewidth = 1.5
          ) +
          ggplot2::ggtitle(paste0(
            "Kernel Density Plot of ",
            rlang::as_name(x2),
            " with horizontal line at the median"
          )) +
          ggplot2::labs(caption = paste0("n = ", non_missing_n_x2, " non-missings")) +
          chosen_theme(...)

        histplot_var1 <-
          ggplot2::ggplot(df_filtered_x1, ggplot2::aes(x = !!x1)) +
          ggplot2::geom_histogram(
            binwidth = (
              max(df_filtered_x1[[rlang::as_name(x1)]], na.rm = TRUE) - min(df_filtered_x1[[rlang::as_name(x1)]], na.rm = TRUE)
            ) / 15,
            fill = color_palette1[5],
            color = "black",
            na.rm = TRUE
          ) +
          ggplot2::ggtitle(paste0("Histogram of ", rlang::as_name(x1))) +
          chosen_theme(...)

        histplot_var2 <-
          ggplot2::ggplot(df_filtered_x2, ggplot2::aes(x = !!x2)) +
          ggplot2::geom_histogram(
            binwidth = (
              max(df_filtered_x2[[rlang::as_name(x2)]], na.rm = TRUE) - min(df_filtered_x2[[rlang::as_name(x2)]], na.rm = TRUE)
            ) / 15,
            fill = color_palette2[5],
            color = "black",
            na.rm = TRUE
          ) +
          ggplot2::ggtitle(paste0("Histogram of ", rlang::as_name(x2))) +
          chosen_theme(...)

        boxplot_var1 <- ggplot2::ggplot(df_filtered_x1, ggplot2::aes(!!x1)) +
          ggplot2::geom_boxplot(
            fill = color_palette1[6],
            color = "black",
            na.rm = TRUE,
            orientation = "y"
          ) +
          ggplot2:: stat_boxplot(geom = "errorbar", width = 0.5) +
          ggplot2::ggtitle(paste0("Boxplot of ", as_name(x1))) +
          chosen_theme(...)

        boxplot_var2 <- ggplot2::ggplot(df_filtered_x2, ggplot2::aes(!!x2)) +
          ggplot2::geom_boxplot(
            fill = color_palette2[6],
            color = "black",
            na.rm = TRUE,
            orientation = "y"
          ) +
          ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
          ggplot2::ggtitle(paste0("Boxplot of ", as_name(x2))) +
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
                   rlang::as_name(x1),
                   "' only.")

          } else if (normalize_x1 == FALSE & normalize_x2 == TRUE) {
            paste0("Min-max normalization was applied to variable '",
                   rlang::as_name(x2),
                   "' only.")

          } else {
            paste0("Min-max normalization was applied to '",
                   rlang::as_name(x1),
                   "' and '",
                   rlang::as_name(x2),
                   "'.")

          }

        # Arrange the plots using patchwork
        combined_plots <- (
          (qqplot_var1 | qqplot_var2) /
            (densityplot_var1 | densityplot_var2) /
            (histplot_var1 | histplot_var2) /
            (boxplot_var1 | boxplot_var2) +
            patchwork::plot_annotation(
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

        cli::cli_h2("Output")

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

#' Compare Grouped Is It Normal
#'
#' Comparative function for one numeric vector and a factor grouping variable
#'
#' The `compare_grouped_is_it_normal` function allows users to analyze the
#' normality of a numeric variable across groups defined by a factor variable.
#' It performs Shapiro-Wilk normality tests and provides descriptive statistics
#'  and visualizations (including Q-Q plots, histograms, kernel density plots,
#'  and boxplots). Users can apply filters, scale the data using min-max
#'  normalization, and customize the output with plot themes.
#'
#'  @section Output:
#'  **Descriptive Statistics**: Summary statistics including mean, standard
#'  deviation, minimum, quartiles, median, maximum, and missing data information
#'  for each group.
#'  **Shapiro-Wilk Test Results**: P-values for the normality tests, along with
#'  a diagnosis of normal or non-normal distribution.
#'  **Visualizations**: Optional plots such as Q-Q plots, histograms, kernel
#'  density plots, and boxplots for each group.
#'
#'  @section features:
#'  # Features
#'  - **Normality Testing**: Uses the Shapiro-Wilk test for normality.
#'  - **Filtering**: Users can filter the data based on conditions using the
#'  filter1, filter2, and operator arguments.
#'  - **Scaling**: Optional min-max normalization for the numeric variable.
#'  - **Customizable Plot Themes**: Allows users to apply custom ggplot2 themes
#'  to the output plots.
#'  - **Comprehensive Descriptive Statistics**: Provides summary statistics
#'  across all groups.

#'
#'
#' @param df The input dataframe containing the data to be analyzed.
#' @param data_name (optional) The name of the dataframe. Defaults to the
#'                  expression used for the df argument.  If user-supplied,
#'                  must be a string vector of length 1.
#' @param x <['tidy-select'][dplyr_tidy_select]> The name of the column containing
#'          the numeric variable to analyze for normality.
#' @param group_var <['tidy-select'][dplyr_tidy_select]> The name of the column
#'        containing a factor variable to be used for grouping the data.
#' @param include_plots Logical whether to include plots in the output (defaults to TRUE)
#' @param filter1 (optional) The first filter condition for the numeric variable,
#'                containing an RHS logical comparator as a string (e.g., "< 50")
#' @param filter2 (optional) The second filter condition for the numeric variable,
#'                containing an RHS logical comparator as a string (e.g., "< 100")
#' @param operator (optional) the logical operator to combine filter1 and filter2.
#' @param scale Logical. Whether to apply min-max normalization to the numeric
#'              variable.(default is FALSE).
#' @param print_out Logical. Whether to print the results to the consule. Defaults
#'              to TRUE.
#' @param plot_theme A function that defines the theme for the plots. Defaults to `theme_cleaner`.
#' @param ... Additional arguments passed to the plot_theme.
#'
#' @return Comparisons
#' @export
#'
#' @examples
#' df <- data.frame(
#'   score = rnorm(100, mean = 50, sd = 10),
#'   group = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
#' )
#'
#' compare_grouped_is_it_normal(df, x = score, group_var = group)
#'
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

    # Check if x is provided

    if (missing(x)) {

      cli::cli_h1("Missing Required Argument {.var x}")

      cli::cli_abort(c(
        "The {.var x} argument is required.",
        "i" = "Please provide a value for {.var x}."
      ))
    }

    # Check if group_var is provided
    if (missing(group_var)) {

      cli::cli_h1("Missing Required Argument {.var group_var}")

      cli::cli_abort(c(
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

      x <- rlang::enquo(x)
      group_var <- rlang::enquo(group_var)

      # Evaluate x within the context of df
      group_eval <- df  |>  dplyr::pull(!!group_var)

      if (!is.integer(df[[rlang::as_name(x)]]) && !is.numeric(df[[rlang::as_name(x)]])) {

        cli::cli_h1("Problem with Input")

        cli::cli_abort(
          paste0(
            "Variable '",
            cli::col_green(cli::style_bold(rlang::as_name(x))),
            "' must be of class {.cls numeric} or {.cls integer}."
          ))

      }

      if (!is.function(plot_theme)) {

        cli::cli_h1("Problem with Input")

        cli::cli_abort(
          "You must supply an object of class {.cls function} to {.var plot_theme}.  You supplied an argument of class {.cls {class(plot_theme)}}. Please supply a valid theme function to {.var plot_theme}."
        )

      } else {

        chosen_theme <- as.function(plot_theme)

      }

      cli::cli_h1("Output Summary")

      # Check and mutate group_var if necessary
      if (!is.factor(df[[rlang::as_name(group_var)]]) && !is.character(df[[rlang::as_name(group_var)]])) {

        cli::cli_inform(
          c("*" = "The value supplied to {.var group_var} was of class {.cls {class(group_eval)}} and will be coerced to {.cls factor}.")
        )

        df[[rlang::as_name(group_var)]] <- factor(df[[rlang::as_name(group_var)]], levels = unique(df[[rlang::as_name(group_var)]]))

        # Get unique levels for the grouping variable
        levels_group_var <- na.omit(levels(df[[rlang::as_name(group_var)]]))  # Remove NA values


      } else if(is.character(df[[rlang::as_name(group_var)]])) {

        # Get unique levels for the grouping variable
        levels_group_var <- na.omit(unique(df[[rlang::as_name(group_var)]]))  # Remove NA values

      } else if(is.factor(df[[rlang::as_name(group_var)]])) {

        # Get unique levels for the grouping variable
        levels_group_var <- na.omit(levels(df[[rlang::as_name(group_var)]]))  # Remove NA values

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
        viridis::viridis(num_colors,
                         begin = 0.9,
                         end = 1,
                         option = "turbo")

      color_palette3 <-
        viridis::viridis(num_colors,
                         begin = 0.75,
                         end = 1,
                         option = "plasma")

      color_palette4 <-
        viridis::viridis(num_colors,
                         begin = 0.15,
                         end = 0.4,
                         option = "inferno")

      color_palette5 <-
        viridis::viridis(num_colors,
                         begin = 0.8,
                         end = 1,
                         option = "viridis")

      color_palette6 <-
        viridis::viridis(num_colors,
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
      cli::cli_inform(c(
        "*" = paste0(
          "Exploratory data analysis on the variable '",
          cli::col_green(cli::style_bold(rlang::as_name(x))),
          "' from the '",
          cli::col_magenta(cli::style_bold(data_name)),
          "' dataset."
        )
      ))


      # Optional filters applied pre-calculations
      # Create a filter object only if the 'filter' argument is not NULL

      if (!is.null(filter1) &
          is.null(filter2) & is.null(operator)) {
        filter_text <- paste0(rlang::as_name(x), filter1)

        condition <- rlang::parse_expr(filter_text)

        cli::cli_inform(c("*" = paste0(
          "Filter applied to '", cli::col_green(cli::style_bold(rlang::as_name(x))), "'."
        )))

      } else if (!is.null(filter1) &
                 !is.null(filter2) & !is.null(operator)) {
        filter_text <- paste0(rlang::as_name(x), " ", filter1, operator, rlang::as_name(x), " ", filter2)

        condition <- rlang::parse_expr(filter_text)

        cli::cli_inform(c("*" = paste0(
          "Filter applied to '", cli::col_green(cli::style_bold(rlang::as_name(x))), "'."
        )))

      } else {

        # Handle the case when filter is NULL
        # For example, you can print a message or proceed without filtering

        condition <- TRUE  # No filtering condition

        cli::cli_inform(c(
          "*" = paste0(
            "No filter applied to '",
            cli::col_green(cli::style_bold(rlang::as_name(x))),
            "'. Proceeding with the original dataframe."
          )
        ))

      }

      # Apply the filter condition

      df_filtered <- df |>  dplyr::filter(!!condition)

      # Message calling out total number of observations in the data
      cli::cli_inform(c("*" = paste0("Descriptive statistics given on ",
                                     pretty_number(nrow(df_filtered), n_decimal = 2),
                                     " observations for the variable '",
                                     cli::col_green(cli::style_bold(rlang::as_name(x))),
                                     "' from the supplied dataset.")))

      if (include_plots == FALSE) {

        cli::cli_inform(c("*" = "Halting plot logic."))

      } else {

        cli::cli_inform(c("*" = "Running plot logic."))

      }

      # Perform optional min-max normalization on the provided variable

      if (scale == TRUE) {
        df_filtered <- df_filtered |>
          dplyr::mutate(!!x := normalize(!!x))

        cli::cli_inform(c("*" = paste0("Min-max normalization was performed on '",
                                       cli::col_green(cli::style_bold(rlang::as_name(x))),
                                       "', and will be reflected in the descriptive statistics below."
        )))

      } else {

        cli::cli_inform(c("*" = paste0("Min-max normalization was not performed on '",
                                       cli::col_green(cli::style_bold(rlang::as_name(x))), "'.")))

      }

      # Plotting for each level using ggplot2
      # Loop through each level of the grouping variable
      # Loop through each level of the grouping variable

      for (i in seq_along(levels_group_var)) {
        level <- levels_group_var[i]
        subset_df <-
          df_filtered |>  dplyr::filter(!!group_var == level)

        # conduct the Shapiro Wilks test on all levels of the group_var

        subset_df_clean <- subset_df |>
          dplyr::filter(!is.na(!!x))

        n_subset <- nrow(subset_df_clean)

        if (n_subset < 3) {
          shapiro_p_value <- NA_real_
          normality_diagnosis <- "Error: n < 3"
        } else if (n_subset > 5000) {
          # Sample 5000 records for Shapiro-Wilk test
          sample_df <- subset_df_clean |>
            sample_n(5000, replace = FALSE)
          shapiro_test_result <- shapiro.test(sample_df[[rlang::as_name(x)]])

          # Store p-value and normality diagnosis
          shapiro_p_value <- shapiro_test_result$p.value
          normality_diagnosis <- dplyr::if_else(shapiro_p_value < 0.05, "Non-normal distribution", "Normal distribution")

        } else {
          # Perform Shapiro-Wilk test on the cleaned data
          shapiro_test_result <- shapiro.test(subset_df_clean[[rlang::as_name(x)]])

          # Store p-value and normality diagnosis
          shapiro_p_value <- shapiro_test_result$p.value
          normality_diagnosis <- dplyr::if_else(shapiro_p_value < 0.05, "Non-normal distribution", "Normal distribution")

        }

        # Use summarize to calculate statistics within the context of subset_df
        summary_stats <- subset_df |>
          dplyr::summarize(
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
        result_df <- dplyr::bind_rows(
          result_df,
          tibble::tibble(
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

        cli::cli_h2("Output")

        print(result_df)

      }

      if (include_plots == T) {

        for (i in seq_along(levels_group_var)) {
          level <- levels_group_var[i]
          subset_df <-
            df_filtered |>  dplyr::filter(!!group_var == level)


          # only calculate the missings for a caption if plot logic is activated
          non_missing_n <- subset_df |>
            dplyr::select(dplyr::all_of(rlang::as_name(x))) |>
            na.omit() |>
            dplyr::summarize(n = dplyr::n()) |>
            dplyr::pull(n)

          # Create ggplot objects for each plot type

          # Normal Q-Q Plots
          qqplot <- subset_df |>  ggplot2::ggplot(ggplot2::aes(sample = !!x)) +
            ggplot2::stat_qq(size = 2,
                             na.rm = TRUE,
                             color = color_palette1[i]) +
            ggplot2::geom_qq_line(
              linewidth = 1.25,
              alpha = 0.7,
              na.rm = TRUE,
              color = color_palette2[i]
            ) +
            ggplot2::ggtitle(paste0("Normal Q-Q Plot for ",
                                    rlang::as_name(x),
                                    " at level '",
                                    level, "'")) +
            chosen_theme(...)

          # Histograms
          histogram <- subset_df |>
            ggplot2::ggplot(ggplot2::aes(!!x)) +
            ggplot2::geom_histogram(
              binwidth = (max_val - min_val) / 15,
              color = "black",
              na.rm = TRUE,
              fill = color_palette3[i]
            ) +
            ggplot2::ggtitle(paste0("Histogram of ",
                                    rlang::as_name(x),
                                    " at level '", level, "'")) +
            chosen_theme(...)

          # Kernel density plots
          density <- subset_df |>  ggplot2::ggplot(ggplot2::aes(!!x)) +
            ggplot2::geom_density(
              color = "black",
              alpha = 0.7,
              na.rm = TRUE,
              fill = color_palette4[i]
            ) +
            ggplot2::geom_vline(
              xintercept = median_val,
              color = color_palette5[i],
              linetype = "dashed",
              linewidth = 1.5
            ) +
            ggplot2::ggtitle(
              paste0(
                "Kernel Density Plot of ",
                rlang::as_name(x),
                " at level '",
                level,
                "'\nwith a horizontal line at the median"
              )
            ) +
            ggplot2::labs(caption = paste0("n = ", non_missing_n, " non-missings")) +
            chosen_theme(...)

          # Boxplots
          boxplot <- subset_df |>
            ggplot2::ggplot(ggplot2::aes(!!x)) +
            ggplot2::geom_boxplot(
              color = "black",
              na.rm = TRUE,
              fill = color_palette6[i],
              orientation = "y"
            ) +
            ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
            ggplot2::ggtitle(paste0("Boxplot of ",
                                    rlang::as_name(x), " at level '", level, "'")) +
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
            paste0("Min-max normalization was applied to '", rlang::as_name(x), "'.")

          } else {
            paste0("Min-max normalization was not applied to '", rlang::as_name(x), "'.")

          }

          # combine the plots with patchwork
          combined <- (qqplot | histogram) / (density | boxplot) +
            patchwork::plot_annotation(
              title = paste0(
                "Normality Test of the '",
                data_name,
                "' dataset, regarding variable '",
                rlang::as_name(x),
                "' at level '",
                level,
                "'"
              ),
              subtitle = paste0(
                "Levels of '",
                rlang::as_name(group_var),
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
