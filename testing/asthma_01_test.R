reprex::reprex({

################################################################################
### Code to test Asthma-01 #####################################################
################################################################################

# load packages

  library(tidyverse)
  library(janitor)
  library(scales)
  library(rlang)

# function

  asthma_01 <- function(df,
                        incident_date_col,
                        patient_DOB_col,
                        eresponse_05_col,
                        esituation_11_col,
                        esituation_12_col,
                        emedications_03_col,
                        ...) {

    # Load necessary packages
    for (pkg in c("tidyverse", "scales", "rlang")) {
      if (!pkg %in% installed.packages())
        install.packages(pkg, quiet = TRUE)
      if (!paste0("package:", pkg) %in% search())
        library(pkg, quietly = TRUE)
    }

    # provide better error messaging if df is missing
    if (missing(df)) {
      cli_abort(
        c(
          "No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
          "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
        )
      )

    }

    # Ensure df is a data frame or tibble
    if (!is.data.frame(df) && !is_tibble(df)) {
      cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
          "i" = "The passed object is of class {.val {class(df)}}."
        )
      )
    }

    # use quasiquotation on the date variables to check format
    incident_date <- enquo(incident_date_col)
    patient_DOB <- enquo(patient_DOB_col)

    if (!is.Date(df[[as_name(incident_date)]]) &
        !is.POSIXct(df[[as_name(incident_date)]]) &
        !is.Date(df[[as_name(patient_DOB)]]) &
        !is.POSIXct(df[[as_name(patient_DOB)]])) {
      cli_abort(
        "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
      )

    }

    if (!exists("pretty_percent")) {
      pretty_percent <- function(variable, n_decimal = 0.1) {
        formatted_percent <- percent(variable, accuracy = n_decimal)

        # If there are trailing zeros after decimal point, remove them
        formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)

        # If it ends with ".%", replace it with "%"
        formatted_percent <- sub("\\.%$", "%", formatted_percent)

        formatted_percent

      }

    }

    # Filter incident data for 911 response codes and the corresponding primary/secondary impressions

    # 911 codes for eresponse.05
    codes_911 <- "2205001|2205003|2205009"

    # get codes as a regex to filter primary/secondary impression fields
    beta_agonist <- "albuterol|ipratropium|duoneb|levalbuterol|metaproterenol"

    # codes for asthma or acute bronchospasm
    asthma_codes <- "J45|J98.01"

    # filter the table to get the initial population ages >= 2 years
    initial_population <- df %>%

      # create the age in years variable

      mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{incident_date_col}},
        time2 = {{patient_DOB_col}},
        units = "days"
      )) / 365) %>%

      # filter down to 911 calls

      dplyr::filter(grepl(
        pattern = codes_911,
        x = {{eresponse_05_col}},
        ignore.case = T
      ),

      # Identify Records that have specified asthma

      if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
        pattern = asthma_codes,
        x = .,
        ignore.case = T
      ))) %>%

      # check to ensure beta agonist was used
      mutate(beta_agonist_check = if_else(grepl(
        pattern = beta_agonist,
        x = {{emedications_03_col}},
        ignore.case = TRUE
      ), 1, 0)) %>%
      filter(patient_age_in_years_col >= 2)

    # Adult and Pediatric Populations

    # filter adult
    adult_pop <- initial_population %>%
      dplyr::filter(patient_age_in_years_col >= 18)

    # filter peds
    peds_pop <- initial_population %>%
      dplyr::filter(patient_age_in_years_col < 18, patient_age_in_years_col >= 2)

    # get the summary of results

    # all
    total_population <- initial_population %>%
      summarize(
        measure = "Asthma-01",
        pop = "All",
        numerator = sum(beta_agonist_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )

    # adults
    adult_population <- adult_pop %>%
      summarize(
        measure = "Asthma-01",
        pop = "Adults",
        numerator = sum(beta_agonist_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )

    # peds
    peds_population <- peds_pop %>%
      summarize(
        measure = "Asthma-01",
        pop = "Peds",
        numerator = sum(beta_agonist_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )

    # summary
    asthma.01 <- bind_rows(adult_population, peds_population, total_population)

    asthma.01


  }

# load data
asthma_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/asthma01_Export.csv") %>%
  clean_names(case = "screaming_snake", sep_out = "_")

# clean
asthma_01_clean <- asthma_01_data %>%
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~  mdy(str_remove_all(., pattern = "\\s12:00:00\\sAM")
                                                                                )
                )
         )

# test the function

asthma_01_clean %>%
  asthma_01(incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
            esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
            emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03
            )

}, venue = "gh", advertise = TRUE)
