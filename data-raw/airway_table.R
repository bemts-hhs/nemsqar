# Create tables for nemsqar test data

# generate_random_ID() creates random sets of 10 upper/lower case letters and
# 10 numbers from 1000000000:9999999999 to generate a random ID number

generate_random_ID <- function(n, set_seed = 12345) {
  # choose whether or not to set the seed

  if (is.numeric(set_seed)) {
    set.seed(set_seed)

  } else if (is.null(set_seed)) {
    message(
      "Set.seed was not called internally, and so your results will not be consistent across applications.  Make [set_seed] any number to set the seed and make reproducible results!"
    )

  }

  random_strings <- vector(mode = "character", length = n)
  for (i in 1:n) {
    random_strings[i] <- paste0(paste0(sample(c(
      LETTERS, letters, LETTERS
    ), size = 10), collapse = ""),
    "-",
    sample(1000000000:9999999999, size = 1))
  }
  return(random_strings)
}

# Function to create unique tibbles
create_sampled_tibble <- function(data, set_seed = 12345) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble")
  }

  local_env <- new.env()

  with(local_env, {
    set.seed(set_seed)

    # Create a named list to store unique values for each column
    unique_values_list <- map(data, ~ unique(.x) |> discard(is.na))

    # Generate intermediate objects in the local environment
    for (col_name in names(unique_values_list)) {
      assign(paste0(col_name, "_col_data"), unique_values_list[[col_name]], envir = environment())
    }

    # Sample 1000 values from each unique values list
    sampled_data <- map_dfc(unique_values_list, ~ sample(.x, size = 1000, replace = TRUE))

    # Rename columns to match the original column names
    names(sampled_data) <- names(data)

    return(sampled_data)


  })

}

# import function

import_table <- function(file_path) {

  src_path <- "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/tables/"

  read_csv(paste0(src_path, file_path))

}

# airway table

airway_data <- import_table("nemsqa_airway_data_Export_2023.csv")

# get the airway tables
airway_interm <- create_sampled_tibble(airway_table)

airway_equal <- create_sampled_tibble(airway_data)

# assign consistent unique IDs
nemsqar_airway_table <- airway_interm |>
  mutate(`Incident Patient Care Report Number - PCR (eRecord.01)` = generate_random_ID(n = nrow(airway_interm)), set_seed = 01282025)

# include the data in the nemsqar package
usethis::use_data(nemsqar_airway_table, overwrite = TRUE)
