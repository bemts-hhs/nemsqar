# load data

respiratory_02_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/respiratory02_Export.csv")

# get the initial population - first filter pass
# create a Unique_ID to get a better estimate of N
respiratory_02_prep <- respiratory_02_data  %>%
  mutate(`Patient Date Of Birth (ePatient.17)` = str_remove_all(`Patient Date Of Birth (ePatient.17)`, pattern = "\\s12:00:00\\sAM"),
         across(
    c(
      `Incident Patient Care Report Number - PCR (eRecord.01)`,
      `Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)`,
      `Patient Date Of Birth (ePatient.17)`,
      `Patient Gender (ePatient.13)`,
      `Patient Home Postal Code (ePatient.09)`
    ),
    ~ if_else(is.na(.), "missing", .)
  )) %>% 
  mutate(Unique_ID = str_c(
      `Incident Patient Care Report Number - PCR (eRecord.01)`,
      `Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)`,
      `Patient Date Of Birth (ePatient.17)`,
      `Patient Gender (ePatient.13)`,
      `Patient Home Postal Code (ePatient.09)`,
    sep = "-"
  ),
  .after = `Incident Patient Care Report Number - PCR (eRecord.01)`,
  .by = c(
    `Incident Patient Care Report Number - PCR (eRecord.01)`,
    `Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)`,
    `Patient Date Of Birth (ePatient.17)`,
    `Patient Gender (ePatient.13)`,
    `Patient Home Postal Code (ePatient.09)`)
  )

# return values assigned as "missing" to NA
# final filter and take the distinct values in the table using the Unique_ID
respiratory_02_init <- respiratory_02_prep %>% 
  filter(
    grepl(
      pattern = "2205001|2205003|2205009",
      x = `Response Type Of Service Requested With Code (eResponse.05)`,
      ignore.case = T
    ),
    `Vitals Pulse Oximetry (eVitals.12)` < 90
  ) %>%
  mutate(
    `Vitals Pulse Oximetry (eVitals.12)` = str_c(`Vitals Pulse Oximetry (eVitals.12)`, collapse = ", "),
    .by = Unique_ID
  ) %>% 
  distinct(Unique_ID, .keep_all = T) %>% # this will ensure each row is an observation, and each column is a feature
  replace_with_na_at(.vars = c(
    "Incident Patient Care Report Number - PCR (eRecord.01)",
    "Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)",
    "Patient Date Of Birth (ePatient.17)",
    "Patient Gender (ePatient.13)",
    "Patient Home Postal Code (ePatient.09)"
  ),
  condition = ~ .x == "missing"
  )

# get population 1 for respiratory-02, peds
respiratory_02_peds <- respiratory_02_init %>% 
  filter(`Patient Age In Years (ePatient.15)` < 18,
         `Patient Age In Days (ePatient.15)` >= 24 
         )

# get population 2 for respiratory-02, adults

respiratory_02_adults <- respiratory_02_init %>% 
  filter(`Patient Age In Years (ePatient.15)` >= 18)

# calculations for peds

peds_calculation <- respiratory_02_peds %>% 
  summarize(pop = "Peds",
            numerator = sum(
              grepl(pattern = "7806", x = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`) | 
                grepl(pattern = "57485005", x = `Patient Attempted Procedure Codes List (eProcedures.03)`), na.rm = T
            ),
            denominator = n(),
            prop = numerator / denominator,
            prop_label = pretty_percent(prop, n_decimal = 0.01)
            )

# calculations for adults

adults_calculation <- respiratory_02_adults %>% 
  summarize(pop = "Adults",
            numerator = sum(
              grepl(pattern = "7806", x = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`) | 
                grepl(pattern = "57485005", x = `Patient Attempted Procedure Codes List (eProcedures.03)`), na.rm = T
            ),
            denominator = n(),
            prop = numerator / denominator,
            prop_label = pretty_percent(prop, n_decimal = 0.01)
            )

# overall calculation

total_population <- respiratory_02_init %>% 
  summarize(pop = "All",
            numerator = sum(
              grepl(pattern = "7806", x = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`) | 
                grepl(pattern = "57485005", x = `Patient Attempted Procedure Codes List (eProcedures.03)`), na.rm = T
            ),
            denominator = n(),
            prop = numerator / denominator,
            prop_label = pretty_percent(prop, n_decimal = 0.01)
            )

# bind rows of calculations for final table

resp_02 <- bind_rows(total_population, peds_calculation, adults_calculation)

resp_02
