### R Function (Testing) ### 

## Set workspace --------------------------------------------------------------

# Set working directory 
setwd("/Users/meredithspalmer/Desktop")
rm(list=ls()); gc()

# Required packages 
library(move2)
library(survival)
library(survminer)
library(ggplot2)   
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(forcats)

# User-defined inputs ---------------------------------------------------------

## Time period of interest
# Input: Start and end dates over which analysis is to be conducted 
# Default: Leave blank (NA), encompasses data from entire study 
# Notes: This and following code assumes move2 object is in UTC (MoveBank standard)
time_period_start    <- as.POSIXct("2001-01-01", tz= "UTC")  
time_period_end      <- NA

## Fix NA times: 
# 1a) Replace NA deploy_off_timestamp with time_period_end
# Input: Yes/No
reset_na_end <- "YES"

# 1b) Replace NA deploy_off_timestamp with current date 
# Input: Yes/No
reset_systime_end <- "YES"

# 2) Remove tracks with no deploy_on_timestamp and/or deploy_off_timestamp 
NEED TO CODE THIS 


# Survival year 
# Input: Month of year as %M (e.g, "06" for June, "11" for November)
# Default: Leave blank (NA), survival year assumed to start in January  
survival_year_start  <- "06"

# Censor capture-related mortality
# Input: Number of days (integer) to exclude from analysis post-capture 
QUESTION: do we want to 1) exclude records where individuals die in this period OR 2) do 1 and start all capture histories this no. days post-capture? Assuming 2, but check 
# Inclusive of capture date 
# Default: 0 
censor_capture_mortality <- 2 

# Group comparisons
# Input: Variable to compare across
# Options: sex, lifestage, reproCond_pregnant, reproCond_offspring
group_comparsion_individual <- "sex"          

# Additional plots & diagnostics 
# Input: Yes/No 

# Combined plots as well as individual plots 
combined_survival_hazard_plot <- "YES"        


# Load data -------------------------------------------------------------------

movebank_store_credentials("RBook", "Obstberg1", force = T)

# Mountain caribou in British Columbia: 216040785
mountain_caribou <- movebank_download_study(study_id = 216040785, 'license-md5'='65f5187abc85645e0101229c87a6d93f') 


# Ya Ha Tinda elk project, Banff National Park: 72264071
yahatinda_elk <- movebank_download_study(study_id = 72264071)

# Ya Ha Tinda elk project, Banff National Park, 2001-2024 (females): 897981076
yahatinda_elk_f <- movebank_download_study(study_id = 897981076, 'license-md5'='e0919bb4de3bfa550f80ca513572d5e3')

# Elk in southwestern Alberta: 933711994
swalberta_elk <- movebank_download_study(study_id = 933711994)

# ABoVE: Boutin Alberta Moose: 302664172
alberta_moose <- movebank_download_study(study_id = 302664172)

# Peters Hebblewhite Alberta-BC Moose: 178994931
albertaBC_moose <- movebank_download_study(study_id = 178994931)


## Review ---------------------------------------------------------------------

# Main data 
data_main <- as.data.frame(data)
head(data_main)

# Track data 
data_tracks_full <- as.data.frame(mt_track_data(data))
head(data_tracks_full)
sort(names(data_tracks_full))
desired_cols <- c("deployment_id", "individual_id", 
                  "tag_id", "attachment_type", 
                  "deploy_on_timestamp", "deploy_off_timestamp", 
                  "sex", "animal_life_stage", "animal_reproductive_condition",
                  "deployment_end_comments", "deployment_end_type",
                  "death_comments", "mortality_location",
                  "timestamp_first_deployed_location",
                  "timestamp_last_deployed_location")
data_tracks <- data_tracks_full |> dplyr::select(any_of(desired_cols))
# Notes: keep checking different animals; should be mortality status, mortality type, birth date information 

# Dates 
min(data_tracks$deploy_on_timestamp, na.rm=T)
min(data_tracks$timestamp_first_deployed_location)
max(data_tracks$deploy_off_timestamp, na.rm=T)
max(data_tracks$timestamp_last_deployed_location)
# -> use deploy_on_timestamp instead of timestamp_first_deployed_location 
# -> use deploy_off_timestamp instead of timestamp_last_deployed location 

# Individual IDs
n_total            <- nrow(data_tracks)
n_distinct_deploy  <- n_distinct(data_tracks$deployment_id)
n_distinct_individ <- n_distinct(data_tracks$individual_id)
cat("Total rows:", n_total, "\n")
cat("Unique deployment_ids:", n_distinct_deploy, "\n")
cat("All unique deployment ids?", n_distinct_deploy == n_total, "\n")
cat("Unique individual_ids:", n_distinct_individ, "\n")
cat("All unique individual ids?", n_distinct_individ == n_total, "\n")


## Clean data -----------------------------------------------------------------

data <- mountain_caribou

## Basic cleaning --- 

# Exclude empty locations: 
data <- dplyr::filter(data, !sf::st_is_empty(data)) 
# Exclude marked outliers: 
data <- mt_filter_unique(data)
# Exclude data marked "test": 
data <- data %>% filter_track_data(is_test == FALSE)


## Aggregate across multiple deployments where present --- 

# Extract event-level data 
events <- data |>
  as_tibble() |>
  dplyr::select(deployment_id, timestamp)  

# Extract relevant track-level attributes
tracks <- mt_track_data(data) |>
  mutate(mortality_location_filled = if_else(
    is.na(mortality_location) | st_is_empty(mortality_location),
    0L, 1L)) |> 
  dplyr::select(any_of(c(
    "deployment_id",
    "individual_id",
    "sex",
    "animal_life_stage",
    "animal_reproductive_condition",
    "attachment_type",
    "study_site",
    "deploy_on_timestamp",
    "deploy_off_timestamp",
    "deployment_end_type",
    "deployment_end_comments",
    "death_comments",
    "mortality_location_filled", 
    "mortality_date",
    "mortality_type", 
    "timestamp_first_deployed_location",
    "timestamp_last_deployed_location")))

# Join track attributes to every event row
events_with_ind <- events |>
  left_join(tracks, by = "deployment_id") |>
  relocate(individual_id,deployment_id, timestamp,
           .before = everything())

# Summarize timestamps and location count per individual
MAKE THIS A "IF EXIST" FOR COLUMNS 

summary_table <- events_with_ind |>
  group_by(individual_id) |>
  summarise(first_timestamp = min(as.Date(timestamp), na.rm = TRUE),
            last_timestamp = max(as.Date(timestamp), na.rm = TRUE),
            n_locations = n(),
            n_deployments = n_distinct(deployment_id),
            timestamp_first_deployed_location = min(timestamp_first_deployed_location,na.rm = TRUE),
            timestamp_last_deployed_location = max(timestamp_last_deployed_location, na.rm = TRUE),
            deploy_on_timestamp = if(all(is.na(deploy_on_timestamp))) {
              as.POSIXct(NA)
            } else {
              min(deploy_on_timestamp, na.rm = TRUE)
            }, 
            deploy_off_timestamp = if(all(is.na(deploy_off_timestamp))) {
              as.POSIXct(NA)
            } else {
              min(deploy_off_timestamp, na.rm = TRUE)
            }, 
            mortality_location_filled = max(mortality_location_filled, na.rm = TRUE), 
            sex = str_c(unique(sex[!is.na(sex)]), collapse = " | "),
            death_comments = str_c(unique(death_comments[!is.na(death_comments)]), collapse = " | "),
            deployment_end_comments = str_c(unique(deployment_end_comments[!is.na(deployment_end_comments)]), collapse = " | "),
            deployment_end_type = str_c(unique(deployment_end_type[!is.na(deployment_end_type)]), collapse = " | "),
            animal_life_stage = str_c(unique(animal_life_stage[!is.na(animal_life_stage)]), collapse = " | "),
            animal_reproductive_condition = str_c(unique(animal_reproductive_condition[!is.na(animal_reproductive_condition)]), collapse = " | "),
            attachment_type = str_c(unique(attachment_type[!is.na(attachment_type)]), collapse = " | "),
            .groups = "drop"
  ) |>
  mutate(
    across(
      c(death_comments, deployment_end_comments, deployment_end_type,
        animal_life_stage, animal_reproductive_condition),
      ~ if_else(. == "", NA_character_, .)
    ), 
    deploy_on_timestamp  = as.Date(deploy_on_timestamp),
    deploy_off_timestamp = as.Date(deploy_off_timestamp)
  )


## Clean dates --- 

## Replace "deploy_on_timestamp" with "first_timestamp" 
summary_table <- summary_table %>% 
  mutate(missing_timestamp_start = is.na(deploy_on_timestamp))
n_missing <- sum(is.na(summary_table$deploy_on_timestamp), na.rm = TRUE)

# (diagnostic - remove from final code)
bad_tracks <- summary_table %>%
  filter(missing_timestamp_start) %>%
  dplyr::select(individual_id,
         first_timestamp, 
         last_timestamp,
         deploy_on_timestamp,
         deploy_off_timestamp,
         missing_timestamp_start)
nrow(bad_tracks)
print(bad_tracks)

summary_table <- summary_table %>%
  mutate(deploy_on_timestamp = if_else(
    is.na(deploy_on_timestamp),
    as.Date(first_timestamp),
    deploy_on_timestamp)) %>% 
  dplyr::select(-missing_timestamp_start)

# Warning for data update 
if (n_missing > 0) {
  warning(
    sprintf("Warning: Replaced %d missing deploy_on_timestamp value%s with first_timestamp.", n_missing,
            if (n_missing == 1) "" else "s"), call. = FALSE, immediate. = TRUE)
}


## Replace "deploy_off_timestamp" with "mortality_date" 
WHEN HAVE AN EXAMPLE OF THIS, CODE UP 


## USER-DEFINED: Replace NA "deploy_off_timestamp" with "last_timestamp" 
# Input: Yes/No 
reset_na_end <- "YES"

if(reset_na_end == "YES"){

    summary_table <- summary_table %>%
      mutate(missing_timestamp_end = is.na(deploy_off_timestamp))
    n_missing <- sum(is.na(summary_table$deploy_off_timestamp), na.rm = TRUE)
    
    # (diagnostic - remove from final code)
    bad_tracks <- summary_table %>%
      filter(missing_timestamp_end) %>%
      dplyr::select(individual_id,
                    first_timestamp, 
                    last_timestamp,
                    deploy_on_timestamp,
                    deploy_off_timestamp,
                    missing_timestamp_end)
    nrow(bad_tracks)
    print(bad_tracks)
  
    summary_table <- summary_table %>%
      mutate(deploy_off_timestamp = if_else(
        is.na(deploy_off_timestamp),
        as.Date(last_timestamp),
        deploy_off_timestamp))%>% 
      dplyr::select(-missing_timestamp_end)
    
      # Warning for data update 
      if (n_missing > 0) {
        warning(
          sprintf("Warning: Replaced %d missing deploy_off_timestamp value%s with last_timestamp.", n_missing,
                  if (n_missing == 1) "" else "s"), call. = FALSE, immediate. = TRUE)
      }
}


## USER-DEFINED: Replace NA deploy_off_timestamp with current date 
# Input: Yes/No
reset_systime_end <- "NO" 

if(reset_systime_end == "YES"){
  
  summary_table <- summary_table %>%
    mutate(missing_timestamp_end = is.na(deploy_off_timestamp))
  n_missing <- sum(is.na(summary_table$deploy_off_timestamp), na.rm = TRUE)
  
  # (diagnostic - remove from final code)
  bad_tracks <- summary_table %>%
    filter(missing_timestamp_end) %>%
    dplyr::select(individual_id,
                  first_timestamp, 
                  last_timestamp,
                  deploy_on_timestamp,
                  deploy_off_timestamp,
                  missing_timestamp_end)
  nrow(bad_tracks)
  print(bad_tracks)
  
  summary_table <- summary_table %>%
    mutate(deploy_off_timestamp = if_else(
      is.na(deploy_off_timestamp),
      Sys.Date(), 
      deploy_off_timestamp))%>% 
    dplyr::select(-missing_timestamp_end)
  
  if (n_missing > 0) {
    warning(
      sprintf("Warning: Replaced %d missing deploy_off_timestamp value%s with current date.", n_missing,
              if (n_missing == 1) "" else "s"), call. = FALSE, immediate. = TRUE)
  }
}


## Remove data for individuals where "deploy_off_timestamp" occurs before "deploy_on_timestamp" 
n_original <- nrow(summary_table) 
summary_table <- summary_table %>%
  filter(deploy_off_timestamp >= deploy_on_timestamp)

# Warning for data loss 
n_removed <- n_original - nrow(summary_table)  
if (n_removed > 0) {
  warning(sprintf("Warning: Removed %d individual%s where deploy_off_timestamp < deploy_on_timestamp.",
                  n_removed, if (n_removed == 1) "" else "s"),
    call. = FALSE, immediate. = TRUE)
}


## Crop data ---

## USER-DEFINED: Removed censored data (mortalities within set period of capture)
n_before <- nrow(summary_table)

summary_table <- summary_table %>%
  
  # Save raw data 
  mutate(raw_deploy_on_timestamp = deploy_on_timestamp) %>%
  
  # Calculate end of censoring window
  mutate(censor_cutoff = deploy_on_timestamp + lubridate::days(censor_capture_mortality)) %>%
  
  # Remove records where end date is within this window 
  mutate(remove_due_to_early_end = !is.na(deploy_off_timestamp) & deploy_off_timestamp <= censor_cutoff) %>%
  filter(!remove_due_to_early_end) %>%
  
  # Shift deploy_on forward to after censor period for kept records 
  mutate(deploy_on_timestamp = censor_cutoff) %>%
  
  # Clean 
  select(-censor_cutoff, -remove_due_to_early_end)

n_after  <- nrow(summary_table)
n_removed <- n_before - n_after

if (n_removed > 0) {
  warning(
    paste0("Warning: Removed ", n_removed, " individual(s) because deploy_off_timestamp occurred within ",
           censor_capture_mortality, " day(s) after deploy_on_timestamp"),
    call. = FALSE, immediate. = TRUE)
  } 
  

## USER DEFINED: Crop to study period (update based on user-defined inputs): 

# Define window 
effective_start <- if (is.na(time_period_start)) {
  min(summary_table$deploy_on_timestamp, na.rm = TRUE)
} else {
  time_period_start
}

effective_end <- if (is.na(time_period_end)) {
  max(summary_table$deploy_off_timestamp, na.rm = TRUE)
} else {
  time_period_end
}

# Crop to window 
n_original <- nrow(summary_table) 
summary_table <- summary_table %>%
  
  # Retain raw data 
  mutate(raw_deploy_off_timestamp = deploy_off_timestamp) %>%    # "on" already saved 
  
  # Determine if the deployment overlaps study window 
  mutate(overlaps_study = deploy_on_timestamp <= effective_end & 
           deploy_off_timestamp  >= effective_start) %>%
  filter(overlaps_study | is.na(overlaps_study)) %>%    
  
  # Crop to window 
  mutate(first_timestamp = pmax(deploy_on_timestamp, effective_start, na.rm = TRUE),
         last_timestamp  = pmin(deploy_off_timestamp, effective_end,   na.rm = TRUE)) %>%
  
  # Clean 
  select(-overlaps_study) 

# Warning for data loss 
n_removed <- n_original - nrow(summary_table)
if (n_removed > 0) {
  warning(sprintf("Warning: %d record%s did not overlap the user-defined study window and were removed.",
                  n_removed, if (n_removed == 1) "" else "s"),
          call. = FALSE, immediate. = TRUE)
}

# (diagnostic - remove from final code)
summary_table %>%
  filter(raw_deploy_on_timestamp != raw_deploy_on_timestamp | raw_deploy_off_timestamp  != last_timestamp) %>%
  dplyr::select(individual_id,
                raw_deploy_on_timestamp, first_timestamp,
                raw_deploy_off_timestamp, last_timestamp) %>%
  slice_head(n = 10) %>%
  print()


## Calculate entry time and exit time ---
# (for staggered entry; instead of duration days)
origin_date <- if_else(is.na(time_period_start),
                       min(summary_table$deploy_on_timestamp, na.rm = TRUE),
                       time_period_start)

summary_table <- summary_table %>%
  mutate(origin_date = origin_date, 
         entry_time_days  = as.numeric(difftime(deploy_on_timestamp, origin_date, units = "days")),
         exit_time_days   = as.numeric(difftime(deploy_off_timestamp, origin_date, units = "days")))


## (diagnostic - remove from final code) Quick clean 
summary_table <- summary_table %>%
  dplyr::select(-c(timestamp_first_deployed_location, timestamp_last_deployed_location,
                   origin_date)) %>%
  relocate(individual_id, deploy_on_timestamp, deploy_off_timestamp, first_timestamp,
           last_timestamp, raw_deploy_on_timestamp, raw_deploy_off_timestamp, 
           .before = n_locations) %>%
  relocate(entry_time_days, exit_time_days, .before = n_locations)


## Survival event indicator  -- 
# Here, event = 1 if end_type indicates observed death, 0 if censore

## (diagnostic - remove from final code) death variables
names(summary_table)
unique(summary_table$mortality_event) #NEED AN EXAMPLE OF THIS; THEN ADD TO CODE 
unique(summary_table$mortality_type) #NEED AN EXAMPLE OF THIS; THEN ADD TO CODE 
# - note: this should be hardcoded: 
# -> if mortality_type is %in%  c("bycatch", "capture", "electrocution", "harvest", "disease", "natural-death", "other", "parasites", "poison", "predation", "starvation", "unknown", "vehicle-collision")
unique(summary_table$mortality_date) #NEED AN EXAMPLE OF THIS; THEN ADD TO CODE 
unique(data_tracks$deployment_end_comments) #IF ANYTHING USEFUL POPS, ADD TO CODE 
unique(summary_table$death_comments)
unique(summary_table$mortality_location_filled)
unique(summary_table$deployment_end_type)
levels(summary_table$deployment_end_type)
# -> look at other variables jasmine has flagged 


summary_table <- summary_table %>%
  
  # Initialize mortality event 
  mutate(mortality_event = NA_real_) %>%
  
  # Identify survivors (individuals who last beyond study)
  mutate(
    survived_beyond_study = !is.na(raw_deploy_off_timestamp) &
      raw_deploy_off_timestamp > as.Date(effective_end),
    
    mortality_event = if_else(survived_beyond_study, 0L, mortality_event),
    
    # Update columns to remove ambiguity (e.g., if animal dies after study window)
    death_comments = if ("death_comments" %in% names(.)) {
      if_else(survived_beyond_study, "survived beyond study", death_comments)
    } else death_comments,
    
    deployment_end_comments = if ("deployment_end_comments" %in% names(.)) {
      if_else(survived_beyond_study, "survived beyond study", deployment_end_comments)
    } else deployment_end_comments,
    
    deployment_end_type = if ("deployment_end_type" %in% names(.)) {
      if_else(survived_beyond_study, "survived beyond study", deployment_end_type)
    } else deployment_end_type,
    
    mortality_location_filled = if ("mortality_location_filled" %in% names(.)) {
      if_else(survived_beyond_study, 0L, mortality_location_filled)
    } else mortality_location_filled
  ) %>%
  
  # Search for mortality indicators 
  # A. death_comments keywords
  mutate(
    mortality_event = case_when(
      "death_comments" %in% names(.) &
        str_detect(tolower(death_comments),
           "dead|death|cod|predation|predator|vehicle|collision|killed|poach|shot|hunt|harvest") ~ 1L,
      mortality_event == 1L ~ 1L,
      TRUE ~ mortality_event
    )
  ) %>%
  
  # B. mortality_location_filled
  mutate(
    mortality_event = case_when(
      "mortality_location_filled" %in% names(.) &
        mortality_location_filled >= 1 ~ 1L,
      
      mortality_event == 1L ~ 1L,
      TRUE ~ mortality_event
    )
  ) %>%
  
  # C. deployment_end_type  
  mutate(
    mortality_event = case_when(
      mortality_event == 1L ~ 1L,
      
      # Mortality indication
      "deployment_end_type" %in% names(.) &
        str_detect(tolower(deployment_end_type), "\\bdead\\b|\\bdeath\\b") ~ 1L,
      
      # Censoring indication
      "deployment_end_type" %in% names(.) &
        tolower(deployment_end_type) %in% c("removal", "other", "unknown", "survived beyond study") ~ 0L,
      
      # Missing column OR NA value → censored 
      (!"deployment_end_type" %in% names(.) | is.na(deployment_end_type)) &
        is.na(mortality_event) ~ 0L,
      
      TRUE ~ mortality_event
    )
  ) %>%
  
  # Final censoring: remaining NA → 0 
  mutate(
    mortality_event = if_else(
      is.na(mortality_event) & !is.na(deploy_off_timestamp),
      0L,
      mortality_event
    )
  ) %>%
  
  # Clean up & relocate
  select(-survived_beyond_study) %>%
  relocate(mortality_event, .after = deployment_end_type)


## Flag sample size issues 

# No deaths 
n_mort_events <- sum(summary_table$mortality_event == 1, na.rm = TRUE)
if (n_mort_events == 0) {
  warning("Cannot run survival analysis: no mortality events detected.",
    call. = FALSE, immediate. = TRUE)
}

# Small proportion of deaths 
# Note: number of events more important than number of individuals for statistical power 
if (n_mort_events <= 10) {
  warning(sprintf("Few (%d) deaths detected. Model may have low statistical power, potentially resulting in unreliable estimates and poor predictive power.", n_mort_events),
          call. = FALSE, immediate. = TRUE)
}


## USER-DEFINED: Attributes --- 
# User can pick ONE ATTRIBUTE by which to segregate their data 

## Sex: m, f, u 
# (diagnostic - remove from final code) 
unique(summary_table$sex); table(summary_table$sex)
# -> if user selections this option, remove NA rows 
QUESTION: can you do three groups or need to remove u as well? 
  
if(group_comparison_individual == "sex"){
  n_original <- nrow(summary_table)
  summary_table <- summary_table[!is.na(summary_table$sex),] 
  n_lost <- n_original - nrow(summary_table)
  if (n_lost > 0) {
    warning(sprintf("%d individuals with NA sex removed from study.", n_lost), 
            call. = FALSE, immediate. = TRUE)
    }
}
# need to check this works on real data 


## Life stage: can be more >2 options 
# (diagnostic - remove from final code) 
unique(summary_table$animal_life_stage); table(summary_table$animal_life_stage)
# -> IF USER SELECTS THIS ATTRIBUTE: clean data, remove NA rows
if(group_comparison_individual == "lifestage"){
  n_original <- nrow(summary_table)
  summary_table <- summary_table %>%
    filter(!is.na(animal_life_stage)) %>%
    mutate(
      animal_life_stage = str_trim(animal_life_stage),
      animal_life_stage = str_replace_all(animal_life_stage, "\\s+", ""),
      animal_life_stage = str_extract(animal_life_stage, "^[^|]+"),
      animal_life_stage = str_replace(animal_life_stage, "–", "-") 
    )
  n_life_stages <- length(unique(summary_table$animal_life_stage))
  warning(sprintf("%d life-stages detected.", n_life_stages), call. = FALSE, immediate. = TRUE)
  n_lost <- n_original - nrow(summary_table)
  if (n_lost > 0) {
    warning(sprintf("%d individuals with NA life stage removed from study.", n_lost_ind), 
            call. = FALSE, immediate. = TRUE)
  }
# need to check this works on real data 
  
  
## Attachment type: 15 options
# (diagnostic - remove from final code) 
unique(summary_table$attachment_type); table(summary_table$attachment_type)
# -> IS USER SELECTIONS THIS OPTION: remove NAs, take first attachment type if muliple 

CODE THIS 
  
  
## Reproductive condition: can be > 2 options
# (diagnostic - remove from final code) 
unique(summary_table$animal_reproductive_condition); table(summary_table$animal_reproductive_condition)

# -> IF USER SELECTS THIS ATTRIBUTE, clean data, remove NA rows
DO THIS ON REAL DATA 
if(group_comparison_individual = "reproCond_pregnant"){
  # Remove males & unknowns 
  data <- filter_track_data(data, !is.na(sex) & sex != "m") 
  
  # Divide females into pregnant at capture vs. not 
  data <- data |>
    mutate_track_data(
      animal_reproductive_condition = if_else(
        is.na(animal_reproductive_condition) | animal_reproductive_condition == "",
        "not pregnant",
        animal_reproductive_condition
      )
    )
  LOG: what life stages are found 
  # NOTE: THERE MAY BE DIFFERENT REPRODUCTIVE CONDITIONS LISTED! TEST ON DIF SPECIES 
  
  # Redo sample size 
  n.ind <- length(unique(mt_track_data(data)$individual_id))
  LOG WARNING: sample size is now smaller; n.ind == XXX 
}

# -> IF USER SELECTS THIS ATTRIBUTE, clean data, remove NA rows
if(group_comparison_individual = "reproCond_offspring"){
  
  # Remove males & unknowns 
  n_original <- nrow(summary_table)
  summary_table <- summary_table[!(is.na(summary_table$sex) | summary_table$sex == "m"),]
    filter_track_data(data, !is.na(sex) & sex != "m") 
  n_lost <- n_original - nrow(summary_table)
  if (n_lost > 0) {
    warning(sprintf("%d individuals with NA sex and/or males removed from study.", n_lost_ind), 
            call. = FALSE, immediate. = TRUE)
  }
  
  # Divide females into with calf and without/unknown ... or keep original designations (but may conflate with prgegnancy...)
  unique(summary_table$animal_reproductive_condition)
    # moose: "with calf: N"   "with calf: Unk" "with calf: Y"  
  data <- data |>
    mutate_track_data(
      animal_reproductive_condition = if_else(
        is.na(animal_reproductive_condition) | animal_reproductive_condition == "",
        "not pregnant",
        animal_reproductive_condition
      )
    )
  LOG: what life stages are found 
}

# (for internal review) 
str(data)
summary(data)
names(mt_track_data(data))
summary(mt_track_data(data))


# Basic summary ---------------------------------------------------------------

# Plot length of each individual's tracking: 
# Create a mapping from deployment_id → individual_id (one-to-one)
deployment_to_ind <- mt_track_data(data) |>
  select(deployment_id, individual_id) |>
  distinct()

# Now add individual_id to every location row via join
data_with_ind <- data |>
  left_join(deployment_to_ind, by = "deployment_id")

# Summarise per individual using the actual timestamps
track_times <- data_with_ind |>
  group_by(individual_id) |>
  summarise(
    start    = min(timestamp, na.rm = TRUE),   # or mt_time(.) if you prefer the accessor
    end      = max(timestamp, na.rm = TRUE),
    n_locs   = n(),
    n_deploy = n_distinct(deployment_id),
    .groups  = "drop"
  ) |>
  # Join back full individual/track metadata (sex, taxon, etc.)
  left_join(
    mt_track_data(data) |>
      distinct(individual_id, .keep_all = TRUE),  # keep one row per individual
    by = "individual_id"
  ) |>
  mutate(
    duration_days = round(as.numeric(difftime(end, start, units = "days")), 1),
    # Ordered factor for chronological y-axis
    track_label = fct_reorder(as.character(individual_id), start)
    # If you prefer the local name (often more readable):
    # track_label = fct_reorder(as.character(individual_local_identifier), start)
  ) |>
  arrange(start)

# Quick sanity checks
print(nrow(track_times))                # number of unique individuals
print(summary(track_times$duration_days))
print(head(track_times[, c("individual_id", "start", "end", "duration_days", "n_locs", "n_deploy")]))

# Plot 
ggplot(track_times) +
  geom_segment(
    aes(x = start, xend = end, y = track_label, yend = track_label),
    linewidth = 3, color = "steelblue"
  ) +
  geom_point(aes(x = start, y = track_label), color = "darkgreen", size = 3.5) +
  geom_point(aes(x = end,   y = track_label), color = "firebrick",  size = 3.5) +
  labs(
    title    = "Tracking History per Individual",
    subtitle = sprintf("%d unique individuals • %d total deployments • %d locations",
                       nrow(track_times),
                       sum(track_times$n_deploy, na.rm = TRUE),
                       sum(track_times$n_locs, na.rm = TRUE)),
    x        = "Time",
    y        = "Individual ID"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y"
  )

SAVE THIS PLOT 


# Summarize -------------------------------------------------------------------

# Kaplan-Meier with staggered entry
km_fit <- survfit(Surv(entry_time, exit_time, event) ~ 1, data = data)
plot(km_fit)

# Cox Model with staggered entry
cox_fit <- coxph(Surv(entry_time, exit_time, event) ~ treatment, data = data)
summary(cox_fit)


# Is few n_locations sketchy? 
View(summary_table[summary_table$n_locations == 1,])
# -> nothing to suggest died; 0 day duration
# -> note that timestamp_last_deployed_location, timestamp_first_deployed_location very different from first_ and last_timestamps 
# -> remove? 

# Did mortality location filled work? 
unique(summary_table$mortality_location_filled)
# -> revisit original data 



also check if mortality_location filled
other checks? mortality_date not included? 
  
  
  # Handle potential NAs or inconsistencies
  # - For example, if life_stage varies, this uses first; inspect summary_table for issues
  # - If many NAs, consider filtering or imputing downstream
  
  # View or save the table
  View(summary_table)
table(summary_table$survival_event)


## Survival Analysis ----------------------------------------------------------

## Univariate -----------------------------------------------------------------

# Kaplan-Meier with staggered entry
km_fit <- survfit(Surv(entry_time, exit_time, event) ~ 1, data = data)
plot(km_fit)

# Cox Model with staggered entry
cox_fit <- coxph(Surv(entry_time, exit_time, event) ~ treatment, data = data)
summary(cox_fit)

# Fit survival object 
surv_obj <- Surv(time = summary_table$duration_days, event = summary_table$survival_event)
km_fit <- survfit(surv_obj ~ 1)  

# Life table 
times <- round(seq(min(summary_table$duration_days), max(summary_table$duration_days), 
                   length.out = 10))
s <- summary(km_fit, times = times)
life_table <- data.frame(time       = s$time,
                         n_risk     = s$n.risk,
                         n_event    = s$n.event,
                         survival   = s$surv,
                         std_err    = s$std.err,
                         lower_95   = s$lower,
                         upper_95   = s$upper)

# -> Write lifetable to CSV artefact 
write.csv(life_table, file = "survival_statistics.csv", row.names = FALSE)


# KM Survival Curve 
km_summary <- summary(km_fit)

km_df <- data.frame(time    = km_summary$time,
                    surv    = km_summary$surv,    
                    n_risk  = km_summary$n.risk,   
                    n_event = km_summary$n.event,  
                    lower   = km_summary$lower,   
                    upper   = km_summary$upper,  
                    std_err = km_summary$std.err)

n.ind <- nrow(summary_table)
n.events <- nrow(summary_table[summary_table$survival_event == 1,])
n.days <- as.numeric(summary(km_fit)$table["median"])

km_curve <- ggplot(km_df, aes(x = time, y = surv)) +
  
  # Confidence intervals
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#AED6F1", alpha = 0.4) +
  
  # Step line 
  geom_step(linewidth = 1.2, color = "#21618C") +
  
  # Scale
  scale_x_continuous(breaks = seq(0, max(km_df$time, na.rm = TRUE), by = 200),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     expand = c(0, 0)) +
  
  # Labels 
  labs(title    = "Kaplan-Meier Survival Curve",
       subtitle = paste0("N = ", n.ind, ", Events = ", n.events, ", Median Survival =", 
                         n.days),
       x        = "Time (days)",
       y        = "Survival Probability",
       caption  = "95% Confidence Interval Shaded") +
  
  # Theme
  theme_classic(base_size = 14) +
  theme(
    plot.title   = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(color = "black"),
    panel.grid.major.y = element_line(color = "gray90"), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin  = margin(10, 10, 10, 10)
  )

# -> save plot attribute 
DO THIS 

ADD MEDIAN SURVIVAL 
ADD RISK TABLE, CENSOR TABLE 

# Kaplan-Meier Survival Curve
(p_surv <- ggsurvplot(km_fit,
                      data = summary_table,
                      title = "Kaplan-Meier Survival Curve",
                      xlab = "Time (days)",
                      ylab = "Survival Probability",
                      palette = "#2c7bb6", 
                      size = 1.2,
                      censor.shape = "|",
                      censor.size = 3,
                      conf.int = TRUE,
                      risk.table = TRUE,
                      legend = "none",
                      ggtheme = theme_minimal(base_size = 13) +
                        theme(
                          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                          axis.title = element_text(face = "bold"))))

# -> save plot attribute 
DO THIS 

# Cumulative hazard plot 
(p_cuminc <- ggsurvplot(km_fit,
                        data = summary_table,
                        fun = "event",       # 1 - survival = cumulative incidence/mortality
                        title = "Cumulative Mortality",
                        xlab = "Time (days)",
                        ylab = "Cumulative Incidence",
                        palette = "#d7191c",
                        size = 1.1,
                        censor = FALSE,
                        conf.int = TRUE,
                        legend = "none", 
                        ggtheme = theme_minimal(base_size = 13) +
                          theme(plot.title = element_text(hjust = 0.5, face = "bold", 
                                                          size = 14),
                                axis.title = element_text(face = "bold"))))

# Combine with shared legend & title
p_surv_plot   <- p_surv$plot
p_cuminc_plot <- p_cuminc$plot
library(patchwork) #if decide to keep this method 

(p_surv_plot + p_cuminc_plot) +
  plot_layout(ncol = 1) +
  plot_annotation(title = "Overall Survival and Cumulative Mortality",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold", size = 16))) &
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 11))

# -> save plot attribute 
DO THIS 


# Cumulative Hazard Plot 
na_fit <- survfit(surv_obj ~ 1, type = "fh")   
s <- summary(na_fit)

cumhaz_df <- data.frame(
  time   = s$time,
  cumhaz = s$cumhaz,
  lower  = pmax(0, s$cumhaz * exp(-1.96 * s$std.chaz / s$cumhaz)),  
  upper  = s$cumhaz * exp(+1.96 * s$std.chaz / s$cumhaz)
)

ggplot(cumhaz_df, aes(x = time, y = cumhaz)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#FAD7A8", alpha = 0.35) +
  geom_step(linewidth = 1.3, color = "#D68910") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Cumulative Hazard Function",
    subtitle = paste("n =", na_fit$n, "  Events =", sum(na_fit$n.event)),
    x        = "Time (days)",
    y        = "Cumulative Hazard H(t)",
    caption  = "95% pointwise CI (log-normal approximation) | Fleming–Harrington estimator"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    axis.title    = element_text(face = "bold"),
    plot.caption  = element_text(color = "gray50", size = 9)
  )

# Save plot
DO THIS!! 
  
  
  ## Comparison -----------------------------------------------------------------

if group_comparsion_individual (== TRUE), 
SOFT CODE IN VARIABLE 

# Fit survival object 
km_fit_comp <- survfit(surv_obj ~ sex, data = summary_table)

# Log-Rank test 
survdiff(Surv(duration_days, survival_event) ~ sex, data=summary_table)

# -> generate a nice table that flags significance 


# Comparison survival plot curve 

ggsurvplot(
  km_fit_comp,  
  data = summary_table,  
  surv.median.line = "hv",  
  
  pval = TRUE,                         # can we change size of p-value? 
  pval.coord = c(1800,.91),            # also want to softcode this ... 
  
  conf.int = TRUE,          
  conf.int.style = "step",    #see if we can change this 
  
  xlab = "Time (days)",    
  ylab = "Survival Probability",
  break.time.by = 200,      
  ggtheme = theme_light(),  
  
  risk.table = "abs_pct",   
  risk.table.y.text.col = T, 
  risk.table.y.text = FALSE, 
  
  ncensor.plot = TRUE,       
  legend.title = "Gender",              # change legend labels - MAKE DYNAMIC
  legend.labs = c("Female", "Male"),    # change legend labels - MAKE DYNAMIC 
  font.legend = 10, 
  palette = c("#E7B800", "#2E9FDF"))

# Return the original data unchanged
return(data)
}


# This about whether this could be included: 

# Nice summary for >1 fit variables -- find good case study 
summary_table %>% 
  tabyl(sex, age_cat_small, show_na = F) %>% 
  adorn_totals(where = "both") %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front")

gender         0-4          5-19           20+          Total
f 482 (22.4%) 1,184 (54.9%)   490 (22.7%) 2,156 (100.0%)
m 325 (15.0%)   880 (40.6%)   960 (44.3%) 2,165 (100.0%)
Total 807 (18.7%) 2,064 (47.8%) 1,450 (33.6%) 4,321 (100.0%)

# This prototype provides a basic MoveApps R function app for survival analysis. It assumes the input is a move2::move2_loc object, which is standard in MoveApps workflows for location data from Movebank.