rm(list = ls())

require(dplyr)
require(tidyr)
require(data.table)
require(ggplot2)
require(viridis)
require(scales)

'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data 
source_link <- "http://www.stat.columbia.edu/~gelman/arm/examples/speed.dating/Speed%20Dating%20Data.csv"
df <- data.table::fread(input = source_link)

# Data Plot #1 ------------------------------------------------------------
# Data structure: one row per pairing of participant and his/her partners 
# From multiple rows per participant extract unique ratings
# Rating the importance of several characteristics in a potential partner 
# Allocating 100 points to 6 characteristics...
# ... 1) according to the importance as presumed to be felt by peers  
# ... 2) according to the importance as felt by participants themselves
df_what_look_for <- df %>% 
  dplyr::filter(wave %!in% c(6:9)) %>% # here recorded variables diverge
  dplyr::select(# participants unique ID:
    iid, 
    # rating of participant  
    attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1, 
    # assumed rating of peers
    attr4_1, sinc4_1, intel4_1, fun4_1, amb4_1, shar4_1) %>%
  unique() %>% # only keep one row of unique information per participant
  tidyr::drop_na() # remove rows with missing values

# Create data frame with ratings of participant incl. label variable
df_what_look_for_self <- df_what_look_for %>% 
  dplyr::select(iid, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1) %>%
  dplyr::rename(Attractive = attr1_1, 
                Sincere = sinc1_1, 
                Intelligent = intel1_1,
                Fun = fun1_1,
                Ambitious = amb1_1, 
                `Shared Interests` = shar1_1) %>%
  dplyr::mutate_all(as.integer) %>%
  dplyr::mutate(rating = "Own Ratings") %>%
  as.data.frame()

# Create data frame with assumed ratings of peers incl. label variable
df_what_look_for_peers <- df_what_look_for %>% 
  dplyr::select(iid, attr4_1, sinc4_1, 
                intel4_1, fun4_1, amb4_1, shar4_1) %>%
  dplyr::rename(Attractive = attr4_1, 
                Sincere = sinc4_1, 
                Intelligent = intel4_1,
                Fun = fun4_1,
                Ambitious = amb4_1, 
                `Shared Interests` = shar4_1) %>%
  dplyr::mutate_all(as.integer) %>%
  dplyr::mutate(rating = "Presumed Peer Ratings") %>%
  as.data.frame()

# Combine data and transform to long format
df_what_look_for <- bind_rows(df_what_look_for_self,
                              df_what_look_for_peers) %>%
  tidyr::gather(key = "variable", 
                value = "value", 
                Attractive:`Shared Interests`) 

# To be able to track own rating in all later subsets: add info as added variable 
# Create lookup table
df_what_look_for_lookup <- df_what_look_for %>%
  dplyr::filter(rating == "Own Ratings") %>% 
  dplyr::rename(own_rating = value) %>%
  dplyr::select(iid, variable, own_rating)

# Add recurrent information to data frame
df_what_look_for <- left_join(df_what_look_for, 
                              df_what_look_for_lookup,
                              by = c("iid", "variable")) 


# Data Plot #2 ------------------------------------------------------------
# Data table with dates of the speed dating events
df_lookup_date <- data.table::fread("Waves.csv") 

# Only keep the needed variables
df_match <- df %>%
  dplyr::select(wave, # ID of the speed dating event
                match, # did both partners want a second date
                gender, # sex of the participant
                dec) # did the respective participant want a second date

# Join the raw data and the additional info about the dates
df_match <- dplyr::left_join(df_match,
                             df_lookup_date,
                             by = c("wave")) %>%
  dplyr::mutate(date = as.Date(date, format = "%d.%m.%y"), # correct date format
                gender = ifelse(gender == 1, "Women", "Men")) %>% # recode sex variable
  dplyr::rename(`Interested in 2nd Date` = dec) # rename variable

# Create a lookup frame with sum of matches and number of participants per event 
df_lookup_match <- df_match %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(`Interest is Reciprocal` = sum(match, na.rm = TRUE)/2,
                   num_participants = n())

# Calculate the sum of interest in a 2nd date by gender and per event
df_match <- df_match %>%
  dplyr::group_by(date, gender) %>%
  dplyr::summarise(`Interested in 2nd Date` = sum(`Interested in 2nd Date`, na.rm = TRUE))

# Join data and calculate percentages of matches/interest in 2nd date
df_match <- dplyr::left_join(df_match,
                             df_lookup_match,
                             by = c("date")) %>%
  dplyr::mutate(`Interest is Reciprocal` = `Interest is Reciprocal`/num_participants,
                `Interested in 2nd Date` = `Interested in 2nd Date`/num_participants) 

# Partly rename variables and transform the data to long format
df_match1 <-  df_match %>% 
  dplyr::select(date, `Interested in 2nd Date`, gender) %>% # select
  dplyr::rename(count = `Interested in 2nd Date`, # rename
                info = gender) # rename

df_match2 <- df_match %>% 
  dplyr::select(date, `Interest is Reciprocal`) %>% # select
  dplyr::rename(count = `Interest is Reciprocal`) %>% # rename
  dplyr::mutate(info = "Reciprocal") %>% # create identifier variable
  unique() # only use unique rows

# Stack the data sets
df_match <- bind_rows(df_match1, 
                      df_match2)


# Data Plot 3 -------------------------------------------------------------
# Get the average scores as rated by all dating partners 
df_ratings_others <- df %>% 
  dplyr::filter(wave %!in% c(6:9)) %>% # here recorded variables diverge
  dplyr::select(pid, # ID of rated person
                attr, sinc, intel, # ratings
                fun, amb, shar) %>% 
  dplyr::group_by(pid) %>% # get average value for all IDs
  dplyr::summarise_all(mean, na.rm = TRUE) %>%
  dplyr::rename(Attractive = attr, # rename the rating variables
                Sincere = sinc, 
                Intelligent = intel,
                Fun = fun,
                Ambitious = amb, 
                `Shared Interests` = shar,
                iid = pid) %>% # rename ID for join
  dplyr::mutate_all(as.integer) %>%
  as.data.frame() # to get rid of attributes

# from df_what_look_for only get the ratings made by participant... 
df_what_look_for_subset <- df_what_look_for %>%
  dplyr::filter(rating == "Own Ratings")

# ... and join the ratings made about the participant by ID
df_ratings <- dplyr::left_join(df_what_look_for_subset,
                               df_ratings_others,
                               by = c("iid"))