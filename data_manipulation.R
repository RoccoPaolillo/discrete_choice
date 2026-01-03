library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

setwd("C:/Users/LENOVO/Documents/GitHub/discrete_choice/")

df <- read.csv("Brent_RP.csv",sep=",")
df <- df %>% select(ID,ses,ethnic,choice,frac_white,frac_black,frac_asian,frac_high,frac_mid,frac_low)
names(df)[names(df) == "choice"] <- "district_id"

df <- df %>% 
  mutate(
    ses = case_when(
      ses == "low"  ~ 1L,
      ses == "middle"  ~ 2L,
      ses == "high" ~ 3L,
      TRUE ~ NA_integer_
    ),
    ethnic = case_when(
      ethnic == "white" ~ 1L,
      ethnic == "black" ~ 2L,
      ethnic == "asian" ~ 3L,
      TRUE ~ NA_integer_
    )
  )

# check if not distinct characteristics neighborhood
check <- df %>%
  group_by(district_id) %>%
  summarise(
    n_frac_white = n_distinct(frac_white, na.rm = TRUE),
    n_frac_black = n_distinct(frac_black, na.rm = TRUE),
    n_frac_asian = n_distinct(frac_asian, na.rm = TRUE),
    n_frac_high = n_distinct(frac_high, na.rm = TRUE),
    n_frac_mid = n_distinct(frac_mid, na.rm = TRUE),
    n_frac_low = n_distinct(frac_low, na.rm = TRUE),
    .groups = "drop"
  )
check %>% filter(n_frac_white > 1 | n_frac_black > 1 | n_frac_asian > 1 | n_frac_high > 1 | n_frac_mid > 1 |n_frac_low > 1 )


set.seed(123)  
idx <- sample(seq_len(nrow(df)), size = 0.005 * nrow(df))
df_sub <- df[idx, ]

write.csv(df_sub,file="df_sub.csv",row.names = F)

# expanded df for choice modeling

alts <- df_sub %>%
  group_by(district_id) %>%
  summarise(
    frac_white = first(frac_white),
    frac_black = first(frac_black),
    frac_asian = first(frac_asian),
    frac_high = first(frac_high),
    frac_mid = first(frac_mid),
    frac_low = first(frac_low),
    .groups = "drop"
  ) %>%
  rename(district_id_alt = district_id)

df_sub <- df_sub %>%
  mutate(row_id = row_number())

df_expanded <- df_sub %>%
  transmute(                                   # keep ONLY chooser-level variables here
    row_id,
    district_id_chosen = district_id,
    # keep all the person/household vars you want repeated:
    ID = ID,                                   # if you have an id column; otherwise remove
    ses = ses,
    ethnic = ethnic
    # add more chooser vars as needed
  ) %>%
  crossing(alts) %>%                            # adds district_id_alt + frac_homos/frac_homoe for each alternative
  mutate(choice = as.integer(district_id_alt == district_id_chosen)) %>%
  rename(district_id = district_id_alt) %>%
  select(-c("district_id_chosen"))
write.csv(df_expanded,file="df_expanded.csv",row.names = F)
















