library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

setwd("C:/Users/LENOVO/Documents/GitHub/discrete_choice/")

# test data downloaded ####

df2001 <- read.csv("data/BRENT_2001_ward/Data_ETHGEW_NS_SEC_UNIT.csv",sep=",")
df2011 <- read.csv("data/BRENT_2011_ward/Data_AGE_ETHGRP_NSSEC_UNIT.csv",sep=",")

unique(df2001$GEO_LABEL)
unique(df2011$GEO_LABEL)

df <- read.csv("Brent_RP.csv",sep=",")
#df <- df %>% select(ID,ses,ethnic,choice,frac_white,frac_black,frac_asian,frac_high,frac_mid,frac_low)
df <- df %>% select(ID,ses,ethnic,choice,frac_homos,frac_homoe)
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
  # summarise(
  #   frac_white = first(frac_white),
  #   frac_black = first(frac_black),
  #   frac_asian = first(frac_asian),
  #   frac_high = first(frac_high),
  #   frac_mid = first(frac_mid),
  #   frac_low = first(frac_low),
  #   .groups = "drop"
  # ) %>%
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
    ethnic = ethnic,
    frac_homos = frac_homos,
    frac_homoe = frac_homoe
    # add more chooser vars as needed
  ) %>%
  crossing(alts) %>%                            # adds district_id_alt + frac_homos/frac_homoe for each alternative
  mutate(choice = as.integer(district_id_alt == district_id_chosen)) %>%
  rename(district_id = district_id_alt) %>%
  select(-c("district_id_chosen"))
write.csv(df_expanded,file="df_expanded.csv",row.names = F)

# for district X chooser alternatives homos and homoe
alts <- df_sub %>%
  group_by(ID, district_id) %>%
  summarise(
    frac_homos_alt = first(frac_homos),
    frac_homoe_alt = first(frac_homoe),
    .groups = "drop"
  ) %>%
  rename(district_id_alt = district_id)

df_expanded <- df_sub %>%
  mutate(row_id = row_number()) %>%   # ensure row_id exists; remove if already exists
  transmute(
    row_id,
    district_id_chosen = district_id,
    ses = ses,
    ethnic = ethnic
  ) %>%
  crossing(alts) %>%
  mutate(choice = as.integer(district_id_alt == district_id_chosen)) %>%
  rename(district_id = district_id_alt,
         frac_homos = frac_homos_alt,
         frac_homoe = frac_homoe_alt) %>%
  select(-district_id_chosen)
write.csv(df_expanded,file="df_expanded.csv",row.names = F)


# new data test ####

df_all01 <- NULL   # accumulator
df_all11 <- NULL   # accumulator

files <- list.files("data_ward/2011/")

# 2001

df_all01 <- NULL   # accumulator

files <- list.files("data_ward/2001/")

for (i in files) {
  df <- read.csv(
    paste0("data_ward/2001/",i,"/Data_ETHGEW_NS_SEC_UNIT.csv"),
    sep = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df[1,1:5] <- c("CDU_ID","GEO_CODE","GEO_LABEL","GEO_TYPE","GEO_TYP2")
  names(df) <- df[1,]
  df <- df[-1,]
  df$borough_year <- i
  df$borough <- sub("_(\\d{4})$", "", i)
  df$year <- as.integer(sub(".*_(\\d{4})$", "\\1", i))
  df <- df %>% select(borough_year,borough, year,everything())
  df[,-c(1:8)] <- lapply(df[,-c(1:8)], function(x) as.numeric(x))
  
  df_all01 <- bind_rows(df_all01, df)
}


# 2011

df_all11 <- NULL   # accumulator

files <- list.files("data_ward/2011/")

for (i in files) {
  df <- read.csv(
    paste0("data_ward/2011/",i,"/Data_AGE_ETHGRP_NSSEC_SEX_UNIT.csv"),
           sep = ",",
           stringsAsFactors = FALSE,
           check.names = FALSE
    )
    df[1,1:5] <- c("CDU_ID","GEO_CODE","GEO_LABEL","GEO_TYPE","GEO_TYP2")
    names(df) <- df[1,]
    df <- df[-1,]
    df$borough_year <- i
    df$borough <- sub("_(\\d{4})$", "", i)
    df$year <- as.integer(sub(".*_(\\d{4})$", "\\1", i))
    df <- df %>% select(borough_year,borough, year,everything())
    df[,-c(1:8)] <- lapply(df[,-c(1:8)], function(x) as.numeric(x))
    
    df_all11 <- bind_rows(df_all11, df)
}

# 2021

df_croydon <-   df <- read.csv("data_ward/2021/croydon_2021/croydon_2021_1.csv",
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_all21 <- NULL   # accumulator

files <- list.files("data_ward/2021/")

for (i in c("croydon_2021_1","croydon_2021_2")) {
  df <- read.csv(
    paste0("data_ward/2021/","croydon_2021","/",i,".csv"),
    sep = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df[1,1:5] <- c("CDU_ID","GEO_CODE","GEO_LABEL","GEO_TYPE","GEO_TYP2")
  names(df) <- df[1,]
  df <- df[-1,]
  df$borough_year <- i
  df$borough <- sub("_(\\d{4})$", "", i)
  df$year <- as.integer(sub(".*_(\\d{4})$", "\\1", i))
  df <- df %>% select(borough_year,borough, year,everything())
  df[,-c(1:8)] <- lapply(df[,-c(1:8)], function(x) as.numeric(x))
  
  df_all21 <- bind_rows(df_all21, df)
}



# label
x <- "White \ British - Socio-economic Classification (NS-SeC) : Class 2 (Lower managerial and professional occupations) - Unit : People"

label <- tolower(paste0(
  # first word before the first "\"
  sub(" .*", "", trimws(sub("\\\\.*$", "", x))),
  "_",
  # class + number
  sub(".*Class ([0-9]+).*", "class\\1", x)
))

label


# df_2011a <- read.csv("data_ward/2001/barking_and_dagenham_2001/Data_ETHGEW_NS_SEC_UNIT.csv",sep=",")
# df_2011b <- read.csv("data_ward/2001/barnet_2001/Data_ETHGEW_NS_SEC_UNIT.csv",sep=",")
# dfbind <- rbind(df_2011a,df_2011b)
# dfbind <- bind_rows(df_2011a, df_2011b)
# 
# df_2011a2 <- df_2011a[-1, ]
# df_2011b2 <- df_2011b[-1, ]
# 
# # (optional but recommended) ensure same column order
# df_2011b2 <- df_2011b2[, names(df_2011a2)]
# dfbind <- rbind(df_2011a2, df_2011b2)

# keep
dfbind[,-c(1:5)] <- lapply(dfbind[,-c(1:5)], function(x) as.numeric(x))


