library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(stringr)
library(sf)
library(readxl)
library(spdep)
library(writexl)
library(ggrepel)
library(patchwork)



setwd("C:/Users/LENOVO/Documents/GitHub/discrete_choice/")
for (i in c("index_table","plotfacet","picsconc")) {
dir.create(i, recursive = FALSE, showWarnings = FALSE)
}


# dataset ####

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
df_all01 <- df_all01[,-114]

recode_01 <- read.csv("data_ward/recode_2001.csv",sep=";", header = F)
recode_01 <- recode_01[-1,]
r_df01 <- as.data.frame(t(recode_01))
names(r_df01) <- r_df01[1,]
r_df01 <- r_df01[-1,]

df_all01_r <- rbind(r_df01,df_all01)
check_dfall01 <- t(df_all01_r[1,])

names(df_all01_r) <- df_all01_r[1,]
df_all01_r <- df_all01_r[-1,]

# city of London

london01 <- read.csv("data_ward/city_london_westminster/city_of_london_2001.csv",sep=",")
london01[1,1:5] <- c("CDU_ID","GEO_CODE","GEO_LABEL","GEO_TYPE","GEO_TYP2")
names(london01) <- london01[1,]
london01 <- london01[-1,]

london01$borough_year <- "city_london_2001"
london01$borough <- "city_london"
london01$year <- 2001
london01 <- london01 %>% select(borough_year,borough,year,everything())
london01 <- london01[,-114]

london01_r <- rbind(r_df01,london01)
names(london01_r) <- london01_r[1,]
london01_r <- london01_r[-1,]
london01_r[,-c(1:8)] <- lapply(london01_r[,-c(1:8)], function(x) as.numeric(x))

# placeholder and aggregation, already passed
# london01_r$CDU_ID <- "city_london"
# london01_r$GEO_CODE <- "city_london"
# london01_r$GEO_LABEL <- "city_london"
# london01_r$GEO_TYPE <- "city_london"
# london01_r$GEO_TYP2 <- "city_london"
# london01_r <- london01_r %>%
#   group_by(borough_year,borough, year) %>%
#   summarise(
#     across(
#       -c(CDU_ID, GEO_CODE, GEO_LABEL, GEO_TYPE, GEO_TYP2),
#       ~sum(.x, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   )

london01_r <- london01_r %>% select(borough_year,borough,year,CDU_ID,GEO_CODE,GEO_LABEL,GEO_TYPE,GEO_TYP2,everything())

# Westminster

westminster01 <- read.csv("data_ward/city_london_westminster/westminster_2001.csv",sep=",")
westminster01[1,1:5] <- c("CDU_ID","GEO_CODE","GEO_LABEL","GEO_TYPE","GEO_TYP2")
names(westminster01) <- westminster01[1,]
westminster01 <- westminster01[-1,]

westminster01$borough_year <- "city_westminster_2001"
westminster01$borough <- "city_westminster"
westminster01$year <- 2001
westminster01 <- westminster01 %>% select(borough_year,borough,year,everything())
westminster01 <- westminster01[,-114]

westminster01_r <- rbind(r_df01,westminster01)
names(westminster01_r) <- westminster01_r[1,]
westminster01_r <- westminster01_r[-1,]
westminster01_r[,-c(1:8)] <- lapply(westminster01_r[,-c(1:8)], function(x) as.numeric(x))


# placeholder and aggregation, already passed
# westminster01_r$CDU_ID <- "city_westminster"
# westminster01_r$GEO_CODE <- "city_westminster"
# westminster01_r$GEO_LABEL <- "city_westminster"
# westminster01_r$GEO_TYPE <- "city_westminster"
# westminster01_r$GEO_TYP2 <- "city_westminster"
# westminster01_r <- westminster01_r %>%
#   group_by(borough_year,borough, year) %>%
#   summarise(
#     across(
#       -c(CDU_ID, GEO_CODE, GEO_LABEL, GEO_TYPE, GEO_TYP2),
#       ~sum(.x, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   )

westminster01_r <- westminster01_r %>% select(borough_year,borough,year,CDU_ID,GEO_CODE,GEO_LABEL,GEO_TYPE,GEO_TYP2,everything())

df_all01_r <- rbind(df_all01_r,london01_r,westminster01_r)



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


df_all11 <- df_all11[,-121]

recode_11 <- read.csv("data_ward/recode_2011.csv",sep=";", header = F)
recode_11 <- recode_11[-1,]
r_df11 <- as.data.frame(t(recode_11))
names(r_df11) <- r_df11[1,]
r_df11 <- r_df11[-1,]

df_all11_r <- rbind(r_df11,df_all11)
check_dfall11 <- t(df_all11_r[1,])

names(df_all11_r) <- df_all11_r[1,]
df_all11_r <- df_all11_r[-1,]


# City of London
london11 <- read.csv("data_ward/city_london_westminster/city_of_london_2011.csv",sep=";")
london11[1,1:5] <- c("CDU_ID","GEO_CODE","GEO_LABEL","GEO_TYPE","GEO_TYP2")
names(london11) <- london11[1,]
london11 <- london11[-1,]

london11$borough_year <- "city_london_2011"
london11$borough <- "city_london"
london11$year <- 2011
london11 <- london11 %>% select(borough_year,borough,year,everything())

london11_r <- rbind(r_df11,london11)
names(london11_r) <- london11_r[1,]
london11_r <- london11_r[-1,]
london11_r[,-c(1:8)] <- lapply(london11_r[,-c(1:8)], function(x) as.numeric(x))

# placeholder and aggregation, already passed
# london11_r <- london11_r %>%
#   group_by(borough_year,borough, year) %>%
#   summarise(
#     across(
#       -c(CDU_ID, GEO_CODE, GEO_LABEL, GEO_TYPE, GEO_TYP2),
#       ~sum(.x, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   )
# 
# london11_r$CDU_ID <- "city_london"
# london11_r$GEO_CODE <- "city_london"
# london11_r$GEO_LABEL <- "city_london"
# london11_r$GEO_TYPE <- "city_london"
# london11_r$GEO_TYP2 <- "city_london"

london11_r <- london11_r %>% select(borough_year,borough,year,CDU_ID,GEO_CODE,GEO_LABEL,GEO_TYPE,GEO_TYP2,everything())

# Westminster

westminster11 <- read.csv("data_ward/city_london_westminster/westminster_2011.csv",sep=";")
westminster11[1,1:5] <- c("CDU_ID","GEO_CODE","GEO_LABEL","GEO_TYPE","GEO_TYP2")
names(westminster11) <- westminster11[1,]
westminster11 <- westminster11[-1,]

westminster11$borough_year <- "city_westminster_2011"
westminster11$borough <- "city_westminster"
westminster11$year <- 2011
westminster11 <- westminster11 %>% select(borough_year,borough,year,everything())

westminster11_r <- rbind(r_df11,westminster11)
names(westminster11_r) <- westminster11_r[1,]
westminster11_r <- westminster11_r[-1,]
westminster11_r[,-c(1:8)] <- lapply(westminster11_r[,-c(1:8)], function(x) as.numeric(x))

# placeholder and aggregation, already passed
# westminster11_r$CDU_ID <- "city_westminster"
# westminster11_r$GEO_CODE <- "city_westminster"
# westminster11_r$GEO_LABEL <- "city_westminster"
# westminster11_r$GEO_TYPE <- "city_westminster"
# westminster11_r$GEO_TYP2 <- "city_westminster"
# westminster11_r <- westminster11_r %>%
#   group_by(borough_year,borough, year) %>%
#   summarise(
#     across(
#       -c(CDU_ID, GEO_CODE, GEO_LABEL, GEO_TYPE, GEO_TYP2),
#       ~sum(.x, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   )

westminster11_r <- westminster11_r %>% select(borough_year,borough,year,CDU_ID,GEO_CODE,GEO_LABEL,GEO_TYPE,GEO_TYP2,everything())


df_all11_r <- rbind(df_all11_r,london11_r,westminster11_r)

# harmonizing 2001 - 2011
# setdiff(names(df_all01_r), names(df_all11_r))

df_all01_r <- df_all01_r %>%
  rename(
    white_ewsnib_sc1 = white_british_sc1,
    white_ewsnib_sc2 = white_british_sc2,
    white_ewsnib_sc3 = white_british_sc3,
    white_ewsnib_sc4 = white_british_sc4,
    white_ewsnib_sc5 = white_british_sc5,
    white_ewsnib_sc6 = white_british_sc6,
    white_ewsnib_sc7 = white_british_sc7,
    asian_chinese_sc1 = chineseot_chinese_sc1,
    asian_chinese_sc2 = chineseot_chinese_sc2,
    asian_chinese_sc3 = chineseot_chinese_sc3,
    asian_chinese_sc4 = chineseot_chinese_sc4,
    asian_chinese_sc5 = chineseot_chinese_sc5,
    asian_chinese_sc6 = chineseot_chinese_sc6,
    asian_chinese_sc7 = chineseot_chinese_sc7
  )

df_all11_r <- df_all11_r %>% select(-c("white_gypsy_sc1", "white_gypsy_sc2", "white_gypsy_sc3",
                                       "white_gypsy_sc4","white_gypsy_sc5","white_gypsy_sc6",
                                       "white_gypsy_sc7"))


mismatchbourough <- df_all01_r %>% filter(!borough %in% df_all11_r$borough)

df_all01_11 <- rbind(df_all01_r,df_all11_r)

rm(list = setdiff(ls(), "df_all01_11"))

# 2021

df_all21 <- NULL   # accumulator

files <- list.files("data_ward/2021/")

for (i in files) {
  df <- read.csv(
    paste0("data_ward/2021/",i,"/",i,".csv"),
    sep = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  df$borough_year <- i
  df$borough <- sub("_[^_]*$", "", i)
  df$year <- sub(".*_", "", i)
  
    
  df_all21 <- bind_rows(df_all21, df)
}

#richtames, croydon,  ealing,  havering split and to be included separetely

fileslist <- list.files("data_ward/other_include")
df_other <- NULL   # accumulator
for(d in fileslist){
  other <- read.csv(paste0("data_ward/other_include/", d),
                  sep = ",",
                  stringsAsFactors = FALSE,
                  check.names = FALSE
)
  other$borough_year <- sub("^(.+_2021).*", "\\1", d)
  other$borough <- sub("_2021.*$", "", d)
  other$year <- 2021
df_other <- bind_rows(df_other, other)
}


df_all21 <- rbind(df_all21,df_other)

# City of London and Westminster 2021 recoded (12 NS-SeC categories to 10)

rename_ethsoc12 <- function(df,ethnic,ethmaj,ethmin){
  df <- df %>%
    mutate(
      recoded = case_when(
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "Does not apply" ~ 
          paste0(ethmaj,"_",ethmin,"_notapply"),
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L1 and L2: Large employers and higher managerial and administrative occupations" ~
          paste0(ethmaj,"_",ethmin,"_sc1"),
        
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L3: Higher professional occupations" ~
          paste0(ethmaj,"_",ethmin,"_sc1"),
        
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L4, L5 and L6: Lower managerial, administrative and professional occupations" ~
          paste0(ethmaj,"_",ethmin,"_sc2"),
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L7: Intermediate occupations" ~
          paste0(ethmaj,"_",ethmin,"_sc3"),     
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L8 and L9: Small employers and own account workers" ~
          paste0(ethmaj,"_",ethmin,"_sc4"),     
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L10 and L11: Lower supervisory and technical occupations" ~
          paste0(ethmaj,"_",ethmin,"_sc5"),     
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L12: Semi-routine occupations" ~
          paste0(ethmaj,"_",ethmin,"_sc6"),  
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L13: Routine occupations" ~
          paste0(ethmaj,"_",ethmin,"_sc7"),        
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L14.1: Never worked" ~
          paste0(ethmaj,"_",ethmin,"_sc8"),        
        
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L14.2: Long-term unemployed" ~
          paste0(ethmaj,"_",ethmin,"_sc8"), 
        
        `Ethnic group (20 categories)` == ethnic & 
          `National Statistics Socio-economic Classification (NS-SeC) (12 categories)` == "L15: Full-time students" ~
          paste0(ethmaj,"_",ethmin,"_students"), 
        TRUE ~ recoded
      )
    )
}

df_all21$recoded <- NA

df_all21 <- rename_ethsoc12(df_all21,"Does not apply","notapply","notapply")
df_all21 <- rename_ethsoc12(df_all21, "Asian, Asian British or Asian Welsh: Bangladeshi","asian","bangladeshi")
df_all21 <- rename_ethsoc12(df_all21,"Asian, Asian British or Asian Welsh: Chinese" ,"asian","chinese")
df_all21 <- rename_ethsoc12(df_all21,"Asian, Asian British or Asian Welsh: Indian" ,"asian","indian")
df_all21 <- rename_ethsoc12(df_all21,"Asian, Asian British or Asian Welsh: Pakistani" ,"asian","pakistani")
df_all21 <- rename_ethsoc12(df_all21,"Asian, Asian British or Asian Welsh: Other Asian" ,"asian","other")
df_all21 <- rename_ethsoc12(df_all21,"Black, Black British, Black Welsh, Caribbean or African: African" ,"black","african")
df_all21 <- rename_ethsoc12(df_all21,"Black, Black British, Black Welsh, Caribbean or African: Caribbean" ,"black","carribean")
df_all21 <- rename_ethsoc12(df_all21,"Black, Black British, Black Welsh, Caribbean or African: Other Black" ,"black","other")
df_all21 <- rename_ethsoc12(df_all21,"Mixed or Multiple ethnic groups: White and Asian" ,"mixed","whiteasian")
df_all21 <- rename_ethsoc12(df_all21,"Mixed or Multiple ethnic groups: White and Black African" ,"mixed","whiteblafrican")
df_all21 <- rename_ethsoc12(df_all21, "Mixed or Multiple ethnic groups: White and Black Caribbean","mixed","whiteblcarribean")
df_all21 <- rename_ethsoc12(df_all21, "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups","mixed","other")
df_all21 <- rename_ethsoc12(df_all21,"White: English, Welsh, Scottish, Northern Irish or British" ,"white","ewsnib")
df_all21 <- rename_ethsoc12(df_all21,"White: Irish" ,"white","irish")
df_all21 <- rename_ethsoc12(df_all21,"White: Gypsy or Irish Traveller"  ,"white","gypsy")
df_all21 <- rename_ethsoc12(df_all21,"White: Roma" ,"white","roma")
df_all21 <- rename_ethsoc12(df_all21,"White: Other White" ,"white","other")
df_all21 <- rename_ethsoc12(df_all21,"Other ethnic group: Arab" ,"other","arab")
df_all21 <- rename_ethsoc12(df_all21, "Other ethnic group: Any other ethnic group","other","other") 

# City of London 

london21 <- read.csv("data_ward/city_london_westminster/city_london_2021.csv", sep = ",",
                     stringsAsFactors = FALSE,
                     check.names = FALSE)


london21$recoded <- NA

london21 <- rename_ethsoc12(london21,"Does not apply","notapply","notapply")
london21 <- rename_ethsoc12(london21, "Asian, Asian British or Asian Welsh: Bangladeshi","asian","bangladeshi")
london21 <- rename_ethsoc12(london21,"Asian, Asian British or Asian Welsh: Chinese" ,"asian","chinese")
london21 <- rename_ethsoc12(london21,"Asian, Asian British or Asian Welsh: Indian" ,"asian","indian")
london21 <- rename_ethsoc12(london21,"Asian, Asian British or Asian Welsh: Pakistani" ,"asian","pakistani")
london21 <- rename_ethsoc12(london21,"Asian, Asian British or Asian Welsh: Other Asian" ,"asian","other")
london21 <- rename_ethsoc12(london21,"Black, Black British, Black Welsh, Caribbean or African: African" ,"black","african")
london21 <- rename_ethsoc12(london21,"Black, Black British, Black Welsh, Caribbean or African: Caribbean" ,"black","carribean")
london21 <- rename_ethsoc12(london21,"Black, Black British, Black Welsh, Caribbean or African: Other Black" ,"black","other")
london21 <- rename_ethsoc12(london21,"Mixed or Multiple ethnic groups: White and Asian" ,"mixed","whiteasian")
london21 <- rename_ethsoc12(london21,"Mixed or Multiple ethnic groups: White and Black African" ,"mixed","whiteblafrican")
london21 <- rename_ethsoc12(london21, "Mixed or Multiple ethnic groups: White and Black Caribbean","mixed","whiteblcarribean")
london21 <- rename_ethsoc12(london21, "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups","mixed","other")
london21 <- rename_ethsoc12(london21,"White: English, Welsh, Scottish, Northern Irish or British" ,"white","ewsnib")
london21 <- rename_ethsoc12(london21,"White: Irish" ,"white","irish")
london21 <- rename_ethsoc12(london21,"White: Gypsy or Irish Traveller"  ,"white","gypsy")
london21 <- rename_ethsoc12(london21,"White: Roma" ,"white","roma")
london21 <- rename_ethsoc12(london21,"White: Other White" ,"white","other")
london21 <- rename_ethsoc12(london21,"Other ethnic group: Arab" ,"other","arab")
london21 <- rename_ethsoc12(london21, "Other ethnic group: Any other ethnic group","other","other") 

london21$borough_year <- "city_london_2021"
london21$borough <- "city_london"
london21$year <- "2021"


rename_ethsoc10 <- function(df,ethnic,ethmaj,ethmin){
  df <- df %>%
    mutate(
      recoded = case_when(
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "Does not apply" ~ 
  paste0(ethmaj,"_",ethmin,"_notapply"),
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L1, L2 and L3: Higher managerial, administrative and professional occupations" ~
  paste0(ethmaj,"_",ethmin,"_sc1"),
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L4, L5 and L6: Lower managerial, administrative and professional occupations" ~
  paste0(ethmaj,"_",ethmin,"_sc2"),
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L7: Intermediate occupations" ~
  paste0(ethmaj,"_",ethmin,"_sc3"),     
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L8 and L9: Small employers and own account workers" ~
  paste0(ethmaj,"_",ethmin,"_sc4"),     
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L10 and L11: Lower supervisory and technical occupations" ~
  paste0(ethmaj,"_",ethmin,"_sc5"),     
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L12: Semi-routine occupations" ~
  paste0(ethmaj,"_",ethmin,"_sc6"),  
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L13: Routine occupations" ~
  paste0(ethmaj,"_",ethmin,"_sc7"),        
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L14.1 and L14.2: Never worked and long-term unemployed" ~
  paste0(ethmaj,"_",ethmin,"_sc8"),          
`Ethnic group (20 categories)` == ethnic & 
  `National Statistics Socio-economic Classification (NS-SeC) (10 categories)` == "L15: Full-time students" ~
  paste0(ethmaj,"_",ethmin,"_students"), 
TRUE ~ recoded
)
)
}

# Westminster
westminster21 <- read.csv("data_ward/city_london_westminster/westminster_2021.csv", sep = ",",
                          stringsAsFactors = FALSE,
                          check.names = FALSE)

westminster21$recoded <- NA

westminster21 <- rename_ethsoc10(westminster21,"Does not apply","notapply","notapply")
westminster21 <- rename_ethsoc10(westminster21,"Asian, Asian British or Asian Welsh: Bangladeshi","asian","bangladeshi")
westminster21 <- rename_ethsoc10(westminster21,"Asian, Asian British or Asian Welsh: Chinese","asian","chinese")
westminster21 <- rename_ethsoc10(westminster21,"Asian, Asian British or Asian Welsh: Indian","asian","indian")
westminster21 <- rename_ethsoc10(westminster21,"Asian, Asian British or Asian Welsh: Pakistani","asian","pakistani")
westminster21 <- rename_ethsoc10(westminster21,"Asian, Asian British or Asian Welsh: Other Asian","asian","other")
westminster21 <- rename_ethsoc10(westminster21,"Black, Black British, Black Welsh, Caribbean or African: African","black","african")
westminster21 <- rename_ethsoc10(westminster21,"Black, Black British, Black Welsh, Caribbean or African: Caribbean","black","carribean")
westminster21 <- rename_ethsoc10(westminster21,"Black, Black British, Black Welsh, Caribbean or African: Other Black","black","other")
westminster21 <- rename_ethsoc10(westminster21,"Mixed or Multiple ethnic groups: White and Asian","mixed","whiteasian")
westminster21 <- rename_ethsoc10(westminster21,"Mixed or Multiple ethnic groups: White and Black African" ,"mixed","whiteblafrican")
westminster21 <- rename_ethsoc10(westminster21,"Mixed or Multiple ethnic groups: White and Black Caribbean","mixed","whiteblcarribean")
westminster21 <- rename_ethsoc10(westminster21,"Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups" ,"mixed","other")
westminster21 <- rename_ethsoc10(westminster21,"White: English, Welsh, Scottish, Northern Irish or British","white","ewsnib")
westminster21 <- rename_ethsoc10(westminster21,"White: Irish","white","irish")
westminster21 <- rename_ethsoc10(westminster21,"White: Gypsy or Irish Traveller","white","gypsy")
westminster21 <- rename_ethsoc10(westminster21,"White: Roma","white","roma")
westminster21 <- rename_ethsoc10(westminster21,"White: Other White","white","other")
westminster21 <- rename_ethsoc10(westminster21,"Other ethnic group: Arab","other","arab")
westminster21 <- rename_ethsoc10(westminster21,"Other ethnic group: Any other ethnic group" ,"other","other")

westminster21$borough_year <- "city_westminster_2021"
westminster21$borough <- "city_westminster"
westminster21$year <- "2021"

# merge df_all21, citylondon21, westminster21
# to sum same NS-SeC level across subcategories

df_all21 <- df_all21 %>%
  group_by(across(-c(Observation,`National Statistics Socio-economic Classification (NS-SeC) (12 categories) Code`, 
                     `National Statistics Socio-economic Classification (NS-SeC) (12 categories)`))) %>%
  summarise(
    Observation = sum(Observation, na.rm = TRUE),
    .groups = "drop"
  )

london21  <- london21 %>%
  group_by(across(-c(Observation,`National Statistics Socio-economic Classification (NS-SeC) (12 categories) Code`, 
                     `National Statistics Socio-economic Classification (NS-SeC) (12 categories)`))) %>%
  summarise(
    Observation = sum(Observation, na.rm = TRUE),
    .groups = "drop"
  )

# only westminster21 left with NS-SeC category number
westminster21 <- westminster21 %>% select(-c(`National Statistics Socio-economic Classification (NS-SeC) (10 categories)`,
                                   `National Statistics Socio-economic Classification (NS-SeC) (10 categories) Code`))

setdiff(unique(df_all21$recoded),unique(westminster21$recoded))

df_all21 <- rbind(df_all21,london21,westminster21)

df_all21 <- df_all21 %>%
  pivot_wider(
    id_cols = c(`Electoral wards and divisions Code`,`Electoral wards and divisions`,borough_year,borough, year),
    names_from = recoded,
    values_from = Observation
  ) 

df_all21$GEO_CODE <- df_all21$`Electoral wards and divisions`
df_all21$GEO_LABEL <- df_all21$`Electoral wards and divisions`
df_all21$GEO_TYPE <- df_all21$`Electoral wards and divisions`
df_all21$GEO_TYP2 <- df_all21$`Electoral wards and divisions`
df_all21$CDU_ID <- "combined_2021"


rm(list = setdiff(ls(), c("df_all01_11","df_all21")))

# merge df_all21, df_all01_11

df_all21 <- df_all21 %>% select(any_of(names(df_all01_11)))

df_final <- rbind(df_all01_11,df_all21)

df_final <- df_final %>%
  mutate(
    across(
      -c(borough_year, borough, year, CDU_ID,
         GEO_CODE, GEO_LABEL, GEO_TYPE, GEO_TYP2),
      as.numeric
    )
  )  %>%
  group_by(borough_year, borough, year) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# sanity check
# check <- df_final %>%
#   distinct(borough, year) %>%
#   count(borough) %>%
#   arrange(n)

# Final dataset combined
rm(list = setdiff(ls(), c("df_final")))

# excluded subcategory ethnicity "others"
df_final <- df_final %>%
  select(
    -matches("^[^_]+_other_[^_]+$")
  )

# Compute reference groups total ####

# compute population of each neighborhood
df_final <- df_final %>%
  group_by(borough_year) %>%
  mutate(
    pop_borough = sum(
      unlist(across(where(is.numeric))),
      na.rm = TRUE
    )
  )  %>%
  ungroup()

# total ethnic major group
total_group <- function(group) {
  
  new_var <- paste0("total_", group)
  
  df_final <- df_final %>%
    mutate(
      !!new_var := rowSums(
        pick(starts_with(paste0(group, "_"))),
        na.rm = TRUE
      )
    )
}

for (d in c("asian","white","black","mixed")) {
  df_final <- total_group(d)
}

# compute total specific ethnic group
total_mingroup <- function(mingroup) {
  
  new_var <- paste0("total_", mingroup)
  
  df_final <- df_final %>%
    mutate(
      !!new_var := rowSums(
        pick(matches(paste0("^[^_]+_", mingroup, "_"))),
        na.rm = TRUE
      )
    )
}

for (f in c("bangladeshi","indian","pakistani","chinese","african","carribean","ewsnib","irish",
            "whiteasian","whiteblcarribean","whiteblafrican")) {
  df_final <- total_mingroup(f)
}

# compute total social status group
total_status <- function(status) {
  
  new_var <- paste0("total_", status)
  
  df_final <- df_final %>%
    mutate(
      !!new_var := rowSums(
        pick(ends_with(paste0("_",status))),
        na.rm = TRUE
      )
    )
}

for (z in c("sc1","sc2","sc3","sc4","sc5","sc6","sc7")) {
  df_final <- total_status(z)
}

# compute proportion ethnic group X social status and 
# proportion aggregated group (social class, major ethnic group) for each neighborhood
# alert: it loops over names of variables, it is critical to do at this step (or set the names)

frac_group <- function(fracgroup) {
  
  new_var <- paste0("frac_",fracgroup)
  
  df_final <- df_final %>%
    mutate(
      !!new_var := .data[[fracgroup]] / pop_borough
    )
}

for (t in names(df_final %>% select(- c(borough_year,borough,year,pop_borough)))) {
  df_final <- frac_group(t)
}


# compute the proportion of ethnic group within social status class
# alert: it loops over selected names, it is critical to be done at this step (or set the names)
frac_status <- function(substatus, classtat){
  
    new_var <- paste0("fracsub_",substatus)
    df_final <- df_final %>%
      mutate(
        !!new_var := .data[[substatus]] / .data[[classtat]]
      )
  
}

for (p in c("_sc1","_sc2","_sc3","_sc4","_sc5","_sc6","_sc7")) {
  for (g in names(df_final %>% select(ends_with(p) & ! starts_with("total") & ! starts_with("frac") ))) {
    df_final <- frac_status(g,paste0("total",p))
  }
}

# Shannon ethnic entropy normalized for the specific ethnic groups (1 = max diversity, 0 = 1 group)
df_final <- df_final %>%
  rowwise() %>%
  mutate(
    shannon_ethnic_norm = {
      x <- c_across(c(total_bangladeshi, total_indian, total_pakistani, total_chinese,
                      total_african, total_carribean, total_ewsnib, total_irish,
                      total_whiteasian, total_whiteblcarribean, total_whiteblafrican
                      ))
      p <- x / sum(x)
      p <- p[p > 0]
      (-sum(p * log(p))) / log(11)
    }
  ) %>%
  ungroup()


# Shannon social status entropy normalized for the specific ethnic groups (1 = max diversity, 0 = 1 group)
df_final <- df_final %>%
  rowwise() %>%
  mutate(
    shannon_status_norm = {
      x <- c_across(c(total_sc1, total_sc2, total_sc3, total_sc4, total_sc5, total_sc6, total_sc7))
      p <- x / sum(x)
      p <- p[p > 0]
      (-sum(p * log(p))) / log(7)
    }
  ) %>%
  ungroup()


# Figures for neighborhood concentration and shapefile ####

# shapefile upload and preparation (used also for Moran's I)
list_boroughsp <- read_excel("shapefile/match_boroughdf.xlsx")

shapefile_df <- NULL
for (s in list_boroughsp[[1]]) {
  print(s)
  dfshape <- st_read(paste0("shapefile/LB_MSOA2021_shp/msoa2021/",s,".shp"))
  shapefile_df <- bind_rows(shapefile_df, dfshape)
}

shapefile_df <- shapefile_df %>%
  left_join(
    list_boroughsp,
    by = c("lad22nm" = "name_shapefile")
  ) %>%
  mutate(lad22nm = name_dataset) %>%
  select(-name_dataset)

shapefile_df <- shapefile_df %>%
  rename(borough = lad22nm)

shapefile_df <- merge(shapefile_df,df_final, by = c("borough"))

borough_borders <- shapefile_df %>%
  group_by(borough) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Pictures of neighborhood compositions and Shannon enthropy

picneighborhood <- function(variable, labelname,fold){
ggplot(shapefile_df) +
  geom_sf(aes(fill = .data[[variable]]), color = NA) +
  scale_fill_gradientn(colours = c("beige", "orange"),
                       name = labelname) + 
  geom_sf(
    data = borough_borders,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) + 
  facet_grid(~ year) +
    ggtitle(gsub("_"," ",variable)) + 
  theme_bw() +
theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank()
)
ggsave(paste0(fold,"/",variable,".jpg"), width = 6, height = 2)
}

for (w in  names(df_final %>% select(starts_with("frac") | starts_with("shannon") ))) {
  picneighborhood(w,"","picsconc")
}

# Plots for changes in the population ethnic composition and social classes ethnic composition ####

# proportion ethnicity over status, aggregated at population level by year
fracplotstatus <- df_final %>%
  select(
    borough_year,
    borough,
    year,
    matches("^(black|white|asian|mixed).*_sc[1-7]$"),
    matches("^total.*_sc[1-7]$")
  ) %>%
  group_by(
    year
  ) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  )

vars <- names(fracplotstatus %>%
                select(matches("^(black|white|asian|mixed).*_sc[1-7]$")))

for (v in vars) {
  sc <- sub(".*_(sc[1-7])$", "\\1", v)
  denom <- paste0("total_", sc)
  fracplotstatus[[paste0("frac_", v)]] <- fracplotstatus[[v]] / fracplotstatus[[denom]]
}

ethss_prop <- fracplotstatus %>%
  pivot_longer(
    cols = matches("^frac_.*_sc[1-7]$"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  select( year, variable, value) %>%
    mutate(
      socstatus = sub(".*_(sc[1-7])$", "\\1", variable),
      specific = sub("^frac_[^_]+_(.*?)_sc[1-7]$", "\\1", variable)
    )  %>%
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(color = specific)) +
  geom_line(aes(color = specific, group = specific)) +
  facet_wrap(~ socstatus, nrow = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(y = NULL,
       color = "Ethnic group") +
  ggtitle("Ethnic composition within occupational class") +
  theme_bw() +
  guides(
    color = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme(#legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0("plotfacet/prop_sceth.jpg"), width = 9, height = 3)

# Ethnic composition of the population, aggregated by year
  
ethnicgroup <- c("bangladeshi","indian","pakistani","chinese","african","carribean","ewsnib","irish",
    "whiteasian","whiteblcarribean","whiteblafrican") 
  
fracplotpop <- df_final %>%
  select(
    borough_year,
    borough,
    year,
    matches(
      paste0("^total_(", paste(ethnicgroup, collapse = "|"), ")$")
    )
  ) %>%
    group_by(
      year
    ) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    poptotal = rowSums(across(starts_with("total_")), na.rm = TRUE)
  )

  varspop <- names(fracplotpop %>%
                  select(matches("^total")))
  
  for (v in varspop) {
    
    fracplotpop[[paste0("frac_", v)]] <- fracplotpop[[v]] / fracplotpop[["poptotal"]]
  }
  
ethpr_pop <- fracplotpop %>%
  select(year,
         poptotal,
         matches("^frac")) %>%
  pivot_longer(
    cols = matches("^frac"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    specific = sub(".*_", "", variable)
  )  %>%
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(color = specific)) +
  geom_line(aes(color = specific, group = specific)) + 
   scale_x_discrete(
    breaks = c("2001", "2011", "2021"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = "Year",
    y = NULL,
    color = "Ethnic group"
  ) + 
  ggtitle("Ethnic composition of Greater London") + 
  guides(
    color = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme_bw() 
 ggsave(paste0("plotfacet/prop_ethpop.jpg"), width = 6, height = 3)  

# Social class composition of the  whole population
 
 # Ethnic composition of the population, aggregated by year
 
 socialgroup <- c("sc1","sc2","sc3","sc4","sc5","sc6","sc7") 
 
 fracplotpopsc <- df_final %>%
   select(
     borough_year,
     borough,
     year,
     matches(
       paste0("^total_(", paste(socialgroup, collapse = "|"), ")$")
     )
   ) %>%
   group_by(
     year
   ) %>%
   summarise(
     across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
     .groups = "drop"
   ) %>%
   mutate(
     poptotal = rowSums(across(starts_with("total_")), na.rm = TRUE)
   )
 
 varspopsc <- names(fracplotpopsc %>%
                    select(matches("^total")))
 
 for (v in varspopsc) {
   
   fracplotpopsc[[paste0("frac_", v)]] <- fracplotpopsc[[v]] / fracplotpopsc[["poptotal"]]
 }
 
 scpr_pop <- fracplotpopsc %>%
   select(year,
          poptotal,
          matches("^frac")) %>%
   pivot_longer(
     cols = matches("^frac"),
     names_to = "variable",
     values_to = "value"
   ) %>%
   mutate(
     specific = sub(".*_", "", variable)
   )  %>%
   ggplot(aes(x = year, y = value)) +
   geom_point(aes(color = specific)) +
   geom_line(aes(color = specific, group = specific)) + 
   scale_x_discrete(
     breaks = c("2001", "2011", "2021"),
     expand = expansion(mult = c(0.02, 0.02))
   ) +
   scale_y_continuous(labels = scales::label_percent()) +
   labs(
     x = "Year",
     y = NULL,
     color = "Ethnic group"
   ) + 
   ggtitle("Occupational class composition of Greater London") + 
   guides(
     color = guide_legend(ncol = 2, byrow = TRUE)
   ) +
   theme_bw() 
 ggsave(paste0("plotfacet/prop_scpop.jpg"), width = 6, height = 3)  
  
# sanity check
# fracplot %>%
#   pivot_longer(
#     cols = matches("^frac_.*_sc[1-7]$"),
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(
#     socstatus = sub(".*_(sc[1-7])$", "\\1", variable)
#   ) %>%
#   group_by(year, socstatus) %>%
#   summarise(
#     total = sum(value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(!near(total, 1))

# Distribution of neighborhood compositions
# function for boxplot distribution
# it generates plots for each dimension, then combined
# median(df_final$frac_total_white[df_final$year == 2021], na.rm = TRUE)
 
boxplot_outliers <- function(var){
  
  outliers <- df_final %>%
    group_by(year) %>%
    filter(
      .data[[var]] >
        boxplot.stats(.data[[var]])$stats[5]
    ) %>%
    ungroup()
  
  p <- ggplot(df_final,
              aes(x = factor(year), y = .data[[var]])) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(data = outliers, color = "red") +
    # scale_y_continuous(
    #   labels = scales::label_percent()
    # ) +
    geom_text_repel(
      data = outliers,
      aes(label = borough),
      size = 3,
      max.overlaps = Inf
    ) +
#    ggtitle( str_to_title(sub("^frac_total_", "Borough Percentage ", var))) +
#    ggtitle("Shannon occupational diversity") +
    ggtitle("Shannon ethnic diversity") +    
    labs(
      x = "Year",
      y = NULL
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5)
      )
  
  ggsave(
    paste0("plotfacet/", var, "_boxplot.jpg"),
    plot = p,
    width = 6,
    height = 4
  )
  
  p
}

# Ethnic composition population change
ethnicgroup <- c("bangladeshi","indian","pakistani","chinese","african","carribean",
                 "ewsnib","irish", "whiteasian","whiteblcarribean","whiteblafrican")
vars_ethnicgroup <- paste0("frac_total_",ethnicgroup) 

plotsEC <- lapply(vars_ethnicgroup, boxplot_outliers)

plotshannonethnic <- boxplot_outliers("shannon_ethnic_norm")

combinedEC <-
  wrap_plots(c(plotsEC,plotshannonethnic), ncol = 3) /
  ( ethpr_pop | plot_spacer()) +
  plot_layout(
    heights = c(3.5, 0.8),
    widths = c(1, 1)
  )
ggsave("plotfacet/combined_distribethPOP.jpg", width = 11, height = 13)

# Ethnic composition within social classes
socstgroup <- c(paste0("total_sc", 1:7))
vars_socstgroup <- paste0("frac_",socstgroup) 
plotsSC <- lapply(vars_socstgroup, boxplot_outliers)

# combinedSC <-
#   wrap_plots(plotsSC, ncol = 3) /
#   wrap_plots(scpr_pop) /
#   wrap_plots(ethss_prop ) +
#   plot_layout(
#     heights = c(3, 0.8,1),
#     widths = c(2,0.3,0.3)
#   )

plotshannonsc <- boxplot_outliers("shannon_status_norm")

scpr_short <- 
  (scpr_pop | plot_spacer() |plot_spacer()) +
  plot_layout(widths = c(0.8,0.8, 0.8))

ethss_short <- 
  (ethss_prop | plot_spacer() |plot_spacer()) +
  plot_layout(widths = c(3,0.2, 0.2))


combinedSC <-
  wrap_plots(c(plotsSC,plotshannonsc), ncol = 3) /
  scpr_short /
  ethss_short +
  plot_layout(
    heights = c(4, 1, 1)
  )
ggsave("plotfacet/combined_distribethSC.jpg", width = 11, height = 13)


# Moran's I global

# aggregate at the borough level per year, focus on the proportion variables and shannon
borough_sf <- shapefile_df %>%
  group_by(borough, year) %>%
  summarise(
    across(
      c(starts_with("frac"), starts_with("shannon")),
      first
    ),
    .groups = "drop"
  )

# Moran's I function per year and variable
moranyear <- function(yearmoran, variablemoran) {
  
  dfmoran <- borough_sf %>%
    filter(year == yearmoran)
  
  nb <- poly2nb(dfmoran)
  lw <- nb2listw(nb)
  
  moran.test(
    dfmoran[[variablemoran]],
    lw
  )
}

# to prepare the dataset: years and variables to compute over
years <- c(2001, 2011, 2021)
variablemoranidx <- c("shannon_ethnic_norm","shannon_status_norm",
                      paste0("frac_total_",c("bangladeshi","indian","pakistani","chinese","african","carribean","ewsnib","irish",
                                             "whiteasian","whiteblcarribean","whiteblafrican")),
                      names(df_final %>% select(matches("^frac_.*_sc[1-7]$")))
                      )

dfm <- expand.grid(
  year = years,
  variable = variablemoranidx
)

dfm$MoransI <- NA_real_
dfm$pvalue <- NA_real_

# loop to compute the dataset over rows, each row is a variable per year, Morans' I between boroughs
for (r in seq_len(nrow(dfm))) {
  
  dfm_moran <- moranyear(
    yearmoran = dfm$year[r],
    variablemoran = dfm$variable[r]
  )
  
  dfm$MoransI[r] <- dfm_moran$estimate[[1]]
  dfm$pvalue[r] <- dfm_moran$p.value
}

# to show p-value of Moran's I
dfm <- dfm %>%
  mutate(
    sign = case_when(
      pvalue <= 0.001 ~ "***",
      pvalue <= 0.01  ~ "**",
      pvalue <= 0.05  ~ "*",
      TRUE ~ ""
    )
  )


write_xlsx(dfm,"index_table/dfm_moran.xlsx")

# Segregation indices for intersecting ethnicXclass categories ####

# Functions

# Duncan between ethnic and class groups
duncan_two_groups <- function(df, subgroup, comparison_group) {
  
  x_i <- df[[subgroup]]
  y_i <- df[[comparison_group]]
  
  X <- sum(x_i, na.rm = TRUE)
  Y <- sum(y_i, na.rm = TRUE)
  
  if (X == 0 || Y == 0) return(NA_real_)
  
  0.5 * sum(abs((x_i / X) - (y_i / Y)), na.rm = TRUE)
}


# Duncan within-class and within-ethnic group
duncan_comp <- function(df, other_group, subgroup) {
  
  x_i <- df[[subgroup]]
  y_i <- df[[other_group]] - x_i
  
  X <- sum(x_i, na.rm = TRUE)
  Y <- sum(y_i, na.rm = TRUE)
  
  if (X == 0 || Y == 0) return(NA_real_)
  
  0.5 * sum(
    abs((x_i / X) - (y_i / Y)),
    na.rm = TRUE
  )
}

# Exposure index
exposure_index <- function(df, subgroup, reference_group,  total_pop ) {
  
  x_i <- df[[subgroup]]
  
  y_i <- df[[reference_group]]
  
  t_i <- df[[total_pop]]
  
  X <- sum(x_i, na.rm = TRUE)
  
  
  
  if (X == 0) return(NA_real_)
  
  sum((x_i / X) * (y_i / t_i), na.rm = TRUE)
  
} 

# Diversity index (weighted ethnic and class Shannon computed before)
diversity_exposure <- function(df,
                               subgroup,
                               diversity_var) {
  
  x_i <- df[[subgroup]]
  H_i <- df[[diversity_var]]
  
  X <- sum(x_i, na.rm = TRUE)
  
  if (X == 0) return(NA_real_)
  
  sum(
    (x_i / X) * H_i,
    na.rm = TRUE
  )
}

# Function to assemble the indices for each group, for each year
# it writes down for each group, and df_index for the assembled dataset, as R object only df_index

index_groups <- function(group_tocompute){

results <- list()
for (q in c("sc1","sc2","sc3","sc4","sc5","sc6","sc7")) {
  
groups <- paste0( group_tocompute ,"_",q) 
years <- c(2001,2011,2021)
ethnicspecific <-  sub("^[^_]*_", "", group_tocompute)

results_q <- expand.grid(
  year = years,
  group = groups,
  ethnicspecific = ethnicspecific,
  socstatus = q
)


duncan_eth <- "DuncanEth"
results_q[[duncan_eth]] <- NA_real_
duncan_ethsc <- "DuncanEthSc" 
results_q[[duncan_ethsc]] <- NA_real_
duncan_sc <- "DuncanSc"
results_q[[duncan_sc]] <- NA_real_
duncan_sceth <- "DuncanScEth"
results_q[[duncan_sceth]] <- NA_real_ 
exposure_eth <- "ExpEth"
results_q[[exposure_eth]] <- NA_real_
exposure_ethsc <- "ExpEthSc"
results_q[[exposure_ethsc]] <- NA_real_
exposure_sc <- "ExpSC"
results_q[[exposure_sc]] <- NA_real_
exposure_sceth <- "ExpScEth"
results_q[[exposure_sceth]] <- NA_real_
diveth <- "ShanEth"
results_q[[diveth]] <- NA_real_
divstatus <- "ShanStatus"
results_q[[divstatus]] <- NA_real_

for (r in seq_len(nrow(results_q))) {
  
  df_y <- df_final %>%
    filter(year == results_q$year[r]) %>%
    mutate(
      not_ethnic =  pop_borough - .data[[paste0("total_", sub("^[^_]*_", "", group_tocompute))]],
      not_sc = pop_borough - .data[[paste0("total_", q)]]
    )
  
  results_q[[duncan_eth]][r] <- duncan_two_groups(
    df_y,
    comparison_group = "not_ethnic",
    subgroup = results_q$group[r]
  )
  
  results_q[[duncan_ethsc]][r] <- duncan_comp(
    df_y,
    other_group = paste0("total_", sub("^[^_]*_", "", group_tocompute)),
    subgroup = results_q$group[r]
  )
  
  results_q[[duncan_sc]][r] <- duncan_two_groups(
    df_y ,
    comparison_group = "not_sc",
    subgroup = results_q$group[r]
  )
   
  results_q[[duncan_sceth]][r] <- duncan_comp(
    df_y,
    other_group = paste0("total_", q),
    subgroup = results_q$group[r]
  )
   
  results_q[[exposure_eth]][r] <- exposure_index(
    df_y,
    subgroup =  results_q$group[r],
    total_pop = "pop_borough",
    reference_group = paste0("total_", sub("^[^_]*_", "", group_tocompute))
  )
  
  results_q[[exposure_ethsc]][r] <- exposure_index(
    df_y,
    subgroup =  results_q$group[r],
    total_pop = paste0("total_", sub("^[^_]*_", "", group_tocompute)),
    reference_group = results_q$group[r]
  )
  
  results_q[[exposure_sc]][r] <- exposure_index(
    df_y,
    subgroup =  results_q$group[r],
    total_pop = "pop_borough",
    reference_group = paste0("total_", q)
  )
  
  results_q[[exposure_sceth]][r] <- exposure_index(
    df_y,
    subgroup =  results_q$group[r],
    total_pop = paste0("total_", q),
    reference_group = results_q$group[r]
  )
  
  results_q[[diveth]][r] <- diversity_exposure(
    df_y,
    subgroup = results_q$group[r],
    diversity_var = "shannon_ethnic_norm"
  )
  
  results_q[[divstatus]][r] <- diversity_exposure(
    df_y,
    subgroup = results_q$group[r],
    diversity_var = "shannon_status_norm"
  )
  
}
results[[q]] <- results_q
}
  
results_final <- bind_rows(results)
write_xlsx(results_final,paste0("index_table/",group_tocompute,".xlsx"))
results_final
}


# Tables for segregation indices

df_list <- list()
for (n in c(
  "asian_bangladeshi",
  "asian_indian",
  "asian_pakistani",
  "asian_chinese",
  "black_african",
  "black_carribean",
  "white_ewsnib",
  "white_irish",
  "mixed_whiteasian",
  "mixed_whiteblcarribean",
  "mixed_whiteblafrican")){
 
 df_list[[n]] <- index_groups(n)
 
}
df_index <- bind_rows(df_list, .id = "major_minor")
write_xlsx(df_index,"index_table/df_index.xlsx")

# function to plot customized index (x-axis = year, y = index, group = group1, fecet = facetvar)

plotfig <- function(var1,group1, facetvar,ylabb,colorlabb) {
  
  p <- df_index %>%
    ggplot(aes(x = year, y = .data[[var1]])) +
    geom_point(aes(color =  .data[[group1]], group = .data[[group1]] )) +
    geom_line(aes(color = .data[[group1]], group = .data[[group1]])) +
    scale_x_continuous(breaks = c(2001, 2011, 2021)) + 
    facet_wrap(vars(.data[[facetvar]]), nrow = 1) +
    labs(
      y = ylabb,
      colour = colorlabb
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
 
  ggsave(
    filename = paste0("plotfacet/", var1, "_", group1, "_" ,facetvar,  ".jpg"),
    plot = p,
    width = 13
  )
  
  p
}

# Here every single index can be plotted

ethduncan <- plotfig(var1 = "DuncanEth",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Ethnic Duncan", colorlabb = "Social Status")
ethscduncan <- plotfig(var1 = "DuncanEthSc",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Within-ethnic Class Duncan", colorlabb = "Social Status")
socduncan <- plotfig(var1 = "DuncanSc",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Class Duncan", colorlabb = "Social Status")
scethduncan <- plotfig(var1 = "DuncanScEth",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Within-class Ethnic Duncan", colorlabb = "Social Status")
plotfig(var1 = "ExpEth",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Ethnic Exposure", colorlabb = "Social Status")
plotfig(var1 = "ExpSC",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Class Exposure", colorlabb = "Social Status")
plotfig(var1 = "ExpEthSc",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Within-ethnic Class Exposure", colorlabb = "Social Status")
plotfig(var1 = "ExpScEth",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Within-class Ethnic Exposure", colorlabb = "Social Status")
shaneth <- plotfig(var1 = "ShanEth",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Ethnic Shannon", colorlabb = "Social Status")
shansc <- plotfig(var1 = "ShanStatus",group1 = "socstatus",facetvar = "ethnicspecific", ylabb = "Class Shannon", colorlabb = "Social Status")

(ethduncan / socduncan) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("plotfacet/ethsoc_duncan.jpg", width = 12, height = 8)

(ethscduncan / scethduncan) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("plotfacet/within_duncan.jpg", width = 12, height = 8)

(shaneth / shansc) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("plotfacet/shannonwithin.jpg", width = 12, height = 8)

# sanity check
# to check distribution for each subgroup
# subgroup <- "asian_chinese_sc1"
# df_final %>%
# filter(year == 2021) %>%
# arrange(desc(.data[[subgroup]])) %>%
# select(borough, all_of(subgroup))





# Moran's I local

borough_sf <- shapefile_df %>%
 group_by(borough, year) %>%
  summarise(
   frac_total_asian = first(frac_total_asian),
   .groups = "drop"
  )

borough_sf_2001 <- borough_sf %>%
  filter(year == 2001)

nb <- poly2nb(borough_sf_2001)
lw <- nb2listw(nb)

localI <- localmoran(
  borough_sf_2001$frac_total_asian,
  lw
)

borough_sf_2001$localI <- localI[, "Ii"]
borough_sf_2001$localI_p <- localI[, "Pr(z != E(Ii))"]

ggplot(borough_sf_2001) +
  geom_sf(aes(fill = localI)) +
  theme_void()

mean_asian <- mean(borough_sf_2001$frac_total_asian, na.rm = TRUE)

lag_asian <- lag.listw(
  lw,
  borough_sf_2001$frac_total_asian
)

borough_sf_2001$lag_asian <- lag_asian
borough_sf_2001 %>%
  filter(borough == "harrow") %>%
  select(frac_total_asian, lag_asian, localI, localI_p)
