#' ---
#' title: "Kings County Washington COVID-19 Cases Count and Income data."
#' output: html_document
#' date: "2024-02-20"
 
## --------------------------------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
cov_df <- read_excel("COVID-19 DATA CENSUS TRACK.xlsx")
head(cov_df)

#' 
## --------------------------------------------------------------------------------------------------------------------
library(readr)
income_df <- read_csv("Income by Location 10.csv")
income_df <- income_df %>%
  filter(Year %in% c(2019, 2020, 2021))
head(income_df)

#' 
## --------------------------------------------------------------------------------------------------------------------
income_df$geo_id <- as.numeric(sub("14000US", "", income_df$`ID Geography`))

merged_df <- merge(income_df, cov_df, by = "geo_id", all = TRUE) 
merged_df


#' 
## --------------------------------------------------------------------------------------------------------------------
sorted_df <- merged_df[order(merged_df$Year), ]

row.names(sorted_df) <- NULL
head(sorted_df)

## --------------------------------------------------------------------------------------------------------------------
sorted_df <- select(sorted_df, -pop)
sorted_df <- select(sorted_df, -`ID Year`)
sorted_df

## --------------------------------------------------------------------------------------------------------------------

colSums(is.na(sorted_df))

#' 
## --------------------------------------------------------------------------------------------------------------------
# Removing all rows that have any NA values from the dataframe
clean_df <- na.omit(sorted_df)
head(clean_df)
colSums(is.na(clean_df))

## --------------------------------------------------------------------------------------------------------------------
clean_df <- clean_df %>%
  rename(Income =`Household Income by Race` )

#' 
## --------------------------------------------------------------------------------------------------------------------

clean_df$Cases_Per_Thousand_Dollar_Income <- with(clean_df, case_count / (Income / 1000))
clean_df

## --------------------------------------------------------------------------------------------------------------------

library(dplyr)

clean_df <- clean_df %>%
  mutate(Income_Level = case_when(
    Income <= 20000 ~ "Low",
    Income > 20000 & Income <= 50000 ~ "Medium",
    Income > 50000 ~ "High",
  ))
colSums(is.na(clean_df))


## --------------------------------------------------------------------------------------------------------------------

summary_df <- clean_df %>%
  group_by(Income_Level) %>%
  summarise(across(c(Income, case_count, hosp_count,death_count,total_hosp_incidental,Cases_Per_Thousand_Dollar_Income ), list(
    mean = ~mean(. ,na.rm=TRUE),
    median = ~median(. ,na.rm=TRUE),
    min = ~min(. ,na.rm=TRUE),
    max = ~max(. ,na.rm=TRUE),
    IQR = ~IQR(., na.rm=TRUE)
  ))
  )
summary_df 
st(clean_df,group = "Income_Level")

#' 
## --------------------------------------------------------------------------------------------------------------------
write.csv(clean_df, "cleaned_dataframe.csv", row.names = TRUE)


#' 
