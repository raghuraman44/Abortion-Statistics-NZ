# Author: Raghuraman Srinivasan
# Date: 13 February 2021

# ------------------------------------------------------------------------------------------------------------------------------------

# Note: Install the required Packages of all the libraries that are used below
# All of the libraries are CRAN and other external libraries are not used

# ------------------------------------------------------------------------------------------------------------------------------------

# Loading the Libraries
# Shiny Dashboard Basics
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinybusy)
library(shinydashboard)

# Data Manipulation
library(datasets)
library(DT)
library(tidyverse)
library(dplyr)
library(summarytools)
library(lubridate)
library(incidence2)

# Data Visualization
library(ggpubr)
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(systemfonts)
library(hrbrthemes)
library(viridis)

# Forecast Analysis
library(forecast)

# ------------------------------------------------------------------------------------------------------------------------------------

      # Data Wrangling, Preprocessing and Feature Engineering

# ------------------------------------------------------------------------------------------------------------------------------------

# Loading the Datasets provided as a part of the task
# Replacing the Missing Fields by inserting Null values

# AbortionRate contains number of Abortions per 1000 Women (Year Wise 1980 to 2019)
AbortionRate <- read.csv("as-dec-19-general-abortion-rate.csv", header = TRUE, na.strings = c("#N/A", "", "Unassigned", "N/A"))

# AbortionCount contains the total number of Abortions (Year Wise 1980 to 2019)
AbortionCount <- read.csv("as-dec-19-induced-abortions.csv", header = TRUE, na.strings = c("#N/A", "", "Unassigned", "N/A"))

# AbortionRateAge contains number of Abortions per 1000 Women (Year Wise (2000 to 2019) across different Age groups)
AbortionRateAge <- read.csv("as-dec19-abortion-rates-by-age-of-woman.csv", header = TRUE, na.strings = c("#N/A", "", "Unassigned", "N/A"))

# AbortionCountAge contains the total number of Abortions (Year Wise (2000 to 2019) across different Age groups)
AbortionCountAge <- read.csv("as-dec19-abortions-by-age-of-woman.csv", header = TRUE, na.strings = c("#N/A", "", "Unassigned", "N/A"))

# Removing all the null columns in the datasets occurring due to nature of dataset
AbortionRate <- AbortionRate[,colSums(is.na(AbortionRate))<nrow(AbortionRate)]
AbortionCount <- AbortionCount[,colSums(is.na(AbortionCount))<nrow(AbortionCount)]
AbortionRateAge <- AbortionRateAge[,colSums(is.na(AbortionRateAge))<nrow(AbortionRateAge)]
AbortionCountAge <- AbortionCountAge[,colSums(is.na(AbortionCountAge))<nrow(AbortionCountAge)]

# Combining the Datasets based on Year using inner join
AbortionCR <- inner_join(AbortionRate, AbortionCount)
AbortionCRAge <- inner_join(AbortionRateAge, AbortionCountAge)

# Removing the initial datasets after joining the datasets
remove(AbortionRate)
remove(AbortionCount)
remove(AbortionRateAge)
remove(AbortionCountAge)

# Removing all the duplicates by using the combination of Period and Age Group as a Primary Key
AbortionCRAge <- AbortionCRAge [!duplicated(AbortionCRAge[c(1,2)]),]

# Converting the factor type variables with high cardinality as Character type variables
AbortionCR$Period <- as.character(AbortionCR$Period)
AbortionCRAge$Period <- as.character(AbortionCRAge$Period)
AbortionCRAge$Age_of_woman <- as.factor(AbortionCRAge$Age_of_woman)
AbortionCRAge$Induced_abortions <- as.integer(AbortionCRAge$Induced_abortions)
AbortionCR$Period <- as.factor(AbortionCR$Period)
AbortionCRAge$Period <- as.factor(AbortionCRAge$Period)

# Handling the Missing Values
# The count of Abortions in the AbortionCRAge dataset is missing for certain Age categories for the year 2019
# From the AbortionsCR dataset, we can see that the overall Abortion count for 2019 is less than 2018
# Assumption: Assuming the population in 2019 is same as the population in 2018 
# the following formula can be used to impute the missing values.
# MissingValue = RateIn2019 * (CountRatio)

CountRatio = AbortionCR$Induced_abortions[AbortionCR$Period == "2019"] / AbortionCR$Induced_abortions[AbortionCR$Period == "2018"]
AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2019" & AbortionCRAge$Age_of_woman == "15-19"] <- AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2018" & AbortionCRAge$Age_of_woman == "15-19"] * CountRatio
AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2019" & AbortionCRAge$Age_of_woman == "20-24"] <- AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2018" & AbortionCRAge$Age_of_woman == "20-24"] * CountRatio
AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2019" & AbortionCRAge$Age_of_woman == "25-29"] <- AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2018" & AbortionCRAge$Age_of_woman == "25-29"] * CountRatio
AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2019" & AbortionCRAge$Age_of_woman == "30-34"] <- AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2018" & AbortionCRAge$Age_of_woman == "30-34"] * CountRatio
AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2019" & AbortionCRAge$Age_of_woman == "35-39"] <- AbortionCRAge$Induced_abortions[AbortionCRAge$Period == "2018" & AbortionCRAge$Age_of_woman == "35-39"] * CountRatio

AbortionCR$Women_Population = (AbortionCR$Induced_abortions * 1000) / (AbortionCR$General_abortion_rate)
AbortionCRAge$Women_Population = (AbortionCRAge$Induced_abortions * 1000) / (AbortionCRAge$Abortion_rate)

# ------------------------------------------------------------------------------------------------------------------------------------

        # Forecast Modeling - Predictive Analysis 

# ------------------------------------------------------------------------------------------------------------------------------------

# Creating a Date Column for Timeseries Analysis and Forecast Modeling
AbortionCR$Date <- as.Date(paste(AbortionCR$Period, 12, 31, sep = "-"))
AbortionCRAge$Date <- as.Date(paste(AbortionCRAge$Period, 12, 31, sep = "-"))

# Performing an optimized ARIMA Model to forecast the Abortion Rate from 2020 to 2025
# Converting the Dataset into a Timeseries Data for Annual Frequency 
tsdata <- ts(AbortionCR$General_abortion_rate)

plot(tsdata)
# Creating an ARIMA model optimised using auto which returns the best ARIMA model
ArimaModel <- auto.arima(tsdata)

# Forecasting Abortion Rate trends till 2025
ArimaForecast <- forecast(ArimaModel, h=6)

head(AbortionCR)
             