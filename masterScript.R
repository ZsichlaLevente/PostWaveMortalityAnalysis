#### Pull data if necessary
#source("C:/Users/leven/OneDrive/Dokumentumok/Dokumentumok/Work/Projects/COVID-19/YPL Project/_PostWaveMortalityAnalysis/pullData.R")

#### Libraries
library(tidyverse)
library(data.table)
library(gsubfn)
library(lubridate)
library(ggpubr)
library(tidyquant)
library(ggformula)


#### Load data
# set working directory
setwd("C:/Users/leven/OneDrive/Dokumentumok/Dokumentumok/Work/Projects/COVID-19/YPL Project/_PostWaveMortalityAnalysis/Data")
# Read in data
owid<-read_csv(file="owid_covid_data.csv")
owidEM<-read_csv(file="owid_excess_mortality.csv")
stmf<-read_csv(file="stmf.csv",skip=1)
ecdc <- read.csv("ecdc.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
who<-read_csv(file="who.csv")
COVerAGE<-fread(file="inputDB.csv",skip = 1)

#### Process COVerAGE dataset
source("C:/Users/leven/OneDrive/Dokumentumok/Dokumentumok/Work/Projects/COVID-19/YPL Project/_PostWaveMortalityAnalysis/COVerAGEmanipulation.R")
list[COVerAGE, countryList]<-processCOVerAGE(data=COVerAGE)

#### Load data visualization functions
source("C:/Users/leven/OneDrive/Dokumentumok/Dokumentumok/Work/Projects/COVID-19/YPL Project/_PostWaveMortalityAnalysis/DataVisualization.R")

#### Visualization

## visualize stmf dataset (make plots for all age categories)
# Args: country or region (options: AUS, AUT, BEL BGR, CAN, CHE, CHL, CZE, DEUTNP, DNK, ESP, EST, FIN, FRATNP, GBRTENW, GBR_NIR, GBR_SCO, GRC, HRV, HUN, ISL, ISR, ITA, KOR, LTU, LUX, LVA, NLD, NOR, NZL_NP, POL, PRT, RUS, SVK, SVN, SWE, TWN, USA)
#       sex (options: b,m,f)
#       target year (options: 2020-2022)
#       measure (options: deaths, deathrates)
#       reference years (options: 1990-2019)
#       reference level calculation (options: week-specific averages (need more?))
# show: reference line (blue); 
#       previous years (grey); 
#       target year (black); 

showExcess(CCode = "GBR_NIR",sex="b",targetYear = 2021,measure="deathrates",refYears=2010:2019,scaleAll=F)

## show p scores by age group from the OWID excess mortality table
# Args: country or region (options: unique(owidEM$location))
#       target year (options: 2020-2022)
#       measure (options: p_scores, p_proj)
# show: reference line (blue); 
#       target year (black); 

showPScores(Location = "Italy",targetYear = 2021,measure="p_scores",scaleAll=T)

## compare reported number of deaths in the who and COVerAGE datasets
# Args: none
# show: who (black);
#       COVerAGE (red)
# Plots: 1. Weekly number of COVID-19 deaths by sex (15 countries)
#        2. Weekly number of COVID-19 deaths by age (10 countries)
#        3. Weekly number of deaths compared to the WHO estimates (19 countries)

showCOVerAGErestoration()

## compare excess mortality P scores and COVID-19 mortality
showPscoresAndDeaths()

#### Association between excess mortality P scores and COVID-19 mortality (age- and sex disaggregated analysis)


