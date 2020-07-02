# Seasonal Kendall Test, Semi-Automated
# Author: Wes Slaughter, Sierra Streams Institute
# Date: 5/6/2020

# Ref EPA document on trend analysis. Some important things 
# not done in this data analysis: Flow weighted (FWMC) and 
# time weighted mean concentration (TWMC)

# This script also uses imputatation through MICE package (Multiple Imputation thrrough Chained Equations)
# Seed=1 and m=9. These can be changed easily. Imputation is a method for "guessing" missing values.
# Seed is set so the random number generator is controlled, insuring the same results every run
# m=9 because literature suggests 5-10 chained equations are sufficient for accuracy
# some literature suggests up to 40, which can sacrifice run-time, but with datasets this small, is not too long

# Load the necessary packages

# Note/Warning: if not already present, this code will install them
# on your computer!

if(!require(mice)){install.packages("mice")}
if(!require(Kendall)){install.packages("Kendall")}
if(!require(trend)){install.packages("trend")}

library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)

setwd("C:/Users/Wes/Documents/SSI/Climate Data Project")

# First need to load in desired dataframe,
# in thise case, SSI wq 2001-2020
df <- read.csv("Climate Data Raw/Water Quality/ForR/Master_DC_WQ.csv")

# Create time series that matches your data
# x= start date, "mm/dd/yyyy"
# x= end date, "mm/dd/yyyy"

timer <- function(x,y){
  data.frame(
    Month= month(seq(as.POSIXlt(x,format="%m/%d/%Y"),as.POSIXlt(y,format="%m/%d/%Y"),by="month")),
    Year= year(seq(as.POSIXlt(x,format="%m/%d/%Y"),as.POSIXlt(y,format="%m/%d/%Y"),by="month")))
}

# in this case, I want March 2004, to Feb 2020
ts_ssi <- timer("3/1/2004","2/1/2020")

#### Semi-Auto Test & Report Functions #####

#  Automatic Seasonal Mann-Kendall Test (AutoSMK)
#  d= dataframe (wq data)
#  t= timeframe (timeseries from 'timer' function)
#  q= parameter, in ""
#  x= Site
#  y = start time yr
#  z = start time mo

AutoSMK <- function(d,t,q,x,y,z) {
  # Create new dataframe with just Site, Month, Year, 
  # Parameter, in that order. Built using match() for flexibility
  d[,c(match("Site",colnames(d)),
       match("Month",colnames(d)),
       match("Year",colnames(d)),
       match(q,colnames(d)))]%>%
    # Subset to desire site  
    subset(Site == x) %>%
    distinct(Year,Month,Site, .keep_all = TRUE)%>%
    merge(t,by=c("Year","Month"),all.y=TRUE)%>%
    select(-3) %>%
    # Imputing missing values. This step will have more or less impact depending on
    # the completeness of your dataset.   
    mice(seed = 1, m=10)%>%
    complete()%>%
    select(3)%>%
    ts(
      frequency=12,
      start=c(y,z))
}


#  Automatic Report of Test Results
#  x= AutoSMK product
#  y= title, likely "Site 'x', parmater 'q'"

AutoReport <- function(x,y){
  sink('trend-analysis_site.txt')
  print(y)
  summary(SeasonalMannKendall(x))
  print(sea.sens.slope(x))
  sink()
}

# Example, SSI

AutoSMK(df,ts_ssi,"NO3",18,2004,3) %>%
AutoReport("Site 18 NO3")

# A txt file should have been written into your working directory
# titled 'trend-analysis_site.txt'

# Example, SYRCL

# Important: datframes need a "Month" and "Year" column to work
# with AutoSMK
# Reformatting date, and creating new DF with Month and Year

SyDF <- read.csv("C:/Users/Wes/Documents/SSI/Covid-19 Response/TrendAnalysis/CSV/SYRCL/WaterQualityData.csv")
SyDF$Date <- as.Date(SyDF$Date,format="%m/%d/%Y")
SyDF_cl <-  transform(SyDF,  
                Month = month(as.POSIXlt(SyDF$Date,format="%m/%d/%Y")),
                Year = year(as.POSIXlt(SyDF$Date,format="%m/%d/%Y")))

# AutoSMK on SYRCL data
ts <- timer("10/10/2000","9/18/2018")

# Run analysis, print results to working directory
AutoSMK(SyDF_cl,ts,"Turb_Mean",2,2005,1) %>%
  AutoReport("Site 2, Mean Turbidity")

# Plotting timeseries
SyASMK <- AutoSMK(SyDF_cl,ts,"Turb_Mean",2,2005,1)
plot(SyASMK)
plot(decompose(SyASMK))
