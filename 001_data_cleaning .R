
#=================================================================================================================================================
# STEP 0. Package Setup and Initialization 
#=================================================================================================================================================
library(RMySQL)
library(dplyr)

# Check which user is running the script
user <- Sys.getenv("HOME")

# SET YOUR LOCAL WORKING DIRECTORY HERE 
# pwd <- ...

if (user == "/Users/yanjin.li"){
  # Set working directory
  setwd("/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Advanced Data Analysis /ADA_porject")
  # Set local saving directory
  save.path <- "/export_dataframes"
} else {
  
  # Set working directory
  setwd(pwd)
  # Set local saving directory
  # save.path <- "/export_dataframes"
}

#=================================================================================================================================================
# STEP 1. Dataset Preparation 
#=================================================================================================================================================
datraw.2011 <- read.csv("original_dataframes/Energy_and_Water_Data_Disclosure_for_Local_Law_84__2011_.csv",
                       stringsAsFactors = FALSE
                       #, na.strings = c("", "#NULL!", "#REF!", "Does NotApply")
                       )

datraw.2012 <- read.csv("original_dataframes/Energy_and_Water_Data_Disclosure_for_Local_Law_84__2012_.csv",
                        stringsAsFactors = FALSE
                        #, na.strings = c("", "#NULL!", "#REF!", "Does NotApply")
                        )

datraw.2013 <- read.csv("original_dataframes/Energy_and_Water_Data_Disclosure_for_Local_Law_84__2013_.csv",
                        stringsAsFactors = FALSE
                        #, na.strings = c("", "#NULL!", "#REF!", "Does NotApply")
                        )

datraw.2014 <- read.csv("original_dataframes/Energy_and_Water_Data_Disclosure_for_Local_Law__2014_.csv",
                        stringsAsFactors = FALSE
                        #, na.strings = c("", "#NULL!", "#REF!", "Does NotApply")
                        )

#=================================================================================================================================================
# STEP 2. Data Cleaning 
#=================================================================================================================================================
# 2.1 Data Clearning (# Shorten column names  )
# Year of 2012 
dat.2012 <- datraw.2012 %>% 
  rename(Weather.Normalized.Source = Weather.Normalized.Source.EUI.kBtu.ft2.,
         Indoor.Water.Intensity = Indoor.Water.Intensity..All.Water.Sources..gal.ft2., # Need to be recheck? 
         Total.GHG.Emissions = Total.GHG.Emissions.MtCO2e.,
         Property.Buildngs.and.Parking = Property.Floor.Area..Buildngs.and.Parking..ft2.,
         Primary.Property.Type = Primary.Property.Type...Self.Selected) %>%
  select(BBL, Street.Number, Street.Name, Borough,
         Zip, Benchmarking.Submission, Site.EUI.kBtu.ft2., 
         Weather.Normalized.Source, Indoor.Water.Intensity,
         Reported.Water.Method, ENERGY.STAR.Score, 
         Total.GHG.Emissions, Property.Buildngs.and.Parking,
         Primary.Property.Type, Number.of.Buildings, Reported.BINs) %>%
  mutate(Record.Number = NA, BBL.status = NA, BBLs.Co.reported = NA, 
         Weather.Normalized.Site.EUI.kBtu.ft2. = NA, Source.EUI.kBtu.ft2. = NA,
         Automatic.Water.Benchmarking.Eligible = NA, Direct.GHG.Emissions.MtCO2e. = NA,
         Indirect.GHG.Emissions.MtCO2e. = NA, Reported.Property.building = NA,
         BBL.on.the.Covered.Buildings.List = NA, 
         # Creat a time/year indicator 
        year = 2012)

# Year of 2013   
dat.2013 <- datraw.2013 %>% 
  rename(BBL = NYC.Borough..Block..and.Lot..BBL., 
         Zip = Zip.Code, 
         Benchmarking.Submission = DOF.Benchmarking.Submission.Status,
         Weather.Normalized.Source = Weather.Normalized.Source.EUI.kBtu.ft2.,
         Indoor.Water.Intensity = Municipally.Supplied.Potable.Water...Indoor.Intensity..gal.ft.., # Need to be recheck? 
         Total.GHG.Emissions = Total.GHG.Emissions.MtCO2e.,
         Property.Buildngs.and.Parking = DOF.Property.Floor.Area..Buildngs.and.Parking..ft2.,
         Primary.Property.Type = Primary.Property.Type...Self.Selected,
         Number.of.Buildings = DOF.Number.of.Buildings,
         Reported.BINs = Reported.NYC.Building.Identificaiton.Numbers..BINs.,
         BBL.status = Co.reported.BBL.Status,
         Reported.Property.building = Reported.Property.Floor.Area..Building.s....ft..) %>%
  select(BBL, Street.Number, Street.Name, Borough,
         Zip, Benchmarking.Submission, Site.EUI.kBtu.ft2., 
         Weather.Normalized.Source, Indoor.Water.Intensity,
         Reported.Water.Method, ENERGY.STAR.Score, 
         Total.GHG.Emissions, Property.Buildngs.and.Parking,
         Primary.Property.Type, Number.of.Buildings, 
         Reported.BINs, Record.Number, BBL.status, BBLs.Co.reported, 
         Weather.Normalized.Site.EUI.kBtu.ft2., Source.EUI.kBtu.ft2.,
         Automatic.Water.Benchmarking.Eligible, Direct.GHG.Emissions.MtCO2e.,
         Indirect.GHG.Emissions.MtCO2e., Reported.Property.building) %>%
  mutate(BBL.on.the.Covered.Buildings.List = NA, 
         # Creat a time/year indicator 
         year = 2013)


# Year of 2014
dat.2014 <- datraw.2014 %>% 
  rename(BBL = NYC.Borough..Block..and.Lot..BBL., 
         Zip = Zip.Code, 
         Benchmarking.Submission = DOF.Benchmarking.Submission.Status,
         Weather.Normalized.Source = Weather.Normalized.Source.EUI.kBtu.ft2.,
         Indoor.Water.Intensity = Municipally.Supplied.Potable.Water...Indoor.Intensity..gal.ft.., # Need to be recheck? 
         Total.GHG.Emissions = Total.GHG.Emissions.MtCO2e.,
         Property.Buildngs.and.Parking = DOF.Property.Floor.Area..Buildngs.and.Parking..ft2.,
         Primary.Property.Type = Primary.Property.Type...Self.Selected,
         Number.of.Buildings = DOF.Number.of.Buildings,
         Reported.BINs = Reported.NYC.Building.Identification.Numbers..BINs.,
         BBL.status = Co.reported.BBL.Status,
         Reported.Property.building = Reported.Property.Floor.Area..Building.s....ft..) %>%
  select(BBL, Street.Number, Street.Name, Borough,
         Zip, Benchmarking.Submission, Site.EUI.kBtu.ft2., 
         Weather.Normalized.Source, Indoor.Water.Intensity,
         Reported.Water.Method, ENERGY.STAR.Score, 
         Total.GHG.Emissions, Property.Buildngs.and.Parking,
         Primary.Property.Type, Number.of.Buildings, 
         Reported.BINs, Record.Number, BBL.status,
         BBLs.Co.reported, Weather.Normalized.Site.EUI.kBtu.ft2.,
         Source.EUI.kBtu.ft2., Automatic.Water.Benchmarking.Eligible,
         Direct.GHG.Emissions.MtCO2e., Indirect.GHG.Emissions.MtCO2e.,
         Reported.Property.building, BBL.on.the.Covered.Buildings.List) %>%
  # Creat a time/year indicator 
  mutate(year = 2014)


# Rbind data from year 2012, 2013, and 2014 into a complete dataframe 
dat.all <- rbind(dat.2012, dat.2013, dat.2014) 
# Make a copy 
datclean.all <- dat.all

# Save dataframe to drive 
write.csv(datclean.all, "export_dataframes/datclean.all.csv")
save(datclean.all, file="export_dataframes/datclean.all")

# Test on Data-loading
load("export_dataframes/datclean.all")













