#format survey data
library(dplyr)
library(lubridate)
library(oce)

#Define file paths
PathtoPre2022Data <- "./Data/pre2022data/VisitSummary_14022025.csv"
PathtoPost2022Data <- "./Data/post2022data/VisitSummary_14022025.csv"
PathtoPost2022Summaries <- "./Data/post2022data/SurveySummary_14022025.csv"
PathtoSurvSumOutput <- "./Data/Outputs/SurveySummary_19022025.csv"
  
PathtoOutput <- "./Data/Outputs/SurveyVisit_18022025.csv"

#Load data
OldDat <- read.csv(PathtoPre2022Data)
NewDat <- read.csv(PathtoPost2022Data)
NewSurvSum <- read.csv(PathtoPost2022Summaries)
OutSurvSum <- read.csv(PathtoSurvSumOutput)

#Rename columns found in both sheets to a common name
#Add an identifier to each sheet: "pre" for the pre2022 data and "post" for the post2022 data
OldRenamed <- OldDat%>%
  rename(SiteYrID = ID,
         DetectionType = DetectType,
         TimeDetected = TimeDetect,
         UTMDetectionX = Detection_X,
         UTMDetectionY = Detection_Y,
         LatDetection = Lat,
         LonDetection = Long,
         UTMCorrectedX = Corrected_X,
         UTMCorrectedY = Corrected_Y)%>%
  mutate(Version = "Pre")%>%
  filter(SiteYrID != "")
NewRenamed <- NewDat%>%
  rename(SiteYrID = SITEYRID,
         YBCUNumber = YBCUNum,
         NumCalls = NumCallsResponse,
         UTMDetectionX = Detection_X,
         UTMDetectionY = Detection_Y,
         LatDetection =Detection_Lat,
         LonDetection =Detection_Long,
         UTMCorrectedX = Corrected_X,
         UTMCorrectedY = Corrected_Y)%>%
  mutate(Version = "Post",
         SurveyDate = mdy(SurveyDate),
         Year = year(SurveyDate),
         SiteYrID = as.character(SiteYrID),
         SurveyPeriod = as.character(SurveyPeriod))%>%
  mutate(SurveyDate = as.character(SurveyDate),
         YBCUNumber = as.character(YBCUNumber))
#extract transect names to add to new data
TransectNames <- NewSurvSum%>%
  select(SITEYRID, Site.Name)%>%
  rename(SiteYrID = SITEYRID,
         SiteName = Site.Name)
AddSiteNames <- merge(NewRenamed, TransectNames, by = "SiteYrID")

#merge old and new data into one df
FullVisitDat <- bind_rows(OldRenamed, AddSiteNames)%>%
  select(c(Version, 
           SiteYrID, 
           Year, 
           SurveyType, 
           SiteName), 
         everything())

#Convert UTMs to lat/long
#make a dataframe with position data and identifiers; convert data to numeric, leaving non-numeric entires as NA
PosDat <- FullVisitDat%>%
  select(Version, SiteYrID, UTMDetectionX, UTMDetectionY, UTMCorrectedX, UTMCorrectedY, LatDetection, LonDetection)%>%
  mutate(SiteYrID = as.numeric(SiteYrID))
PosDat <- PosDat %>%
  mutate(across(c(UTMDetectionX, UTMDetectionY, UTMCorrectedX, UTMCorrectedY, LatDetection, LonDetection), 
                ~ as.numeric(.)))
#add UTM zones from summary sheet
zones <- OutSurvSum%>%
  select(Version, SiteYrID, UTMZone)
#make columns of data to use in the conversion: use corrected UTM values first, then supplement with raw UTM values for entired where corrected values are unavailable
PosDatwZone <- left_join(PosDat, zones, by = c("Version", "SiteYrID"))%>%
  mutate(UTMforConversionX = case_when(!is.na(UTMCorrectedX) ~ UTMCorrectedX,
                                      is.na(UTMCorrectedX) ~ UTMDetectionX),
         UTMforConversionY = case_when(!is.na(UTMCorrectedY) ~ UTMCorrectedY,
                                       is.na(UTMCorrectedY) ~ UTMDetectionY))%>%
  mutate(UTMZoneConversion = case_when(!is.na(UTMZone) ~ UTMZone,
                                       is.na(UTMZone)~ 12))
#do conversion, adding new columns for the calculated lat/lon
ConvertedUTM <- utm2lonlat(PosDatwZone$UTMforConversionX, PosDatwZone$UTMforConversionY, zone = PosDatwZone$UTMZoneConversion, hemisphere = "N")%>%
  bind_rows()%>%
  rename(calc_Lat = latitude,
         calc_Lon = longitude)
#add calculated values back to the data frame with all position data
#Supplement manually entered lat/lon with the calculated values
PosDatCalc <- bind_cols(PosDatwZone, ConvertedUTM)%>%
  mutate(Full_Lat = case_when(!is.na(LatDetection)~LatDetection,
                              is.na(LatDetection)~ calc_Lat),
         Full_Lon = case_when(!is.na(LonDetection)~LonDetection,
                              is.na(LonDetection)~ calc_Lon))
#add calculated values back to the data frame with all position data
#format final df and save file to output path. 
FinalLL <- PosDatCalc%>%
  select(Full_Lat, Full_Lon)
DatwLL <- bind_cols(FinalLL, FullVisitDat)%>%
  select(Version, SiteYrID, SurveyVisitID, Year, SiteName, SurveyType, SurveyNumber, SurveyPeriod, SurveyDate, StartTime, StopTime, TotalHours, TotalNumCuckoos,
         YBCUNumber, TimeDetected, DetectionMethod, DetectionType, VocalType, NumCalls, BehObs,
         UTMDetectionX, UTMDetectionY, Distance, Bearing, UTMCorrectedX, UTMCorrectedY, Full_Lat, Full_Lon, Comments)%>%
  rename(LatDetection = Full_Lat, 
         LonDetection = Full_Lon)

write.csv(DatwLL, PathtoOutput, row.names = F)
