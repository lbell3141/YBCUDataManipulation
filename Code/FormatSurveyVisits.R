#format survey data
library(dplyr)
library(lubridate)

#Define file paths
PathtoPre2022Data <- "./Data/pre2022data/VisitSummary_14022025.csv"
PathtoPost2022Data <- "./Data/post2022data/VisitSummary_14022025.csv"
PathtoPost2022Summaries <- "./Data/post2022data/SurveySummary_14022025.csv"

#Load data
OldDat <- read.csv(PathtoPre2022Data)
NewDat <- read.csv(PathtoPost2022Data)
NewSurvSum <- read.csv(PathtoPost2022Summaries)

#Rename columns found in both sheets to a common name
#Add an identifier to each sheet: "pre" for the pre2022 data and "post" for the post2022 data
OldRenamed <- OldDat%>%
  rename(SiteYrID = ID,
         DetectionType = DetectType,
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
         everything())%>%
  filter(SiteYrID != "")

#Convert UTMs to lat/long
#pull zone from summary sheet
#if a corrected value DNE, use the raw value
#if a lat/lon values exists, supplement calulates values with this
#working in separate df for clarity
PosDat <- FullVisitDat%>%
  select(SiteYrID, UTMDetectionX, UTMDetectionY, UTMCorrectedX, UTMCorrectedY, LatDetection, LonDetection)
zones <- YBCU_data%>%
  select(SiteYrID, UTMZone)
PosDatwZone <- merge(PosDat,zones, by = "SiteYrID")