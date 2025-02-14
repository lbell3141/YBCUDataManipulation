#Combine survey summary data

#Load necessary packages
library(dplyr)

#Define file paths
PathtoPre2022Data <- "./Data/pre2022data/SurveySummary_14022025.csv"
PathtoPost2022Data <- "./Data/post2022data/SurveySummary_14022025.csv"

#Load data
OldDat <- read.csv(PathtoPre2022Data)
NewDat <- read.csv(PathtoPost2022Data)

#Rename columns found in both sheets to a common name
#Add an identifier to each sheet: "pre" for the pre2022 data and "post" for the post2022 data
OldRenamed <- OldDat%>%
  rename(SiteYrID = ID,
         WaterBody = WaterbodyName,
         UTMZone = Zone, 
         UTMStartX = StartX_UTME,
         UTMStartY = StartY_UTMN,
         LatStart =Start_Lat,
         LonStart =Start_Long,
         UTMEndX = StopX,
         UTMEndY = StopY,
         LatEnd = StopLat,
         LonEnd = StopLong,
         SurveyLength = Survey.Length..KM.,
         SurveyArea = surveyArea_ha,
         TotalSurveys = X..Surveys,
         TotalHours = X..Survey.Hours,
         TotalDetections = NumDetected,
         PO = PO.Territories,
         PR = PR.Territories,
         CO = CO.Territories,
         TotalNests = NumNests)%>%
  mutate(Version = "Pre")
NewRenamed <- NewDat%>%
  rename(SiteYrID = SITEYRID,
         SiteName = Site.Name,
         WaterBody = Water.Body..creek..river.canyon.or.lake.name.,
         UTMStartX = Start_X,
         UTMStartY = Start_Y,
         LatStart =Start_Lat,
         LonStart =Start_Long,
         UTMEndX = Stop_X,
         UTMEndY = Stop_Y,
         LatEnd = Stop_Lat,
         LonEnd = Stop_Long,
         TotalSurveys = NumSurveys,
         TotalDetections = Detections,
         TotalNests = NumNests)%>%
  mutate(Version = "Post",
         TotalSurveys = as.character(TotalSurveys))

#Combine old and new data sheets and rearrange important columns to be at the front of the df 
FullDat <- bind_rows(OldRenamed, NewRenamed)%>%
  select(c(Version, 
           SiteYrID, 
           Year, 
           SurveyType, 
           SiteName, 
           County, 
           WaterBody, 
           SurveyLength, 
           SurveyArea, 
           TotalHours, 
           TotalDetections, 
           PO, 
           PR, 
           CO, 
           TotalNests), 
         everything())

#Convert UTMs to lat/lon