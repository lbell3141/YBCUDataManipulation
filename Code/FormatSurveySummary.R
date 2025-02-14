#Combine survey summary data

library(dplyr)


PathtoPre2022Data <- "./Data/pre2022data/SurveySummary_14022025.csv"
PathtoPost2022Data <- "./Data/post2022data/SurveySummary_14022025.csv"

OldDat <- read.csv(PathtoPre2022Data)
NewDat <- read.csv(PathtoPost2022Data)

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
         SyrveyArea = surveyArea_ha,
         TotalSurveys = X..Surveys,
         TotalHours = X..Survey.Hours,
         TotalDetections = NumDetected,
         PO = PO.Territories,
         PR = PR.Territories,
         CO = CO.Territories,
         TotalNests = NumNests)
NewRenamed <- NewDat%>%
  rename(WaterBody = Water.Body..creek..river.canyon.or.lake.name.,
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
         TotalNests = NumNests)