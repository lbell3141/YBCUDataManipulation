#Combine survey summary data

#Load necessary packages
library(dplyr)
library(oce)

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
conUTMdf <- FullDat%>%
  mutate(across(c(UTMStartX, UTMStartY, UTMEndX, UTMEndY, UTMZone),
                ~as.numeric(na_if(.x, ""))))%>%
  mutate(UTMZone = substr(UTMZone, 1,2))%>%
  mutate(UTMZone = case_when(
    is.na(UTMZone) & (rowSums(!is.na(select(., UTMStartX, UTMStartY, UTMEndX, UTMEndY))) > 0) ~ "12",
    T ~ UTMZone))%>%
  mutate(UTMZone = as.numeric(UTMZone))

#Converting start coords:
StartLL <- utm2lonlat(conUTMdf$UTMStartX, conUTMdf$UTMStartY, zone = conUTMdf$UTMZone, hemisphere = "N")%>%
  bind_rows()%>%
  rename(calc_LatStart = latitude,
         calc_LonStart = longitude)
#Converting end coords:
EndLL <- utm2lonlat(conUTMdf$UTMEndX, conUTMdf$UTMEndY, zone = conUTMdf$UTMZone, hemisphere = "N")%>%
  bind_rows()%>%
  rename(calc_LatEnd = latitude,
         calc_LonEnd = longitude)
#Combine results
AllLLs <- bind_cols(StartLL, EndLL)
#Add UTM-derived lat/long to full dataset
LLwithDat <- bind_cols(AllLLs, conUTMdf)
#If NA, supplement the existing lat/long columns with the UTM-derived coords, then remove those extra calculation columns
YBCU_data <- LLwithDat %>%
  mutate(LatStart = if_else(is.na(LatStart), calc_LatStart, LatStart),
         LonStart = if_else(is.na(LonStart), calc_LonStart, LonStart),
         LatEnd = if_else(is.na(LatEnd), calc_LatEnd, LatEnd),
         LonEnd = if_else(is.na(LonEnd), calc_LonEnd, LonEnd)) %>%
  select(-calc_LatStart, -calc_LonStart, -calc_LatEnd, -calc_LonEnd)
