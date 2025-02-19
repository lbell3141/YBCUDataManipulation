#Combine and format daily data

#Load necessary packages
library(dplyr)
library(oce)

#Define file paths
PathtoPre2022Data <- "./Data/pre2022data/DailyData_14022025.csv"
PathtoPost2022Data <- "./Data/post2022data/DailyData_14022025.csv"
PathtoSurvSumOutput <- "./Data/Outputs/SurveySummary_19022025.csv"

PathtoOutput <- "./Data/Outputs/DailyData_19022025.csv"

#Load data
OutSurvSum <- read.csv(PathtoSurvSumOutput)
OldDat <- read.csv(PathtoPre2022Data)
NewDat <- read.csv(PathtoPost2022Data)

#Rename columns found in both sheets to a common name
#Add an identifier to each sheet: "pre" for the pre2022 data and "post" for the post2022 data
OldRenamed <- OldDat%>%
  rename(SiteYrID = SITEYRID,
         StartCloudCover = X_CloudCoverStart.,
         StartPrecip = PrecipStart,
         StartNoise =NoiseStart,
         StartTemp = TempStart_F,
         EndCloudCover =X_CloudCoverEnd,
         EndPrecip = PrecipEnd,
         EndNoise = NoiseEnd,
         EndTemp = TempEnd_F,
         UTMCallPtX = CallPt_UTMEast,
         UTMCallPtY = CallPt_UTMNorth)%>%
  mutate(Version = "Pre")%>%
  mutate(across(everything(), as.character))%>%
  select(Version, everything())
NewRenamed <- NewDat%>%
  rename(SiteYrID = SITEYRID,
         StartPrecip = PrecipStart,
         StartNoise =NoiseStart,
         StartTemp = TempStart_F,
         EndPrecip = PrecipEnd,
         EndNoise = NoiseEnd,
         EndTemp = TempEnd_F,
         UTMCallPtX = CallPt_UTMEast,
         UTMCallPtY = CallPt_UTMNorth)%>%
  mutate(Version = "Post")%>%
  mutate(across(everything(), as.character))%>%
  select(Version, everything())

#merge old and new data into one df
CombdDaily <- bind_rows(OldRenamed, NewRenamed)

#convert UTMs to lat/lon
#move UTMs to new df for calculations
PosDat <- CombdDaily%>%
  select(Version, SiteYrID, UTMCallPtX, UTMCallPtY)%>%
  mutate(across(c(UTMCallPtX,UTMCallPtY, SiteYrID ), ~as.numeric(.)))
#extract UTM zones based on SiteYrID from survey summary sheet
zones <- OutSurvSum%>%
  select(Version, SiteYrID, UTMZone)
#add to position data
PosDatwZone <- left_join(PosDat, zones, by = c("Version", "SiteYrID"))
#convert to lat/lon
ConvertedUTM <- utm2lonlat(PosDatwZone$UTMCallPtX, PosDatwZone$UTMCallPtY, zone = PosDatwZone$UTMZone, hemisphere = "N")%>%
  bind_rows()%>%
  rename(LatCallPt = latitude,
         LonCallPt = longitude)
#add back to daily data
DailywLL <- bind_cols(CombdDaily, ConvertedUTM)%>%
  relocate(LatCallPt, LonCallPt, .after = UTMCallPtY)%>%
  rename(I_P = X_I_P,
         A_V_B = X_A_V_B)

#save dataframe to output path 
write.csv(DailywLL, PathtoOutput, row.names = F)
