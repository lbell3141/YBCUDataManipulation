#Combine and format daily data

#Load necessary packages
library(dplyr)

#Define file paths
PathtoPre2022Data <- "./Data/pre2022data/DailyData_14022025.csv"
PathtoPost2022Data <- "./Data/post2022data/DailyData_14022025.csv"

PathtoOutput <- "./Data/Outputs/DailyData_19022025.csv"

#Load data
OldDat <- read.csv(PathtoPre2022Data)
NewDat <- read.csv(PathtoPost2022Data)



