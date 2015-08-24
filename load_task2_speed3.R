# load_task2_speed3.R
# 0804

vecPack <- c("rgdal", "dplyr", "data.table")
lapply(vecPack, require, character.only = TRUE)

print("loading pack complete!!")

source("preprocess_GpsData.R")
source("identifyAlight_improve.R")

preprocess_GpsData(fileGPSData = "/home/public/data/GPS_DATA.csv",
                   fileRoadNetwork = "/home/public/data/ROAD_NETWORK.geojson",
                   fileResult = "GPS_DATA_Preprocess.csv",
                   fileRouteDic = "/home/public/data/BUS_ROUTE_DIC.csv")

identifyAlight_improve(fileAfcData = "/home/public/data/AFC_DATA.csv", 
               fileStop = "/home/user045/test_task1_mp3/RESULT_STOP_LIST_S3_MP.csv", 
               fileRouteDic = "/home/public/data/BUS_ROUTE_DIC.csv", 
               fileGPSData = "GPS_DATA_Preprocess.csv", 
#                fileRouteIDList = "/home/public/data/TRAINING_BUSSTOPS_ROUTEID_LIST.csv", 
               fileAfcTrainList = "/home/public/data/TRAINING_ALIGHTING_GUID_LIST.csv",
               fileRoadNetwork = "/home/public/data/ROAD_NETWORK.geojson",
               fileResult = "RESULT_ALIGHT_LIST_SPEED_3.csv",   
                useTrainList = FALSE          
                )
        