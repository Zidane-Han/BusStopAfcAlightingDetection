# load_task1_multiProcess.R
# 0801

pkgTest <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep=TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

vecPack <- c("rgdal", "dplyr", "cluster", "fpc", "data.table")
lapply(vecPack, pkgTest)
lapply(vecPack, require, character.only = TRUE)

print("loding pack complete!!")
source("do_task1_multiProcess.R")

# task 1
# do_task1_multiProcess <- function(fileRouteDic, fileGPSData, fileRoadNetwork, threSpeed, clusParaEPS, clusParaMinPts, fileRouteIDList, fileResult, userAllRoute = TRUE)


do_task1_multiProcess(fileRouteIDList = "/home/public/data/TRAINING_BUSSTOPS_ROUTEID_LIST.csv",
                  fileRouteDic = "/home/public/data/BUS_ROUTE_DIC.csv", 
                  fileGPSData = "/home/public/data/GPS_DATA.csv", 
                  fileRoadNetwork = "/home/public/data/ROAD_NETWORK.geojson", 
                  threSpeed = 3, 
                  clusParaEPS = 30, 
                  clusParaMinPts = 20, 
                  fileResult = "RESULT_STOP_LIST_S3_MP.csv",
                  userAllRoute = TRUE)