# task1_findstop.R
# 0801
args <- commandArgs(TRUE)
library(fpc)
library(dplyr)

para_file_name <- args[2]
paraEPS <- as.double(args[3])
paraMinPts <- as.double(args[4])
para_result_file_name <- args[5]

dfgps <- read.csv(file = para_file_name, header = TRUE)
## do clustering
dfTotalRes <- NULL

# iTask <- 0
for(iRoute in unique(dfgps$route_id))
{
        for(jDir in c(TRUE, FALSE))
        {
                tmpdf <- filter(dfgps, route_id == iRoute,
                                realDirec == jDir)
                resCluster <- dbscan(select(tmpdf, lng, lat), 
                                     eps = paraEPS, 
                                     MinPts = paraMinPts)
                print(nrow(tmpdf))
                print("Finish one dbscan clustering!")
                # get the center of cluster
                # sort them
                lngStartPt <- tmpdf[1, "lng"]
                latStartPt <- tmpdf[1, "lat"]
                
                dfCluster <- tmpdf %>%
                        select(lng, lat, bus_id, GeneralSec) %>%
                        mutate(clusNO = resCluster$cluster) %>%
                        filter(clusNO != 0) %>%
                        group_by(clusNO) %>%
                        summarise(clusLng = mean(lng),
                                  clusLat = mean(lat))
                
                print(nrow(dfCluster))
                #############################
                if(nrow(dfCluster) == 0)
                        next
#                 cat("Task finished: ", iTask,"\n")
#                 iTask <- iTask + 1
#                 cat("Time consumed ", Sys.time() - startTime, "\n")
                
                dfCluster <- dfCluster %>%
                        mutate(clusDist = sqrt(
                                (clusLng - lngStartPt)^2 + 
                                        (clusLat - latStartPt)^2)) %>%
                        arrange(clusDist) %>%
                        mutate(clusOrder = row_number()) %>%
                        mutate(route_id = iRoute, dir = as.integer(jDir)) %>%
                        select(route_id, dir, clusOrder, clusLng, clusLat)
                
                # update lastRowNum
                #         lastRowNum <- lastRowNum + nrow(dfCluster)
                
                dfTotalRes <- rbind(dfTotalRes, dfCluster)
                
        }
}

dfTotalRes <- dfTotalRes %>%
        select(route_id, dir, clusOrder, clusLng, clusLat)

## finish clustering


write.table(x = dfTotalRes, file = para_result_file_name, row.names = FALSE,
            col.names = TRUE, sep = ",")