# preprocess_GpsData.R
# 0803
# outlier removal. add route_id. projection. add direction
# add a column of direction to Gps data


preprocess_GpsData <- function(fileGPSData, fileRoadNetwork, fileResult, fileRouteDic)
{
        # package loading
        library(dplyr)
        library(rgdal)
        library(data.table)
        # return: the data frame
        # outlier removal
        
        if(! "GeoJSON" %in% ogrDrivers()$name)
                stop("cannot process GeoJSON file")
        
        #         map <- readOGR(dsn = fileRoadNetwork, layer = "OGRGeoJSON" )
        #plot(map)
        roadMapInfo <- ogrInfo(dsn = fileRoadNetwork, layer="OGRGeoJSON")        
        roadMapExt <- roadMapInfo$extent 
        
        dfRouteDic <- read.csv(fileRouteDic)
        ##      use all data
#         if(userAllRoute == TRUE)
#         {
#                 vecRouteList <- order(unique(dfRouteDic$route_id), decreasing = FALSE)
#                 cat("Len of Route_list ", length(vecRouteList), "\n")
#         }else
#                 ##      use training data only
#         {
#                 dfRouteList <- read.csv(fileRouteIDList, header = FALSE)
#                 vecRouteList <- dfRouteList[,1]
#                 dfRouteDic <- filter(dfRouteDic, route_id %in% vecRouteList)
#                 cat("Len of Route_list ", length(vecRouteList), "\n")
#         }
        
        
        dfGps <- fread(input = fileGPSData, data.table = FALSE)
        cat("Orginal data rows: ", nrow(dfGps), "\n")
        
        # add route_id
        # remove those whose route_id is not required
        dfGps <- inner_join(x = dfGps, y = dfRouteDic, by = c("bus_id"))
        cat("Data rows after removing unrequired rows: ", nrow(dfGps), "\n")
        
        # remove points outside the range & whose bus_id is not required
        dfGps <- filter(dfGps, lng >= roadMapExt[1], lng <= roadMapExt[3],
                        lat >= roadMapExt[2], lat <= roadMapExt[4], 
                        bus_id %in% dfRouteDic$bus_id)
        
        # remove: repeated points
        dfGps <- arrange(dfGps, bus_id, day, time)
        
        dfGps <- distinct(dfGps)
        #nrow(dfGps)
        cat("Data rows after outlier removal: ", nrow(dfGps), "\n")
        
        
        # projection
        noProj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 
                      +towgs84=0,0,0")
        transPos <- SpatialPoints(dfGps[,c("lng","lat")], 
                                  proj4string = noProj)
        transPnt <- spTransform(transPos, CRS("+init=epsg:32650"))
        dfGps$lng <- transPnt@coords[,1]
        dfGps$lat <- transPnt@coords[,2]
        
        cat("Projected!")
        # add the general second
        # add direction label
        # add route_id
        # mark starting points and ending points
        dfGps <- markDirection(dfGps, dfRouteDic = dfRouteDic,
                               bufferLen = 800, busID = 1, 
                               useBusID = FALSE)
        
        cat("Direction marked!")
        dfGps <- dfGps %>% 
        mutate(direction = as.integer(realDirec)) %>%
        select(lng, lat, day, time, bus_id, route_id, GeneralSec, direction)

        write.table(dfGps, file = fileResult, row.names = FALSE,
            col.names = TRUE, sep = ",")
        
}

# the same as do_task1_multiProcess.R
markDirection <- function(dfgps, dfRouteDic, bufferLen = 200, busID = 1, useBusID = FALSE)
{
        startTime <- Sys.time()
        #         dfRouteDic <- read.csv(fileRouteDic)
        
        
        # assume the first point in any route id as the starting point
        
        markRealDir <- function(vecIsStart, vecIsEnd, vecTmpDirec)
        {
                # process the vectors 
                indLeft <- 0
                for(iRow in 1:length(vecIsStart))
                {
                        if(vecIsStart[iRow] == FALSE & indLeft != 0)
                        {
                                vecIsStart[indLeft:(iRow - 1)] <- FALSE
                                vecIsStart[(indLeft + iRow - 1) / 2] <- TRUE
                                indLeft <- 0
                        } else if(vecIsStart[iRow] == TRUE & indLeft == 0) 
                                indLeft <- iRow                                
                }
                
                indLeft <- 0
                for(iRow in 1:length(vecIsEnd))
                {
                        if(vecIsEnd[iRow] == FALSE & indLeft != 0)
                        {
                                vecIsEnd[indLeft:(iRow - 1)] <- FALSE
                                vecIsEnd[(indLeft + iRow - 1) / 2] <- TRUE
                                indLeft <- 0
                        } else if(vecIsEnd[iRow] == TRUE & indLeft == 0) 
                                indLeft <- iRow                                
                }
                
                
                vecReadDirec <- ifelse(vecIsEnd, FALSE, TRUE)
                for(iRow in 1:length(vecTmpDirec))
                {
                        if(iRow == 1 | vecIsStart[iRow] == TRUE) 
                        {
                                vecReadDirec[iRow] <- TRUE
                        }else if(iRow == length(vecTmpDirec))
                        {
                                vecReadDirec[iRow] <- vecReadDirec[iRow - 1]
                        }else
                        {
                                vecReadDirec[iRow] <- vecReadDirec[iRow - 1] & vecReadDirec[iRow + 1] & vecReadDirec[iRow]
                        }                      
                }
                vecReadDirec
        }
        
        # the starting point is the same for buses of the same route. use to calculate distance
        # assuming every bus starts from the starting point every day                
        
        dfgps <- dfgps %>%
                mutate(GeneralSec = (day - 1)*86400 + time) %>%                
                group_by(route_id) %>%
                #                 mutate(GeneralSec = (day - 1)*86400 + time) %>%
                mutate(dist = sqrt((lng - nth(lng,1))^2 + (lat - nth(lat,1))^2)) %>%
                ungroup() %>%
                group_by(bus_id, day) %>%
                mutate(isStart = abs(dist) < bufferLen) %>%
                mutate(isEnd = abs(dist - max(dist)) < bufferLen) %>%
                mutate(tmpDirec = ifelse(isEnd, FALSE, TRUE)) %>%
                mutate(realDirec = markRealDir(isStart, isEnd, tmpDirec)) %>%
                ungroup()
        
        #         ggplot(dfgps, aes(GeneralSec, dist)) + geom_point(size = 2)        
        
        #         if(isTRUE(useBusID))
        #                 ggplot(dfgps, aes(GeneralSec, dist)) + geom_point(size = 2)        
        if(isTRUE(useBusID))
                dfgps <- filter(dfgps, bus_id == busID)
        select(dfgps, lng, lat, day, time, bus_id, route_id, GeneralSec, dist, realDirec, isStart, isEnd)
}

