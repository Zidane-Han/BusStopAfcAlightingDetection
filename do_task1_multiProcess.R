# do_task1_multiProcess.R
# 0801

do_task1_multiProcess <- function(fileRouteDic, fileGPSData, fileRoadNetwork, threSpeed, clusParaEPS, clusParaMinPts, fileRouteIDList, fileResult, userAllRoute = TRUE)
{
        # return: the data frame
        # outlier removal
        if(! "GeoJSON" %in% ogrDrivers()$name)
                stop("cannot process GeoJSON file")
        
        roadMapInfo <- ogrInfo(dsn = fileRoadNetwork, layer="OGRGeoJSON")        
        roadMapExt <- roadMapInfo$extent 
        
        dfRouteDic <- read.csv(fileRouteDic)
        ##      use all data
        if(userAllRoute == TRUE)
        {
                vecRouteList <- order(unique(dfRouteDic$route_id), decreasing = FALSE)
                cat("Len of Route_list ", length(vecRouteList), "\n")
        }else
                ##      use training data only
        {
                dfRouteList <- read.csv(fileRouteIDList, header = FALSE)
                vecRouteList <- dfRouteList[,1]
                dfRouteDic <- filter(dfRouteDic, route_id %in% vecRouteList)
                cat("Len of Route_list ", length(vecRouteList), "\n")
        }
        
        
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
        
        # add the general second
        # add direction label
        # add route_id
        # mark starting points and ending points
        dfGps <- markDirection(dfGps, dfRouteDic = dfRouteDic,
                               bufferLen = 800, busID = 1, 
                               useBusID = FALSE)
        
        cat("Direction marked!")
        # speed filter. speed < 0.2 m/s is stationary points                
        dfGps <- addSpeed(dfgps = dfGps)
        
        #summary(dfGps$speed)
        #         browser()
        
        dfGpsStationaryPt <- filter(dfGps, speed < threSpeed)
        
        cat("Speed added!\n")
        cat("Data rows after speed filtering: ", 
            nrow(dfGpsStationaryPt), "\n")
        
        # dbscan
        # route wise
        startTime <- Sys.time()
        
        dfTotalRes <- findStop_multiProcess(dfGpsStationaryPt, 
                               paraEPS = clusParaEPS,
                               paraMinPts = clusParaMinPts, 
                               vecRouteID = vecRouteList,
                               numBranch = 10)
        
        cat("Time consumed on dbscan ", Sys.time() - startTime, "\n")
        cat("Stops detected!\n")
        # projectinon
        noProj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 
                      +towgs84=0,0,0")
        dfTotalRes <- as.data.frame(dfTotalRes)
        transPos <- SpatialPoints(dfTotalRes[,c("clusLng","clusLat")], 
                                  proj4string = CRS("+init=epsg:32650"))
        transPnt <- spTransform(transPos, CRSobj = noProj)
        dfTotalRes$clusLng <- transPnt@coords[,1]
        dfTotalRes$clusLat <- transPnt@coords[,2]
        
        
        # write result
        
        # vecRouteList: order of route list
        dfTotalRes <- dfTotalRes %>%
                ungroup() %>%
                mutate(category = factor(route_id, levels = vecRouteList)) %>%
                arrange(category, clusOrder) %>%
                select(-category)
        
        dfTotalRes <- dfTotalRes %>% mutate(id = row_number()) %>% select(id) %>% bind_cols(dfTotalRes)
		
        write.table(dfTotalRes, file = fileResult, row.names = FALSE,
                    col.names = FALSE, sep = ",")
        
        cat("File written!")
}

#############################
# any point within bufferLen meters of the starting point or the ending point is treated as the starting points or ending points
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
        select(dfgps, lng, lat, day, bus_id, route_id, GeneralSec, dist, realDirec, isStart, isEnd)
}

##############################
# addSpeed column
addSpeed <- function(dfgps)
{
        addSpeedBuswise <- function(vecSecond, vecLng, vecLat)
        {
                vecSpeed <- vector(mode = "double", 
                                   length = length(vecSecond))
                vecDiffSec <- c(1, diff(vecSecond))
                vecDiffSec[which(vecDiffSec < 0.1)] <- 0.1
                vecDiffLng <- c(0, diff(vecLng))
                vecDiffLat <- c(0, diff(vecLat))
                vecSpeed <- sqrt(vecDiffLng^2 + vecDiffLat^2)/vecDiffSec
                # if the sec gap is larger than 3600 (1 hour), the speed is set as 0
                vecSpeed[which(vecDiffSec > 3600)] <- 0
                vecSpeed
        }
        
        dfgps %>%
                group_by(bus_id) %>%
                arrange(GeneralSec) %>%
                mutate(speed = addSpeedBuswise(GeneralSec, lng, lat)) %>%
                ungroup() 
}

################################
# use the dbscan method to find the stop. Route wise and direction wise
findStop_multiProcess <- function(dfgps, paraEPS, paraMinPts, vecRouteID, numBranch)
{
        
        dfTotalRes <- NULL
        dfStopNum <- NULL
        #         vecRouteID <- unique(dfgps$route_id)
        print(length(vecRouteID))
        iTask <- 1
        ### turn this part into multi processes
        # write a file of gps data
        # divide all routes into 10 groups
        # set up 10 branches
        
        # write gps file separately
        
        #numBranch <- 10

        stopifnot(length(vecRouteID) %% numBranch == 0)
        
        vecVec <- 1:(numBranch - 1) * length(vecRouteID) / numBranch + 1
        dfgps <- mutate(dfgps, branch = findInterval(route_id, vecVec) + 1)
        # tag starts from 1!
        
        for(curBranch in 1:numBranch)
        {
                # filter
                dfgpsTMP <- dfgps %>% 
                        filter(branch == curBranch) %>%
                        select(-branch)
                
                # write file
                fileNameTMP <- paste0("GPS_DATA_BRANCH_", curBranch, ".csv")
                resultFileNameTMP <- paste0("CLUSTER_RESULT_", curBranch, ".csv")
                write.table(dfgpsTMP, file = fileNameTMP, row.names = FALSE,
                            col.names = TRUE, sep = ",")
                # start process
                system(paste0("nohup Rscript task1_findstop.R --args ",
                              fileNameTMP, 
                              " ", paraEPS, " ", paraMinPts, 
                              " ", resultFileNameTMP), 
                       wait = FALSE)
        }
        
        # wait for all files ready
        while(TRUE)
        {
                vecFileName <- list.files()
                
                numReadyFile <- length(grep(value = FALSE, pattern = "CLUSTER_RESULT_[1-9]+", x = vecFileName))
                if(numReadyFile < (numBranch))
                {
                        cat("Number of files READY: ", numReadyFile, "\n")
                        cat("sleep", "\n")
                        Sys.sleep(1200) 
                        # sleep for 1200s because not ready
                        
                }else
                {
                        cat("Number of files READY: ", numReadyFile, "\n")
                        cat("Go for combining!", "\n")
                        vecVALIDFileName <- grep(value = TRUE, 
                                                 pattern = "CLUSTER_RESULT_[1-9]+", 
                                                 x = vecFileName)
                        fileFinal <- NULL
                        for(fileN in vecVALIDFileName)
                        {
                                fileFinal <- rbind(fileFinal, read.csv(fileN, header = TRUE))
                                
                        }
                        # double check
                        break
                }
        }
        fileFinal        
        
}



