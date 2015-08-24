# identifyAlight_improve.R
# 0804

identifyAlight_improve <- function(fileAfcData, fileStop, 
                           fileRouteDic, fileGPSData, 
                           fileAfcTrainList, 
                           fileRoadNetwork, fileResult,
                           useTrainList = FALSE)
{
        startTime <- Sys.time()
        
        # control randomness
        set.seed(1000)
        
        # about transformation
        noProj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 
                      +towgs84=0,0,0")
        planeProjCRS <- CRS("+init=epsg:32650")
        
        # check if fileGPSData exist. If not, sleep for 5 min and recheck. If yes, sleep for 5min and break
        print("begin to do!")
        
        ######################## read AFC data and process
        
        dfAfc <- fread(input = fileAfcData, data.table = FALSE)
        
        dfAfc <- dfAfc %>% 
                mutate(totalSec = (day - 1) * 86400 + time) %>%
                arrange(card_id, totalSec)
        
        dfAfc$next_tran_sec <- c(tail(dfAfc$totalSec, -1), tail(dfAfc$totalSec, 1))        
        dfAfc$next_tran_busId <- c(tail(dfAfc$bus_id, -1), tail(dfAfc$bus_id, 1))
        dfAfc$next_card_id <- c(tail(dfAfc$card_id, -1), tail(dfAfc$card_id, 1))
        
        print("Finish reading AFC data!")
        
        ############### read route_dic and match AFC data
        dfRouteDic <- read.csv(fileRouteDic)
#         browser()
        dfAfc <- inner_join(dfAfc, dfRouteDic, by = c("bus_id"))
#         browser()
        print("Finish matching AFC data!")
        ############### FINISH reading route_dic and match AFC data
        
        ################################### read stop file
        dfStop <- fread(input = fileStop, data.table = FALSE, header = FALSE)
        names(dfStop) <- c("stop_id", "route_id", "direction", 
                           "sequence", "lng", "lat")
        print("Finish reading dfStop data!")
        
        # projection
        
        transPos <- SpatialPoints(dfStop[,c("lng","lat")], 
                                  proj4string = noProj)
        transPnt <- spTransform(transPos, planeProjCRS)
        dfStop$lng <- transPnt@coords[,1]
        dfStop$lat <- transPnt@coords[,2]
        
        print("Stop data projected!")
        
        ################################### Finish reading stop file
        
        if(isTRUE(useTrainList))
        {                
                dfAfcList <- fread(input = fileAfcTrainList, data.table = FALSE, header = FALSE)
                vecGuidList <- dfAfcList[,1]
                # filter
                dfAfc <- filter(dfAfc, guid %in% vecGuidList)
        }
        # or use inner_join
        
        ################################### read GPS data. Donot filter GPS data!
        dfGps <- fread(input = fileGPSData, data.table = FALSE)
        print("Finish reading GPS data")
        print(Sys.time() - startTime)
        # add route_id
        
        # no need. GPS data are processed
        #         dfGps <- inner_join(x = dfGps, y = dfRouteDic, by = c("bus_id"))
        #         cat("Data rows", nrow(dfGps), "\n")
        # remove outlier
        #         roadMapInfo <- ogrInfo(dsn = fileRoadNetwork, layer="OGRGeoJSON")        
        #         roadMapExt <- roadMapInfo$extent 
        #         # remove points outside the range & whose bus_id is not required
        #         dfGps <- filter(dfGps, lng >= roadMapExt[1], lng <= roadMapExt[3],
        #                         lat >= roadMapExt[2], lat <= roadMapExt[4], 
        #                         bus_id %in% dfRouteDic$bus_id)
        #         
        #         # remove repeated points
        #         dfGps <- arrange(dfGps, bus_id, day, time)
        #         dfGps <- distinct(dfGps)        
        #         
        #         # projection
        # 
        #         library(raster)
        #         noProj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 
        #                       +towgs84=0,0,0")
        #         planeProjCRS <- CRS("+init=epsg:32650")
        # 
        #         transPos <- SpatialPoints(dfGps[,c("lng","lat")], 
        #                                   proj4string = noProj)
        #         transPnt <- spTransform(transPos, CRSobj = planeProjCRS)
        #         dfGps$lng <- transPnt@coords[,1]
        #         dfGps$lat <- transPnt@coords[,2]
        #         detach("package:raster", unload=TRUE)
        # 
        #         dfGps <- dfGps %>% mutate(GeneralSec = day * 86400 + time)
        ################################### FINISH reading GPS data
                
        ############## combine two functions into one
#         find_lng_lat_nearestStop <- function(nextLng, nextLat, last_bus_id, last_direction, last_route_id, last_card_id, next_card_id)
        find_lng_lat_nearestStop_directly <- function(dfAfc_oneTrans, dfGPS, dfSTOP)
        {
                # get nextLng, nextLat
#                 browser()
                stopifnot(nrow(dfAfc_oneTrans) == 1)
                
                nextAfcSec <- dfAfc_oneTrans$next_tran_sec
                curAfcSec <- dfAfc_oneTrans$totalSec
                # add direction to dfAfc_oneTrans
                tmpCurAfcDirection <-dfGPS %>% 
                        filter(bus_id == dfAfc_oneTrans$bus_id) %>%
                        filter(abs(GeneralSec - curAfcSec) == min(abs(GeneralSec - curAfcSec))) %>%
                        .$direction
                        
                #filter stop data
                dfStopTmp <- dfSTOP %>% 
                        filter(route_id == dfAfc_oneTrans$route_id, 
                               direction == tmpCurAfcDirection)
#                 dfAfc_oneTrans$curAfcDirection <- tmpCurAfcDirection
                

                
                dfGPS <-dfGPS %>% 
                        filter(bus_id == dfAfc_oneTrans$next_tran_busId) %>%
                        select(GeneralSec, lng, lat)
                
                # find the last and next GeneralSec and 
#                 nextAfcSec <- dfAfc_oneTrans$next_tran_sec
                
                
                dfNextLngLat <- bind_rows(
                        dfGPS %>% 
        filter((GeneralSec - nextAfcSec) > 0) %>% 
        filter((GeneralSec - nextAfcSec) == min(GeneralSec - nextAfcSec)) 
, 
                        dfGPS %>% 
        filter((GeneralSec - nextAfcSec) <= 0) %>% 
        filter((GeneralSec - nextAfcSec) == max(GeneralSec - nextAfcSec))) %>%
                        summarise_each(funs(mean)) %>%
                        select(nextLng = lng, nextLat = lat)

                
                print("nextLng & nextLat found!")
                # find nearestStop
                
                

#                 with(dfAfc_oneTrans,
                if(dfAfc_oneTrans$card_id == dfAfc_oneTrans$next_card_id)
                {
                        dfStopTmp <- dfStopTmp %>%
                                mutate(dist_to_nextPos = sum(( c(lng,lat) - c(dfNextLngLat$nextLng, dfNextLngLat$nextLat))^2)) %>%
                                filter(dist_to_nextPos == min(dist_to_nextPos))
                        
                        dfResAligh <- data.frame(alighLng = dfStopTmp[1, "lng"], alignLat = dfStopTmp[1, "lat"])   
                }else
                {
                        dfStopTmp <- sample_n(dfStopTmp)
                        dfResAligh <- data.frame(alighLng = dfStopTmp[1, "lng"], alignLat = dfStopTmp[1, "lat"])   
                }
                dfResAligh
#                 )
                
        }
        wrapper_find_nearestStop_directly <- function(df_Afc_oneTrans){ 
                find_lng_lat_nearestStop_directly(dfAfc_oneTrans = df_Afc_oneTrans, dfGPS = dfGps, dfSTOP = dfStop)}

        # find stop directly
#         browser()
#         find_lng_lat_nearestStop_directly(dfAfc_oneTrans = dfAfc[1,], dfGps, dfStop)
#         wrapper_find_nearestStop_directly(dfAfc[1,])

        
#         dfAfc <- bind_cols(dfAfc, foreach(irow = 1:nrow(dfAfc), .combine=rbind) %dopar% {
#                 wrapper_find_nearestStop_directly(dfAfc[irow,])
#                 })
#         dtFinal <- dfAfc
############### use a data.table merge method
# add direction to AFC
dfAfcOld <- dfAfc
# browser()
setkey(setDT(dfGps), bus_id)
setkey(setDT(dfAfc), bus_id)
# "guid"            "card_id"         "day"             "time"
# "bus_id"          "totalSec"        "next_tran_sec"   "next_tran_busId"
# "next_card_id"    "route_id"
dfAfc <- dfGps[dfAfc, .(afc_direction = direction[which.min(abs(i.totalSec - GeneralSec))] ,
                        guid = i.guid, 
                        card_id = i.card_id, day = i.day, 
                        time = i.time, bus_id = i.bus_id, 
                        totalSec = i.totalSec, next_tran_sec = i.next_tran_sec, 
                        next_tran_busId = i.next_tran_busId,
                        next_card_id = i.next_card_id, 
                        route_id = i.route_id), by =.EACHI]

# browser()
# stage one. find nextLng, nextLat
        setkey(setDT(dfGps), bus_id)
        setkey(setDT(dfAfc), next_tran_busId)
        dtAfcGps <- dfGps[dfAfc, .(next_tran_sec = i.next_tran_sec, 
                                   guid = i.guid,
                                   next_card_id = i.next_card_id,
                                   card_id = i.card_id,
                                   curRouteId = i.route_id,
                                   bus_id = i.bus_id,
                                   gps.Sec = GeneralSec[which.min(abs(i.next_tran_sec - GeneralSec))],
                                   route_id = i.route_id, 
                                   afc_direction = i.afc_direction, 
                                   totalSec = i.totalSec,
                                   nextLng = lng[which.min(abs(i.next_tran_sec - GeneralSec))], 
                                   nextLat = lat[which.min(abs(i.next_tran_sec - GeneralSec))]
        ), by =.EACHI]

        setkey(setDT(dfGps), bus_id)
        setkey(setDT(dtAfcGps), bus_id)
        
        dtAfcGps <- dfGps[dtAfcGps, .( 
                                   guid = i.guid,
                                   next_card_id = i.next_card_id,
                                   card_id = i.card_id,
                                   curRouteId = i.route_id,
                                   bus_id = i.bus_id,
                                   gps.Sec = GeneralSec[which.min(abs(i.next_tran_sec - GeneralSec))],
                                   route_id = i.route_id, 
                                   curDirection = i.afc_direction, 
                                   curLng = lng[which.min(abs(i.totalSec - GeneralSec))], 
                                   curLat = lat[which.min(abs(i.totalSec - GeneralSec))],
                                   nextLng = i.nextLng,
                                   nextLat = i.nextLat),
                          by =.EACHI]


                  


        # stage two. find AlighLng, AlighLat
        setkey(setDT(dfStop), route_id, direction)
        setkey(dtAfcGps, curRouteId, curDirection)
        
        dtFinal <- dfStop[dtAfcGps, .(guid = i.guid,
                                      AlighLng = lng[which.min( (lng - i.nextLng)^2 + (lat - i.nextLat)^2 )],
                                      AlighLat = lat[which.min( (lng - i.nextLng)^2 + (lat - i.nextLat)^2 )],
                                      Afc_stop_sequence = sequence[which.min( (i.curLng - lng)^2 + (curLat - lat)^2 )],
                                      card_id = i.card_id,
                                      next_card_id = i.next_card_id,
                                      route_id = i.route_id, 
                                      curDirection = i.curDirection), 
                          by = .EACHI]
        
        ############### use a data.table merge method
        #         browser()
                
        #special issues
                
        # deal with special issues (AlighLng == NA or AlighLat == NA or card_id != next_card_id)
        setkey(dtFinal, route_id, curDirection)
        setkey(setDT(dfStop), route_id, direction)
        
        browser()
        dtFinal <- dfStop[dtFinal, 
                          .(card_id = i.card_id,
                          next_card_id = i.next_card_id,
                          AlighLng = i.AlighLng,
                          AlighLat = i.AlighLat,
                          rdStopLng = lng[ceiling(.N / 2)], # to avoid NA
                          rdStopLat = lat[ceiling(.N / 2)],
                          guid = i.guid),
                          by = .EACHI]
        
        browser()
        dtFinal <- dtFinal[is.na(AlighLng) | is.na(AlighLat) | (card_id != next_card_id),
                           `:=`(AlighLng = rdStopLng, AlighLat = rdStopLat),]
        
        cat("Complete rows have: ", length(which(complete.cases(dtFinal))), "\n")
        
        # fill in NAs
        set.seed(10000)
        dtFinal <- dtFinal[,lapply(.SD,function(x){ifelse(is.na(x), sample(x,1,prob = ifelse(is.na(x),0,1)),x)})]
        
        cat("Complete rows have: ", length(which(complete.cases(dtFinal))), "\n")
        print("Alighting points found!")
        
        
        dtFinal <- select(dtFinal, guid, AlighLng, AlighLat)
#         browser()
        # projection
        transPos <- SpatialPoints(dtFinal[,.(AlighLng,AlighLat)], 
                                  proj4string = planeProjCRS)
        transPnt <- spTransform(transPos, CRSobj = noProj)
        dtFinal$AlighLng <- transPnt@coords[,1]
        dtFinal$AlighLat <- transPnt@coords[,2]
        
        write.table(dtFinal, file = fileResult, 
                    col.names = FALSE, row.names = FALSE, sep = ",")
        print("File Written!")        
}