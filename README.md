# BusStopAfcAlightingDetection

*****************************************************

本研究使用的数据来自深圳大学空间信息智能感知与服务深圳市重点实验室组织的“ISPRS Scientific Initiative ---- Open Data Challenge”大赛项目。该项目得到ISPRS、深圳大学、深圳市科创委基金相关项目的资助。
The research data of this project come from ISPRS Scientific Initiative --- Open Data Challenge which is organized by Shenzhen Key Laboratory of Spatial Smart Sensing and Services, Shenzhen University in 2015. This project has been jointly funded by International Society for Photogrammetry and Remote Sensing (ISPRS), Shenzhen University, and Science and Technology Innovation Commission of Shenzhen Municipality (SZSTI).

*****************************************************

Author:

陈焕发 Huanfa Chen

高晗 Han Gao

邹斌 Bin Zou

*****************************************************

Contact:

Name: Huanfa Chen

Organisation: University College London, UK

Email: huanfa.chen@ucl.ac.uk; chenhuanfa@gmail.com

*****************************************************

Language: R version 3.1.2

Operating System: Unix or Linux

*****************************************************

Execution:

1. The following data are essential and shoud be put in the working directory:

BUS_ROUTE_DIC.csv
GPS_DATA.csv
ROAD_NETWORK.geojson
AFC_DATA.csv

2. The following data are optional. Only essential is the code is run on the trainning data


TRAINING_BUSSTOPS_ROUTEID_LIST.csv
TRAINING_ALIGHTING_GUID_LIST.csv

3. Running task 1(bus stop detection)

// only run on Unix-like platform
source("load_task1_multiProcess.R")

4. Running task 2(AFC alighting detection)

source("load_task2_speed3.R")

*****************************************************

Explanation:

1. Bus stop detection

1) outlier removal (outside the range of road map)
2) speed filtering (keep points with speed less than 3m/s)
3) direction identification (find out start points and end ponints for each route, and then add direction tag to each point)
4) DBSCAN clustering on each direction of each route(epsilon = 30 meters. MinPoints = 20)

2. AFC alighting detection

1) AFC tagging: route_id and direction
2) next_AFC lookup: find the next AFC(time, bus_id) of the same card, for each AFC
3) next_AFC mapping: find the position of next_AFC (using the GPS data of the bus)
4) Alighting stop detection: for each AFC, find out the bus stop on the same route and direction and which is the closest to the position of its next_AFC, based on the bus stop result from task1
