options(dplyr.print_max = 10)
options(digits=10)
require("dplyr")
require("data.table")
require("lubridate")
#Here I consider 

rt<-fread("Data/realtime.csv",data.table=FALSE,verbose=TRUE,na.strings = "\\N")
rt$timestamp<-ymd_hms(rt$timestamp)
#rt$timestamp<-as.Date(rt$timestamp)
#rt<-filter(rt,timestamp=="2015-01-28")


#Median unique trips
by_vehicle<-group_by(rt,vehicle_id)
by_vehicle_unique_trips<-summarise(by_vehicle,unique_trips=n_distinct(trip_id))
median(by_vehicle_unique_trips$unique_trips)


#Max total number of trips per route
#Reading possible routes from file - max routes
#all_routes<-read.csv("Data/Scheduled/routes.txt",as.is=T)$route_id
Mtrips<-unique(rt$trip_id)[grepl("_M\\d+_",unique(rt$trip_id))]
Mroutes<-sapply(strsplit(Mtrips,split="_"),"[[",3)
max(table(Mroutes))

#Mroutes<-all_routes[grepl("^M",all_routes)]

#ntrips<-sapply(Mroutes,function (route) sum(grepl(paste0("_",route,"_"),unique(rt$trip_id))))
#ntrips
#max(ntrips)



  

#Filtering for M bus trips - Average speed
x<-sapply(Mroutes,function(route) grep(route,rt$trip_id))
rtM<-filter(rt,trip_id %in% Mtrips)
#rtM$route<-sapply(strsplit(rtM$trip_id,split="_"),"[[",3)
by_Mtrips<-group_by(rtM,trip_id)
by_Mtrips<-summarise(
                by_Mtrips,count=n(),
                distance_meters=as.numeric(max(dist_along_route)),
                duration_sec=as.numeric(as.duration(timestamp[n()]-timestamp[1])),
                speed_m_s=distance_meters/duration_sec,
                speed_mph=speed_m_s*3600/1609.344) # 1 mile= 1609.344 meters, 1 hour= 3600 seconds.


by_Mtrips$route<-sapply(strsplit(by_Mtrips$trip_id,split="_"),"[[",3) # Adding route labels
#arrange(by_Mtrips,desc(speed_mph))

arrange(aggregate(speed_mph~route,by_Mtrips,FUN=mean),speed_mph)


#Headways, initial_station==401998, first_next_stop=405315

rtM$route<-sapply(strsplit(rtM$trip_id,split="_"),"[[",3)
M116_SW<-filter(rtM,route=="M116",bearing>180,bearing<270) #%>% group_by(next_stop_id)

headway_list<-sapply(unique(M116_SW$next_stop_id),function (stop_id) {
  x<-filter(M116_SW,next_stop_id==stop_id)
  y<-as.numeric(as.duration(diff(x$timestamp)))
})
names(headway_list)<-unique(M116_SW$next_stop_id)
headway_list<-headway_list[!names(headway_list)=="405315"]
sd(as.numeric(unlist(headway_list))/60) #REMOVE FIRST STATION


#Lateness
st<-fread("./Data//Scheduled/stop_times.txt",data.table=FALSE,stringsAsFactors = F)
st$route<-sapply(strsplit(stop_times$trip_id,split="_"),"[[",3)
M116st<-filter(st,route=="M116",trip_id %in% M116_SW$trip_id)






