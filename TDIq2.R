setwd("c:/Users/Udi/SkyDrive/Data Incubator/Challenge/Data/")

options(dplyr.print_max = 10)
options(digits=10)
require("dplyr")
require("data.table")
require("lubridate")


#Reading realtime data, correcting for \N entries and transformind date fields
rt<-fread("realtime.csv",data.table=FALSE,verbose=TRUE,na.strings = "\\N")
rt$timestamp<-ymd_hms(rt$timestamp)



#Median unique trips
by_vehicle<-group_by(rt,vehicle_id)
by_vehicle_unique_trips<-summarise(by_vehicle,unique_trips=n_distinct(trip_id))
print ("Median unique trips:")
median(by_vehicle_unique_trips$unique_trips)


#Max total number of trips per M route
Mtrips<-unique(rt$trip_id)[grepl("_M\\d+_",unique(rt$trip_id))]
Mroutes<-sapply(strsplit(Mtrips,split="_"),"[[",3)
table(Mroutes)
print ("Maximum numbers of trips per route:")
max(table(Mroutes))

#Average speed - Filtering for M bus trips - 
rtM<-filter(rt,trip_id %in% Mtrips)
by_Mtrips<-group_by(rtM,trip_id)
by_Mtrips<-summarise(
                by_Mtrips,count=n(),
                distance_meters=as.numeric(max(dist_along_route)),
                duration_sec=as.numeric(as.duration(timestamp[n()]-timestamp[1])),
                speed_m_s=distance_meters/duration_sec,
                speed_mph=speed_m_s*3600/1609.344) # 1 mile= 1609.344 meters, 1 hour= 3600 seconds.


by_Mtrips$route<-sapply(strsplit(by_Mtrips$trip_id,split="_"),"[[",3) # Adding route labels

avg_speed<-aggregate(speed_mph~route,by_Mtrips,FUN=mean)

print ("Second highest average speed:")
arrange(avg_speed,desc(speed_mph))[2,] #Aggregating by route


#Headways, initial_station==401998, first_next_stop=405315
rtM$route<-sapply(strsplit(rtM$trip_id,split="_"),"[[",3) # Adding route labels
M116_SW<-filter(rtM,route=="M116",bearing>180,bearing<270) #Extracting M116 S/W routes (S-180,W-270)

headway_list<-sapply(unique(M116_SW$next_stop_id),function (stop_id) {   #Extraccting all headways for all stations for M116_SW
  x<-filter(M116_SW,next_stop_id==stop_id)
  y<-as.numeric(as.duration(diff(x$timestamp)))
})
names(headway_list)<-unique(M116_SW$next_stop_id)#Adding labels
headway_list<-headway_list[!names(headway_list)=="405315"] #REMOVE FIRST STATION

print ("Headway variance in m^2 == standard deviation (sec/60):")
sd(as.numeric(unlist(headway_list))/60) 

