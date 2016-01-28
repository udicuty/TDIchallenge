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

rtg<-group_by(rt,vehicle_id)
rtg_unique_trips<-summarise(rtg,unique_trips=n_distinct(trip_id))
median(rtg_unique_trips$unique_trips)

#Filtering for M bus trips
rt_M<-filter(rt,grepl("^M.*",trip_id),progress=0) %>% group_by(trip_id)
rt_M_summary<-summarise(
                rt_M,count=n(),
                distance_meters=as.numeric(max(dist_along_route)),
                duration_sec=as.numeric(as.duration(timestamp[n()]-timestamp[1])),
                average_speed_m_s=distance_meters/duration_sec,
                average_mph=average_speed_m_s*3600/1609.344) # 1 mile= 1609.344 meters, 1 hour= 3600 seconds.
arrange(rt_M_summary,desc(average_mph))


rt_Mg<-group_by(rt_M,trip_id)
rt_Mg_unique_trips<-summarise(rt_Mg,unique_trips=n_distinct(trip_id))
max(rt_Mg_unique_trips$unique_trips)
group_by(rt_M,trip_)

head(rt[grep("^M.*",rt$trip_id),])


median(table(rt$vehicle_id))


aggregate(rt,by = vehicle_id,mean)


