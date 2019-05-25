setwd("C:\\Users\\SonyaIlieva\\Desktop\\UNI\\R\\Summer semester\\NETERA")
can_dd=read.csv("can_bus_events.csv", na.string=c("","NA"," ","?","\\N"),stringsAsFactors = F)
gps_dd=read.csv("gps_events.csv",na.string=c("","NA"," ","?","\\N"),stringsAsFactors = F)

colnames(gps_dd)=c("gps_event_id","device_id","latitude","longitude","speed","azimuth","altitude","current_work_hours","mileage","power_voltage","gps_utc_time", "satellites")
names(gps_dd)
colnames(can_dd)=c("id","device_id","total_distance","total_fuel_used","engine_rpm","vehicle_speed","engine_coolant_temperature","engine_temperature","accelerator_pedal_pressure","e.range",
                  "total_engine_hours"," total_driving_time"," total_engine_idle_time","total_idle_fuel_used","detailed_fuel_low","detailed_driver_seatbelt","detailed_air_conditioning","detailed_cruise_control",
                   "detailed_brake_pedal","detailed_handbrake","detailed_central_lock","detailed_reverse_gear","detailed_running_lights","detailed_low_beams","detailed_high_beams","detailed_rear_fog_lights","detailed_front_fog_lights","detailed_doors","detailed_trunk","doors_driver_door","doors_passenger_door","doors_rear_left_door","doors_rear_right_door",
                   "doors_trunk","doors_hood","gps_utc_time","trip_id"," fuel_level_litres","fuel_level_percentage","gps_event_id")

#Merge the two datasets
library(dplyr)
can_gps=left_join(gps_dd,can_dd,by=c("device_id","gps_event_id"))

#Remove observations which have NAs in trip_id column
sum(is.na(can_gps$trip_id))
can_gps=can_gps[!(is.na(can_gps$trip_id)==T),]

# Inconsistencies and missing values ----
DD=data.frame(vn=names(can_gps),vc=sapply(can_gps,class),vna=colSums(is.na(can_gps))/nrow(can_gps), vmax=apply(can_gps,2,max,na.rm=T),vmin=apply(can_gps,2,min,na.rm=T))

# Remove variables with more than 20% missing values for can_dd
can_gps=can_dd[,!names(can_gps) %in% DD$vn[DD$vna>=0.2]]
DD=DD[-which(DD$vna>=0.2),]

names(can_gps)

#Replacing the missing calues with 0
sum(is.na(can_gps$device_id))
can_gps[is.na(can_gps)]=0

#Fixing the types of the variables
library(lubridate)
can_gps$gps_utc_time[1:5]
can_gps$gps_utc_time=ymd_hms(can_gps$gps_utc_time)
class(can_gps$gps_utc_time)

can_gps$gps_utc_time=ymd_hms(can_gps$gps_utc_time)
class(can_gps$gps_utc_time)

#Factor variables
unique(can_gps$detailed_fuel_low)
names(can_gps[,-c(12:32)])








#Distance indicator
max(can_dd1$total_distance)#outlier - 4211081
hist(can_dd1$total_distance,xlim=c(min(can_dd1$total_distance),max(can_dd1$total_distance)))
quantile(can_dd1$total_distance)
#0%     25%     50%     75%    100% 
#2241   49930  105459  225626 4211081 