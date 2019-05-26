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
can_gps=left_join(gps_dd,can_dd,by=c("device_id","gps_event_id","gps_utc_time"))

#Remove observations which have NAs in trip_id column
sum(is.na(can_gps$trip_id))
can_gps=can_gps[!(is.na(can_gps$trip_id)==T),]

# Checking classes and missing values ----
DD=data.frame(vn=names(can_gps),vc=sapply(can_gps,class),vna=colSums(is.na(can_gps))/nrow(can_gps))

# Remove variables with more than 20% missing values for can_dd
can_gps=can_gps[,!names(can_gps) %in% DD$vn[DD$vna>=0.2]]
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


#GPS speed underestimates the exact speed - remove this variable
can_gps=can_gps[,-(5)]
DD=DD[-c(5),]


#Factor variables
unique(can_gps$detailed_fuel_low)
ll=list()
for (i in 1:ncol(can_gps)){
  ll[[i]]=unique(can_gps[[i]])
}
#(22:43) - factor variables
names(can_gps[,c(10,22:43)])

#During one trip these events can happen several times - we will aggregate the data and find the average numbers of these events
#length(unique(can_gps$device_id))
#length(unique(can_gps$trip_id))
trip_aggr=aggregate(can_gps[,-c(1,2,10,11,12,44)], by=list(can_gps$trip_id), FUN="mean")
names(trip_aggr)[1]<-"trip_id"
DD_aggr=data.frame(vmax=apply(trip_aggr,2,max,na.rm=T),vmin=apply(trip_aggr,2,min,na.rm=T),vmean=apply(trip_aggr,2,mean,na.rm=T))


# Have a look at correlation of features without the factor variables
windows()
library(psych)
cor.plot(trip_aggr,numbers=T, las=2, main="Correlation matrix of the aggregated variables",cex.axis=0.8) 

#Dotuk kodat e gore-dolu navarzan, grupiraneto sam go napravila no ne izcqlo mai i moje da go doobsadim. Po-nadilu ima chasti koito moje da izpolzvame po-natatak.

#Group the variebles into the 3 main categories/factors
aux_economy=trip_aggr[grep("total_idle_fuel_used|total_distance|total_fuel_used|mileage|e.range|total_engine_hours_total_driving_time|fuel_level_percantage|total_engine_idle_time|detailed_air_conditioning|detailed_cruise_control|detailed_trunk|doors_driver_door|doors_passenger_door|doors_rear_left_door|doors_rear_right_door|detailed_doors|detailed_reverse_gear|altitude",names(trip_aggr))]
aux_danger=trip_aggr[grep("vehicle_speed|engine_rpm|detailed_brake_pedal|detailed_handbrake|detailed_break_pedal|detailed_high_beams|detailed_low_beams|detailed_rear_fog_lights|detailed_front_fog_lights|doors_hood|detailed_running_lights",names(trip_aggr))]
aux_ecology=trip_aggr[grep(".fuel|total_distance|e.range",names(trip_aggr))]


#Calculate the weights
a1=alpha(aux1,check.keys = T)
a1$keys
a2=alpha(aux2,check.keys = T)
a2$keys
a3=alpha(aux3,check.keys = T)
a3$keys
a4=alpha(aux4,check.keys = T)
a4$keys

a1$total#0.8267967
a2$total#0.838298
a3$total#0.8430904
a4$total#0.6206835


ddn2=dd[,names(dd) %in% c("Informative","OK")]
ddn2$F2=rowSums(dd[,names(dd) %in% c("Courageous","Persuasive","Inspiring")])/3
ddn2$F1=rowSums(dd[,names(dd) %in% c("Fascinating","Beautiful","Jaw-dropping","Ingenious","Funny")])/5
ddn2$F3=rowSums(dd[,names(dd) %in% c("Confusing","Longwinded","Unconvincing","Obnoxious")])/4





# use 3/4 of the available observations for training purposes
set.seed(2)
smp_size <- floor(0.75 * nrow(can_gps))
train_ind <- sample(seq_len(nrow(can_gps)), size = smp_size)
train <- can_gps[train_ind, ]
test <- can_gps[-train_ind, ]










#Distance indicator
#max(can_dd1$total_distance)#outlier - 4211081
#hist(can_dd1$total_distance,xlim=c(min(can_dd1$total_distance),max(can_dd1$total_distance)))
#quantile(can_dd1$total_distance)
#0%     25%     50%     75%    100% 
#2241   49930  105459  225626 4211081 



#-------------------Perform classification via:

# -----------Logistic regression---------------
library(ISLR)
library(MASS)

# use all available features at level
x=dd[,c(2,6,8)]
eq1=glm(response~glucCncrt+massInd+age,data=dd[train,],family=binomial)
summary(eq1)


# forecast in-sample probabilities using eq1
eq1Pr=predict(eq1, type="response")
# forecast in-sample response values using eq1
eq1F=rep("Negative",576)
eq1F[eq1Pr>0.5]="Positive"
# construct a confusion matrix
table(eq1F,r[train])
# forecast out-of-sample probabilities
eq1PrV=predict(eq1,dd[-train,1:8],type="response")
#forecast out-of-sample response values
eq1FV=rep("Negative",length(eq1PrV))
eq1FV[eq1PrV>0.5]="Positive" # please note that you might wish to augment the threshold probability (e.g. it might be 0.35 rather than 0.5)

#construct out-of-sample confusion matrix
table(eq1FV,r[-train])
a=table(eq1FV,r[-train])
View(a)

a=as.data.frame(a)

# True positive rate (the fraction of diabetes suffering correctly identified)
tp=a$Freq[4]/(a$Freq[3]+a$Freq[4])
# False positive rate (the fraction on non-diabetes suffering incorrectly classified)
fp=a$Freq[2]/(a$Freq[1]+a$Freq[2])

# Derive the ROC curve:
g=seq(from=0,to=0.99,by=0.0001)# a grid of treshold probs
roc=matrix(,nrow=length(g),ncol=2)

for (i in 1:length(g)){
  eq1FV=rep("Negative",length(eq1PrV))
  eq1FV[eq1PrV>g[i]]="Positive" 
  a=data.frame(table(eq1FV,r[-train]))
  tp=a$Freq[4]/(a$Freq[3]+a$Freq[4])
  fp=a$Freq[2]/(a$Freq[1]+a$Freq[2])
  roc[i,1]=fp
  roc[i,2]=tp
  rm(eq1FV,a,tp,fp)
}
roc=data.frame(roc)
colnames(roc)=c("FalsePositive", "TruePositive")
roc=na.omit(roc)
plot(roc$FalsePositive,roc$TruePositive,type="l",col="blue",lwd=2)
