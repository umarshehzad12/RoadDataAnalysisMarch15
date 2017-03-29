rm(list=ls())
getwd()
setwd("G:\\Thesis\\march11 data 15\\complete gyro data")
directori<-list.dirs()
gps_dir<-list()
gyro_dir<-list()
gyro_data<-read.table("combined_gyro_data.csv", na.strings=c("","NA", " ", "0"), sep = " ", fill = TRUE, header = TRUE)
gps_data<-read.table("combined_gps_data.csv", na.strings=c("","NA", " ", "0"), sep = ",", fill = TRUE, header = TRUE)
gps_data<-gps_data[order(gps_data$date_time),]

gps_data<-gps_data[!duplicated(gps_data),]
gps_data$date_time<-as.POSIXct(gps_data$date_time, format="%Y-%m-%d %H:%M:%OS", tz="GMT")
gps_data<-na.omit(gps_data)
gyro_data<-gyro_data[order(gyro_data$date_time),]

gyro_data<-gyro_data[!duplicated(gyro_data),]
gyro_data$date_time<-as.POSIXct(gyro_data$date_time, format="%Y-%m-%d %H:%M:%OS", tz="GMT")
gyro_data<-na.omit(gyro_data)

for(i in 2:length(directori))
{
stringer<-c(directori[i],"keylog.txt") 
stringer2<-c(directori[i],"gps_vals.txt") 
stringer3<-c(directori[i],"gyro_vals.txt")
directori[i]<-paste(stringer,collapse = "/")
gps_dir[i]<-paste(stringer2,collapse = "/")
gyro_dir[i]<-paste(stringer3,collapse = "/")

#gsub("/", "\\", directori[i],ignore.case=T)
intmed_df<-read.table(directori[i], na.strings=c("","NA", " ", "0"), sep = ",", header = TRUE)
intmed_df$date_time<-as.POSIXct(intmed_df$date_time, tz="GMT")
start_time<-intmed_df$date_time[1]
end_time<-intmed_df$date_time[length(intmed_df$date_time)]

gyro_range<-(gyro_data$date_time>(start_time-5) & gyro_data$date_time<(end_time+5))
gyro_selected<-gyro_data[gyro_range,c(1,2,3,4)]

gps_range<-(gps_data$date_time>(start_time-5) & gps_data$date_time<(end_time+5))
gps_selected<-gps_data[gps_range,]

write.table(gps_selected,file = gps_dir[[i]], row.names = FALSE, sep = ", ")
write.table(gyro_selected,file = gyro_dir[[i]], row.names = FALSE, sep = ", ")
}

