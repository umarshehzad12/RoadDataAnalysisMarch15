{
rm(list=ls())
getwd()
setwd("G:\\Thesis\\march11 data 15\\complete gyro data")
directori<-list.dirs()

intmed_df_gyro<-data.frame()
intmed_df_gps<-data.frame()
intmed_df_key<-data.frame()

gps_data<-list()
merged_data<-list()
events_data<-list()
non_events_data<-list()
gyro_f_dir<-list()
gps_f_dir<-list()
keylog_f_dir<-list()
merged_f_dir<-list()

gyro_length<-vector()
gps_length<-vector()

all_f_dir<-list()
events_f_dir<-list()
non_f_dir<-list()


# gps_time_intmed<-data.frame()
# gyro_time_intmed<-data.frame()

# gps_time<-list()
# gyro_time<-list()

# for(i in 2:length(directori))
# {
#   colselector<-function(data)
#   {
#     names(data)<-c("x", "x_normalized", "y", "y_normalized", "z", "z_normalized", "time")
#     data<-na.omit(data)
#     data<-data[,c(1,3,5,7)]
#     return(data)
#   }
# }

options(digits.secs=2)
colselector<-function(data)
{
  data<-na.omit(data)
  data<-data[,c(1,2,3,4,6,7)]
  return(data)
}

colselector_events<-function(data)
{
  data<-na.omit(data)
  data<-data[,c(1,2,3,4,6,7,9)]
  return(data)
}


}

i<-4
for(i in 2:length(directori))
{
  
  {
  gps_df<-data.frame()
  stringer<-c(directori[i],"gyro_vals.txt")
  stringer2<-c(directori[i],"gps_vals.txt")
  stringer3<-c(directori[i],"redundant_keylog.txt")
  #gsub("/", "\\", new_f_dir[i],ignore.case=T)
  gyro_f_dir[i]<-paste(stringer,collapse = "/")
  gps_f_dir[i]<-paste(stringer2,collapse = "/")
  keylog_f_dir[i]<-paste(stringer3,collapse = "/")
  intmed_df_gyro<-read.csv(gyro_f_dir[[i]], na.strings=c("","NA", " ", "0"))
  intmed_df_gps<-read.csv(gps_f_dir[[i]], na.strings=c("","NA", " ", "0"))
  intmed_df_key<-read.csv(keylog_f_dir[[i]], na.strings=c("","NA", " ", "0"))
  
  
  gyro_length_1<-length(intmed_df_gyro$date_time)
  gps_length_1<-length(intmed_df_gps$date_time)
  
  
  #intmed_df_gps<-colselector(intmed_df_gps)
  intmed_df_gps_duplicate<-intmed_df_gps[duplicated(intmed_df_gps$date_time),]
  intmed_df_gps<-intmed_df_gps[!duplicated(intmed_df_gps$date_time),]
  intmed_df_gyro_duplicate<-intmed_df_gyro[duplicated(intmed_df_gyro$date_time),]
  intmed_df_gyro<-intmed_df_gyro[!duplicated(intmed_df_gyro$date_time),]
  intmed_df_gps$date_time<- as.POSIXct(intmed_df_gps$date_time,format="%Y-%m-%d %H:%M:%OS", tz="GMT")
  intmed_df_gyro$date_time<- as.POSIXct(intmed_df_gyro$date_time,format="%Y-%m-%d %H:%M:%OS", tz="GMT")
  intmed_df_key$date_time<- as.POSIXct(intmed_df_key$date_time,format="%Y-%m-%d %H:%M:%OS", tz="GMT")
  
  intmed_df_gps[[3]]=as.POSIXct(intmed_df_gps[[3]], tz="GMT")
  #gps_length[[i-1]]<-length(intmed_df_gps$date_time)
  gyro_length_2<-length(intmed_df_gyro$date_time)
  gps_length_2<-length(intmed_df_gps$date_time)
  # 
  
    # intmed_df_gps[gps_length+1,]<-intmed_df_gps[gps_length,]
  # intmed_df_gps[gps_length+2,]<-intmed_df_gps[gps_length,]
  # 
  # intmed_df_gps[gps_length+1,3]<-intmed_df_gps[gps_length+1,3]+1
  # intmed_df_gps[gps_length+2,3]<-intmed_df_gps[gps_length+2,3]+2
  
  
  intmed_df_gps[gps_length_2+1,]<-intmed_df_gps[gps_length_2,]
  intmed_df_gps[gps_length_2+1,3]<-intmed_df_gps[gps_length_2+1,3]+1
  #j<-692
  }
  for(j in 1:(length(intmed_df_gps$date_time)-1))
  {
    
    
    d_<-as.integer(((intmed_df_gps[[3]][[j+1]]-intmed_df_gps[[3]][[j]]))*100)
    intmed_df_gps_rep<-intmed_df_gps[rep(j,each=d_),]
    
    
    {
      
      for(k in 1:length(intmed_df_gps_rep[,1]))
      {
        intmed_df_gps_rep[[3]][[k]]<-(intmed_df_gps_rep[[3]][[k]]+(0.01*(k%%(d_))))
      }
      
      
      intmed_df_gps_rep<-intmed_df_gps_rep[order(intmed_df_gps_rep$date_time),]
      gps_data[[j]]<-intmed_df_gps_rep
      
    }
  
  
}
  
  prev_df_l<-1
  
  
  
  
  for(m in 1:length(gps_data))
  {
    current_df_l<-length(gps_data[[m]][[1]])+prev_df_l-1
    gps_df[prev_df_l:current_df_l,1]<-gps_data[[m]][[1]]
    gps_df[prev_df_l:current_df_l,2]<-gps_data[[m]][[2]]
    gps_df[prev_df_l:current_df_l,3]<-as.POSIXct(gps_data[[m]][[3]])
    names(gps_df)<-c("Latitude", "Longitude", "date_time")
    prev_df_l<-current_df_l+1
    
    if(m<2)
    {
    gps_df[1,3]<-"1-1-1 1:1:1"
    gps_df[,3]<-as.POSIXct(gps_df[,3],format="%Y-%m-%d %H:%M:%OS", tz="GMT")
    }
    
  }
  
  
  gps_df<-na.omit(gps_df)
  options(digits.secs=2)
  intmed_df_gyro[[4]]=as.POSIXct(intmed_df_gyro[[4]], tz="GMT")
  gps_df$time_f<-as.factor(gps_df[[3]])
  intmed_df_gyro$time_f<-as.factor(intmed_df_gyro$date_time)
  intmed_df_key$time_f<-as.factor(intmed_df_key[[2]])
  merged<-merge(intmed_df_gyro,gps_df, "time_f")
  merged_length_1<-length(merged$time_f)
  rm(gps_df)
  merged<-merged[!duplicated(merged),]
  
  merged_length_2<-length(merged$time_f)
  
  events<-merge(merged,intmed_df_key, "time_f")
  
  events<-na.omit(events)
  merged<-colselector(merged)
  #table(!duplicated(merged))["TRUE"]
  events<-colselector_events(events)
  non_events<-merged[ !(merged$time_f %in% events$time_f), ]
  merged_data[[i-1]]<-merged
  events_data[[i-1]]<-events
  non_events_data[[i-1]]<-non_events
}


for(n in 2:length(directori))
{
stringer4<-c(directori[n],"merged_all.csv")
stringer5<-c(directori[n],"merged_events.csv")
stringer6<-c(directori[n],"merged_non_events.csv")
#gsub("/", "\\", new_f_dir[i],ignore.case=T)
all_f_dir[n-1]<-paste(stringer4,collapse = "/")
events_f_dir[n-1]<-paste(stringer5,collapse = "/")
non_f_dir[n-1]<-paste(stringer6,collapse = "/")
write.table(merged_data[[n-1]],file= all_f_dir[[n-1]],row.names = FALSE)
write.table(events_data[[n-1]],file= events_f_dir[[n-1]],row.names = FALSE)
write.table(non_events_data[[n-1]],file= non_f_dir[[n-1]],row.names = FALSE)
}
