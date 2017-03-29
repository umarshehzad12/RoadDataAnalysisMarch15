rm(list=ls())

library(manipulate)
getwd()
setwd("G:\\Thesis\\march11 data 15\\complete gyro data")
directori<-list.dirs()

# intmed_df_all<-data.frame()
# intmed_df_events<-data.frame()
# intmed_df_non<-data.frame()

all_f_dir<-list()
events_f_dir<-list()
non_f_dir<-list()


all_data<-list()
events_data<-list()
non_data<-list()

for(i in 2:length(directori))
{
stringer<-c(directori[i],"merged_all.csv")
stringer2<-c(directori[i],"merged_events.csv")
stringer3<-c(directori[i],"merged_non_events.csv")
#gsub("/", "\\", new_f_dir[i],ignore.case=T)
all_f_dir[i]<-paste(stringer,collapse = "/")
events_f_dir[i]<-paste(stringer2,collapse = "/")
non_f_dir[i]<-paste(stringer3,collapse = "/")
all_data[[i-1]]<-read.table(all_f_dir[[i]], na.strings=c("","NA", " ", "0"), sep = " ", header = TRUE)
events_data[[i-1]]<-read.csv(events_f_dir[[i]], na.strings=c("","NA", " ", "0"), sep = " ", header = TRUE)
non_data[[i-1]]<-read.csv(non_f_dir[[i]], na.strings=c("","NA", " ", "0"), sep = " ", header = TRUE)

all_data[[i-1]][[1]]<-as.POSIXct(all_data[[i-1]][[1]], format="%Y-%m-%d %H:%M:%OS", tz="GMT")
events_data[[i-1]][[1]]<-as.POSIXct(events_data[[i-1]][[1]], format="%Y-%m-%d %H:%M:%OS", tz="GMT")
non_data[[i-1]][[1]]<-as.POSIXct(non_data[[i-1]][[1]], format="%Y-%m-%d %H:%M:%OS", tz="GMT")
}

mean_table<-data.frame(Data_Set=c(rep(0,each=(length(directori)-1))), x_mean_non_events=c(rep(0.0,each=(length(directori)-1))), x_mean_full=c(rep(0.0,each=(length(directori)-1))), x_mean_events=c(rep(0.0,each=(length(directori)-1))), y_mean_non_events=c(rep(0.0,each=(length(directori)-1))), y_mean_full=c(rep(0.0,each=(length(directori)-1))), y_mean_events=c(rep(0.0,each=(length(directori)-1))), z_mean_non_events=c(rep(0.0,each=(length(directori)-1))), z_mean_full=c(rep(0.0,each=(length(directori)-1))), z_mean_events=c(rep(0.0,each=(length(directori)-1))))
var_table<-data.frame(Data_Set=c(rep(0,each=(length(directori)-1))), x_var_non_events=c(rep(0.0,each=(length(directori)-1))), x_var_full=c(rep(0.0,each=(length(directori)-1))), x_var_events=c(rep(0.0,each=(length(directori)-1))), y_var_non_events=c(rep(0.0,each=(length(directori)-1))), y_var_full=c(rep(0.0,each=(length(directori)-1))), y_var_events=c(rep(0.0,each=(length(directori)-1))), z_var_non_events=c(rep(0.0,each=(length(directori)-1))), z_var_full=c(rep(0.0,each=(length(directori)-1))), z_var_events=c(rep(0.0,each=(length(directori)-1))))
sd_table<-data.frame(Data_Set=c(rep(0,each=(length(directori)-1))), x_sd_non_events=c(rep(0.0,each=(length(directori)-1))), x_sd_full=c(rep(0.0,each=(length(directori)-1))), x_sd_events=c(rep(0.0,each=(length(directori)-1))), y_sd_non_events=c(rep(0.0,each=(length(directori)-1))), y_sd_full=c(rep(0.0,each=(length(directori)-1))), y_sd_events=c(rep(0.0,each=(length(directori)-1))), z_sd_non_events=c(rep(0.0,each=(length(directori)-1))), z_sd_full=c(rep(0.0,each=(length(directori)-1))), z_sd_events=c(rep(0.0,each=(length(directori)-1))))
names(mean_table)<-c("Route", "non_events_x", "all_data_x", "events_x", "non_events_y", "all_data_y", "events_y", "non_events_z", "all_data_z", "events_z")
names(sd_table)<-c("Route", "non_events_x", "all_data_x", "events_x", "non_events_y", "all_data_y", "events_y", "non_events_z", "all_data_z", "events_z")
names(var_table)<-c("Route", "non_events_x", "all_data_x", "events_x", "non_events_y", "all_data_y", "events_y", "non_events_z", "all_data_z", "events_z")



for(i in 2:length(directori))
{
name_holder<-sub("./", "", directori[[i]])
mean_table[[1]][[i-1]]<-name_holder
var_table[[1]][[i-1]]<-name_holder
sd_table[[1]][[i-1]]<-name_holder

mean_table[[4]][[i-1]]<-mean(events_data[[i-1]]$x)
mean_table[[3]][[i-1]]<-mean(all_data[[i-1]]$x)
mean_table[[2]][[i-1]]<-mean(non_data[[i-1]]$x)
var_table[[4]][[i-1]]<-var(events_data[[i-1]]$x)
var_table[[3]][[i-1]]<-var(all_data[[i-1]]$x)
var_table[[2]][[i-1]]<-var(non_data[[i-1]]$x)
sd_table[[4]][[i-1]]<-sd(events_data[[i-1]]$x)
sd_table[[3]][[i-1]]<-sd(all_data[[i-1]]$x)
sd_table[[2]][[i-1]]<-sd(non_data[[i-1]]$x)

mean_table[[7]][[i-1]]<-mean(events_data[[i-1]]$y)
mean_table[[6]][[i-1]]<-mean(all_data[[i-1]]$y)
mean_table[[5]][[i-1]]<-mean(non_data[[i-1]]$y)
var_table[[7]][[i-1]]<-var(events_data[[i-1]]$y)
var_table[[6]][[i-1]]<-var(all_data[[i-1]]$y)
var_table[[5]][[i-1]]<-var(non_data[[i-1]]$y)
sd_table[[7]][[i-1]]<-sd(events_data[[i-1]]$y)
sd_table[[6]][[i-1]]<-sd(all_data[[i-1]]$y)
sd_table[[5]][[i-1]]<-sd(non_data[[i-1]]$y)

mean_table[[10]][[i-1]]<-mean(events_data[[i-1]]$z)
mean_table[[9]][[i-1]]<-mean(all_data[[i-1]]$z)
mean_table[[8]][[i-1]]<-mean(non_data[[i-1]]$z)
var_table[[10]][[i-1]]<-var(events_data[[i-1]]$z)
var_table[[9]][[i-1]]<-var(all_data[[i-1]]$z)
var_table[[8]][[i-1]]<-var(non_data[[i-1]]$z)
sd_table[[10]][[i-1]]<-sd(events_data[[i-1]]$z)
sd_table[[9]][[i-1]]<-sd(all_data[[i-1]]$z)
sd_table[[8]][[i-1]]<-sd(non_data[[i-1]]$z)

}


library(RColorBrewer)
plotter<-function(data,para)
{
display.brewer.all()
cols<-brewer.pal(n=5,name="Set1")
cols_t1<-cols[data$Event]
library(manipulate)
manipulate(
  plot(para, col=cols_t1, xlim=c(x.min,x.max)),  
  x.min=slider(0,(length(para)-20)), x.max=slider(20,length(para)))
legend("bottomleft", legend = names(attr(cols_t1, "event types")), title="events", fill=attr(cols_t1, "event types"))
}

plotter(events_data[[3]],events_data[[3]]$x)


# plotter<-function(data2,data)
# {
#   data<-as.numeric(data)
# manipulate(
#   plot(data2, xlim=c(x.min,x.max)),  
#   x.min=slider(min(data),max(data)),
#   x.max=slider(min(data),max(data))) 
#   # y.min=slider(min(data2),max(data2)),
#   # y.max=slider(min(data2),max(data2)))
# }
# 
# plotter(all_data[[1]][[1]],all_data[[1]][[2]])
# summary(all_data[[1]][[1]])
rm(mean_table)
rm(sd_table)
rm(var_table)
