rm(list=ls())
library(manipulate)
options(digits.secs=5)
getwd()
setwd("G:\\Thesis\\march11 data 15\\complete gyro data")
intmed_df<-read.table("combined_gyro_data.csv", na.strings=c("","NA", " ", "0"), sep = " ", fill = TRUE, header = TRUE)

intmed_df2<-intmed_df[order(intmed_df$date_time),]

intmed_df2<-intmed_df2[!duplicated(intmed_df2),]
intmed_df2$date_time<-as.POSIXct(intmed_df2$date_time, format="%Y-%m-%d %H:%M:%OS", tz="GMT")
intmed_df2$row_num<-c(1:length(intmed_df2$date_time))

manipulate(
  plot(intmed_df2$date_time, xlim = c(x.min,x.max)),
  x.min=slider(as.numeric(intmed_df2[[3]][[1]]),as.numeric(intmed_df2[[3]][[10400]])),
  x.max=slider(as.numeric(intmed_df2[[3]][[300]]),as.numeric(intmed_df2[[3]][[10453]]))
  
)

manipulate(
  plot(intmed_df2$date_time, xlim = c(x.min, x.max), ylim = c(y.min,y.max)),
  x.min=slider(min(intmed_df2$row_num),max(intmed_df2$row_num)),
  x.max=slider(min(intmed_df2$row_num),max(intmed_df2$row_num)),
  y.min=slider(min(as.numeric(intmed_df2$date_time)),max(as.numeric(intmed_df2$date_time))),
  y.max=slider(min(as.numeric(intmed_df2$date_time)),max(as.numeric(intmed_df2$date_time)))
)

intmed_df<-intmed_df2[,c(1,2,3,4)]
intmed_df<-na.omit(intmed_df)
write.table(intmed_df,file = "combined_gyro_cleaned.csv", row.names = FALSE)
