options(digits.secs=2)
{
rm(list=ls())
setwd("G:\\Thesis\\march11 data 15\\complete gyro data")
directori<-list.dirs()
keylog_data<-list()
intmed_df<-data.frame()
new_f_dir<-list()
redundant_f_dir<-list()
}

colselector<-function(data)
{
  data<-data[,c(1,2)]
  names(data)<-c("Event", "time")
  data<-na.omit(data)
  return(data)
}

colselector2<-function(data)
{
  data<-data[,c(1,3)]
  data<-na.omit(data)
  return(data)
}


i=2
for(i in 2:length(directori))
{
  {
  stringer<-c(directori[i],"file.log") 
  stringer2<-c(directori[i],"keylog.txt")
  stringer3<-c(directori[i],"redundant_keylog.txt")
  directori[i]<-paste(stringer,collapse = "/")
  new_f_dir[i]<-paste(stringer2,collapse = "/")
  redundant_f_dir[i]<-paste(stringer3,collapse = "/")
  #gsub("/", "\\", directori[i],ignore.case=T)
  intmed_df<-read.table(directori[i], na.strings=c("","NA", " ", "0"), sep = "\t")
  rows<-nrow(intmed_df)
  intmed_df<-intmed_df[1:(rows-1),]
  intmed_df<-colselector(intmed_df)
  intmed_df$date_time<-paste("2017-3-11", intmed_df$time, sep=" ")
  intmed_df<-colselector2(intmed_df)
  #intmed_df<-sub("\t", ", ", intmed_df)
  keylog_data[[i-1]]<-intmed_df
  }



{
intmed_df[[2]]<-as.POSIXct(intmed_df$date_time,format="%Y-%m-%d %H:%M:%OS", tz="GMT")
}



#intmed_f_2<-intmed_df
for(j in 2:length(intmed_df[,1]))
{
  if((intmed_df[[2]][[j]]-intmed_df[[2]][[j-1]])<1.1)
    intmed_df[[2]][[j]]<-intmed_df[[2]][[j]]+1
    #intmed_f_2<-intmed_df[-c(j),]
}

write.csv(intmed_df, file = new_f_dir[[i]], row.names = FALSE)
#intmed_f<-intmed_f_2

{
intmed_f_keylog_minus<-intmed_df
intmed_f_keylog_plus<-intmed_df
intmed_f_keylog_minus[[2]]<-intmed_df[[2]]-1
intmed_f_keylog_plus[[2]]<-intmed_df[[2]]+1
intmed_f_keylog_minus<-intmed_f_keylog_minus[rep(1:nrow(intmed_f_keylog_minus),each=100),]
intmed_f_keylog_plus<-intmed_f_keylog_plus[rep(1:nrow(intmed_f_keylog_plus),each=100),]
for(j in 1:length(intmed_f_keylog_minus[,1]))
{
  intmed_f_keylog_minus[[2]][[j]]<-(intmed_f_keylog_minus[[2]][[j]]+(0.01*(j%%(100))))
  intmed_f_keylog_plus[[2]][[j]]<-(intmed_f_keylog_plus[[2]][[j]]-(0.01*(j%%(100))))
}
intmed_df<-rbind.data.frame(intmed_f_keylog_minus,intmed_f_keylog_plus)
intmed_df<-intmed_df[order(intmed_df$date_time),] 
}

write.csv(intmed_df, file = redundant_f_dir[[i]], row.names = FALSE)
}  
