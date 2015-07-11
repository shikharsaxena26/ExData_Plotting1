plot3<- function(){
  #Read the data from the text file
  data<-read.table("household_power_consumption.txt", header=TRUE, sep=";")
  
  #Merge the date and time for the x-axis
  data<-within(data, dateTime<-paste(data[,1],data[,2], sep=" "))
  data$dateTime<-as.POSIXct(data$dateTime, format="%d/%m/%Y %H:%M:%S")
  
  #Format the date and time column
  data[,1]<-as.Date(data[,1], format="%d/%m/%Y")
  
  #Create start and end Dates
  stDate<-as.Date("2007-02-01")
  endDate<-as.Date("2007-02-02")
  
  #Extract data within the prescribed date
  new_data<-data[data$Date==stDate | data$Date==endDate, ]
  
  #Convert the sub metering column to numeric
  new_data[,7]<-as.numeric(as.character(new_data[,7]))
  new_data[,8]<-as.numeric(as.character(new_data[,8]))
  new_data[,9]<-as.numeric(as.character(new_data[,9]))
  
  #Create DateTime and mutiple columns matrix for plot generation 
  dt<-new_data$dateTime
  m<-new_data[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3")]
  
  #Create multi-plots and save in png file
  matplot(dt,m,type="l",xlab="Days", xaxt="n", ylab="Energy sub metering", col=c("black","red","blue"))
  axis(1,at=c(1170300000,1170350000,1170400000), labels=c("Thu","Fri","Sat"))
  legend("topright", lwd=1, lty=c(1,1,1), pch=c(NA,NA,NA), col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex=0.75)  
  dev.copy(png,file="plot3.png")
  dev.off()
}
