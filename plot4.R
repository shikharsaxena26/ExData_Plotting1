plot4<- function(){
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
  
  par(mfrow=c(2,2), mar=c(4,4,2,1))
  
  #Create xy plot for Golbal active power
  plot(new_data$dateTime, new_data$Global_active_power, xlab="Days", ylab="Global Active Power (kilowatts)", type="n")
  lines(new_data$dateTime, new_data$Global_active_power)
      
  #Create xy plot for Voltage
  plot(new_data$dateTime, new_data$Voltage, xlab="datetime", ylab="Voltage", type="n")
  lines(new_data$dateTime, new_data$Voltage)
  
  
  #Convert the sub metering column to numeric
  new_data[,7]<-as.numeric(as.character(new_data[,7]))
  new_data[,8]<-as.numeric(as.character(new_data[,8]))
  new_data[,9]<-as.numeric(as.character(new_data[,9]))
  
  #Create DateTime and mutiple columns matrix for plot generation 
  dt<-new_data$dateTime
  m<-new_data[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3")]
  
  #Create multi-plot of sub meter readings
  matplot(dt,m,type="l",xlab="Days", xaxt="n", ylab="Energy sub metering", col=c("black","red","blue"))
  axis(1,at=c(1170300000,1170350000,1170400000), labels=c("Thu","Fri","Sat"))
  legend("topright", lwd=1, lty=c(1,1,1), pch=c(NA,NA,NA), col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),cex=0.5)  
  
  #Create xy plot for Golbal reactive power
  plot(new_data$dateTime, new_data$Global_reactive_power, xlab="datetime", ylab="Global reactive power", type="n")
  lines(new_data$dateTime, new_data$Global_reactive_power)
  
  dev.copy(png,file="plot4.png")
  dev.off()
}  