plot2<- function(){
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
  
  #Convert the GAP column to numeric
  new_data[,3]<-as.numeric(as.character(new_data[,3]))
    
  #head(new_data)
  #nrow(new_data)
  
  #Create xy plot and save in png file
  plot(new_data$dateTime, new_data$Global_active_power, xlab="Days", ylab="Global Active Power (kilowatts)", type="n")
  lines(new_data$dateTime, new_data$Global_active_power)
  dev.copy(png,file="plot2.png")
  dev.off()
}
