plot1<- function(){
  #Read the data from the text file
  data<-read.table("household_power_consumption.txt", header=TRUE, sep=";")
  
  #Format the date column
  data[,1]<-as.Date(data[,1], format="%d/%m/%Y")
  
  #Create start and end Dates
  stDate<-as.Date("2007-02-01")
  endDate<-as.Date("2007-02-02")
  
  #Extract data within the prescribed date
  new_data<-data[data$Date==stDate | data$Date==endDate, ]
  
  #Convert the GAP column to numeric
  new_data[,3]<-as.numeric(as.character(new_data[,3]))
  
  #Create histogram and save in png file
  hist(new_data$Global_active_power, main="Global Active Power", col="red", xlab="Global Active Power(kilowatts)")
  dev.copy(png,file="plot1.png")
  dev.off()
}
