## This function reads the household power consumption file and generates Date Time Vs Energy Sub meeting plot usagefor the dates 01/02/2007 and 02/02/2007 .  

plot3<-function(filename){
  
  ## Read the ';' delimited household power consumption file
  dataset<- read.delim(filename,header=T,sep=";")
  
  ## Keep the data from the dates 01/02/2007 and 02/02/2007
  dataset[,1]<-as.Date( dataset[,1],"%d/%m/%Y")
  dataset <- dataset[(dataset$Date== as.Date("01/02/2007","%d/%m/%Y"))|(dataset$Date== as.Date("02/02/2007","%d/%m/%Y")),]
  print(nrow(dataset))
  ## Discard anyrows where there is no value for Sub_metering_1, Sub_metering_2, Sub_metering_3
  dataset<- dataset[dataset$Sub_metering_1!="?",]
  dataset$Sub_metering_1 <- as.numeric(dataset$Sub_metering_1)
  print(nrow(dataset))
  dataset<- dataset[dataset$Sub_metering_2!="?",]
  dataset$Sub_metering_2 <- as.numeric(dataset$Sub_metering_2)
  print(nrow(dataset))
  dataset<- dataset[dataset$Sub_metering_3!="?",]
  dataset$Sub_metering_3 <- as.numeric(dataset$Sub_metering_3)
  print(nrow(dataset))
  ## Create a new column by combining Date and time and converting it to datetime. 
  dataset$DateTime<- strptime(paste(dataset$Date,dataset$Time),"%Y-%m-%d %H:%M:%S")
  
  ## Generate a plot as a png file.
  
  with(dataset,plot(DateTime, Sub_metering_1, type="l", col="black", ylab="Energy sub metering", xlab=""))
  with(dataset,lines(DateTime, Sub_metering_2/5, type="l", col="red", ylab="", xlab=""))
  with(dataset,lines(DateTime, Sub_metering_3, type="l", col="blue", ylab="", xlab=""))
  legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1),lwd=c(1,1,1),
         col=c("black", "red", "blue"))
  dev.copy(png,file="plot3.png")
  dev.off()
}