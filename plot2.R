## This function reads the household power consumption file and generates Date Time Vs Global Active power plot usagefor the dates 01/02/2007 and 02/02/2007 .  

plot2<-function(filename){
  
  ## Read the ';' delimited household power consumption file
  dataset<- read.delim(filename,header=T,sep=";")
  
  ## Keep the data from the dates 01/02/2007 and 02/02/2007
  dataset[,1]<-as.Date( dataset[,1],"%d/%m/%Y")
  dataset <- dataset[(dataset$Date== as.Date("01/02/2007","%d/%m/%Y"))|(dataset$Date== as.Date("02/02/2007","%d/%m/%Y")),]
  
  ## Discard anyrows where there is no value for Global Active Power
  dataset<- dataset[dataset$Global_active_power!="?",]
  dataset$Global_active_power <- as.numeric(dataset$Global_active_power)
  
  ## Create a new column by combining Date and time and converting it to datetime. 
  dataset$DateTime<- strptime(paste(dataset$Date,dataset$Time),"%Y-%m-%d %H:%M:%S")
  
  ## Generate a plot as a png file.
  with(dataset,plot(DateTime, Global_active_power/500, type="l", ylab="Global Active Power (kilowatts)", xlab=""))
  dev.copy(png,file="plot2.png")
  dev.off()
}