## This function reads the household power consumption file and generates histogram for Global Active power usagefor the dates 01/02/2007 and 02/02/2007 .  

plot1<-function(filename){
  ## Read the ';' delimited household power consumption file
   dataset<- read.delim(filename,header=T,sep=";")
   
   ## Keep the data from the dates 01/02/2007 and 02/02/2007
   dataset[,1]<-as.Date( dataset[,1],"%d/%m/%Y")
   dataset <- dataset[(dataset$Date== as.Date("01/02/2007","%d/%m/%Y"))|(dataset$Date== as.Date("02/02/2007","%d/%m/%Y")),]
  
   ## Discard anyrows where there is no value for Global Active Power
   dataset<- dataset[dataset$Global_active_power!="?",]
   dataset$Global_active_power <- as.numeric(dataset$Global_active_power)
   
   ## Generate a histogram as a png file.
   with(dataset,hist(Global_active_power/500,col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)"))
   dev.copy(png,file="plot1.png")
   dev.off()
}