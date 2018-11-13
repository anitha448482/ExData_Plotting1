#Week 1 Course 4 Exploratory Data Analysis
if(!file.exists("household_power_consumption.txt")) {
  print("File not found")
}

#Read the data set into a dataframe
hpc <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, stringsAsFactors = FALSE)

#Create another table for the days we are interested in
feb_hpc <- subset(hpc, Date == '1/2/2007' | Date == '2/2/2007')
feb_hpc$DateTime <- paste(feb_hpc$Date, feb_hpc$Time)

#Convert the Date and DateTime column to POSIXlt format and set time zone to Europe/Paris
feb_hpc$Date <- as.Date(strptime(feb_hpc$Date, "%d/%m/%Y",tz = "Europe/Paris"))
feb_hpc$DateTime <- strptime(feb_hpc$DateTime, "%d/%m/%Y %H:%M:%S", tz="Europe/Paris")

#Add the name of the day
feb_hpc$Day <- weekdays(feb_hpc$Date)

#Convert the char column to numeric
feb_hpc$Global_active_power <- as.numeric(feb_hpc$Global_active_power)

#First plot: Frequency of Global Active Power
hist(feb_hpc$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatt)")
#Write this into png file
png(filename = "FrequencyGlobalActivePower.png", width = 480, height = 480)
hist(feb_hpc$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatt)")
dev.off()
dev.cur()

#Second plot: Global active power vs Day
png(filename = "2.png", width = 480, height = 480)
plot(feb_hpc$DateTime, feb_hpc$Global_active_power, type="l", ylab = "Global Active Power(kilowatt)", xlab=" ")
dev.off()
feb_hpc_bak <- feb_hpc
feb_hpc$Sub_metering_1 <- as.numeric(feb_hpc$Sub_metering_1)
feb_hpc$Sub_metering_2 <- as.numeric(feb_hpc$Sub_metering_2)

#Third plot: 
png(filename = "3.png", width = 480, height = 480)
# Create a first line
plot(feb_hpc$DateTime, feb_hpc$Sub_metering_1, type = "l", col = "black", ylab = "Global Active Power(kilowatt)", xlab=" ")
# Add a second line
lines(feb_hpc$DateTime, feb_hpc$Sub_metering_2, type = "l", col = "red")
lines(feb_hpc$DateTime, feb_hpc$Sub_metering_3, type = "l", col = "blue")
# Add a legend to the plot
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lty = 1)
dev.off()

#Fourth plot: Voltage vs Day
plot(feb_hpc$DateTime, feb_hpc$Voltage, type = "l", col = "black", ylab = "Voltage", xlab="datetime")
#Fifth plot: Global reactive power vs datetime
plot(feb_hpc$DateTime, feb_hpc$Global_reactive_power, type = "l", col = "black", ylab = "Global_reactive_power", xlab="datetime")

#Multiplegraphs on a page
#First set the parameters: 2 rows and 2 cols
par(mfrow=c(2,2))
#First plot: 1,1
plot(feb_hpc$DateTime, feb_hpc$Global_active_power, type="l", ylab = "Global Active Power(kilowatt)", xlab=" ")
#Second plot: 1,2
plot(feb_hpc$DateTime, feb_hpc$Voltage, type = "l", col = "black", ylab = "Voltage", xlab="datetime")
#Third plot: 2,1
plot(feb_hpc$DateTime, feb_hpc$Sub_metering_1, type = "l", col = "black", ylab = "Global Active Power(kilowatt)", xlab=" ")
# Add a second line
lines(feb_hpc$DateTime, feb_hpc$Sub_metering_2, type = "l", col = "red")
lines(feb_hpc$DateTime, feb_hpc$Sub_metering_3, type = "l", col = "blue")
# Add a legend to the plot
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lty = 1)

#Fourth plot: 2,2
plot(feb_hpc$DateTime, feb_hpc$Global_reactive_power, type = "l", col = "black", ylab = "Global_reactive_power", xlab="datetime")

#Write into png file
png(filename = "4.png", width = 480, height = 480)
par(mfrow=c(2,2))
#First plot: 1,1
plot(feb_hpc$DateTime, feb_hpc$Global_active_power, type="l", ylab = "Global Active Power(kilowatt)", xlab=" ")
#Second plot: 1,2
plot(feb_hpc$DateTime, feb_hpc$Voltage, type = "l", col = "black", ylab = "Voltage", xlab="datetime")
#Third plot: 2,1
plot(feb_hpc$DateTime, feb_hpc$Sub_metering_1, type = "l", col = "black", ylab = "Global Active Power(kilowatt)", xlab=" ")
# Add a second line
lines(feb_hpc$DateTime, feb_hpc$Sub_metering_2, type = "l", col = "red")
lines(feb_hpc$DateTime, feb_hpc$Sub_metering_3, type = "l", col = "blue")
# Add a legend to the plot
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lty = 1)

#Fourth plot: 2,2
plot(feb_hpc$DateTime, feb_hpc$Global_reactive_power, type = "l", col = "black", ylab = "Global_reactive_power", xlab="datetime")
dev.off()
