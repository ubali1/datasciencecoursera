## This code is to generate a lineplot for exercise 1 with subsetted data and output the graph as a png device
dat <- read.table('household_power_consumption.txt', header = TRUE, skip = 66636, nrows = 2880,  sep =';', colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
colnames(dat) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
datetime <- paste(dat$Date,dat$Time) ## using the paste function to combine date and time 
dat$datetime <- strptime(datetime, "%d/%m/%Y %H:%M:%S") ## converted to POSIXlt vector using strptime()

png(file = "plot3.png") ## Opens png device and creates plot3.png in the working directory
plot(dat$datetime, dat$Sub_metering_1, type = "l", xlab = "", ylab = "", col = "black")
lines(dat$datetime, dat$Sub_metering_2, type = "l", xlab = "", ylab = "", col = "red")
lines(dat$datetime, dat$Sub_metering_3, type = "l", xlab = "", ylab = "", col = "blue")
legend("topright", pch = "-", col = c("black", "red", "blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.off() ## closes the png file device