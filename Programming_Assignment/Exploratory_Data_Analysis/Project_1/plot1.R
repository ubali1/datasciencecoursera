## This code is to generate a histogram for exercise 1 and output the graph as a png device
dat <- read.table('household_power_consumption.txt', header = TRUE, skip = 66636, nrows = 2880,  sep =';', colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
colnames(dat) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
png(file = "plot1.png") ## Opens png device and creates plot1.png in the working directory
hist(dat$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
dev.off() ## closes the png file device
