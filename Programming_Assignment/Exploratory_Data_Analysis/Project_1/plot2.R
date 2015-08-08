## This code is to generate a lineplot for exercise 1 and output the graph as a png device
dat <- read.table('household_power_consumption.txt', header = TRUE, skip = 66636, nrows = 2880,  sep =';', colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
colnames(dat) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
datetime <- paste(dat$Date,dat$Time) ## using the paste function to combine date and time 
dat$datetime <- strptime(datetime, "%d/%m/%Y %H:%M:%S") ## converted to POSIXlt vector using strptime()

## dat$days <- weekdays(dat$datetime) ## this is not required as plotting dat$days returns a boxplot
## dat$days <- as.factor(dat$days) ##dat$days is defaulted to a character vector which needs to be converted to a factor vector prior to plotting

png(file = "plot2.png") ## Opens png device and creates plot2.png in the working directory
plot(dat$datetime, dat$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.off() ## closes the png file device
