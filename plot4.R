library(data.table)

# fread will deal with the semi-colon cleanly but I like being explicit.
# so I'm setting the column seperator.
sep = ";"
file = "../data/household_power_consumption.txt"

# Might be a better way of doing this; but I'm creating my own functions for the colClasses.
dateFunc = "ddMMyyyyFunc";
nFnc = "numericFunc"

setClass(dateFunc)
setClass(nFnc)

#  Creates functions to use for the conversion 
setAs("character",dateFunc,   function(from) as.Date(from, format="%d/%m/%Y") )
setAs("numeric", nFnc, function(from) if (from == "?")  NA else as.numeric(from)  )

# This is returning everything as character classes instead of paying attention to the colClasses
# like I thought it would.   Don't have the time to investigate other options.
# Resorting to subsetting and coercing by hand.
# pcdt ->  power consumption data table.
pcdt = fread(file, sep=sep, colClasses=c("Date", "time", nFnc, nFnc, nFnc, nFnc, nFnc, nFnc, nFnc))
pcdt[, Date2 := as.Date(Date, format="%d/%m/%Y")]
pcdt = pcdt[(pcdt$Date2 >= as.Date("2007-02-01")) & (pcdt$Date2 <= as.Date("2007-02-02"))]
pcdt[, DateTime := as.POSIXct(strptime(paste(pcdt$Date, pcdt$Time), "%d/%m/%Y %H:%M:%S", tz="UTC"))]
pcdt[, Global_active_power := as.numeric(Global_active_power)]
pcdt[, Global_reactive_power := as.numeric(Global_reactive_power)]
pcdt[, Voltage := as.numeric(Voltage)]
pcdt[, Global_intensity := as.numeric(Global_intensity)]
pcdt[, Sub_metering_1 := as.numeric(Sub_metering_1)]
pcdt[, Sub_metering_2 := as.numeric(Sub_metering_2)]
pcdt[, Sub_metering_3 := as.numeric(Sub_metering_3)]

png(filename = "plot4.png", width=480, height=480)
par(mfrow= c(2,2))

with(pcdt, 
     {
      plot(Global_active_power ~ DateTime, type="n", ylab="Global Active Power (kilowatts)", xlab="")
      lines(Global_active_power ~ DateTime)
      
      plot(Voltage ~ DateTime, type="n", ylab="Voltage", xlab="datetime")
      lines(Voltage ~ DateTime)
      
      plot(Sub_metering_1 ~ DateTime, type="n", ylab="Energy sub metering", xlab="")
      lines(Sub_metering_1 ~ DateTime, col="black")
      lines(Sub_metering_2 ~ DateTime, col="red")
      lines(Sub_metering_3 ~ DateTime, col="blue")
      legend("topright", bty="n", lty=c(1,1), col=c("black", "blue", "red"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
      
      plot(Global_reactive_power ~ DateTime, type="n", xlab="datetime")
      lines(Global_reactive_power ~ DateTime)
  }
)

dev.off()
