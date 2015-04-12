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
pcdt = fread(file, sep=sep, colClasses=c("Date", "time", nFnc, nFnc, nFnc, nFnc, nFnc, nFnc, nFnc))
pcdt[, Date := as.Date(Date, format="%d/%m/%Y")]
pcdt = pcdt[(pcdt$Date >= as.Date("2007-02-01")) & (pcdt$Date <= as.Date("2007-02-02"))]
pcdt[, Global_active_power := as.numeric(Global_active_power)]
pcdt[, Global_reactive_power := as.numeric(Global_reactive_power)]
pcdt[, Voltage := as.numeric(Voltage)]
pcdt[, Global_intensity := as.numeric(Global_intensity)]
pcdt[, Sub_metering_1 := as.numeric(Sub_metering_1)]
pcdt[, Sub_metering_2 := as.numeric(Sub_metering_2)]
pcdt[, Sub_metering_3 := as.numeric(Sub_metering_3)]

# Make sure we have the correct counts.
# grep "^1/2/2007;" household_power_consumption.txt | wc
# grep "^2/2/2007;" household_power_consumption.txt | wc
pcdt = pcdt[(pcdt$Date >= as.Date("2007-02-01")) & (pcdt$Date <= as.Date("2007-02-02"))]
#str(pcdt)

# Looks like the png device defaults to 480 but providing it here in case windows/linux does something different.
png(filename = "plot1.png", width=480, height=480)
#quartz()
hist(pcdt$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()
