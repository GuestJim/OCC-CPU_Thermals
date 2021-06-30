TEXT	=	readLines("AMD-CPU Profile.csv",	warn = FALSE,	n = 100)
for (LINE in 1:length(TEXT))	{	if (TEXT[LINE] == "PROFILE RECORDS")	break	}
#	the above loads in the uProf file to find when the data actually starts, R can properly and automatically load it.
#	to save time, it only loads in the first 100 lines, which should be enough for most any consumer CPU, but increase that number if necessary

uProf	=	read_csv("AMD-CPU Profile.csv", skip = LINE)

threadFREQ	=	pivot_longer(uProf[, c(1, grep("thread", colnames(uProf)))],
			cols			=	-1,
			names_to		=	"Thread",
			names_pattern	=	"thread([:digit:]+)-core-effective-frequency",
			# names_ptypes	=	list(Thread = numeric()),
			values_to		=	"Frequency"
)
threadFREQ$Core	=	floor(as.numeric(threadFREQ$Thread)/2)

coreENG		=	pivot_longer(uProf[, c(1, grep("core.*-energy", colnames(uProf)))],
			cols			=	-1,
			names_to		=	"Core",
			names_pattern	=	"core([:digit:]+)-energy",
			names_ptypes	=	list(Core = factor(ordered = TRUE)),
			values_to		=	"Core_Energy"
)

sockENG		=	pivot_longer(uProf[, c(1, grep("socket", colnames(uProf)))],
			cols			=	-1,
			names_to		=	"Socket",
			names_pattern	=	"socket([:digit:]+)-package-energy",
			names_ptypes	=	list(Socket = factor(ordered = TRUE)),
			values_to		=	"Socket_Energy"
)

sockENG$Uncore_Energy	=	rowSums(uProf[, grep("socket(.*)-package-energy", colnames(uProf))]) - rowSums(uProf[, grep("core(.*)-energy", colnames(uProf))])
#	subtracts the energy measured for each core from the Socket energy measurement, giving us the remaining Uncore energy usage
#		curiously can result in negative values which I am taking to be a measurement error and just ignoring

uProfTALL	=	merge(threadFREQ,	coreENG,	by = c("RecordId", "Core"),	sort = FALSE)
uProfTALL	=	merge(uProfTALL,	sockENG,	by = "RecordId",			sort = FALSE)

colnames(uProfTALL)[grep("RecordId", colnames(uProfTALL))]	=	"Time"


GPUz	=	read_csv("GPU-Z Sensor Log.txt")
GPUz	=	GPUz[, pmatch(c("Date", "CPU Temperature"), colnames(GPUz))]
colnames(GPUz)	=	c("Timestamp", "CPUTemp")

# GPUz$Time	=	as.numeric(GPUz$Timestamp) - as.numeric(GPUz$Timestamp[1]) + 1
GPUz$Time	=	1:nrow(GPUz)
#	it seems GPUz is not perfect at keeping to 1 second intervals, causing multiple measurements at the same reported timestamp
#	forcing it to 1 Hz sampling rate

dataALL		=	merge(uProfTALL, GPUz[, -grep("Timestamp", colnames(GPUz))], by = "Time", sort = FALSE)
# dataALL$Time	=	dataALL$Time - warm

PERIODS	=	function(DATA,	BREAKS = c(warm, duration),	LABELS = levsPER){
	out	=	ifelse(DATA$Time <= BREAKS[1], LABELS[1],
			ifelse(BREAKS[1] < DATA$Time & DATA$Time <= BREAKS[2] + BREAKS[1], LABELS[2],
			ifelse(BREAKS[2] + BREAKS[1] < DATA$Time, LABELS[3], NA
				)))
	out	=	ordered(out, levels = LABELS)
	return(out)
}

dataALL$Period	=	PERIODS(dataALL)
dataALL$Time	=	dataALL$Time - warm

diff.CONS	=	function(DATA, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(DATA, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(DATA, lag = lag)))
}

dataALL$CPUTempDiff	=	diff.CONS(dataALL$CPUTemp, lag = length(unique(dataALL$Thread)))

dataALL	=	dataALL[order(dataALL$Time, dataALL$Socket, dataALL$Core, dataALL$Thread),]

write_csv(dataALL, "Combined.csv.bz2")