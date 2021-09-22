TEXT	=	readLines("AMD-CPU Profile.csv",	warn = FALSE,	n = 100)
for (LINE in 1:length(TEXT))	{	if (TEXT[LINE] == "PROFILE RECORDS")	break	}
#	the above loads in the uProf file to find when the data actually starts, R can properly and automatically load it.
#	to save time, it only loads in the first 100 lines, which should be enough for most any consumer CPU, but increase that number if necessary

uProf	=	read_csv("AMD-CPU Profile.csv", skip = LINE)

threadFREQ	=	pivot_longer(uProf[, c(1, 2, grep("thread.*-core-effective-frequency", colnames(uProf)))],
			cols			=	-c(1, 2),
			names_to		=	"Thread",
			names_pattern	=	"thread([:digit:]+)-core-effective-frequency",
			# names_ptypes	=	list(Thread = numeric()),
			values_to		=	"Frequency"
)
threadFREQ$Core	=	floor(as.numeric(threadFREQ$Thread)/2)

coreENG		=	pivot_longer(uProf[, c(1, 2, which(startsWith(colnames(uProf), "core")))],
			cols			=	-c(1, 2),
			names_to		=	"Core",
			names_pattern	=	"core([:digit:]+)-",
			# names_ptypes	=	list(Core = factor(ordered = TRUE)),
			values_to		=	"Core_Energy"
)

sockENG		=	pivot_longer(uProf[, c(1, 2, grep("socket.*-package", colnames(uProf)))],
			cols			=	-c(1, 2),
			names_to		=	"Socket",
			names_pattern	=	"socket([:digit:]+)-package",
			names_ptypes	=	list(Socket = factor(ordered = TRUE)),
			values_to		=	"Socket_Energy"
)

if (any(grepl("socket.*-temperature", colnames(uProf))))	{
	sockTEMP	=	pivot_longer(uProf[, c(1, 2, grep("socket.*-temperature", colnames(uProf)))],
				cols			=	-c(1, 2),
				names_to		=	"Socket",
				names_pattern	=	"socket([:digit:]+)-temperature",
				names_ptypes	=	list(Socket = factor(ordered = TRUE)),
				values_to		=	"Socket_Temp"
	)
	sockENG	=	merge(sockENG,	sockTEMP,	by = c("RecordId", "Timestamp", "Socket"),	sort = FALSE)
}

if (max(coreENG$Core_Energy) < 1000)	coreENG$Core_Energy		=	coreENG$Core_Energy * 1000
if (max(sockENG$Socket_Energy) < 1000)	sockENG$Socket_Energy	=	sockENG$Socket_Energy * 1000
#	newer versions of uProf use Watts instead of mJ, so for consistency the measurements are multiplied when necessary

sockENG$Uncore_Energy	=	rowSums(uProf[, grep("socket(.*)-package-energy", colnames(uProf))]) - rowSums(uProf[, grep("core(.*)-energy", colnames(uProf))])
#	subtracts the energy measured for each core from the Socket energy measurement, giving us the remaining Uncore energy usage
#		curiously can result in negative values which I am taking to be a measurement error and just ignoring

uProfTALL	=	merge(threadFREQ,	coreENG,	by = c("RecordId", "Timestamp", "Core"),	sort = FALSE)
uProfTALL	=	merge(uProfTALL,	sockENG,	by = c("RecordId", "Timestamp"),			sort = FALSE)

uProfTALL$Time	=	strptime(uProfTALL$Timestamp, "%H:%M:%S")
uProfTALL$Time	=	as.numeric(uProfTALL$Time - min(uProfTALL$Time)) + 1
# colnames(uProfTALL)[grep("RecordId", colnames(uProfTALL))]	=	"Time"


GPUz	=	read_csv("GPU-Z Sensor Log.txt")
GPUz	=	GPUz[, pmatch(c("Date", "CPU Temperature"), colnames(GPUz))]
colnames(GPUz)	=	c("Timestamp", "CPU_Temp")

GPUz$Time	=	as.numeric(GPUz$Timestamp)
GPUz$Time	=	GPUz$Time - min(GPUz$Time) + 1
#	converts Timestamp to number of seconds and then removes the minimum to make measurements relative, and starts at 1 to match uProf

#	there is an issue with GPU-z not keeping time properly, resulting in two recordings at the same Timestamp, and mis-times where there is a double recording and then a skipped second
DOUB	=	which(diff(GPUz$Time) == 0)	;	MISS	=	which(diff(GPUz$Time) == 2)
#	when GPU-z doubles a second				;	#	when GPU-z misses a second

MIUB	=	intersect(MISS + 1, DOUB)				#	when a miss precedes a double
GPUz[MIUB, "Time"]	=	GPUz[MIUB, "Time"] - 1		#	pulling double to fill miss

DOUB	=	which(diff(GPUz$Time) == 0)	;	MISS	=	which(diff(GPUz$Time) == 2)
DOSS	=	intersect(MISS, DOUB + 1)				#	when a double precedes a miss
GPUz[DOSS, "Time"]	=	GPUz[DOSS, "Time"] + 1		#	pushing double to fill miss

#	removes misses that cannot be corrected with doubles
if (any(diff(GPUz$Time) == 0))	GPUz	=	GPUz[-(which(diff(GPUz$Time) == 0)), ]
#	it is necessary to check if any doubles exist as trying to remove numeric(0) columns breaks things

# GPUz$Time	=	1:nrow(GPUz)
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
dataALL$CPU		=	ordered(CPUname)
dataALL$Cooler	=	ordered(COOLERname)
dataALL$Test	=	ordered(TESTname)

diff.CONS	=	function(DATA, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(DATA, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(DATA, lag = lag)))
}

dataALL$CPU_Temp_Diff	=	diff.CONS(dataALL$CPU_Temp, lag = length(unique(dataALL$Thread)))

dataALL	=	dataALL[order(dataALL$Time, dataALL$Socket, dataALL$Core, dataALL$Thread),]

write_csv(dataALL, "Combined.csv.bz2")