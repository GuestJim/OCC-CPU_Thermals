CSV	=	list.files(pattern = "*.csv$", recursive = TRUE)

TEXT	=	readLines(CSV,	warn = FALSE,	n = 100)
for (LINE in 1:length(TEXT))	{	if (TEXT[LINE] == "PROFILE RECORDS")	break	}
#	the above loads in the uProf file to find when the data actually starts, R can properly and automatically load it.
#	to save time, it only loads in the first 100 lines, which should be enough for most any consumer CPU, but increase that number if necessary

uProf	=	read_csv(CSV, skip = LINE, guess_max = 10, lazy = TRUE, show_col_types = FALSE)

threadFREQ	=	pivot_longer(uProf[, c(1, 2, grep("thread.*-core-effective-frequency", colnames(uProf)))],
			cols			=	-c(1, 2),
			names_to		=	"Thread",
			names_pattern	=	"thread([[:digit:]]+)-core-effective-frequency",
			# names_ptypes	=	list(Thread = numeric()),
			# names_ptypes	=	list(Thread = factor(ordered = TRUE)),
			names_transform		=	list(Thread = as.numeric),
			values_to		=	"Frequency"
)
threadFREQ$Core	=	floor(as.numeric(threadFREQ$Thread)/2)

threadSTATE	=	pivot_longer(uProf[, c(1, 2, grep("thread.*-p-state", colnames(uProf)))],
			cols			=	-c(1, 2),
			names_to		=	"Thread",
			names_pattern	=	"thread([[:digit:]]+)-p-state",
			# names_ptypes	=	list(Thread = numeric()),
			# names_ptypes	=	list(Thread = factor(ordered = TRUE)),
			names_transform		=	list(P_State = as.ordered),
			values_to		=	"P_State"
)
threadFREQ	=	merge(threadFREQ, threadSTATE, by = c("RecordId", "Timestamp", "Thread"))

coreENG		=	pivot_longer(uProf[, c(1, 2, which(startsWith(colnames(uProf), "core")))],
			cols			=	-c(1, 2),
			names_to		=	"Core",
			names_pattern	=	"core([[:digit:]]+)-",
			names_ptypes	=	list(Core = factor(ordered = TRUE)),
			# names_transform		=	list(Core = as.numeric),
			values_to		=	"Core_Energy"
)

sockENG		=	pivot_longer(uProf[, c(1, 2, grep("socket.*-package", colnames(uProf)))],
			cols			=	-c(1, 2),
			names_to		=	"Socket",
			names_pattern	=	"socket([[:digit:]]+)-package",
			names_ptypes	=	list(Socket = factor(ordered = TRUE)),
			values_to		=	"Socket_Energy"
)

sockENG$Uncore_Energy	=	rowSums(uProf[, grep("socket(.*)-package", colnames(uProf))]) - rowSums(uProf[, which(startsWith(colnames(uProf), "core"))])
#	subtracts the energy measured for each core from the Socket energy measurement, giving us the remaining Uncore energy usage
#		curiously can result in negative values which I am taking to be a measurement error and just ignoring

if (any(grepl("socket.*-temperature", colnames(uProf))))	{
	sockTEMP	=	pivot_longer(uProf[, c(1, 2, grep("socket.*-temperature", colnames(uProf)))],
				cols			=	-c(1, 2),
				names_to		=	"Socket",
				names_pattern	=	"socket([[:digit:]]+)-temperature",
				names_ptypes	=	list(Socket = factor(ordered = TRUE)),
				values_to		=	"CPU_Temp"
	)
	sockTEMP$CPU_Temp	=	sockTEMP$CPU_Temp - DATA$Toff
	sockENG	=	merge(sockENG,	sockTEMP,	by = c("RecordId", "Timestamp", "Socket"),	sort = FALSE)
}

if (max(coreENG$Core_Energy) < 1000 & max(sockENG$Socket_Energy) < 1000)	{
	coreENG$Core_Energy		=	coreENG$Core_Energy * 1000
	sockENG$Socket_Energy	=	sockENG$Socket_Energy * 1000
	sockENG$Uncore_Energy	=	sockENG$Uncore_Energy * 1000
}
#	newer versions of uProf use Watts instead of mJ, so for consistency the measurements are multiplied when necessary

uProfTALL	=	merge(threadFREQ,	coreENG,	by = c("RecordId", "Timestamp", "Core"),	sort = FALSE)
uProfTALL	=	merge(uProfTALL,	sockENG,	by = c("RecordId", "Timestamp"),			sort = FALSE)

uProfTALL$Time	=	strptime(uProfTALL$Timestamp, "%H:%M:%S")
uProfTALL$Time	=	as.numeric(uProfTALL$Time - min(uProfTALL$Time)) + 1
# colnames(uProfTALL)[grep("RecordId", colnames(uProfTALL))]	=	"Time"


dataALL		=	uProfTALL

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
assign("dataALL", dataALL, envir = .GlobalEnv)

# write_csv(dataALL, "Combined.csv.bz2")