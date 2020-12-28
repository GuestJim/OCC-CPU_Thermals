CPUdata	=	read_csv("Intel-CPU Profile.csv")

dataFILT	=	CPUdata[1:(nrow(CPUdata) - 15), pmatch(c(
	"Elapsed Time (sec)",
	"CPU Frequency_",
	"Processor Power_",
	"Package Temperature_"), colnames(CPUdata))
	]

dataALL	=	pivot_longer(dataFILT,
	cols			=	-1,
	names_to		=	c(".value", "Socket"),
	names_pattern	=	"(.*)_([:digit:]*)",
	names_ptypes	=	list(Socket = factor(ordered = TRUE))
)
#	Each column has an ID number for the processor, and this will remove it from each
#		".value" will get the value from the column itself, with that column named for the first segment
#		"Socket" gets the [:digit:]* value in the column name

colnames(dataALL)	=	c("Time", "Socket", "Frequency", "Socket_Energy", "CPUTemp")

PERIODS	=	function(DATA,	BREAKS = c(warm, duration),	LABELS = c("Warm-up", TESTname, "Cooldown")){
	out	=	ifelse(DATA$Time <= BREAKS[1], LABELS[1],
			ifelse(BREAKS[1] < DATA$Time & DATA$Time <= BREAKS[2] + BREAKS[1], LABELS[2],
			ifelse(BREAKS[2] + BREAKS[1] < DATA$Time, LABELS[3], NA
				)))
	out	=	ordered(out, levels = LABELS)
	return(out)
}

dataALL$Period	=	PERIODS(dataALL)
dataALL$Time	=	dataALL$Time - warm

dataALL$Core			=	factor(0)
dataALL$Thread			=	factor(0)
dataALL$Socket_Energy	=	dataALL$Socket_Energy*1000
#	to convert to mW, matching AMD data
dataALL$Core_Energy		=	dataALL$Socket_Energy
dataALL$Uncore_Energy	=	0

diff.CONS	=	function(DATA, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(DATA, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(DATA, lag = lag)))
}

dataALL$CPUTempDiff	=	diff.CONS(dataALL$CPUTemp, lag = length(unique(dataALL$Thread)))

dataALL	=	dataALL[order(dataALL$Time, dataALL$Socket, dataALL$Core, dataALL$Thread),]

write_csv(dataALL, "Combined.csv.bz2")