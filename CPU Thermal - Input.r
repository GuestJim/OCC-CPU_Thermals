library(readr)
library(tidyr)
library(ggplot2)

DATA	=	new.env()

DATA$duration	=	!DUR!
DATA$warm		=	!WARM!
DATA$TESTname	=	"!TEST!"
DATA$MULTI		=	!MULTI!
DATA$CPUname	=	"!CPU!"
DATA$COOLERname	=	"!COOLER!"
DATA$PULSE		=	!PULSE!
DATA$FREQspec	=	NULL	#	for frequency specifications, can take vector
DATA$Toff		=	10

DATA$levsPER		=	c("Warm-up", DATA$TESTname, "Cooldown")

for (obj in ls(DATA, all.names = TRUE))	assign(obj, get(obj, DATA))

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

gWIDTH	=	16
gHEIGH	=	9
app.BREAK	=	TRUE
FREQ.COEF	=	NULL	#	Output.r will automatically set this based on maximum power and clock values

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}

FILE	=	paste0(DATA$TESTname, " ~ ", DATA$COOLERname)

if (!file.exists(	paste0(FILE, ".RData")	))	{
	hold	=	new.env()
	if	(file.exists("~CPU Thermal - Data - AMD.r"))		source("~CPU Thermal - Data - AMD.r",	local = hold)
	if	(file.exists("~CPU Thermal - Data - Intel.r"))		source("~CPU Thermal - Data - Intel.r",	local = hold)
	#	with the hold environment, everything the Data scripts do is placed into hold, but dataALL will be placed in Global, so hold can be removed at the end
	DATA$dataALL	=	dataALL
	DATA$GROUPS	=	list(
		Period			=	dataALL$Period,
		Socket			=	dataALL$Socket
		)
	GROUPS	<-	DATA$GROUPS
	DATA$DATAS	=	list(
		CPU_Temp		=	dataALL$CPU_Temp,
		Frequency		=	dataALL$Frequency,
		Socket_Energy	=	dataALL$Socket_Energy,
		Core_Energy		=	dataALL$Core_Energy,
		Uncore_Energy	=	dataALL$Uncore_Energy
		)
	DATAS	<-	DATA$DATAS
	
	saveRDS(DATA, paste0(FILE,".RData"), compress="xz")
	write_csv(dataALL, "Combined.csv.bz2")
	
	rm(hold)
}	else	{	
	DATA	=	readRDS(paste0(FILE, ".RData"))
	for (obj in ls(DATA, all.names = TRUE))	assign(obj, get(obj, DATA))	;	rm(DATA)
	if (!file.exists("Combined.csv.bz2"))	write_csv(dataALL, "Combined.csv.bz2")
}

source("@CPU Thermal - Output.r")

#	Output saving commands
sinkTXT()
sinkHTML()

if	(MULTI)	{
	message("Frequency - Mean")
	customSave("Frequency - Mean",	plot = graphMEAN())
}	else	{
	message("Frequency - Max")
	customSave("Frequency - Max",	plot = graphMAX())
}
message("Frequency")
customSave("Frequency", 		plot = graphFREQ(),		height	=	2 * length(unique(dataALL$Thread)))
message("Core Power")
customSave("Core Power",		plot = graphPOWER(),	height	=	2 * length(unique(dataALL$Core)))
message("Temperature by Period")
customSave("Hist - Temperature",	plot = HIST.Temp,		width	=	gHEIGH * 1.25)
message("Frequency by Period")
customSave("Hist - Frequency",		plot = HIST.Frequency,	width	=	gHEIGH * 1.25)
message("Socket Power by Period")
customSave("Hist - Socket",			plot = HIST.Socket,		width	=	gHEIGH * 1.25)
message("Core Power by Period")
customSave("Hist - Core",			plot = HIST.Core,		width	=	gHEIGH * 1.25)
# message("Uncore Power by Period")
# customSave("Hist - Uncore",			plot = HIST.Uncore,		width	=	gHEIGH * 1.25)