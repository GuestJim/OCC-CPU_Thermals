library(readr)
library(tidyr)
library(ggplot2)

DATA	=	new.env()

DATA$duration	=	!DUR!
DATA$warm		=	!WARM!
DATA$TESTname	=	"!TEST!"
DATA$MULTI		=	!MULTI!
DATA$CPUname		=	"!CPU!"
DATA$COOLERname	=	"!COOLER!"
DATA$PULSE		=	!PULSE!

DATA$levsPER		=	c("Warm-up", DATA$TESTname, "Cooldown")

for (obj in ls(DATA, all.names = TRUE))	assign(obj, get(obj, DATA))

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

gWIDTH	=	16
gHEIGH	=	9
app.BREAK	=	TRUE
FREQ.COEF	=	NULL	#	Output.r will automatically set this based on maximum power and clock values
FREQspec	=	NULL	#	for frequency specifications, can take vector

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}

FILE	=	paste0(DATA$TESTname, " ~ ", DATA$COOLERname)

if (!file.exists(	paste0(FILE, ".env")	))	{
	hold	=	new.env()
	if	(file.exists("~CPU Thermal - Data - AMD.r"))		source("~CPU Thermal - Data - AMD.r",	local = hold)
	if	(file.exists("~CPU Thermal - Data - Intel.r"))		source("~CPU Thermal - Data - Intel.r",	local = hold)
	#	with the hold environment, everything the Data scripts do is placed into hold, but dataALL will be placed in Global, so hold can be removed at the end
	DATA$dataALL	=	dataALL
	saveRDS(DATA, paste0(FILE,".env"), compress="bzip2")
	write_csv(dataALL, "Combined.csv.bz2")
	
	rm(hold)
}	else	{	
	DATA	=	readRDS(paste0(FILE, ".env"))
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