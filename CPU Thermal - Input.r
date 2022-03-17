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

if (!file.exists("Combined.csv.bz2"))	{
	hold	=	new.env()
	# if	(grepl("AMD", CPUname))		source("~CPU Thermal - Data - AMD.r",	local = hold)
	# if	(grepl("Intel", CPUname))	source("~CPU Thermal - Data - Intel.r",	local = hold)
	if	(file.exists("~CPU Thermal - Data - AMD.r"))		source("~CPU Thermal - Data - AMD.r",	local = hold)
	if	(file.exists("~CPU Thermal - Data - Intel.r"))		source("~CPU Thermal - Data - Intel.r",	local = hold)
	#	with the hold environment, everything the Data scripts do is placed into hold, but dataALL will be placed in Global, so hold can be removed at the end
	write_csv(dataALL, "Combined.csv.bz2")
	rm(hold)
}	else	{
	dataALL	=	read_csv("Combined.csv.bz2", guess_max = 10, lazy = TRUE, show_col_types = FALSE)
}

dataALL$Thread	=	ordered(dataALL$Thread)
dataALL$Core	=	ordered(dataALL$Core)
dataALL$Socket	=	ordered(dataALL$Socket)
dataALL$Period	=	ordered(dataALL$Period, levels = levsPER)
dataALL$CPU		=	ordered(dataALL$CPU)
dataALL$Cooler	=	ordered(dataALL$Cooler)
dataALL$Test	=	ordered(dataALL$Test)

DATA$dataALL	=	dataALL
saveRDS(DATA, "DATA.env", compress="bzip2")

source("@CPU Thermal - Output.r")
