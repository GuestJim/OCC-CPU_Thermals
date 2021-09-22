library(readr)
library(tidyr)
library(ggplot2)

duration	=	!DUR!
warm		=	!WARM!
TESTname	=	"!TEST!"
MULTI		=	!MULTI!
CPUname		=	"!CPU!"
COOLERname	=	"!COOLER!"
PULSE		=	!PULSE!

levsPER		=	c("Warm-up", TESTname, "Cooldown")

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
	# if	(grepl("AMD", CPUname))		source("~CPU Thermal - Data - AMD.r")
	# if	(grepl("Intel", CPUname))	source("~CPU Thermal - Data - Intel.r")
	if	(file.exists("~CPU Thermal - Data - AMD.r"))		source("~CPU Thermal - Data - AMD.r")
	if	(file.exists("~CPU Thermal - Data - Intel.r"))		source("~CPU Thermal - Data - Intel.r")
}	else	{
	dataALL	=	read_csv("Combined.csv.bz2")
}

dataALL$Thread	=	ordered(dataALL$Thread)
dataALL$Core	=	ordered(dataALL$Core)
dataALL$Socket	=	ordered(dataALL$Socket)
dataALL$Period	=	ordered(dataALL$Period, levels = levsPER)
dataALL$CPU		=	ordered(dataALL$CPU)
dataALL$Cooler	=	ordered(dataALL$Cooler)
dataALL$Test	=	ordered(dataALL$Test)

source("@CPU Thermal - Output.r")
