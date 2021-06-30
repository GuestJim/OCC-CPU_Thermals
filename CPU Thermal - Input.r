library(readr)
library(tidyr)
library(ggplot2)

duration	=	!DUR!
warm		=	!WARM!
TESTname	=	"!TEST!"
MULTI		=	!MULTI!
CPUname		=	"!CPU!"
COOLERname	=	"!COOLER!"
Steady		=	0.3
PULSE		=	!PULSE!	#only relevant if test were pulsed

levsPER		=	c("Warm-up", TESTname, "Cooldown")

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

gWIDTH	=	16
gHEIGH	=	9
app.BREAK	=	TRUE
FREQ.COEF	=	NULL
# FREQ.COEF	=	1/1000*20
#	this will be set automatically in Output.r based on maximum power and clock values
#	it might be desirable to manually tweak this value, and if you do here it will not be altered

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

source("@CPU Thermal - Output.r")
