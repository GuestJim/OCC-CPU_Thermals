labelBreak	=	function(breaks, SEC = FALSE, r = 2)	{
	if (!app.BREAK)	return(breaks)
	if (is.numeric(breaks))	breaks	=	round(breaks, r)
	BREAK	=	c("", "\n")
	if	(is.numeric(breaks)	&	0 %in% breaks)	if	((which(breaks %in% 0) %% 2) == 0)	BREAK	=	rev(BREAK)
	if	(!SEC)	return(	paste0(rep(BREAK, length.out = length(breaks)),	breaks)	)
	if	(SEC)	return(	paste0(breaks, rep(BREAK, length.out = length(breaks)))	)
}
#	can be disabled by setting app.BREAK to FALSE

sci2norm	=	function(DATA)	format(DATA, scientific = FALSE)
rem_		=	function(INPUT)	gsub("_", " ", INPUT)
round2	=	function(DATA, r = 2)	{
	numCOL		=	sapply(DATA, is.numeric)
	DATA[numCOL]	=	round(DATA[numCOL], r)
	return(DATA)
}

nearCEIL	=	function(DATA, VAL)	ceiling(max(DATA) / VAL) * VAL
nearFLOOR	=	function(DATA, VAL)	floor(max(DATA) / VAL) * VAL

maxPWR		=	nearCEIL(dataALL$Socket_Energy,	5000)
maxCLK		=	nearCEIL(dataALL$Frequency,		100)
if	(is.null(FREQ.COEF))	{
	FREQ.COEF	=	1/1000*nearFLOOR(maxPWR/maxCLK, 10) * 0.5
	#	with the 0.5 the Frequency will be placed more centrally on the graph
}

stats		=	function(DATA)	{
	return(c(
		Min		=	min(DATA),
		Median	=	median(DATA),
		Mean	=	mean(DATA),
		Max		=	max(DATA)
	)	)
}

sepCOL	=	function(aggOUT)	{
	matCOL	=	sapply(aggOUT, is.matrix)
	out		=	aggOUT[, !matCOL]
	for (FUN in which(matCOL))	{
		DATA			=	as.data.frame(aggOUT[, FUN])
		colnames(DATA)	=	paste(colnames(aggOUT)[FUN], colnames(DATA), sep = " - ")
		
		out	=	cbind(out, DATA)
	}
	return(out)
}

unitCOL	=	function(DATA)	{
	levs	=	levels(DATA)
	if	(is.character(DATA))	levs	=	DATA
	levs[grep("CPUTemp", levs)]		=	paste0(levs[grep("CPUTemp", levs)],		" (°C)")
	levs[grep("Frequency", levs)]	=	paste0(levs[grep("Frequency", levs)],	" (MHz)")
	levs[grep("Energy", levs)]		=	paste0(levs[grep("Energy", levs)],		" (mJ)")
	
	return(rem_(levs))
}

GROUPS	=	list(
		Period			=	dataALL$Period,
		Socket			=	dataALL$Socket
		)
DATAS	=	list(
		CPUTemp			=	dataALL$CPUTemp,
		Frequency		=	dataALL$Frequency,
		Socket_Energy	=	dataALL$Socket_Energy,
		Core_Energy		=	dataALL$Core_Energy,
		Uncore_Energy	=	dataALL$Uncore_Energy
		)

dataSUM	=	sepCOL(aggregate(DATAS, GROUPS, stats))

longSUM	=	pivot_longer(dataSUM,
	cols			=	-c(1:2),
	names_to		=	c("Measurement", ".value"),
	names_sep		=	' - ',
	names_ptypes	=	list(Measurement = factor(ordered = TRUE))
)

levels(longSUM$Measurement)	=	unitCOL(levels(longSUM$Measurement))
longSUM	=	round2(longSUM)
if (unique(longSUM$Socket) == 0)	longSUM$Socket	= NULL

temp.STEADY	=	function(DATA, PERIOD, DIFF = Steady, LIST = 10, ORD = 10)	{
	out	=	DATA[DATA$Thread == "0" & DATA$Period == PERIOD, c("Time", "CPUTemp", "CPUTempDiff")]
	
	out$TEST	=	c(diff(out$CPUTemp, differences = ORD), rep(0, ORD))
#	by using the differences argument to increase the order of the difference, more values are used, effectively searching for a wider list of stable temperatures
	return(out[abs(out$TEST) <= DIFF,	c("Time", "CPUTemp", "CPUTempDiff")][1:LIST, ])
}

#	the below function works but is much slower than the above, even if it is technically more appropriate as it uses the first-order difference
temp.STEADY.exp = function(DATA, DIFF = Steady, ORD = 10)	{
#	ORD is different here, being the number of the temperature measurements to use
	out	=	data.frame("Time" = DATA[DATA$Thread == 0, "Time"], "Steady" = NA)
	
	for (place in 1:nrow(out))	{
		out$Steady[place]	=	 all(	abs(DATA[DATA$Thread == 0, ][seq(place, place + ORD - 1), ]$CPUTempDiff)	<= DIFF	)
	}
	
	return(merge(DATA, out, by = "Time"))
}

#	adds a Steady column to dataALL that identifies if the 10 consecutive CPUTempDiff values are below 0.5
# dataALL	=	temp.STEADY.exp(dataALL, Steady)

#	LIST	=	10
# dataALL[dataALL$Thread == 0, dataALL$Steady == TRUE &	dataALL$Period == "Warm-up",	c("Time", "CPUTemp", "CPUTempDiff")][1:LIST, ]
# dataALL[dataALL$Thread == 0, dataALL$Steady == TRUE &	dataALL$Period == TESTname,		c("Time", "CPUTemp", "CPUTempDiff")][1:LIST, ]
# dataALL[dataALL$Thread == 0, dataALL$Steady == TRUE &	dataALL$Period == "Cooldown",	c("Time", "CPUTemp", "CPUTempDiff")][1:LIST, ]


#	bins of CPUTemp data, not used but useful to keep I think
# TEMPbins	=	as.data.frame(table(round2(dataALL[, c("Period", "CPUTemp")], 0)))
# TEMPbins[TEMPbins$Period == "Warm-up"	& TEMPbins$Freq != 0, ]
# TEMPbins[TEMPbins$Period == TESTname	& TEMPbins$Freq != 0, ]
# TEMPbins[TEMPbins$Period == "Cooldown"	& TEMPbins$Freq != 0, ]

tempQUART	=	function(DATA, QUAN, LIST = 10)	{
	if (length(unique(DATA$Thread)) > 1)	DATA	=	DATA[DATA$Thread == 0, ]

	if (QUAN	<	0.5)	out	=	DATA[	DATA$CPUTemp >= quantile(DATA$CPUTemp, QUAN), ]
	if (QUAN	>	0.5)	out	=	DATA[	DATA$CPUTemp <= quantile(DATA$CPUTemp, QUAN), ]
	if (QUAN	==	0.5)	out	=	DATA[	DATA$CPUTemp == quantile(DATA$CPUTemp, QUAN), ]
	
	return(out[, c("Time", "CPUTemp", "CPUTempDiff")][1:LIST, ])
}

sinkTXT	=	function()	{
	options(width = 1000)
	printFrame	=	function(FRAME, ...)	print.data.frame(FRAME, row.names = FALSE, ...)

	sink(paste0(TESTname, " - Stats.txt"), split = TRUE)
		writeLines(TESTname)
		writeLines(CPUname)
		writeLines(COOLERname)
		
		writeLines("\nIgnore negative Uncore Energy")
		
		writeLines("\nWarm-up Period")
		printFrame(longSUM[longSUM$Period == "Warm-up", ])
		
		# writeLines("\nSteady Temperature (Warm-up Period)")
		# writeLines(paste0("\nStarting at: ", -warm))
		# printFrame(temp.STEADY(dataALL, "Warm-up", Steady))
		
		
		writeLines(paste0("\n", TESTname, " Period"))
		printFrame(longSUM[longSUM$Period == TESTname, ])
		
		# writeLines(paste0("\nSteady Temperature (", TESTname, " Period)"))
		# printFrame(temp.STEADY(dataALL, TESTname, Steady))
		#	the results are not always useful and I feel the quartile stats below are better
		
		writeLines("\nFirst Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == TESTname, ]$CPUTemp, 0.25), " °C\n"))
		printFrame(tempQUART(dataALL[dataALL$Period	== TESTname, ], 0.25))
		
		writeLines("\nCooldown Period")
		printFrame(longSUM[longSUM$Period == "Cooldown", ])
		
		# writeLines("\nSteady Temperature (Cooldown Period)")
		# writeLines(paste0("Starting at: ", duration, "\n"))
		# printFrame(temp.STEADY(dataALL, "Cooldown", Steady))
		#	the results are not always useful and I feel the quartile stats below are better
		
		writeLines("\nThird Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == "Cooldown", ]$CPUTemp, 0.75), " °C\n"))
		printFrame(tempQUART(dataALL[dataALL$Period	== "Cooldown", ], 0.75))
	sink()
}

library(tableHTML)
OCCHTML	=	function(DATA)	{
	out	=	tableHTML(DATA, rownames = FALSE, class="OCC")
	
	out	=	replace_html(out,	'style="border-collapse:collapse;" class=OCC border=1',	'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"')
	out	=	replace_html(out,	' id=\"tableHTML_header_\\d\"',	'',	replace_all = TRUE)
	out	=	replace_html(out,	' id=\"tableHTML_column_\\d\"',	'',	replace_all = TRUE)
	
	return(out)
}

writeOCC	=	function(DATA, dataNAME, name=testNAME, fold = "")	{
	filePath	=	paste0(name, " - ", dataNAME,".html")
	if	(fold != "")	filePath	=	paste0(fold, "\\", filePath)
	
	write_tableHTML(OCCHTML(DATA),	file = filePath)
}

sinkHTML = function()	{
	writeOCC(longSUM,									dataNAME	=	"All",			name	=	TESTname)
	writeOCC(longSUM[longSUM$Period == "Warm-up", ],	dataNAME	=	"Warm-up",		name	=	TESTname)
	writeOCC(longSUM[longSUM$Period == TESTname, ],		dataNAME	=	TESTname,		name	=	TESTname)
	writeOCC(longSUM[longSUM$Period == "Cooldown", ],	dataNAME	=	"Cooldown",		name	=	TESTname)
}


customSave	=	function(type="", device=ggdevice, plot = last_plot(), width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(type, ".png"), plot = plot, device="png", width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(type, ".pdf"), plot = plot, device="pdf", width=width, height=height)
	}
}

CAPTION	=	paste0(TESTname,	"\n",	CPUname)
if (COOLERname != "")	CAPTION =	paste0(CAPTION, "\n", COOLERname)
CAPTION	=	labs(caption = CAPTION)

TEMP_point	=	function(DATA = dataALL, COEF = 1)	{
	geom_point(
		data	=	DATA,
		aes(y	=	CPUTemp*COEF, 			color	=	"Temperature"),
		stat 	=	"unique",
		# color	=	"red",
		shape 	=	3,
		show.legend	=	TRUE
	)
}
SOCK_point	=	function(DATA = dataALL, COEF = 1/1000)	{
	geom_point(
		data	=	DATA,
		aes(y	=	Socket_Energy*COEF,	color	=	"Socket Power"),
		stat	=	"unique",
		# color	=	"green",
		shape 	=	3,
		show.legend	=	TRUE
	)
}
CORE_point	=	function(DATA = dataALL, COEF = 1/1000)	{
	geom_point(
		data	=	DATA,
		aes(y	=	Core_Energy*COEF,	color	=	"Core Power"),
		stat	=	"unique",
		# color	=	"green",
		show.legend	=	TRUE
	)
}
unCORE_point	=	function(DATA = dataALL, COEF = 1/1000)	{
	if (all(DATA$Uncore_Energy == 0))	return(NULL)
	#	cannot get an Uncore from Intel CPUs
	geom_point(
		data	=	DATA,
		aes(y	=	Uncore_Energy*COEF,	color	=	"Uncore Power"),
		# color	=	"green",
		shape	=	18,
		show.legend	=	TRUE
	)
}
FREQ_point	=	function(DATA = dataALL, COEF = 1/1000, MEAN = FALSE, MAX = FALSE, ALPHA = .20)	{
	if (MEAN)	{	return(
		geom_point(
			data	=	DATA,
			aes(y	=	Frequency*COEF,	color	=	"Frequency"),
			alpha	=	ALPHA,
			stat	=	"summary",
			fun		=	mean,
			show.legend	=	TRUE
		)	)
	}
	if (MAX)	{	return(
		geom_point(
			data	=	DATA,
			aes(y	=	Frequency*COEF,	color	=	"Frequency"),
			alpha	=	ALPHA,
			stat	=	"summary",
			fun		=	max,
			show.legend	=	TRUE
		)	)
	}
	return(	geom_point(
				data	=	DATA,
				aes(y	=	Frequency*COEF,	color	=	"Frequency"),
				alpha	=	ALPHA,
				# color	=	"blue",
				show.legend	=	TRUE
		)	)
}

COLORS	=	scale_color_manual(
		name	=	NULL,
		values	=	c(Temperature = "red",	Frequency = "blue",	"Core Power" = "green", "Socket Power" = "darkgreen", "Uncore Power" = "yellowgreen")
	)

themeSCALES	=	function(COEF = FREQ.COEF){
	list(
		theme(
			plot.title.position		=	"plot",
			legend.position			=	"top",
			legend.justification	=	"left",
			legend.margin			=	margin(t = 0, unit = "cm")
			),
		scale_x_continuous(
			name	=	"Time (seconds)",
			# breaks	=	unique(c(seq(0, warm, by = warm/3), seq(warm, 2 * duration + warm, by = duration/6))),
			breaks	=	unique(c(seq(-warm, 0, by = warm/3), seq(0, 2 * duration, by = duration/6))),
			# labels	=	function(x)	labelBreak(x - warm),
			labels	=	labelBreak,
			minor_breaks	=	NULL,
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	NULL,
				# breaks	=	c(warm, duration + warm),
				breaks	=	c(0, duration),
				labels	=	c("Load Start", "Load Stop/End")
			)
		),
		scale_y_continuous(
			breaks		=	seq(0, maxPWR/100, by = 10),
			limits		=	c(0, NA),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frequency (MHz)",
				labels	=	function(IN)	IN / COEF
				)
			),
		COLORS
	)
}


graphMEAN	=	function(COEF = FREQ.COEF)	{
	ggplot(data = dataALL, aes(x=Time)) + 
	ggtitle("Mean Frequency with Temperature and Power") + CAPTION +
	TEMP_point() + 
	SOCK_point() + 
	FREQ_point(COEF = COEF, MEAN = TRUE) + 
	themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

graphMAX	=	function(COEF = FREQ.COEF)	{
	ggplot(data = dataALL, aes(x=Time)) + 
	ggtitle("Max Frequency with Temperature and Power") + CAPTION +
	TEMP_point() + 
	SOCK_point() + 
	FREQ_point(COEF = COEF, MAX = TRUE) + 
	themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

graphFREQ	=	function(COEF = FREQ.COEF)	{
	ggplot(data = dataALL, aes(x=Time)) + 
	ggtitle("Frequency with Temperature and Core Power",
		subtitle = "Even Thread: Physical, Odd Thread: Logical") + CAPTION +
	TEMP_point() + 
	# SOCK_point() + 
	CORE_point() + 
	FREQ_point(COEF = COEF) + 
	facet_grid(rows = vars(Core, Thread),	switch = "y", labeller	=	
		labeller(Core	=	function(IN) paste0("Core: ", IN), Thread	=	function(IN) paste0("Thread: ", IN))
		) + 
	themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

graphPOWER	=	function(COEF = FREQ.COEF)	{	
	ggplot(data = dataALL, aes(x=Time)) + 
	ggtitle("Frequency with Core and Uncore Power",	subtitle = "") + CAPTION +
	FREQ_point(COEF = COEF) + 
	unCORE_point() + 
	CORE_point() + 
	facet_grid(rows = vars(Core),	switch = "y", labeller	=	
		labeller(Core	=	function(IN) paste0("Core: ", IN))
		) + 
	themeSCALES(COEF) + ylab("Power (W)") + expand_limits(y = c(0, 90))
}


graphHIST	=	function(TYPE, TITLE, X.name, X.break, X.limits, FILL.unit, FILL.mid, FILL.limits, FILL.breaks, binWID = 1, COEF = 1)	{
	ggplot(data = dataALL, aes(x = get(TYPE)*COEF)) +
	ggtitle(			TITLE,
		subtitle	=	"Histograms & Box Plots with Red Mean Line"	) + CAPTION + 
	scale_fill_gradient2(FILL.unit, low="blue", mid = "green", midpoint = FILL.mid,  high="red", limits = FILL.limits, breaks = FILL.breaks) + 
	theme(
		plot.title.position			=	"plot",
		legend.position				=	"bottom",
		legend.justification		=	"left",
		legend.margin				=	margin(t = -2, b = -2, l = -2, unit = "lines"),
		legend.key.width			=	unit(0.045, "npc")
		) + 	
	geom_boxplot(outlier.alpha = 0, 				coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) + 
	geom_histogram(aes(y = after_stat(ncount),	fill = after_stat(x)),	binwidth = binWID) + 
	geom_boxplot(outlier.alpha = 0, alpha = 0.15,	coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) + 
	geom_vline(data = dataALL[dataALL$Period == "Warm-up", ],	aes(xintercept = mean(get(TYPE)*COEF)), 	color = "red") + 
	geom_vline(data = dataALL[dataALL$Period == TESTname, ],	aes(xintercept = mean(get(TYPE)*COEF)), 	color = "red") + 
	geom_vline(data = dataALL[dataALL$Period == "Cooldown", ],	aes(xintercept = mean(get(TYPE)*COEF)), 	color = "red") + 
	# facet_grid(rows = vars(Period), switch = "y", labeller = labeller(Period = label_wrap_gen(20))) +
	facet_grid(rows = vars(Period), switch = "y",
		labeller = labeller(Period = function(IN) gsub(" - ", "\n", IN))
		) +
	scale_x_continuous(
		name	=	X.name,
		breaks	=	seq(0, 10000, by = X.break),
		limits	=	X.limits,
		guide 	=	guide_axis(n.dodge = 2),
		expand	=	c(0.02, 0)
		) + 
	scale_y_continuous(name = "", breaks = NULL)
}

#Temperature
HIST.Temp		=	graphHIST(
	TYPE		=	"CPUTemp",
	TITLE		=	"CPU Temperature Normalized Distribution by Period",
	X.name		=	"Temperature (°C)",
	X.break		=	5,
	X.limits	=	c(0, NA),
	FILL.unit	=	"°C",
	FILL.mid	=	60,
	FILL.limits	=	c(25, 95),
	FILL.breaks	=	seq(30, 90, by = 10)
	)

#Frequency
HIST.Frequency	=	graphHIST(
	TYPE		=	"Frequency",
	TITLE		=	"Frequency Normalized Distribution by Period",
	X.name		=	"Frequency (MHz)",
	X.break		=	200,
	X.limits	=	c(2000, NA),
	FILL.unit	=	"MHz",
	FILL.mid	=	3000,
	FILL.limits	=	c(2000, nearCEIL(maxCLK, 500)),
	FILL.breaks	=	seq(0, 10000, by = 500)
	)

#Socket Power
HIST.Socket		=	graphHIST(
	TYPE		=	"Socket_Energy",
	TITLE		=	"Socket Power Normalized Distribution by Period",
	X.name		=	"Power (W)",
	X.break		=	10,
	X.limits	=	c(0, NA),
	FILL.unit	=	"W",
	FILL.mid	=	80,
	FILL.limits	=	c(0, nearCEIL(maxPWR/1000 + 1, 30)),
	FILL.breaks	=	seq(0, nearCEIL(maxPWR/1000 + 1, 30), by = 30),
	COEF		=	1/1000
	)

#Core Power
HIST.Core		=	graphHIST(
	TYPE		=	"Core_Energy",
	TITLE		=	"Core Power Normalized Distribution by Period",
	X.name		=	"Power (W)",
	X.break		=	3,
	X.limits	=	c(0, NA),
	FILL.unit	=	"W",
	FILL.mid	=	3,
	FILL.limits	=	c(0, 20),
	FILL.breaks	=	seq(0, nearCEIL(10/1000 + 1, 20), by = 3),
	COEF		=	1/1000,
	binWID		=	0.01
	)

#Uncore Power
HIST.Uncore		=	graphHIST(
	TYPE		=	"Uncore_Energy",
	TITLE		=	"Uncore Power Normalized Distribution by Period",
	X.name		=	"Power (W)",
	X.break		=	5,
	X.limits	=	c(0, NA),
	FILL.unit	=	"W",
	FILL.mid	=	30,
	FILL.limits	=	c(0, 60),
	FILL.breaks	=	seq(0, nearCEIL(10/1000 + 1, 75), by = 15),
	COEF		=	1/1000,
	binWID		=	0.01
	)

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
customSave("Frequency", 		plot = graphFREQ(),		height	=	2 * length(levels(dataALL$Thread)))
message("Core Power")
customSave("Core Power",		plot = graphPOWER(),	height	=	2 * length(levels(dataALL$Core)))
message("Temperature by Period")
customSave("Hist - Temperature",	plot = HIST.Temp,		width	=	gHEIGH * 1.25)
message("Frequency by Period")
customSave("Hist - Frequency",		plot = HIST.Frequency,	width	=	gHEIGH * 1.25)
message("Socket Power by Period")
customSave("Hist - Socket",			plot = HIST.Socket,		width	=	gHEIGH * 1.25)
# message("Core Power by Period")
# customSave("Hist - Core",			plot = HIST.Core,		width	=	gHEIGH * 1.25)
# message("Uncore Power by Period")
# customSave("Hist - Uncore",			plot = HIST.Uncore,		width	=	gHEIGH * 1.25)