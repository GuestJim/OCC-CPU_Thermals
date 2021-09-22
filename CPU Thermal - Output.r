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

nearCEIL	=	function(DATA, VAL)	ceiling(max(DATA, na.rm = TRUE) / VAL) * VAL
nearFLOOR	=	function(DATA, VAL)	floor(max(DATA, na.rm = TRUE) / VAL) * VAL

maxPWR		=	nearCEIL(dataALL$Socket_Energy,	5000)
maxCLK		=	nearCEIL(dataALL$Frequency,		500)
if	(!is.numeric(FREQ.COEF))	FREQ.COEF	=	signif(exp(round(log(maxPWR/maxCLK / 1000), 0)), 1)

stats		=	function(DATA)	{
	return(c(
		Min		=	min(DATA,		na.rm	=	TRUE),
		Median	=	median(DATA,	na.rm	=	TRUE),
		Mean	=	mean(DATA,		na.rm	=	TRUE),
		Max		=	max(DATA,		na.rm	=	TRUE)
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
remUNI	=	function(IN)	IN[, -intersect(
	which(!sapply(IN, is.numeric)),
	which(lapply(lapply(IN, unique), length) == 1))
	]
#	identifies the columns identifying the groups first so it will not alter the data

unitCOL	=	function(DATA)	{
	levs	=	levels(DATA)
	if	(is.character(DATA))	levs	=	DATA
	levs[grep("CPU_Temp",	levs)]	=	paste0(levs[grep("CPU_Temp",	levs)],	" (°C)")
	levs[grep("Frequency",	levs)]	=	paste0(levs[grep("Frequency",	levs)],	" (MHz)")
	levs[grep("Energy",		levs)]	=	paste0(levs[grep("Energy",		levs)],	" (mJ)")
	
	return(rem_(levs))
}

GROUPS	=	list(
		CPU				=	dataALL$CPU,
		Cooler			=	dataALL$Cooler,
		Test			=	dataALL$Test,
		Period			=	dataALL$Period,
		Socket			=	dataALL$Socket
		)
DATAS	=	list(
		CPU_Temp		=	dataALL$CPU_Temp,
		Frequency		=	dataALL$Frequency,
		Socket_Energy	=	dataALL$Socket_Energy,
		Core_Energy		=	dataALL$Core_Energy,
		Uncore_Energy	=	dataALL$Uncore_Energy
		)

dataSUM	=	sepCOL(aggregate(DATAS, GROUPS, stats))
dataSUM	=	remUNI(dataSUM)

longSUM	=	pivot_longer(dataSUM,
	cols			=	which(sapply(dataSUM, is.numeric)),
	names_to		=	c("Measurement", ".value"),
	names_sep		=	' - ',
	names_ptypes	=	list(Measurement = factor(ordered = TRUE))
)

levels(longSUM$Measurement)	=	unitCOL(levels(longSUM$Measurement))
longSUM	=	round2(longSUM)

tempCROSS	=	function(DATA, PERIOD, QUAN, OP = NULL, LIST = 10)	{
	COLS	=	c("Time", "CPU_Temp", "CPU_Temp_Diff")
	out		=	DATA[DATA$Thread == 0 & DATA$Period == PERIOD, COLS]
	if (PERIOD == "Cooldown")	out$dTime	=	out$Time - duration
	
	if (QUAN < 1)	LIM	=	quantile(out$CPU_Temp, QUAN)
	if (QUAN > 1)	LIM	=	QUAN
	
	if (is.null(OP))	{
		if (PERIOD == TESTname)		OP	=	">="
		if (PERIOD == "Cooldown")	OP	=	"<="
	}
	
	if (OP == "<=")		return(out[out$CPU_Temp <= LIM, ][1:LIST, ])
	if (OP == ">=")		return(out[out$CPU_Temp >= LIM, ][1:LIST, ])
}


#	returns the linear regression slopes for certain variables as a data frame
CPUslopes	=	function(DATA = dataALL, PERIOD = TESTname,	WID = 0.1, OFF = 0.01)	{
	dataTEST	=	dataALL[dataALL$Period == PERIOD, ]
	PERCS	=	c(OFF,	WID + OFF,	1 - WID - OFF,	1 - OFF)
	SECTS	=	quantile(dataTEST$Time, PERCS)
	
	slope	=	function(DATA = dataTEST)	{
		c(
		coef(lm(CPU_Temp			~	Time,	data = DATA))[2],
		coef(lm(Frequency			~	Time,	data = DATA))[2],
		coef(lm(Socket_Energy/1000	~	Time,	data = DATA))[2],
		coef(lm(Core_Energy/1000	~	Time,	data = DATA))[2]
		)
	}
	
	out	=	rbind(
		slope(),
		slope(dataTEST[SECTS[1] <= dataTEST$Time & dataTEST$Time < SECTS[2], ]),
		slope(dataTEST[SECTS[3] <= dataTEST$Time & dataTEST$Time < SECTS[4], ])
		)
	colnames(out)	=	c("CPU_Temp",	"Frequency",	"Socket_Power",	"Core_Power")
	rownames(out)	=	c("Test Period",	paste0(PERCS[1]*100, "% to ", PERCS[2]*100, "%"),	paste0(PERCS[3] * 100, "% to ", PERCS[4]*100, "%"))
	return(out)
}

sinkTXT	=	function()	{
	options(width = 1000)
	printFrame	=	function(FRAME, ...)	print.data.frame(FRAME, row.names = FALSE, ...)

	sink(paste0(TESTname, " - Stats.txt"), split = TRUE)
		writeLines(TESTname)
		writeLines(CPUname)
		writeLines(COOLERname)
		writeLines(ifelse(is.numeric(PULSE),	paste0("Pulse pause length, in addition to loading:\t", PULSE, " s"),	"")	)
		
		writeLines("\nIgnore negative Uncore Energy")
		
		writeLines("\nWarm-up Period")
		printFrame(longSUM[longSUM$Period == "Warm-up", ])
		
		
		writeLines(paste0("\n", TESTname, " Period"))
		printFrame(longSUM[longSUM$Period == TESTname, ])
		
		if (!is.null(FREQspec))	{
			writeLines("\nFrequency Percentages")
			dataTEST	=	dataALL[dataALL$Period == TESTname, ]
			dataTEST	=	if (MULTI)	dataTEST$Frequency else as.vector(by(dataTEST$Frequency, dataTEST$Time, max))
			ECDF	=	ecdf(dataTEST)
			BASE	=	min(FREQspec)
			
			LESS	=	ECDF(FREQspec - 0.01)				;	names(LESS)	=	paste0("<",	FREQspec,	" MHz")
			EQUA	=	diff(ECDF(c(BASE - 0.01, BASE)))	;	names(EQUA)	=	paste0("=",	BASE,		" MHz")

			print(round(c(LESS, EQUA)[order(substring(names(c(LESS, EQUA)), 2))] * 100, 2))
		}
		
		writeLines("\nLinear Model Slopes:")
		print(CPUslopes())
		writeLines("\nLinear Model Slopes (minute):")
		print(CPUslopes() * 60)
		
		writeLines("\nFirst Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == TESTname, ]$CPU_Temp, 0.25), " °C\n"))
		printFrame(tempCROSS(dataALL, TESTname, 0.25, ">="))
		
		
		writeLines("\nCooldown Period")
		printFrame(longSUM[longSUM$Period == "Cooldown", ])

		writeLines("\nThird Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == "Cooldown", ]$CPU_Temp, 0.75), " °C\n"))
		printFrame(tempCROSS(dataALL, "Cooldown", 0.75, "<="))
	sink()
}

writeOCC	=	function(DATA, dataNAME, name=TESTname, fold = "")	{
	if (!require(tableHTML))	return(NULL)
	#	if tableHTML is not present to be loaded, no HTML files will be produced
	OCCHTML	=	function(DATA)	{
		tableHTML(DATA, rownames = FALSE, class="OCC") %>%
		replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
		replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
		replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
	}

	if	(fold != "")	{
		write_tableHTML(OCCHTML(DATA), file = paste0(fold, "\\", name, " - ", dataNAME,".html"))
	}	else	{
		write_tableHTML(OCCHTML(DATA), file = paste0(name, " - ", dataNAME,".html"))
	}
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

CAPTION	=	c(CPUname, COOLERname,	ifelse(is.null(PULSE), TESTname, paste0(TESTname, " (", PULSE, " s)"))	)
CAPTION	=	labs(caption = paste(CAPTION, collapse = "\n"))

TEMP_point	=	function(DATA = dataALL, COEF = 1)	{
	geom_point(
		data	=	DATA,
		aes(y	=	CPU_Temp*COEF, 			color	=	"Temperature"),
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
FREQ_point	=	function(DATA = dataALL, COEF = FREQ.COEF, MEAN = FALSE, MAX = FALSE, ALPHA = .20)	{
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
		values	=	c(
			Temperature		=	"red",
			Frequency		=	"blue",
			"Core Power"	=	"green",
			"Socket Power"	=	"darkgreen",
			"Uncore Power"	=	"yellowgreen")
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
#	geom_smooth(aes(y = Frequency * FREQ.COEF, group = Period))	 for smooth line

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
	geom_vline(data = aggregate(dataALL[, TYPE], GROUPS, mean, na.rm = TRUE),	aes(xintercept = get(TYPE)*COEF), 	color = "red") +
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

FREQspec_line	=	function(FREQ	=	FREQspec)	{
	if	(!is.numeric(FREQ))	return(NULL)
	
	FREQdata	=	list(
		Period	=	ordered(levsPER[1], levsPER),	x	=	FREQ,	y	=	Inf,
		TEXT	=	FREQ,
		ECDF	=	round2(ecdf(dataALL[dataALL$Period == TESTname, ]$Frequency)(FREQ))
		)
		
	list(geom_vline(
			xintercept	=	FREQ,
			color		=	"black",
			linetype	=	"dashed"
		), 
		geom_text(data	=	data.frame(FREQdata),
			aes(x = x,	y = y,	label = TEXT),
			vjust	=	-0.5
		),
		coord_cartesian(clip = "off")
	)
}

#Temperature
HIST.Temp		=	graphHIST(
	TYPE		=	"CPU_Temp",
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
	X.limits	=	c(round(min(dataALL$Frequency)-500, -3), NA),
	FILL.unit	=	"MHz",
	FILL.mid	=	3000,
	FILL.limits	=	c(round(min(dataALL$Frequency)-500, -3), maxCLK),
	FILL.breaks	=	seq(0, 10000, by = 500)
	)	+	FREQspec_line(FREQspec)

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
	COEF		=	1/1000,
	binWID		=	0.1
	)

#Core Power
HIST.Core		=	graphHIST(
	TYPE		=	"Core_Energy",
	TITLE		=	"Core Power Normalized Distribution by Period",
	X.name		=	"Power (W)",
	X.break		=	1,
	X.limits	=	c(0, NA),
	FILL.unit	=	"W",
	FILL.mid	=	3,
	FILL.limits	=	c(0, nearCEIL(dataALL$Core_Energy/1000, 3)),
	FILL.breaks	=	seq(0, nearCEIL(dataALL$Core_Energy/1000, 5), by = 3),
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
	FILL.limits	=	c(0, nearCEIL(dataALL$Uncore_Energy/1000, 5)),
	FILL.breaks	=	seq(0, nearCEIL(dataALL$Uncore_Energy/1000, 5), by = 15),
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
customSave("Frequency", 		plot = graphFREQ(),		height	=	2 * length(unique(dataALL$Thread)))
message("Core Power")
customSave("Core Power",		plot = graphPOWER(),	height	=	2 * length(unique(dataALL$Core)))
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