import sys, os, shutil

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"

os.chdir(droppedPath)

Z	=	1

for file in os.listdir(droppedPath):
	if file.endswith("Stats.txt"):
		droppedSTAT	=	file

TYPE	=	"Data CPU\\"
if TYPE in droppedPath:
	droppedCOOL	=	droppedPath.split(TYPE)[1].split("\\", 1)[0]
	droppedList	=	droppedPath.split(TYPE)[0]	+	TYPE
elif "Data\\" in droppedPath:
	droppedCOOL	=	droppedPath.split("Data\\")[1].split("\\", 1)[0]
	droppedList	=	droppedPath.split("Data\\")[0]	+	"Data\\"

droppedCPU	=	open(droppedSTAT, 'r').readlines()[1].strip("\n")
if "CPUs.txt"	in	os.listdir(droppedList):
	CPUs	=	open(droppedList + "CPUs.txt", 'r').readlines()
	CPUs	=	[line.rstrip('\n') for line in CPUs]
else:
	CPUs	=	""

if	"Coolers.txt"	in	os.listdir(droppedList):
	COOL	=	open(droppedList + "Coolers.txt", 'r').readlines()
	COOL	=	[line.rstrip('\n') for line in COOL]
else:
	COOL	=	""

BENCHs	=	[\
["Cinebench R20",	1],\
["Cinebench R23",	2],\
["Prime95",			3],\
["3DMark",			9]
]

TESTcpu	=	[\
["Multi-thread",		0],\
["Single-thread",		1],\
["Prime95 Torture Test",	0],\
["Fire Strike - Physics",	0],\
["Fire Strike - Combined",	1],\
["Time Spy - CPU",			2]
]

PACE	=	[\
["Constant",	0],\
["Pulse",		1],\
["Prime95 Torture Test",	0],\
["3DMark",		1]
]

TYPEs	=	[\
['Frequency - Mean',	1],\
['Frequency - Max',		1],\
['Frequency',			2],\
['Core Power',			3],\
['Hist - Temperature',	4],\
['Hist - Socket', 		5],\
['Hist - Frequency',	6],\
['Hist - Core',			7],\
['Hist - Uncore',		8],\
]

def	codFind	(statname, list):
	for i in range(len(list)):
		if list[i][0] in statname:
			return(list[i][1])

def numFind	(filename, list):
	if list == [""]:
		return(0)
	for i in reversed(range(len(list))):
		if list[i] in filename:
			return(i+1)
	return(0)

def codGraph	(filename, list):
	for i in range(len(list)):
		if list[i][0] + ".png" == filename:
			return(list[i][1])

def numGen (filename):
	CODE	=	[	[],	[],	[],	[],	[],	[]	]
	#	CPU, Cooler, Bench, Test, Pace, Graph
	CODE[0]	=	numFind(droppedCPU,		CPUs)
	CODE[1]	=	numFind(droppedCOOL,	COOL)
	CODE[2]	=	codFind(droppedSTAT,	BENCHs)
	CODE[3]	=	codFind(droppedSTAT,	TESTcpu)
	CODE[4]	=	codFind(droppedSTAT,	PACE)
	CODE[5]	=	codGraph(filename,		TYPEs)
	
	CODE.pop(0)	if CODE[0]	==	[0] else None

	code	=	""
	for x in CODE:
		if x != "":
			code	=	code + str(x).zfill(Z)

	return(code)

if not os.path.exists("@Graphs"):
	os.mkdir("@Graphs")

for file in os.listdir(droppedPath):
	if file.endswith(".png"):
		shutil.copyfile(file, "@Graphs\\" + numGen(file) + ".png")

# os.system("pause")