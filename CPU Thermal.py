import os, sys, subprocess, time, psutil, shutil
#	for Intel CPUs, uses Power Gadget
#		https://software.intel.com/en-us/articles/intel-power-gadget
#	for AMD CPUs, uses uProf and GPUz (to get the CPU temperature)
#		https://developer.amd.com/amd-uprof/
#		https://www.techpowerup.com/gpuz/

os.system("title CPU Thermal Testing")

print("For Intel CPUs:\
\n\tPower Gadget must be installed\
\n\thttps://software.intel.com/en-us/articles/intel-power-gadget\
\nFor AMD CPUs:\
\n\tAMD uProf and GPUz must be installed\
\n\thttps://developer.amd.com/amd-uprof/\
\n\thttps://www.techpowerup.com/gpuz/\
\n\tGPU-z needs to place its Sensor Log in the same folder as this script\
\n")

#	defaults
duration	=	3600
#	length of Load period
warm		=	300
#	length of Warm-up Period
coolCOEF	=	1
#	ratio between Load and Cooldown periods
pulse	=	5
#	length of pause between loads, if test is pulsing

#	to add tests it is necessary to add Shortcut name, entry to TESTfunc, its own function, and its options list to the main OPTIONS list

#	shortcut name lists
lnkCINEBENCHR20	=	"Cinebench R20.lnk"
lnkCINEBENCHR23	=	"Cinebench R23.lnk"
lnkAMDuProf		=	"AMDuProfCLI.lnk"
lnkGPUz			=	"GPU-z.lnk"
lnkPowerGadget	=	"PowerLog3.0.lnk"
lnk3DMark		=	"3DMarkCmd.lnk"
lnkPrime95		=	"Prime95.lnk"

#	Function to assign selected test to generic variable
def	TESTfunc(CODE):
	TESTS	=	[
		CINEBENCHr20,	#	1
		CINEBENCHr23,	#	2
		Prime95,		#	3
		None,			#	4
		None,			#	5
		None,			#	6
		None,			#	7
		None,			#	8
		_3DMARK			#	9
		]
	global TEST
	TEST	=	TESTS[int(CODE[0])-1]

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"

def lnkCheck(LNK):
	if not LNK.endswith(".lnk"):
		LNK	=	LNK + ".lnk"
	if not LNK.startswith("\"") and not LNK.endswith("\""):
		if " " in LNK:
			return("\"" + LNK + "\"")
	return(LNK)

lnkCINEBENCHR20	=	lnkCheck(lnkCINEBENCHR20)
lnkCINEBENCHR23	=	lnkCheck(lnkCINEBENCHR23)
lnkAMDuProf		=	lnkCheck(lnkAMDuProf)
lnkGPUz			=	lnkCheck(lnkGPUz)
lnkPowerGadget	=	lnkCheck(lnkPowerGadget)
lnk3DMark		=	lnkCheck(lnk3DMark)
lnkPrime95		=	lnkCheck(lnkPrime95)

def INPUT(DEFAULT, TEXT = "Default: !DEF!"):
	return(input(TEXT.replace("!DEF!", str(DEFAULT))) or DEFAULT)
#	to make changing the default value easier


optCineR20	=	[
	"100 \t-\t Cinebench R20 - Multi-thread - Constant",
	"110 \t-\t Cinebench R20 - Single Thread - Constant",
	"101 \t-\t Cinebench R20 - Multi-thread - Pulse",
	"111 \t-\t Cinebench R20 - Single Thread - Pulse"
]
def CINEBENCHr20(CODE):
	global TESTname
	TESTname	=	"Cinebench R20"
	global MULTI

	if CODE[1] is "0":
		MULTI	=	"TRUE"
		TESTname	=	TESTname + " - Multi-thread"
		cinebenchCpu	=	"g_cinebenchCpuXTest=true"
	else:
		MULTI	=	"FALSE"
		TESTname	=	TESTname + " - Single-thread"
		cinebenchCpu	=	"g_cinebenchCpu1Test=true"

	if CODE[2] is "0":
		TESTname	=	TESTname + " - Constant"
		CinebenchMinimum	=	"g_CinebenchMinimumTestDuration=" + str(duration)
	else:
		TESTname	=	TESTname + " - Pulse"
		CinebenchMinimum	=	""

	return(lnkCINEBENCHR20 + " " + cinebenchCpu + " " + CinebenchMinimum)

optCineR23	=	[
	"200 \t-\t Cinebench R23 - Multi-thread - Constant",
	"210 \t-\t Cinebench R23 - Single Thread - Constant",
	"201 \t-\t Cinebench R23 - Multi-thread - Pulse",
	"211 \t-\t Cinebench R23 - Single Thread - Pulse"
]
def CINEBENCHr23(CODE):
	global TESTname
	TESTname	=	"Cinebench R23"
	global MULTI

	if CODE[1] is "0":
		MULTI	=	"TRUE"
		TESTname	=	TESTname + " - Multi-thread"
		cinebenchCpu	=	"g_cinebenchCpuXTest=true"
	else:
		MULTI	=	"FALSE"
		TESTname	=	TESTname + " - Single-thread"
		cinebenchCpu	=	"g_cinebenchCpu1Test=true"

	if CODE[2] is "0":
		TESTname	=	TESTname + " - Constant"
		CinebenchMinimum	=	"g_CinebenchMinimumTestDuration=" + str(duration)
	else:
		TESTname	=	TESTname + " - Pulse"
		CinebenchMinimum	=	""

	return(lnkCINEBENCHR23 + " " + cinebenchCpu + " " + CinebenchMinimum)

opt3DMark	=	[
	"901 \t-\t 3DMark Fire Strike - Physics",
	"911 \t-\t 3DMark Fire Strike - Combined",
	"921 \t-\t 3DMark Time Spy - CPU",
	"All 3DMark tests pulse due to loading time",
	"It is necessary for this console to be focused for 3DMark to launch correctly"
]
#	notes need to be at the end of the list, as the default code is pulled from the first line
def	_3DMARK(CODE):
	global TESTname
	TESTname	=	"3DMark"

	global MULTI
	MULTI	=	"TRUE"

	if CODE[1] == "0":
		TESTname	=	TESTname + " - Fire Strike - Physics"
		defin	=	"firestrike_CPU"
	if CODE[1] == "1":
		TESTname	=	TESTname + " - Fire Strike - Combined"
		defin	=	"firestrike_Combined"
	if CODE[1] == "2":
		TESTname	=	TESTname + " - Time Spy - CPU"
		defin	=	"timespy_CPU"

	if os.path.exists(scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef"):
		return(lnk3DMark + " --definition=\"" + scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef\" --loop=0 --audio=off --online=off")
	else:
		input("No valid 3DMark definion files found.\n Press Enter to quit.")
		sys.exit("No definition files found")

optPrime95	=	[
	"300 \t-\t Prime95 Torture Test"
]
def Prime95(CODE):
	global TESTname
	TESTname	=	"Prime95 Torture Test"

	global MULTI
	MULTI	=	"TRUE"
	return(lnkPrime95 + " -t")

#	Builds the list of tests to be displayed
OPTIONS	=	[]

if	os.path.exists(scriptPath + lnkCINEBENCHR20.replace("\"", "")):
	OPTIONS	=	OPTIONS + optCineR20 + [""]
if	os.path.exists(scriptPath + lnkCINEBENCHR23.replace("\"", "")):
	OPTIONS	=	OPTIONS + optCineR23 + [""]
if	os.path.exists(scriptPath + lnkPrime95.replace("\"", "")):
	OPTIONS	=	OPTIONS + optPrime95 + [""]
if	os.path.exists(scriptPath + lnk3DMark.replace("\"", "")):
	OPTIONS	=	OPTIONS + opt3DMark + [""]

#	function to pulse the test
def PULSE(TEST):
	t_end	=	time.time() + duration
	while time.time() < t_end:
		Bench	=	subprocess.call(TEST(TESTcode), shell = True)
		if	time.time() >= t_end:
			break
		time.sleep(pulse)

def PULSEx(TEST):
	t_end	=	time.time() + duration

	while time.time() < t_end:
		Bench	=	subprocess.Popen(TEST(TESTcode), shell = True)

		#	Bench.wait() with checking
		while Bench.poll() is None:
			if	time.time() >= t_end:
				kill(Bench.pid)
				Bench.kill()
				return
			time.sleep(1)

		#	time.sleep() with checking
		t_pulse	=	time.time() + pulse
		while time.time() < t_pulse:
			if	time.time() >= t_end:
				return
			time.sleep(1)
	return

#	kills the test after a set period of time, so the load has a controlled length
#		here for future experiment and not for use yet. Potentially would enable Prime95 to be pulsed
def PULSEk(TEST):
	t_end	=	time.time() + duration

	while time.time() < t_end:
		Bench	=	subprocess.Popen(TEST(TESTcode), shell = True)

		#	checking if test priod ends during run of the test
		t_load	=	time.time() + load
		while time.time() < t_load:
			if	time.time() >= t_end:
				kill(Bench.pid)
				Bench.kill()
				return
			time.sleep(1)
		
		#	kill the test after the desired load period has finished
		kill(Bench.pid)
		Bench.kill()

		#	time.sleep() with checking
		t_pulse	=	time.time() + pulse
		while time.time() < t_pulse:
			if	time.time() >= t_end:
				return
			time.sleep(1)
	return

def	kill(proc_pid):
	process	=	psutil.Process(proc_pid)
	for proc in process.children(recursive=True):
		proc.kill()
	process.kill()

def timeFUT(END):
	return(time.strftime("%I:%M %p", time.localtime(time.time() + END)))

import cpuinfo
CPUname		=	BRAND	=	cpuinfo.get_cpu_info()['brand']
CPUname		=	INPUT(CPUname,	"CPU Name (default: !DEF!) : ")

if "Intel" in CPUname or "Intel" in BRAND:
	BRAND	=	"Intel"
elif "AMD" in CPUname or "AMD" in BRAND:
	BRAND	=	"AMD"
else:
	BRAND	=	INPUT("", 		"CPU Brand (AMD or Intel): ")

COOLERname	=	INPUT("",		"CPU Cooler Name (default empty): ")


print("Available Tests (3DMark requires Professional Edition):")
for OPT in OPTIONS:
	print(OPT)
TESTcode	=	INPUT(OPTIONS[0][0:3],	"Test ID Number (default !DEF!): ")
TESTfunc(TESTcode)

duration	=	int(INPUT(duration,	"Duration (default !DEF! s) : "))
warm		=	int(INPUT(warm,		"Warmup Duration (default !DEF! s) : "))
length		=	int((1 + coolCOEF)*duration + warm + 1)

if TESTcode[2] == "1" and TESTcode[0] != "9":
	pulse	=	int(INPUT(pulse,	"Wait Between Runs (default !DEF! s) : "))

if os.path.exists(scriptPath + "GPU-Z Sensor Log.txt"):
	print("")
	DEL	=	INPUT("Y", "Old GPU-Z Sensor Log.txt found. Should it be deleted?\nY/n (Y): ")
	if DEL	==	"Y" or DEL == "y":
		os.remove(scriptPath + "GPU-Z Sensor Log.txt")

TIME	=	time.strftime("%Y-%m-%d %H.%M", time.localtime())

if COOLERname == "":
	COOLERfold	=	""
else:
	COOLERfold	= 	COOLERname + "\\"

if os.path.exists(scriptPath + "GPU Thermal - 3dmdef.py"):
	dataPath	=	scriptPath + "Data CPU\\" + 	COOLERfold + TIME + "\\"
else:
	dataPath	=	scriptPath + "Data\\" + 		COOLERfold + TIME + "\\"

if not os.path.exists(dataPath):
	os.makedirs(dataPath)

if BRAND == "Intel":
	os.system("start " + lnkPowerGadget + " -resolution 1000 -duration " + str(length) + " -file \"" + dataPath + "Intel-CPU Profile.csv\"")

if BRAND == "AMD":
	GPUz	=	subprocess.Popen(lnkGPUz + " -minimized", shell=True)
	# os.system("start " + lnkAMDuProf + " timechart --event energy --event frequency --interval 1000 --duration " + str(length) + " -o \"" + dataPath + "AMD-CPU Profile\"")
	os.system("start " + lnkAMDuProf + " timechart --event Power,Frequency,Temperature,P-State --interval 1000 --duration " + str(length) + " -o \"" + dataPath + "AMD-CPU Profile\"")
#	the arguments apparently changed when AMD updated uProf, but Temperature support was also added
#	start is necessary or else Python will wait until uPROF finishes before continuing

#	if using a Pulsed test, uProf and Power Gadget may run short as the Pulse function allows the last loop of the test to complete, rather than killing it

print("\nWarm-up\tEnds at " + timeFUT(warm))
time.sleep(warm)

print("\nCPU Load\tEnds at " + timeFUT(duration))

if TESTcode[2] == "0" or TESTcode[0] == "9":
	pulse	=	"NULL"
	Bench	=	subprocess.Popen(TEST(TESTcode), shell = True)
	time.sleep(duration)

	kill(Bench.pid)
	Bench.kill()

if TESTcode[2] == "1" and TESTcode[0] != "9":
	# PULSE(TEST)
	PULSEx(TEST)

print("\nCooldown\tEnds at " + timeFUT(duration * coolCOEF))
time.sleep(duration * coolCOEF)

if BRAND == "AMD":
	kill(GPUz.pid)
#	GPUz.kill() doesn't work because subprocess with shell=True makes it a separate process that cannot be controlled by Python
	time.sleep(1)
#	give enough time for GPUz to be killed before trying to move the file
	if os.path.exists(scriptPath + "GPU-Z Sensor Log.txt"):
		shutil.move(scriptPath + "GPU-Z Sensor Log.txt", dataPath + "GPU-Z Sensor Log.txt")
	else:
		DESKTOP	=	"\\".join(scriptPath.split("\\", 3)[:3]) + "\\Desktop\\"
		if os.path.exists(DESKTOP + "GPU-Z Sensor Log.txt"):
			shutil.move(DESKTOP + "GPU-Z Sensor Log.txt", dataPath + "GPU-Z Sensor Log.txt")

if not os.path.exists(dataPath + "@CPU Thermal - Input.r"):
	with open(scriptPath + "CPU Thermal - Input.r", 'r') as fref, open(dataPath + "@CPU Thermal - Input.r", 'w') as fout:
		for line in fref:
			fout.write(line	\
				.replace("!TEST!",		TESTname)		\
				.replace("!CPU!",		CPUname)		\
				.replace("!COOLER!",	COOLERname)		\
				.replace("!DUR!",		str(duration))	\
				.replace("!WARM!",		str(warm))		\
				.replace("!PATH!",		dataPath.replace("\\", "/"))	\
				.replace("!MULTI!",		MULTI)	\
				.replace("!PULSE!",		str(pulse))	\
			)
		fout.close()

if BRAND == "AMD":
	if not os.path.exists(dataPath + "~CPU Thermal - Data - AMD.r"):
		shutil.copyfile(scriptPath + "CPU Thermal - Data - AMD.r",		dataPath + "~CPU Thermal - Data - AMD.r")
elif BRAND == "Intel":
	if not os.path.exists(dataPath + "~CPU Thermal - Data - Intel.r"):
		shutil.copyfile(scriptPath + "CPU Thermal - Data - Intel.r",	dataPath + "~CPU Thermal - Data - Intel.r")
#	with ~ these scripts will be after the @ scripts
if not os.path.exists(dataPath + "@CPU Thermal - Output.r"):
	shutil.copyfile(scriptPath + "CPU Thermal - Output.r", dataPath + "@CPU Thermal - Output.r")