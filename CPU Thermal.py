import os, sys, subprocess, time, psutil, shutil
#	for Intel CPUs, uses Power Gadget
#		https://software.intel.com/en-us/articles/intel-power-gadget
#	for AMD CPUs, uses uProf and GPUz (to get the CPU temperature)
#		https://developer.amd.com/amd-uprof/
#		https://www.techpowerup.com/gpuz/

print("For Intel CPUs:")
print("\tPower Gadget must be installed")
print("\thttps://software.intel.com/en-us/articles/intel-power-gadget")
print("For AMD CPUs:")
print("\tAMD uProf and GPUz must be installed")
print("\thttps://developer.amd.com/amd-uprof/")
print("\thttps://www.techpowerup.com/gpuz/")
print("\tGPU-z needs to place its Sensor Log in the same folder as this script or on the Desktop")
print("")

#	shortcut name lists
lnkCINEBENCHR20	=	"Cinebench R20.lnk"
lnkCINEBENCHR23	=	"Cinebench R23.lnk"
lnkAMDuProf		=	"AMDuProfCLI.lnk"
lnkGPUz			=	"GPU-z.lnk"
lnkPowerGadget	=	"PowerLog3.0.lnk"
lnk3DMark		=	"3DMarkCmd.lnk"
lnkPrime95		=	"prime95.lnk"

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

def CINEBENCH(LNK, multi, length = None):
	global MULTI
	if multi:
		MULTI	=	"TRUE"
		cinebenchCpu	=	"g_cinebenchCpuXTest=true"
	else:
		MULTI	=	"FALSE"
		cinebenchCpu	=	"g_cinebenchCpu1Test=true"
	
	if length is None:
		CinebenchMinimum	=	""
	else:
		CinebenchMinimum	=	"g_CinebenchMinimumTestDuration=" + str(length)

	return(LNK + " " + cinebenchCpu + " " + CinebenchMinimum)

def	_3DMARK(defin):
	global MULTI
	MULTI	=	"TRUE"
	
	if defin == "0":
		defin	=	"firestrike_CPU"
	if defin == "1":
		defin	=	"firestrike_Combined"
	if defin == "2":
		defin	=	"timespy_CPU"
	
	if os.path.exists(scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef"):
		return(lnk3DMark + " --definition=\"" + scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef\" --loop=0 --audio=off --online=off")
	else:
		return(lnk3DMark + " --definition=" + defin + ".3dmdef --loop=0 --audio=off --online=off")

def Prime95():
	global MULTI
	MULTI	=	"TRUE"
	return(lnkPrime95 + " -t")

def	kill(proc_pid):
    process	=	psutil.Process(proc_pid)
    for proc in process.children(recursive=True):
        proc.kill()
    process.kill()


import cpuinfo, datetime
CPUname		=	cpuinfo.get_cpu_info()['brand']
CPUname		=	INPUT(CPUname,	"CPU Name (default: !DEF!) : ")

if "Intel" in CPUname:
	BRAND	=	"Intel"
elif "AMD" in CPUname:
	BRAND	=	"AMD"

COOLERname	=	INPUT("",		"CPU Cooler Name (default empty): ")


# print("Available Tests:")
print("Available Tests (3DMark requires Professional Edition) :")
optCineR20	=	[
	"100 \t-\t Cinebench R20 - Multi-thread - Constant",
	"110 \t-\t Cinebench R20 - Single Thread - Constant",
	"101 \t-\t Cinebench R20 - Multi-thread - Pulse",
	"111 \t-\t Cinebench R20 - Single Thread - Pulse",
	""
	]
optCineR23	=	[
	"200 \t-\t Cinebench R23 - Multi-thread - Constant",
	"210 \t-\t Cinebench R23 - Single Thread - Constant",
	"201 \t-\t Cinebench R23 - Multi-thread - Pulse",
	"211 \t-\t Cinebench R23 - Single Thread - Pulse",
	""]
opt3DMark	=	[
	"All 3DMark tests pulse due to loading time",
	"It is necessary for this console to be focused for 3DMark to launch correctly",
	"901 \t-\t 3DMark Fire Strike - Physics",
	"911 \t-\t 3DMark Fire Strike - Combined",
	"921 \t-\t 3DMark Time Spy - CPU",
	""
	]
optPrime95	=	[
	"300 \t-\t Prime95 Torture Test",
	""
]

options	=	[]

if	os.path.exists(scriptPath + lnkCINEBENCHR20.replace("\"", "")):
	options	=	options + optCineR20
if	os.path.exists(scriptPath + lnkCINEBENCHR23.replace("\"", "")):
	options	=	options + optCineR23
if	os.path.exists(scriptPath + lnkPrime95.replace("\"", "")):
	options	=	options + optPrime95
if	os.path.exists(scriptPath + lnk3DMark.replace("\"", "")):
	options	=	options + opt3DMark

def nameTEST(test):
	if test[0]	==	"1" or test[0] == "2":
		if test[0] == "1":
			out	=	"Cinebench R20"
		if test[0] == "2":
			out	=	"Cinebench R23"
		if test[1]	==	"0":
			out		=	out + " - Multi-thread"
		if test[1]	==	"1":
			out		=	out + " - Single-thread"
			
		if test[2]	==	"0":
			out		=	out + " - Constant"
		if test[2]	==	"1":
			out		=	out + " - Pulse"
	if test[0]	==	"3":
		out	=	"Prime95 Torture Test"
	
	if test[0]	==	"9":
		out	=	"3DMark"
		if test[1]	==	"0":
			out		=	out + " - Fire Strike - Physics"
		if test[1]	==	"1":
			out		=	out + " - Fire Strike - Combined"
		if test[1]	==	"2":
			out		=	out + " - Time Spy - CPU"

	return(out)

for OPT in options:
	print(OPT)
# test	=	input("Test ID Number (default 100 [Cinebench R20 - Multi-thread - Constant]): ") or str(100)
test	=	INPUT("100",	"Test ID Number (default !DEF!): ")

duration	=	int(INPUT(3600,	"Duration (default !DEF! s) : "))
warm		=	int(INPUT(300,	"Warmup Duration (default !DEF! s) : "))
coolCOEF	=	1
length		=	int((1 + coolCOEF)*duration + warm + 1)

if list(test)[2] == "1" and list(test)[0] != "2":
	pulse	=	int(INPUT(5,	"Wait Between Runs (default !DEF! s) : "))

if os.path.exists(scriptPath + "GPU-Z Sensor Log.txt"):
	print("")
	DEL	=	INPUT("Y", "Old GPU-Z Sensor Log.txt found. Should it be deleted?\nY/n (Y): ")
	if DEL	==	"Y" or DEL == "y":
		os.remove(scriptPath + "GPU-Z Sensor Log.txt")

TIME	=	datetime.datetime.now().strftime("%Y-%m-%d %H.%M")

if COOLERname == "":
	COOLERfold	=	""
else:
	COOLERfold	= 	COOLERname + "\\"

if os.path.exists(scriptPath + "GPU Thermal.py"):
	dataPath	=	scriptPath + "Data CPU\\" + 	COOLERfold + TIME + "\\"
else:
	dataPath	=	scriptPath + "Data\\" + 		COOLERfold + TIME + "\\"

if not os.path.exists(dataPath):
	os.makedirs(dataPath)

if BRAND == "Intel":
	os.system("start " + lnkPowerGadget + " -resolution 1000 -duration " + str(length) + " -file \"" + dataPath + "Intel-CPU Profile.csv\"")

if BRAND == "AMD":
	GPUz	=	subprocess.Popen(lnkGPUz + " -minimized", shell=True)
	os.system("start " + lnkAMDuProf + " timechart --event energy --event frequency --interval 1000 --duration " + str(length) + " -o \"" + dataPath + "AMD-CPU Profile\"")
#	start is necessary or else Python will wait until uPROF finishes before continuing

print("\nWarm-up")
time.sleep(warm)

print("\nCPU Load")
TESTname	=	nameTEST(test)
if	list(test)[0]	==	"1" or list(test)[0]	==	"2":
	if list(test)[1]	==	"0":
		THREAD	=	True
	else:
		THREAD	=	False
	
	if list(test)[0]	==	"1":
		LNK	=	lnkCINEBENCHR20
	if list(test)[0]	==	"2":
		LNK	=	lnkCINEBENCHR23
	
	if list(test)[2]	==	"0":
		Bench	=	subprocess.Popen(CINEBENCH(LNK, THREAD, duration), shell = True)
		time.sleep(duration)
	else:
		t_end	=	time.time() + duration
		while time.time() < t_end:
			Bench	=	subprocess.call(CINEBENCH(LNK, THREAD), shell = True)
			if	time.time() >= t_end:
				break
			time.sleep(pulse)
		#	cannot kill the benchmark at the desired time, but this will prevent another iteration

if list(test)[0]	==	"3":
	Bench		=	subprocess.Popen(Prime95(), shell = True)
	time.sleep(duration)

if	list(test)[0]	==	"9":
	Bench		=	subprocess.Popen(_3DMARK(list(test)[1]), shell = True)
	time.sleep(duration)

kill(Bench.pid)
Bench.kill()

print("\nCooldown")
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