# OCC-CPU_Thermals

The repository contains scripts for the automated loading, monitoring, and then processing of CPU-related measurements.
The CPU Thermal.py script will initiate the monitoring of such data as CPU frequency, power utilization, and temperature.
After a Warm-up Period the selected CPU load period will begin.
The script supports Cinebench R20, which is free to use, as well as 3DMark, which requires the Professional Edition.
A Cooldown Period follows the CPU load period and goes for the same length of time as the load period, to hopefully ensure the the full cooling behavior is recorded.

With this script, the idea is to have data before a significant CPU load when it is still idle, record data during the load period to see how the CPU warms up, and then watch as the CPU cools off during the Cooldown period.

If you review the script you may notice a reference to similar GPU scripts.
As these scripts require 3DMark Professional Edition, I am not sharing them as too few will be able to take advantage of them.

### Software Involved:

- [Python](https://www.python.org/)
- Python modules:
	- [psutil](https://pypi.org/project/psutil/)
	- [py-cpuinfo](https://pypi.org/project/py-cpuinfo/)
- [R](https://www.r-project.org)
- R libraries:
	- [readr](https://readr.tidyverse.org/)
	- [ggplot2](https://ggplot2.tidyverse.org/index.html)
	- [tidyr](https://tidyr.tidyverse.org/)
- [Cinebench R20](https://www.maxon.net/en-us/support/downloads/)
- [3DMark Professional Edition]

For AMD CPUs:
- [AMD uProf](https://developer.amd.com/amd-uprof/)
- [GPU-z](https://www.techpowerup.com/gpuz/)

For Intel CPUs:
- [Intel Power Gadget](https://software.intel.com/en-us/articles/intel-power-gadget)

### To Use:

To use these scripts a few things other than setting up the software listed above is necessary.
Most important is the placement of shortcuts to the appropriate EXE files in the same folder as the script.
Rather than require environment variables be set, I have the CPU Thermal.py script configured to use these shortcuts.
You will need one for Cinebench.exe, and unless you want to edit the Python script, it should be named "Cinebench R20".

For those with an AMD CPU you will want a shortcut to AMDuProfCLI, and it should be named "AMDuProfCLI".
As powerful as AMD uProf is, it does not want to report CPU temperature, which is why GPU-z is necessary, so have a shortcut to that application also named "GPU-z".
GPU-z will need to be configured to save a sensor log and it should be placed preferably in the same folder as the scripts.
It should also work if the "GPU-Z Sensor Log.txt" file is saved to the desktop, but I am less confident in it working.

Though I have devised a way to automate it, you may still want to check the output of AMD uProf as it places information describing the data at the top of its output.
The amount of this information will depend on things such as the CPU's thread count.
This information can confuse R if it is not skipped when loading the CSV.
By having R first read it in as a plain text file and searching for "PROFILE RECORDS" R can now find how many lines to skip.
So as to not waste time, I have it set to only read in the first 100 lines for this search, which should be enough for most AMD consumer CPUs.
If it is not you can change it in **CPU Thermal - Data - AMD.r**.

For those with an Intel CPU, you will need a link to PowerLog3.0 that should also be named "PowerLog3.0".
Intel Power Gadget does not give nearly as much information as AMD uProf, but it does record CPU temperature, so GPU-z is not neceessary.

If you wish you can use different shortcut names, if you change the appropriate link name in the Python script.

Once these shortcuts are in place and, if necessary, GPU-z writes its sensor log to the appropriate location, executing CPU Thermal.py is all you need to do to get the test going.

After the script finishes, the data should be collected in a readily identifiable folder named for the data and time the test started.
Aside from the possible editing mentioned earlier, the Python script will handle editing the reference R scripts to identify the run and set where the data is held.
If you intend to process the data on a different machine, you will need to change the **setwd** command in the **@CPU Thermal - Input.r** script or run the script directly with Rscript.exe.