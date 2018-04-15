
**Here is a set of commands that you might want to have in your .chrc file:**
------

**module loads**
* To load R: module load R/R302
* To load python 3.4: module load python/anaconda3-4.0.0
* To allow multi-threading using CPU: module load rocks-openmpi
* To allow multi-threading using GPU: module load openmpi-x86_64

**Cool script from Haim that shows the queues status and occupancy distribution among users:**
* alias q 'perl /groups/pupko/haim/pupkoSVN/trunk/scripts/q.pl'

**Cool qstat aliases from Shiran:**
* To see the full description your running jobs: alias qzstat 'qstat -s r -xml | tr '"'"'\n'"'"' '"'"' '"'"' | sed '"'"'s#<job_list[^>]*>#\n#g'"'"' | sed '"'"'s#<[^>]*>##g'"'"' | grep " " | column -t'
* To see the full description all your jobs: alias qwstat 'qstat -xml | tr '"'"'\n'"'"' '"'"' '"'"' | sed '"'"'s#<job_list[^>]*>#\n#g'"'"' | sed '"'"'s#<[^>]*>##g'"'"' | grep " " | column -t'
* To delete all yorr pending jobs (in status qw or Eqw): alias qwdel 'qstat -s p | awk '"'"'{cmd="qdel " $1; system(cmd); close(cmd)}\'"'"''

<br/><br/>

**Here are a few useful qalter commands:**
------

* To change the the priority of all your jobs (can only reduce the priority, cannot increase it):
  qalter -p <priority> -u <username>
* To change the priority of a specific job ID:
  qalter -p <priority> <jobID>
* To change the resource list of your jobs:
	* If your want to set the resource list by negation: qalter -l hostname=!"comp7.itaym.q@compute-7-1.local" -u <username>
	* Otherwise: 										 qalter -l hostname="comp7.itaym.q@compute-7-0.local" -u <username>


<br/><br/>
	
**Our nodes:**
------

**In Jekyl:**
* compute-7-0 to 7-11  (64G RAM each)
* compute-8-10 to 8-14 (129G RAM each)

**In Lecs2:**
* compute-4-20 to 4-29

<br/><br/>

**Working with power8:**
------

**Address:** power8.tau.ac.il

**Our nodes:**
* compute-0-20 		(7G RAM)
* compute-0-21 		(7G RAM)
* compute-0-22 		(7G RAM)
* compute-0-160 	(125G RAM)
* compute-0-161 	(125G RAM)
* compute-0-159 	(125G RAM)
* compute-0-162 	(125G RAM)
* compute-0-13 		(15G RAM)
* compute-0-14 		(7G RAM)
* compute-0-15 		(31G RAM)
* compute-0-69 		(31G RAM)
* compute-0-68 		(31G RAM)
* compute-0-67 		(31G RAM)
* compute-0-70 		(31G RAM)
* compute-0-71		(31G RAM)

Overall, all the nodes have 172 cores. However, in some nodes, there is only 31GB, so when allocating each job 4GB, for example, then not all the cores could be utilized as the same time.

**Please note: users that at some point during they're studies had a user created for them under computer science directory must request Danny to change their defalut directory in power. The password must be reset as well.**

**Syntax to submit a job:** qsub -q kerenh \<job_file_path\>

**Job syntax: bash. Example:**
```
#!/bin/bash

#PBS -S /bin/bash
#PBS -j oe
#PBS -r y
#PBS -q kerenh
#PBS -v PBS_O_SHELL=bash,PBS_ENVIRONMENT=PBS_BATCH
#PBS -N trait_relax_avg_history_brent
#PBS -e /scratch300/halabikeren/jobs_output/
#PBS -o /scratch300/halabikeren/jobs_output/
#PBS -l select=ncpus=1:mem=4gb 
<commands>
```
Unfotunately, there is no way in PBS to name the log files of the job and also redirect them to a designated directory. So, currently you have two options:

Either name the log files, but have them created in the directory from which the jobs where submitted. Do this by using:
```
#PBS -e /scratch300/halabikeren/jobs_output/$JOB_NAME.$JOB_ID.ER
#PBS -o /scratch300/halabikeren/jobs_output/$JOB_NAME.$JOB_ID.OU
```
Or name the log files by the ID of the job, and have them created in your designated directory. Do this by using:
```
#PBS -e /scratch300/halabikeren/jobs_output/
#PBS -o /scratch300/halabikeren/jobs_output/
```
I will update you as soon as there is a better solution.

**How to enter cluster interactive mode:** Here, you have two options:
The reccomended one (According to Adi's team): enter by submitting a job to cluster interative mode: 
```
qsub clustInteractive.sh
```
Where clustInteractive.sh consists of:
```##############################################################
# Interactive Job Script 0.2
#
# This gives you a command prompt running in the kerenh queue.
# Resources:
# - Bash shell
# - 4 GB RAM
# - 1 CPU core
#####################################
#!/bin/bash
#PBS -S /bin/bash
#PBS -q kerenh
#PBS -N clustInteractive
#PBS -l select=ncpus=1:mem=4gb
#PBS -I
```
This option is better, due to a number of reasons:
* The session is considered, and thus, monitored by the PBS queue system, and you can limit the number of CPUs and memory that it can use ahead (with the ```-I``` argument).
* Unlike the direct connection to a node, the session is not terminated when the connection is interrupted. This is ahuge advantage, because say you run a script that submits jobs in bulk, and then repeatedly check which are done - you no longer need to keep the connection alive to assure that the script finishes its task. If the connection to the cluster is interrupted (because you went home / went to class or any other reason), you can revive the screen of the session. This is done by simply executing the command ```screen``` as soon as the session begins and before interrupting the connection, execute ```ctrl +a```. Dvory also provided addional information on how to utilize this approach:

Please see screen main useful commands below:
* screen                                          # to create a screen in a machine
* ctl +a, d                                       # to detach a screen – go out from a screen – but make it resumeable
* screen –ls                                      # to see all the screens and their IDs
* screen –r <id>                                  # to attach a screen
* ctl+A ?                                         # shows all options in the screen
* clt+A, x                                        # lock the screen for user
* ctl+a, k                                        # kill the screen
* ctl+a, <tab>                                    # resume to last screen
* screen –L                                       # Make terminal logging : to write to a file all the corresponding in the terminal of that screen
* screen -X -S <sessionid> kill                   # kill the screen outside
Also, If you press the arrow down key stroke, you may tell whether you are on a screen or not (if the display blinks – then you are in a screen)


The not recommended option (as we have done so far): 
```
ssh compute-0-20
```
This option is not recommended because it "steals" CPUs from the queue without being monitored by its system.

**Aliases:** Unoftunately, the basic queue commands (qstat, qdel, ect.) in power8 are built differently from the ones in Jekyl and Lecs. Thus, Shiran's lovely aliases for your .chrc file in Jekyl (which should correspond to the .bashrc file in power8) canot use most of them. Also, Haim's famous q.pl script must be copied from Jekyl to power8 in order to be able to use it (with the "q" command). I will update you as soon as I have a set of recommended aliases.

**To check which modules are available:** modules avail

**Tips from HPC team:**
* Use intel's compilers (icc for c and icpc for c++) instead of gcc and g++ (more effective). The icc and icpc equivalent to gcc > 6.0 are available in /powerapps/share/intel/parallel_studio_xe_2018/bin/ 
* The default shell script language in power8 is bash, so in order to set environment vairables, use ```export <VAR_NAME>=<VAR_VALUE>``` (bash compatible) instead of ```setenv <VAR_NAME> <VAR_VALUE>``` (csh / tcsh compatible)


