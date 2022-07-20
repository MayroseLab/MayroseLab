# MayroseLab


**Here is a set of commands that you might want to have in your .chrc file:**
------

**module loads**
* To load R: module load R/R302
* To load python 3.4: module load python/anaconda3-4.0.0
* To allow multi-threading using CPU: module load rocks-openmpi
* To allow multi-threading using GPU: module load openmpi-x86_64

**Cool script from Haim that shows the queues status and occupancy distribution among users:**
* alias q='perl /groups/pupko/haim/pupkoSVN/trunk/scripts/q.pl'


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
	


**Working with power8:**
------


**Address:** power8.tau.ac.il (this is the head node).

In order to reduce the load on the head node, we should connect to the login node instead of the head node:

**Address:** username@powerlogin.tau.ac.il (The jobs should be submitted from here).

* display which nodes belong to the queue itaym: qmgr -c "p n @d" | grep -i itaym | awk '{print $3}'
* display all nodes in the cluster: qmgr -c "p n @d"
* filter based on server name: grep -i itaym
* display only the nodes names: awk '{print $3}'
* Check the health of each machine in queue itaym - `for node in $(qmgr -c "p n @d" | grep -i itaym | awk '{print $3}' | sort | uniq); do echo $node; ssh $node "echo OK"; done`

**Our nodes - with assignments to queue:**  
**Power8:**

| Node          | CPUs | RAM (GB) | itaym | itaymaa | itay_25_1 | itay_25_2 | itay_25_3 | itay_25_4 | lifesciweb |
|---------------|------|----------|-------|---------|-----------|-----------|-----------|-----------|------------|
| compute-0-247 |  40  |    62    |   V   |         |     V     |           |           |           |            |
| compute-0-248 |  40  |    62    |       |         |           |           |           |           |      V     |
| compute-0-249 |  40  |    62    |   V   |         |           |     V     |           |           |      V     |
| compute-0-259 |  40  |    62    |   V   |         |           |           |     V     |           |            |
| compute-0-260 |  40  |    62    |   V   |         |           |           |     V     |           |            |
| compute-0-261 |  40  |    62    |   V   |         |           |           |           |     V     |            |
| compute-0-262 |  40  |    62    |   V   |         |           |           |           |     V     |            |
| compute-0-273 |  48  |    125   |   V   |   V     |     V     |           |           |           |            |
| compute-0-274 |  48  |    125   |   V   |         |           |           |           |     V     |            |
| compute-0-275 |  48  |    125   |   V   |         |           |     V     |           |           |            |
| compute-0-276 |  48  |    125   |   V   |         |           |           |     V     |           |            |
| compute-0-277 |  48  |    125   |   V   |         |           |     V     |           |           |            |
| compute-0-282 |  40  |    62    |   V   |         |     V     |           |           |           |            |
| compute-0-283 |  40  |    62    |   V   |         |           |     V     |           |           |            |
| compute-0-285 |  40  |    62    |   V   |         |           |           |     V     |           |            |
| compute-0-301 |  36  |    188   |   V   |         |           |           |           |     V     |            |
| compute-0-302 |  36  |    188   |   V   |         |     V     |           |           |           |            |
| compute-0-384 |  16  |    125   |   V   |         |           |           |           |     V     |            |

Overall, all the nodes have 728  CPUs and 1476 GB RAM.  
We also have itay_50_1 = itay_25_1&4, itay_50_2 = itay_25_2&3 and itay_75 = itay_25_2&3&4
---------------------------------------------------------------------------------------------------------------------	
**Power9:**

| Node          | CPUs | RAM (GB) | itaym | 
|---------------|------|----------|-------|
| compute-0-284 |  40  |    65    |   V   |
| compute-0-360 |  24  |    131   |   V   |
| compute-0-362 |  24  |    131   |   V   |
| compute-0-361 |  24  |    131   |   V   |
| compute-0-386 |  16  |    131   |   V   |
| compute-0-8   |  96  |    394   |   V   |
| compute-0-9   |  96  |    790   |   V   |

Overall, all the nodes have 320 CPUs and 1773  GB RAM.
-----------------------------------------------------------------------------------------------------------------------
**Please note: users that at some point during their studies had a user under computer science directory must ask Danny to change their defalut directory in power. The password must be reset as well.**

**Syntax to submit a job:** qsub -q kerenh \<job_file_path\>

**Some usefull commands:**
* qstat -u username	Displays all the jobs of the user.
* qstat -f jobid	Displays details of the job identified by the job id.
* qstat -q		Shows details about the queues in power (number of running jobs and waiting jobs per each queue, memory, etc.)
* qstat kerenh		Displays all the jobs submitted to the queue kerenh (or any other queue that is specified instead of kerenh).

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
If you want to run the job on a specific node (for example on compute-0-276), you can specify #PBS -l nodes=compute-0-276,mem=4gb (instead of "#PBS -l select=ncpus=1:mem=4gb").

Unfotunately, there is no way in PBS to name the log files of the job and also redirect them to a designated directory. So, currently you have two options:

Either name the log files, but have them created in the directory from which the jobs were submitted. Do this by using:
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

**Alter job settings after submission:**
* Move job to another queue: qmove -q <queue_name> <job_id>
* Alter resource of jobs: qalter -l select=1:ncpus=<cpus_num>:mem=<memory in gb>gb <job_id>

**Aliases:** Unoftunately, the basic queue commands (qstat, qdel, ect.) in power8 are built differently from the ones in Jekyl and Lecs. Thus, Shiran's lovely aliases for your .chrc file in Jekyl (which should correspond to the .bashrc file in power8) canot use most of them. Also, Haim's famous q.pl script must be copied from Jekyl to power8 in order to be able to use it (with the "q" command). I will update you as soon as I have a set of recommended aliases.

By default, qstat will give you all jobs on all queues. To only view your jobs, you can use `qstat -u <username>`, or you can add the following alias to your `~/.bashrc` file: ``alias qstat="qstat -u `whoami`"``. Note that if you do that you will have to use `\qstat` whenever you want the original behavior (i.e. list jobs for all users).

**To check which modules are available:** modules avail

**To sync all your data from hyde to power (recommened by Dvori: use power5 instead of power8 to run the sync command):**
rsync -avz --progress username@hyde.tau.ac.il:/groups/itay_mayrose/username/ /a/home/cc/students/lifesci/username/

**Tips from HPC team:**
* Use intel's compilers (icc for c and icpc for c++) instead of gcc and g++ (more effective). The icc and icpc equivalent to gcc > 6.0 are available in /powerapps/share/intel/parallel_studio_xe_2018/bin/ 
* The default shell script language in power8 is bash, so in order to set environment vairables, use ```export <VAR_NAME>=<VAR_VALUE>``` (bash compatible) instead of ```setenv <VAR_NAME> <VAR_VALUE>``` (csh / tcsh compatible)

**Useful PDB commands**
* To delete all your jobs: ```qselect -u <username> | xargs qdel```
* To check which nodes your jobs are going to: qstat -1 -n | grep <username> (if usernae fails, try to trim it)

**Useful bash commands**
* To run a user interactive program in a single command while chaining multiple arguemnts, use: ```(printf "n\ny\n7\n" && cat) | some_tool```

**View logs of finished jobs (only a week back)**
1) login to power8.tau.ac.il
2) cd /var/spool/pbs/server_logs/
4) ls will present file with names corresponding to logs by date
3) grep <job_id> <date_of_job_finish_time>
