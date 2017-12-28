
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
* compute-7-0 to 7-11
* compute-8-10 to 8-14

**In Lecs2:**
* compute-4-20 to 4-29

<br/><br/>

**Working with power:**
------

**Address:** power8.tau.ac.il

**Please note: users that at some point during they're studies had a user created for them under computer science directory must request Danny to change their defalut directory in power. The password must be reset as well.**

**Syntax to submit a job:** qsub -q kerenh \<job_file_path\>

**To check which modules are available:** modules avail

**Tips from HPC team:**
* Use intel's compilers (icc for c and icpc for c++) instead of gcc and g++ (more effective). The icc and icpc equivalent to gcc > 6.0 are available in /powerapps/share/intel/parallel_studio_xe_2018/bin/ 
* The default shell script language in power8 is bash, so in order to set environment vairables, use ```export <VAR_NAME>=<VAR_VALUE>``` (bash compatible) instead of ```setenv <VAR_NAME> <VAR_VALUE>``` (csh / tcsh compatible)


