#########################################################################
#
# Track the RAM usage of a job.
# To run:
#       qsub -v "job_id=<job_id>, out_file=<out_file>" RAM_tracker.sh
# where <job_id> is the PBS job ID you want to track
# and <out_file> is the path to the file where RAM usage is written
#
#########################################################################

#!/bin/bash
#PBS -S /bin/bash
#PBS -N RAM_tracker
#PBS -r y
#PBS -q itaym
#PBS -V
#PBS -e ~/RAM_tracker.ER
#PBS -o ~/RAM_tracker.OU

sleep_time=60

stat=$(qstat -f $job_id | grep 'job_state' | awk -F' = ' '{print $2}')
while [ "$stat" = "R" ]
do
	ram=$(qstat -f $job_id | grep 'resources_used.mem' | awk -F' = ' '{print $2}')
	time_stamp=$(date)
	echo -e "$time_stamp\t$ram" >> $out_file
	sleep $sleep_time
	stat=$(qstat -f $job_id | grep 'job_state' | awk -F' = ' '{print $2}')
done
