You are more then welcome to copy the following commands into you ./bashrc file to create the following shortcats commands:

To open interactive job:
```
alias int_job="qsub -I -X -q itaymaa -N <YOUR_NAME>_interactive"
alias int_job9="qsub -I -X -q itaym -N <YOUR_NAME>_interactive"
```
To check the job's status:
```
alias qu="qstat -u $(whoami)"
```
To delete all runing (qrdel) or queueing (qwdel) jobs:
```
alias qwdel='qselect -s Q -u $(whoami) | xargs qdel'
alias qrdel='qselect -s R -u $(whoami) | xargs qdel'
```
To open jupyter notebook at the lab (jlab) of from home (jlabh):
```
alias jlab="jupyter lab --no-browser --ResourceUseDisplay.mem_warning_threshold=0.1 --ResourceUseDisplay.track_cpu_percent=True  --ip=0.0.0.0 2>&1 | grep -m2 $(hostname) | sed -e 's/'"$(hostname)"'/'"$(ip address | grep '132.66' | awk '{print $2}'| awk -F'/' '{print $1}')"'/g' | sed '2!d'"
alias jlabh="jupyter lab --no-browser --ip=0.0.0.0 --port=8080 2>&1 | grep -m2 $(hostname) | sed -e 's/'"$(hostname)"'/'"$(ip address | grep '132.66' | awk '{print $2}'| awk -F'/' '{print $1}')"'/g' | sed '2!d'
```

Copy all of the alias togeher here:
```
alias int_job="qsub -I -X -q itaymaa -N <YOUR_NAME>_interactive"
alias int_job9="qsub -I -X -q itaym -N <YOUR_NAME>_interactive"
alias qu="qstat -u $(whoami)"
alias qwdel='qselect -s Q -u $(whoami) | xargs qdel'
alias qrdel='qselect -s R -u $(whoami) | xargs qdel'
alias jlab="jupyter lab --no-browser --ResourceUseDisplay.mem_warning_threshold=0.1 --ResourceUseDisplay.track_cpu_percent=True  --ip=0.0.0.0 2>&1 | grep -m2 $(hostname) | sed -e 's/'"$(hostname)"'/'"$(ip address | grep '132.66' | awk '{print $2}'| awk -F'/' '{print $1}')"'/g' | sed '2!d'"
alias jlabh="jupyter lab --no-browser --ip=0.0.0.0 --port=8080 2>&1 | grep -m2 $(hostname) | sed -e 's/'"$(hostname)"'/'"$(ip address | grep '132.66' | awk '{print $2}'| awk -F'/' '{print $1}')"'/g' | sed '2!d'"
```
