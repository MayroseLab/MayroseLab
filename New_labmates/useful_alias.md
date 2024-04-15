You are more then welcome to copy the following commands into your ~/.bashrc file to create the following shortcats commands:

To open interactive job, Don't forget to change <YOUR_NAME> to a name for your interactive job:
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

Sometimes jupyter will just stop working - you paste the link to your browser and get "page not found". This happens because of some random port blocking by TAU IT. The bad news is that we have no way of fixing it. The good news is that we have a workaround:

1. Make sure you are connected to VPN

2. In MobaXterm, start a local session and run the command:
```
$ ssh -N -f -L localhost:8080:localhost:8080 <username>@hpcssd.tau.ac.il
```
(replace <username> with your user name)
Keep the session open as long as you are working.

3. Start an ssh session on machine hpcssd - this is done the same way we do on power8/9, except the host name is: hpcssd.tau.ac.il. Use your regular user and password.
One inside, go to the location where you want to start jupyter and run:
```
$ jupyter lab --no-browser --ip=0.0.0.0 --port=8080 &
```


5. After the server has started, open a browser and go to: http://localhost:8080/
And a jupyter client should start!

A few notes:
* There is no way to do that using an interactive job and there is no other machine we can use (which sucks if hpcssd is very busy).
* You can use other ports (e.g. 8081), as long as they match in all commands. This is useful because sometimes ports get stuck on old dead sessions - just switch to a another port.
* If the client asks for a token to access, you can run:
$  jupyter server list
and copy-paste the token


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
