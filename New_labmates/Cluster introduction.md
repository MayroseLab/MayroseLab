# MayroseLab
Welcome to the Mayrose Lab Cluster! This guide will help you get started with using our cluster.

For any information about the cluster, here is an IT power guide- [https://computing.tau.ac.il/pt_hpc_power](https://hpcguide.tau.ac.il/index.php?title=Main_Page)

As the first step, make sure you have a user and appropriate permissions. If you don't have access yet, please get in touch with the HPC and IT team. 

**Connecting to the Cluster**
------
We usually recommend using mobaXterm as a Linux terminal and VS or pycharm to write progrems and debug
You can download mobaXterm here - https://mobaxterm.mobatek.net/download.html


**SSH Access**

To connect to the cluster, you'll need to use SSH (Secure Shell). If you use your terminal, open it and use the following command to log in:
```
ssh your_username@cluster_address
```
If you use mobaXtream: use this guide to define a session - ***need to add guide*** 

The cluster address: powerslurm-login.tau.ac.il

If you are off-campus, you might need to establish a VPN connection first as detailed in this guide - https://computing.tau.ac.il/helpdesk/remote-access/communication/vpn

*Interactive Sessions*
------
In order to prevent overloading the primary node, we use interactive job sessions. To request such a session, use this command:
```
srun --ntasks=56 -p power-general -A power-general-users --pty bash
```

Or to use a scesific machine:
```
srun --ntasks=56 -p power-general -A power-general-users --nodelist="compute-0-12" --pty bash
```

*Job Submission*
------
To run jobs on the cluster, you'll need to submit batch job scripts. A simple example:
```
#!/bin/bash

#SBATCH --job-name=my_job             # Job name
#SBATCH --account=my_account          # Account name for billing
#SBATCH --partition=long              # Partition name
#SBATCH --time=02:00:00               # Time allotted for the job (hh:mm:ss)
#SBATCH --ntasks=4                    # Number of tasks (processes)
#SBATCH --cpus-per-task=1             # Number of CPU cores per task
#SBATCH --gres=gpu:NUMBER_OF_GPUS     # number of GPU's to use in the job
#SBATCH --mem-per-cpu=4G              # Memory per CPU core
#SBATCH --output=my_job_%j.out        # Standard output and error log (%j expands to jobId)
#SBATCH --error=my_job_%j.err         # Separate file for standard error

# your commans here

```

Submit the job with:
```
sbatch your_script.sh
```

You can check your job status by using the following command:
```
squeue -u <your user name>
```
Use the following command to kill your job:
```
scancel <job id> 
```

*Remote Connection with IDE*
------
A guid to use ssh interpeter with pycharm https://github.com/MayroseLab/MayroseLab/blob/master/Setting%20up%20remote%20interperter%20for%20Pycharm.pdf
