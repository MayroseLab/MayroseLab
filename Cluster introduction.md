# MayroseLab
Welcome to the Mayrose Lab Cluster! This guide will help you get started with using our cluster.

For any information about the cluster, here is an IT power guide- https://computing.tau.ac.il/pt_hpc_power

As the first step, make sure you have a user and appropriate permissions. If you don't have access yet, please get in touch with the HPC and IT team. 

**Connecting to the Cluster**
------
We usually recommend using mobaXterm as a Linux terminal.
You can download it here - https://mobaxterm.mobatek.net/download.html


**SSH Access**

To connect to the cluster, you'll need to use SSH (Secure Shell). If you use your terminal, open it and use the following command to log in:
```
ssh your_username@cluster_address
```
If you use mobaXtream: use this guide to define a session - ***need to add guide*** 

We have two common clusters in the lab. 

power8 with the address: powerlogin.tau.ac.il

power9 with the address: power9login.tau.ac.il

If you are off-campus, you might need to establish a VPN connection first as detailed in this guide - https://computing.tau.ac.il/helpdesk/remote-access/communication/vpn

*Interactive Sessions*
------
In order to prevent overloading the primary node, we use interactive job sessions. To request such a session, use this command:
```
qsub -I -X -q <queue> -N <interactive_job_name>
```

*Job Submission*
------
To run jobs on the cluster, you'll need to submit batch job scripts. A simple example:
```
#!/bin/bash
#PBS -S /bin/bash
#PBS -N <job name>
#PBS -r y
#PBS -q itaym
#PBS -V
#PBS -e <error file>
#PBS -o <output file>
#PBS -p 3

#source ~/.bashrc
hostname
#conda activate
#export PATH=$CONDA_PREFIX/bin:$PATH

cd ~/<script directory> 

Your commands here >myout 2>myerr
```

Submit the job with:
```
qsub your_script.sh
```



