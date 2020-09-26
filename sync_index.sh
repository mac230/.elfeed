#!/bin/bash
## This script is designed to sync elfeed databases across my home and work computers.
## It uses an 'if' test to determine which machine we're on and rsync to sync databases
## across machines.  For now, I'm dumping the index file into the albert lab MSI space
## to push the index file to.  This way, I have a central location for the index. 
## the rsync syntax is: rsync [src] [dest] 


## -----
## setup
cd ~/.elfeed
touch ./rsync_log
echo $(date) > ./rsync_log


## -----
## output
## get the sum for if-based comparison 
printf "sha sum of local index backup: $( sum ./index_local_backup | cut -f 1 -d " ")\n" >> ./rsync_log


## -----
## syncing 
## push any existing LOCAL changes to the remote (MSI) index 
rsync -ui ~/.elfeed/index_local_backup mahlon@login.msi.umn.edu:/home/albertf/mahlon/msi_index >> ./rsync_log

