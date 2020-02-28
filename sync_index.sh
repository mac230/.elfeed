#!/bin/bash
## This script is designed to sync elfeed databases across my home and work computers.
## It uses an 'if' test to determine which machine we're on and rsync to sync databases
## across machines.  For now, I'm dumping the index file into the albert lab MSI space
## to push the index file to.  This way, I have a central location for the index. 
## the rsync syntax is: rsync [src] [dest] 


## -----
## setup
cd ~/.elfeed
touch ./rsync_log_elfeed
echo $(date) > ./rsync_log_elfeed
## make a local backup of the index prior to syncing
cp ./index ~/Desktop/emacs/local_elfeed_index_backup


## -----
## output
## get the sum for if-based comparison 
printf "sha sum BEFORE sync: $( sum ./index | cut -f 1 -d " ")\n" >> ./rsync_log_elfeed
cat ./rsync_log_elfeed


## -----
## syncing 
## push any existing LOCAL changes to the remote (MSI) index 
rsync -u --stats ~/.elfeed/index mahlon@login.msi.umn.edu:/home/albertf/mahlon/index 

## write output and sync changes from remote index 
rsync -u --stats mahlon@login.msi.umn.edu:/home/albertf/mahlon/index ~/.elfeed/index
printf "sha sum AFTER sync:  $( sum ./index | cut -f 1 -d " ")\n" >> ./rsync_log_elfeed
printf "script successfully finished at $( date "+%H:%M")\n" >> ./rsync_log_elfeed
cat ./rsync_log_elfeed
