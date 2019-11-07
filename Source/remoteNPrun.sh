#!/bin/bash
# Yamada 8/24/2017
#
# This script packages an NPrun() to run on a remote server
#
# This script assumes successful completion of ./npprep
#   thus, there is no error checking.

# These files describe the desired run:
#
# ~/ClientInfo.txt is written by Pmetrics::.PMrun(); others are output of the
# NPprep_xxx program
# flist='PMcontrol extnum instr.inx npag103.inp npagdriv.f npscript ~/ClientInfo.txt'
flist='PMcontrol extnum instr.inx npag103.inp npagdriv.f ClientInfo.txt'

# Usage:
# Darwin$ ./remoteNPrun.sh # RunNumber RemoteUser@Server ProjectName ModelFile DataFile ContactEmail
#
# Regardless of how it is launched, ./remoteNPrun.sh requires the file ~/ClientInfo.txt AND
# the file remoterunargs.txt to be on the system to initialize the run. See below for details
# of what is in these files (short: 7 lines). ~/ClientInfo.txt should be in the launch directory
# for the script, while remoterunargs.txt should be in the same directory as the output files
# from NPprep_NNN.
#
# 8/24/2017 Ubuntu had no problem launching the script from pwd, while osx wanted to launch
# from $HOME. So, I'm just going to make the file ~/Client.info mandatory. ~/Client.info will
# be copied to the launch directory.

#
# TODO !!! The install "filter" rids the code of all lines that begin w/a c
#  thus cp, cd, etc. get "lost"
#  Write the filter so that it ONLY filters *.f, *.for, *.F, and *.FOR (don't worry
#  about *.f77, *.f95, etc.)

echo Determining local OS
platform='unknown'
osname=$(uname)
if [[ "$osname" == 'Linux' ]]; then
   platform='linux'
elif [[ "$osname" == 'Darwin' ]]; then
   platform='darwin'
elif [[ "$osname" == 'FreeBSD' ]]; then
   platform='freebsd'
fi
echo "Platform is $platform"
echo

# On Darwin this script launches in $HOME, which R found via
# path.expand("~"); on Linux, launch might happen elsewhere.
echo Launch directory:
pwd
echo

# The following argument values are written in ~/ClientInfo.txt
ii=1
while IFS='' read -r line || [[ -n "$line" ]]; do
#    echo "Text read from file: $line"
    if [[ $ii -eq 1 ]] ; then
      calling_directory=$line # ; echo calling_directory is $calling_directory
    fi
    if [[ $ii -eq 2 ]] ; then
      run_no=$line # ; echo run_no is $run_no
    fi
    if [[ $ii -eq 3 ]] ; then
      useratserver=$line # ; echo useratserver is $useratserver
    fi
    if [[ $ii -eq 4 ]] ; then
      projname=$line # ; echo remote projname is $projname
    fi
    if [[ $ii -eq 5 ]] ; then
      modelfile=$line # ; echo modelfile is $modelfile
    fi
    if [[ $ii -eq 6 ]] ; then
      datafile=$line # ; echo datafile is $datafile
    fi
    if [[ $ii -eq 7 ]] ; then
      parallel=$line # ; echo remote run is parallel? parallel
    fi
    if [[ $ii -eq 8 ]] ; then
      overwrite=$line # ; echo delete old remote  data? $overwrite
    fi
    if [[ $ii -eq 9 ]] ; then
      contactemail=$line # ; echo contactemail is $contactemail
    fi
    let ii++
done < ~/ClientInfo.txt

# Compare the $calling_directory, with the first line of remoterunargs.txt file
# (also output by .PMrun()) that should be found in $calling_directory.
if [[ $platform == 'darwin' ]]; then
  line=$(head -n 1 "$calling_directory/$run_no/remoterunargs.txt")
elif [[ $platform == 'linux' ]]; then
  line=$(head -n 1 "$calling_directory/$run_no/remoterunargs.txt")
fi
echo
echo This path from remoterunargs.txt
echo "$line"
echo should be the same as this path from ~/ClientInfo.txt
echo $calling_directory
echo

# 001. Reserve a local directory on client named $run_no to store analysis

# mv the ~/ClientInfo.txt file to /.../Runs/RunNumber
if [[ $platform == 'darwin' ]]; then
  if [ ! -d $calling_directory/$run_no ] ; then
    mkdir "$calling_directory/$run_dir"
  fi
  mv ClientInfo.txt "$calling_directory/$run_no"
  cd "$calling_directory/$run_no"
elif [[ $platform == 'linux' ]]; then
  if [ ! -d "$calling_directory/$run_no" ] ; then
    mkdir "$calling_directory/$run_dir"
  fi
  mv ~/ClientInfo.txt "$calling_directory/$run_no"
  cd "$calling_directory/$run_no"
fi

echo You are now in
pwd
echo
echo Files in your current directory are:
ls
echo

# Confirm run description
echo ""
echo Calling directory on localhost is $calling_directory
echo Remote Project/Dir: $projname/$run_no, Model: $modelfile, Data: $datafile
echo Server: $useratserver
echo Contact Email: $contactemail

echo ""
read -p "Do you wish to prepare files for optimization ? " -n 1 -r
echo    # move to a new line
if [[ $REPLY =~ ^[Nn]$ ]]
then
    exit 0
fi

echo Attempting to archive and bookmark $projname Run $run_no for remote execution

# if [ ! -d $run_no ] ; then
#  echo checking to see if you are already in $run_no
#  rootdir=$(basename "$calling_directory")
#  echo pwd is $calling_directory,  rootdir is $rootdir, and run is $run_no
#  if [ "$rootdir" == "$run_no" ] ; then
#     echo PMrun has moved you into $run_no, making /etc/ and /inputs/
     if [ ! -d etc ] ; then
       mkdir etc
     fi
     if [ ! -d inputs ] ; then
       mkdir inputs
     fi
#  else
#     echo making directory $run_no
#     mkdir $run_no
#     if [ ! -d $run_no/etc ] ; then
#       mkdir $run_no/etc
#     fi
#     if [ ! -d $run_no/inputs ] ; then
#       mkdir $run_no/inputs
#     fi
#  fi
# else
#   echo $run_no already exists
#  echo "Redo NPrun(remote=T,run=NNN,...) with new NNN"
#  exit 1
# fi

# 002. Move the run instructions to ./$run_no/etc/ or ./etc/

echo "Copying description files to $calling_directory/$run_no/etc/"

# if [ -f ClientInfo.txt ] ; then
#   echo " "
#  echo NOTE: Removing old ClientInfo.txt file
#  echo " "
#   rm ClientInfo.txt
#  echo Creating new ClientInfo.txt file
# else
#   echo Creating new ClientInfo.txt file
# fi
# if [ "$rootdir" == "$run_no" ] ; then
# cat << EOT >> ClientInfo.txt
# $calling_directory
# RemoteUser@Server=$useratserver
# ProjectName=$projname
# Contact=$contactemail
# EOT
# else
# cat << EOT >> ClientInfo.txt
# $calling_directory/$run_no
# RemoteUser@Server=$useratserver
# ProjectName=$projname
# Contact=$contactemail
# EOT
# fi

tar -czvf $projname.$run_no.tgz $flist 

# if [ "$rootdir" == "$run_no" ] ; then
  for filename in $flist
  do
    mv $filename ./etc
  done
  mv $projname.$run_no.tgz ./etc
  mv np_prep ./etc
  rm time.txt
  rm log.txt
  echo "./etc/$projname.$run_no.tgz archive is created"
#  if [ ! -d inputs ] ; then
#    mkdir inputs
#  fi
  mv $modelfile inputs/
  mv $datafile inputs/
# else
#   for filename in $flist
#   do
#     cp $filename ./$run_no/etc
#   done
#  mv $projname.$run_no.tgz ./$run_no/etc
#  echo "./$run_no/etc/$projname.$run_no.tgz archive is created"
#  if [ ! -d $run_no/inputs ] ; then
#    mkdir $run_no/inputs
#  fi
#  cp $modelfile $run_no/inputs/
#  cp $datafile $run_no/inputs/
# fi

echo ""
read -p "Archive Created; Do you wish to copy files to remote server ? " -n 1 -r
echo
if [[ $REPLY =~ ^[Nn]$ ]]
then
# Clean up the working directory prior to aborting run
echo "Moving wrk files to wrkcopy"
     if [ ! -d wrkcopy ] ; then
       mkdir wrkcopy
       mv XQZPJ*.ZMQ wrkcopy/
       rm fort.*; rm npag*.*; rm go
       mv *.for ./etc/
     fi
    exit 0
fi

# 003. scp $run_no.tgz to the remote server, ssh into the server and execute
#  requires an automatic ssh connection; see
#  http://www.linuxproblem.org/art_9.html
#  for instructions how to set this up ... but didn't work; still need
#  user password.

# if [ "$rootdir" == "$run_no" ] ; then
#   echo copying ./$run_no/etc/$projname.$run_no.tgz to $useratserver:~/lapk/
#   scp ./$run_no/etc/$projname.$run_no.tgz $useratserver:~/lapk/
# else
  echo copying ./etc/$projname.$run_no.tgz to $useratserver:~/lapk/
  scp ./etc/$projname.$run_no.tgz $useratserver:~/lapk/
# Clean up the working directory prior to scp to server
  echo "Moving wrk files to wrkcopy"
     if [ ! -d wrkcopy ] ; then
       mkdir wrkcopy
       mv XQZPJ*.ZMQ wrkcopy/
       rm fort.*; rm npag*.*; rm go
       mv *.for ./etc/
     fi
# fi

echo Initiating run $projname/$run_no on remote server
remote_command="cd ~/lapk; ./NPrun.sh $projname $run_no $useratserver > $projname_npRrun_$run_no.log 2>&1"
echo ssh -n -f $useratserver $remote_command
ssh -n -f $useratserver $remote_command

echo ""
echo ""
echo Remote run is initiated. An email will be sent to
echo $contactemail
echo with download instructions after the run is completed.
echo ""
echo Do NOT delete, rename, or write to, the directory
echo $calling_directory/$run_no
echo Doing so may result in loss of data and may interfere or 
echo prevent successful completion of local NPrun commands
echo ""




# 
# Retrieval script
#
# The data will be saved by scp to $calling_directory/$run_no/output/
#
# TODO Write new function Pmetrics::NPretrieve(user@server,project,run)
#

# end

# Random Notes
#
# 1) See notes re: $remote_command inside the NPrun.sh script on the
#   server.
#
# 3/30/2017 I could NOT get R to pass the args nicely on the command line using a simple
#  system("string") call. It kept passing "./remoteNPrun.sh run_dir ..." as a single
#  command to run; instead of having bash parse the line from left to right :-(
# 3/30/2017 New strategy is to read the args in from the remoterunargs.txt file, created
#  by Pmetrics:::.PMrun()
# 3/30/2017 OK . . . reading the args from remoteargs.txt is aesthetically unpleasing. So
#  calling Terminal.app via an osascript
#

