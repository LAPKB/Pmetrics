#!/bin/bash

# set a few ENVIRONMENT variables
ATufhpc=false
ATlapkb=false # initially, you make no claim to know "where" you are
ATlapkb03=false
SERVER=`hostname`
if echo "$SERVER" | grep -q "ufhpc"; then # e.g. gator4.ufhcp
  echo "You are on ufhcp in Florida";
#  PMsrc="/home/mneely/lapk/src/Pmetrics/inst/code"
  PMsrc="$HOME/R/x86_64-pc-linux-gnu-library/3.4/Pmetrics/code"
#  PMsrc="/home/mneely/lapk/src/31127/src"
  PMobj="$HOME/.config/Pmetrics/compiledFortran/"
  ATufhpc=true
  # check if code exists, else exit
elif echo "$SERVER" | grep -q "lapkb"; then
  echo "You are on lapkb.usc.edu in California";
  Rlib="/home/$USER/R/x86_64-suse-linux-gnu-library/3.3"
  PMsrc="$Rlib/Pmetrics/code"
  ATlapkb=true
  # check if code exists, else exit
elif echo "$SERVER" | grep -q "dhcp-10-134-4-125"; then
  echo "You are on lapkb03.chla.usc.edu in California";
  Rlib="/home/$USER/R/x86_64-pc-linux-gnu-library/3.4"
  PMsrc="$Rlib/Pmetrics/code"
  PMobj="$HOME/.config/Pmetrics/compiledFortran/"
  ATlapkb03=true
  # check if code exists, else exit
else
  echo "This is wierd: I have no idea where you are!";
fi
rundescrip='PMcontrol extnum instr.inx npag103.inp npagdriv.f ClientInfo.txt'

# Notes (TODO list is at end of file; or in comments throughout script)
# 1) User must install the NPAG engine fortran code and set PMsrc appropriately.
#   Easiest way is to install the pmetrics pkg from R -- this will also aid in
#   future development as the fortran code will already be resident on server
#   at a standard location.
# 2)
#
#

# INSTALLATION:
#
# Step 1:
# Put this NPrun.sh script into $HOME/lapk/ # edit NPrun.sh as needed
# Step 2:
# if (ATufhpc)
#   $ module load ufrc
#   $ module load R
#   $ srundev # this will give you 10 minutes to run R and install pmetrics
#   # 10min is more than enough time; however, you can request more time with:
#   $ srundev --time=60
#   $ R
#    R> install.packages("~/lapk/src/Pmetrics_<N.N.N>.tar", repos=NULL,type="source")
# elseif (ATlapkb OR ATlapkb03)
#   $ R
#    R> install.packages("~/lapk/src/Pmetrics_<N.N.N>.tar.gz", repos=NULL,type="source")
# endif
# R> library(Pmetrics)
# R> PMbuild() # compile and install Pmetrics engines and create pmetrics config files
# ~/.config/Pmetrics/compiledFortran/<Pmetrics.o_files>
# ~/.config/Pmetrics/FortConfig.txt
# R> exit()
#
# User should now be set up for remote run
#

# Initiate Remote NPAG run
echo Initiating $1 Run $2
date
cd ~/lapk/
pwd

# make DIRECTORY TREE / FILE STRUCTURE and UNPACK necessary files
 
# If project directory does not exist, make it 
if [ ! -d $1 ] ; then
  echo making directory  $1
  mkdir $1
fi
cd $1
pwd
# If run directory exists, exit, else make it
if [ ! -d $2 ] ; then
  echo making directory  $2
  mkdir $2
else
  echo "Directory $2 exists"
  echo "Redo NPrun(...,rn=NNN,...) with new NNN"
  exit 1
fi
cd $2
pwd

# Unpack the run description files into the run directory, ~/lapk/$1/$2/
mv ../../$1.$2.tgz .
tar xvfz $1.$2.tgz

# You are now in the remote run directory and ready to begin.
remoteDirectory=$(pwd)

# read USER RUN INSTRUCTIONS

# Parse ClientInfo.txt as necessary
# (This is the mechanism for passing run parameters from user's local compjter
#  to this remote host computer)
# line 1 = parent directory on user's local computer
# line 2 = run directory on user's local computer (an integer)
# line 3 = user@this.server
# line 4 = project name (parent directory on this.server)
# line 5 = regression model
# line 6 = data.csv
# line 7 = parallel run ? TRUE : FALSE
# line 8 = overwrite previous run ? TRUE : FALSE
# last line = user's notification email
localDirectory=$(head -n 1 ClientInfo.txt)
Parallel=`tail -n 3 ClientInfo.txt` # guides compilation
Overwrite=`tail -n 2 ClientInfo.txt` # If remote run exists, delete it!
UserEmail=`tail -n 1 ClientInfo.txt` # Return address is always the last line

# Write file with download instructions (on local machine):
cat << EOT >> retrieve_run_instructions
To download the analysis to your local computer,
execute the following two unix commands:
> mkdir $localDirectory/$2/outputs
> scp $3:$remoteDirectory/* $localDirectory/$2/outputs
LAPK/USC users: Your run is finished and you may now execute 
the above commands
Hipergator/UFL users: You will receive an email from 
Slurm User when your job is completed; do not attempt to
execute the above commands until you receive confirmation
from Slurm User that your job is completed.
EOT

# COMPILE and RUN

echo Compiling NPAG on 
if [ $ATufhpc == true ] ; then # need the gcc modul to compile
  module load ufrc
  module load intel/2018 # module load gcc
  cp $PMsrc/NPeng_120.f .
  cp $PMsrc/dvode_v1.f90 .
  cp /home/mneely/lapk/src/31127/src/Makefile .
  make
  rm *.o
  # rm NPeng_120.f dvode_v1.f90 Makefile
fi
# else ... probably at LAPK so use GCC
# gfortran -O3 -w -fopenmp -fmax-stack-var-size=32768 -o nprun npagdriv.f $PMsrc/NPeng_120.f
# echo NPAG Compiled; echo ""

# ufl needs to run w/ "sbatch script.name"
# Remove old script
if [ $ATufhpc == true ] ; then # write the ufl slurm script here

if [ -f "Slurm$1Run$2.bat" ] ; then
  echo 'WARNING: Overwriting existing slurm script'
  rm Slurm$1Run$2.bat
fi

### #SBATCH --cpus-per-task == OMP_NUM_THREADS = $NoThreads
NoThreads='64'
# mem should be less than 3gb / thread

cat << EOT >> Slurm$1Run$2.bat
#!/bin/sh
#SBATCH --job-name=$1Run$2
#SBATCH --mail-user=$UserEmail
#SBATCH --account=gdrusano
#SBATCH --qos=gdrusano-b # gdrusano limited to 16CPU, gdrusano-b limited to 144
#SBATCH --mail-type=ALL    # (NONE, ALL, BEGIN, END, FAIL)
#SBATCH --output $1-$2-%j.out
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=$NoThreads   # increase this one for OpenMP (threaded, pthreaded) up to 32 on hpg2; 64 on hpg1
#SBATCH --mem=1gb
#SBATCH --time=48:00:00     # Walltime: Hr:Min:Sec
# End of primary Slurm directives
EOT
#  #SBATCH --mem-per-cpu=500mb

# Add script commands to run NPAG
echo "" >> Slurm$1Run$2.bat
echo "export OMP_NUM_THREADS=$NoThreads" >> Slurm$1Run$2.bat
echo 'date;hostname;pwd' >> Slurm$1Run$2.bat
echo "" >> Slurm$1Run$2.bat
echo 'module load ufrc' >> Slurm$1Run$2.bat
# echo 'module load gcc' >> Slurm$1Run$2.bat
echo 'module load intel/2018' >> Slurm$1Run$2.bat
# On Mac
# gfortran -O3 -w -fopenmp -fmax-stack-var-size=32768 \\
#  -o np_run \\
#  '/Users/mas/.config/Pmetrics/compiledFortran/pNPeng.o' \\
#  '/Users/mas/.config/Pmetrics/compiledFortran/pODEsolver.o' \\
#  npagdriv.f
# ../.config/Pmetrics/compiledFortran/pNPeng.o  ../.config/Pmetrics/compiledFortran/pODEsolver.o

# UFL Ticket 31127 -- gfortran replaced by intel compiler, called by make utility
# echo "cp $PMsrc/NPeng_120.f ." >> Slurm$1Run$2.bat
# echo "cp $PMsrc/dvode_v1.f90 ." >> Slurm$1Run$2.bat
# echo "cp $PMsrc/Makefile ." >> Slurm$1Run$2.bat
# echo "make" >> Slurm$1Run$2.bat
# echo "make clean" >> Slurm$1Run$2.bat
# echo "rm NPeng_120.f dvode_v1.f90 Makefile" >> Slurm$1Run$2.bat
#
# echo "gfortran -O3 -w -fopenmp -fmax-stack-var-size=32768 -o nprun $PMobj/pODEsolver.o npagdriv.f $PMsrc/NPeng_120.f" >> Slurm$1Run$2.bat
#
echo "" >> Slurm$1Run$2.bat
echo 'startedAt=$(date +"%s")' >> Slurm$1Run$2.bat
# 31127 remove srundev
echo './nprun' >> Slurm$1Run$2.bat
echo 'finishedAt=$(date +"%s")' >> Slurm$1Run$2.bat
echo "" >> Slurm$1Run$2.bat
echo 'difftimelps=$(($finishedAt-$startedAt))' >> Slurm$1Run$2.bat
# echo 'echo $(($difftimelps / 60)) minutes and $(($difftimelps % 60)) seconds elapsed for NPAG($1 :: $2) Execution.' >> Slurm$1Run$2.bat

# Cleanup
echo 'make cleanall' >> Slurm$1Run$2.bat
echo 'rm fort.*; rm CHMAX*' >> Slurm$1Run$2.bat
echo 'cp ClientInfo.txt ClientInfo.bak' >> Slurm$1Run$2.bat
# echo "for filename in $rundescrip" >> Slurm$1Run$2.bat
# echo 'do' >> Slurm$1Run$2.bat
# echo '  rm $filename' >> Slurm$1Run$2.bat
# echo 'done' >> Slurm$1Run$2.bat
echo "mv ClientInfo.bak ClientInfo.txt; rm $1.$2.tgz; rm nprun" >> Slurm$1Run$2.bat
# Only files that should be scp-ed back to user@calling_machine:~/.../project_name/$2/outputs/
# folder are now left in the server's run directory: ~/lapk/$1/$2/

# Slurm script finished

# 2) TODO make a tmp directory and cd there prior to launching the script (see man pages)
# 3) launch script, use srun or srundev to run the program
  chmod 747 ./Slurm$1Run$2.bat
  sbatch ./Slurm$1Run$2.bat # 'module load ufrc intel/2018' ran prior to entering this block

# Return to ~/lapk/ and email user summary of run + download instructions
  cd ../..; date
  mail -s "$1 NPAG Run $2 Is Submitted" $UserEmail < $1/$2/retrieve_run_instructions
  mv ~/lapk/$2.log ./$1
fi

if $ATlapkb == true || $ATlapkb03 == true ; then
  echo Linking to re-compiled modules in $PMobj
  if $Parallel == TRUE -o $Parallel == T ; then
    gfortran -O3 -w -fopenmp -fmax-stack-var-size=32768 -o nprun npagdriv.f $PMobj/pODEsolver.o $PMobj/pNPeng.o
  else
    gfortran -m64 -w -O3 -o nprun npagdriv.f $PMobj/sODEsolver.o $PMobj/sNPeng.o
  fi
  echo go > go  # the file go is used as input to nprun, why? seems not to be necessary.
  startedAt=$(date +"%s")
  ./nprun
  finishedAt=$(date +"%s")
  difftimelps=$(($finishedAt-$startedAt))
  echo ""
  echo "$(($difftimelps / 60)) minutes and $(($difftimelps % 60)) seconds elapsed for NPAG($1 :: $2) Execution."

# Run is FINISHED / CLEAN UP / INFORM / EMAIL USER

# Cleanup
  rm CHMAX*
  rm fort.*
#  cp ClientInfo.txt ClientInfo.bak
#  for filename in $rundescrip
#  do
#    rm $filename
#  done
#  mv ClientInfo.bak ClientInfo.txt
  rm nprun
  rm $1.$2.tgz
# Only files that should be scp-ed to user@calling_machine:~/.../project_name/$2/outputs/
# folder are now left in the server's run directory: ~/lapk/$1/$2/

# Return to ~/lapk/ and email user summary of run + download instructions
  cd ../..
  date
  mail -s "$1 NPAG Run $2 Finished" $UserEmail < $1/$2/retrieve_run_instructions
  mv ~/lapk/$2.log ./$1
fi


# Random usage and development notes: See bottom of file.
#
#   8) TODO email to user should attach the log; the email body should
#      contain download instructions
#   7) TODO Write add-in script to check npagdriv.f for malicious code
#
#   6) Would prevent potential version mismatch issues if NPeng_NNN.f is 
#      included in the rundescript tarball -- I chose NOT to include
#      NPeng*.f in the rundescript tarball to add one more layer of 
#      control on the server side, i.e. the ability to add in security
#      checks to the NPeng*.f code to ensure no one is compiling malicious
#      code, or attempting to use the server as a means of rewriting the
#      algorithm and potentially causing computer issues.
#
# ssh -n -f wyamada@lapkb.usc.edu './NPrun.sh > NP.log 2>&1'
#   5a) Redirecting to NP.log is reqd -- else the local terminal has stdout
#      printed to it AND ssh does not release control to local user until
#      final CR is given
#   5b) Note 5a) applies to 128.125.81.30 (not sure if true for 10.141.2.240)
#
# ssh -n -f wyamada@machine.IP -c './NPrun.sh > NP.log 2>&1'"
#   4) Above hangs, for machine.IP = 128.125.81.30; but runs for
#      machine.IP = 10.141.2.240
#
#   3) Redirecting to NP.log is reqd -- else the local terminal has stdout
#      printed to it AND ssh does not release control to local user until
#      final CR is given
#      printed to it AND ssh does not release control to local user until
#      final CR is given
#      printed to it AND ssh does not release control to local user until
#      final CR is given
#
#   2) Martin Shortenned command to 
#      ssh -n -f wyamada@lapkb.usc.edu './NPrun.sh > NP.log 2>&1'
#      which seemed to work.
#
