#!/usr/bash #a Bash shell script

#the 4 following lines are Slurm directives
#SBATCH -J Ultra #Ultra is the name of the job
#SBATCH --nodes=1 #the job will run on 1 node (nodes=1)
#SBATCH --ntasks=1 #the job will run on 16 cores (ntasks=16)
#SBATCH --mem=42gb #42gb of memory are required
#SBATCH --time=5:21:56 #the job will be killed if it exceeds 5 h 21 min 56 s

module load gfortran #load the module that corresponds to the program you want to use

cd $TMPDIR #enter the TMPDIR directory on the node where the calculation is performed (typically for serial and OpenMP jobs)
# cd $SCRDIR #enter the SCRDIR directory on the parallel file system (essentially, but not only, for MPI jobs)
cp $HOME/MS_Project . #copy the input file(s) from you HOME to your TMPDIR directory
exec ./MS_Project/bin/PCCP_MC_LJ.x #exec is the executable file of the desired program
cp -r ./Input_output/* $HOME/. #copy the output file(s) to your HOME directory

/bin/rm -rf $TMPDIR $SCRDIR #remove all traces of the calculation