# MS_Project
## --- PCCP ---
## Never forget, every morning: Implicit none.
### Simulation Project

This project is composed of the elaboration of an initial configuration for the simulation of a pure Lennard-Jones fluid with (if we have time), the simulation of a pure Lennard-Jones fluid hoping that the Monte-Carlo simulation of a binary fluid will be done. The program is able to compute n species inside a box.

Steps:

 - Starting set up
 - MC simulation
 - MC simulation of a binary and rank n mixture

Program developed by Alex Delhumeau and Timothée Jamin. 

### How to launch?

First compile everything using the compiler.sh (just launch it on the terminal), then use the command:

./bin/PCCP_MC_LJ.x

## Known problems/inconveniences not corrected yet.

Be careful, the program isn't yet coded to launch everywhere and generate the starting folders and all.

If the number of steps is too high in one go, the program will be slower! You can solve this problem by restarting the program automatically that will average all the previous datas.

The g(r) computation isn't the fastest one, do not expect an excellent speed for such function.

### Note about the builds.

The tests where done using MINGW64 and gfortran. If any problems arise, please, try using the same environement.

### Advancement
    - General matter :
        - READ_ME + Planning. -DONE-
        - Compilator. -DONE-
        - Data input reader. -DONE-
        - Position input reader. -DONE-
        - Position output write. -DONE-
        - Correction of the input reader. -DONE-
    - Set up : 100%
        - Generation of system of atoms (Van der Waals spheres)     -DONE-
        - Molecular liquid (small molecule such as HF, H2O, CH4, ...) /REMOVED/
        - Mixture of atoms of several types (Mixtures of van der Waals spheres) /REMOVED/
    - MC simulation : -DONE-
        - Gestion of input and outputs.                         -DONE-
        - MC algorithm sample the input and output              -DONE-
        - Calculation of the rate of accepted/rejected moves.   -DONE-
        - Calculation of average energy                         -DONE-
        - Calculation of the structure of the fluid and comparison with 1954 results (Rahman, Phys. Rev. 136, 405 (1964))                    -TO COMPARE-
    - MC simulation of binary (or N) mixture : 50%
        - Studying a mixture of two species to see the miscibility of two species.
            This is done by varying the ratios of Lennard-Jones parameters (epsilonA/epsilonB and sigmaA = sigmaB)                       -50%-