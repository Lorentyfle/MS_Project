# PROFESSOR Jean-Christophe Soetens : Statistical Mechanics and Simulations 
## --- PCCP ---
## Never forget, every morning: Implicit none.
### Molecular Simulation Project

This project actually consists of three projects combined: (1) elaboration of an initial configuration for the simulation of a pure Lennard-Jones fluid, (2) simulation of a pure Lennard-Jones fluid using the Monte Carlo method, (3) generalization of the first two parts to a binary mixture will be done. In fact, the program as written is able to compute n species inside a box.

Program developed in FORTRAN90 by Alex Delhumeau and Timothée Jamin. 
Some supplementary tools written in Python and shell script.

### How to launch?

First compile everything using compiler.sh (just launch it on the terminal), then use the command:

./bin/PCCP_MC_LJ.x

## Known problems/inconveniences that will not be corrected:

Be careful, the program isn't yet coded to launch anywhere and generate the starting folders and all.

If the number of steps is too high in one go, the program will be slower because of high memory usage! You can solve this problem by launching the program using launch_loop.sh, which restarts the program automatically every N steps. The program can restart and average all the previous datas.

The g(r) computation isn't done the fastest way to do it, do not expect an excellent speed for such function.

### Note about the builds.

The tests were done using MSYS2 MINGW64 Shell and gfortran. If any problems arise, please, try using the same environement.

### Advancement
    - General matter : 100%
        - READ_ME + Planning. -DONE-
        - Compilator. -DONE-
        - Data input reader. -DONE-
        - Position input reader. -DONE-
        - Position output write. -DONE-
        - Correction of the input reader. -DONE-
    - Set up : 100%
        - Generation of system of atoms (Van der Waals spheres)     -DONE-
        - Molecular liquid (small molecule such as HF, H2O, CH4, ...) /REMOVED/
        - Mixture of atoms of several types (Mixtures of van der Waals spheres) -DONE-
    - MC simulation : 100%
        - Gestion of input and outputs.                         -DONE-
        - MC algorithm sample the input and output              -DONE-
        - Calculation of the rate of accepted/rejected moves.   -DONE-
        - Calculation of average energy                         -DONE-
        - Calculation of the structure of the fluid and comparison with 1954 results (Rahman, Phys. Rev. 136, 405 (1964))   -DONE-
    - MC simulation of binary (or N) mixture : 100%
        - Studying a mixture of two species to see the miscibility of two species.
            This is done by varying the ratios of Lennard-Jones parameters (epsilonA/epsilonB and sigmaA = sigmaB)          -CALCULATIONS COMPLETE- -RESULTS AVAILABLE-

Last updated: 15 December 2023 by Alex Delhumeau