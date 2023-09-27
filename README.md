# MS_Project
## --- PCCP ---
## Never forget, every morning: Implicit none.
### Simulation Project

This project is composed of the elaboration of an initial configuration for the simulation of a pure Lennard-Jones fluid with (if we have time), the simulation of a pure Lennard-Jones fluid hoping that the Monte-Carlo simulation of a finary fluid will be done.

Steps:

 - Starting set up
 - MC simulation
 - MC simulation of a binary mixture

Program developed by Alex Delhumeau and Timothée Jamin. 

### How to launch?

First compile everything using the compiler.sh (just launch it on the terminal), then use the command:

./bin/PCCP_MC_LJ.x

### Note about the builds.

The tests where done using MINGW64 and gfortran. If any problems arise, please, try using the same environement.

### Advancement
    - General matter :
        - READ_ME + Planning. -DONE-
        - Compilator. -DONE-
        - Data input reader. -DONE-
        - Position input reader. -DONE-
        - Position output write. -DONE-
    - Set up : 0%
        - Generation of system of atoms (Van der Waals spheres)
        - Molecular liquid (small molecule such as HF, H2O, CH4, ...)
        - Mixture of atoms of several types (Mixtures of van der Waals spheres)
        - /!\ Ensure that the energy of the configuration is not aberrant. To not put the simulation in failure after few steps. For example we can use MC method and a generic force field to relax the system.
        - /!\ We should be able to write a program capable of generating any configuration.
    - MC simulation : 0%
        - Gestion of input and outputs.
        - MC algorithm sample the input and output
        - Calculation of the rate of accepted/rejected moves.
        - Calculation of average energy
        - Calculation of the structure of the fluid and comparison with 1954 results (Rahman, Phys. Rev. 136, 405 (1964))
    - MC simulation of binary mixture : 0%
        - Studying a mixture of two species to see the miscibility of two species.
            This is done by varying the ratios of Lennard-Jones parameters (epsilonA/epsilonB and sigmaA = sigmaB)