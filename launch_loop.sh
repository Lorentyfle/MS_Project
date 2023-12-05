#!/bin/bash

loop=1
number_rdf=200

while [ $loop -lt $number_rdf ]
do
    echo "Restarting simulation for another 10000 steps."
    echo $loop
    ./bin/PCCP_MC_LJ.x
    loop=$[$loop+1]
done

echo "Simulations complete."
