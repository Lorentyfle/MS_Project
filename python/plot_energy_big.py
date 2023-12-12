import os
import matplotlib.pyplot as plt

directory = './../Input_output/'

for root, dirs, files in os.walk(directory):

    for filename in files:

        file = os.path.join(root, filename)
        if "out_energy - Copy.txt" in file:

            print("reading data from", file)

            # print(method, file)

            data = []
            #atom_type = []
            line_index = []
            energyMC = []
            energy = []

            with open(file,encoding="unicode_escape") as f:

                lines = [line for line in f]
                for index, line in enumerate(lines):
                    line_temp = lines[index]
                    # print(line_temp)
                    data.append(line_temp.split())

                # print(data)

                for i in range(len(data)):
                    #atom_type.append(float(data[i][0]))
                    line_index.append(float(data[i][1]))
                    energyMC.append(float(data[i][2]))
                    energy.append(float(data[i][3]))
                
                # print(line_index, energy)

                plt.rcParams.update(plt.rcParamsDefault)

                fig = plt.figure()
                ax = fig.add_subplot()
                
                xmin = min(line_index)
                xmax = max(line_index)
                dx = (xmax - xmin)/10
                
                ymin = min(energyMC)
                ymax = max(energyMC)
                dy = (ymax - ymin)/10
                
                ax.set_xlim(xmin - dx, xmax + dx)
                ax.set_ylim(ymin - dy, ymax + dy)
                
                x = line_index
                y = energyMC
                
                # print(x, y)
                
                x_color = 'black'
                x_label = 'MC steps'
                ax.set_xlabel(x_label, fontsize=12, color=x_color)
                
                y_color = 'black'
                y_label = 'Potential energy per atom (kJ/mol)'
                ax.set_ylabel(y_label, fontsize=12, color=y_color)
                
                ax.set_title('Monte Carlo Simulation of Ar (T = 87 K)')
                
                # plt.axhline(y = -5.5764, color = 'r', linestyle = 'dashed')  

                plt.plot(x, y, linestyle='-', linewidth=1.5, marker='', ms=2.5, mew=1.5, color='blue')
                
                print("generating figure for " + file)
                
                fig.savefig("energy_plot.png",
                    dpi=4000,
                    format='png',
                    transparent=False,
                    bbox_inches='tight')
                
