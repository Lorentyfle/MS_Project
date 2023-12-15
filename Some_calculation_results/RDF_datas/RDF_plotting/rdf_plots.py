import os
import matplotlib.pyplot as plt
# from numpy import pi
# from scipy.integrate import simpson
import inspect

directory = './'

def retrieve_name(var):
    callers_local_vars = inspect.currentframe().f_back.f_locals.items()
    return [var_name for var_name, var_val in callers_local_vars if var_val is var]

for root, dirs, files in os.walk(directory):

    for filename in files:
        file = os.path.join(root, filename)

        if "rdf" in file and ".txt" in file:

            with open(file) as d:

                lines = [line for line in d]
                data_raw = []
                data = []

                for index, line in enumerate(lines):
                    data_raw = lines[index].split()
                    for i in range(len(data_raw)):
                        data_raw[i] = data_raw[i].replace(",", "")
                        data_raw[i] = data_raw[i].replace("[", "")
                        data_raw[i] = data_raw[i].replace("]", "")
                    data.append(data_raw)

                distance = []
                number = []

                for j in range(len(data)):
                    distance.append(float(data[j][0]))
                    number.append(float(data[j][1]))

                x = distance
                y = number

                plt.rcParams.update(plt.rcParamsDefault)

                fig = plt.figure()
                ax = fig.add_subplot()

                xmin = min(x)
                xmax = max(x)
                ymin = min(y)
                ymax = max(y)

                dy = max((ymax - ymin) / 10, 0.1)
                dx = max((xmax - xmin) / 10, 0.1)

                ax.set_xlim(xmin - dx, xmax + dy)
                ax.set_ylim(ymin - dy, ymax + dy)

                x_color = 'black'
                x_label = 'Distance (Å)'

                ax.set_xlabel(x_label, fontsize=12, color=x_color)

                y_color = 'black'
                y_label = 'g(r)'

                ax.set_ylabel(y_label, fontsize=12, color=y_color)

                oldfile = file
                file = file.replace("./","")
                file = file.replace(".txt","")
                file = file.split("_")
                epsilon_ratio = file[1]
                pair_kind = file[2]
                pair_kind = pair_kind.split("Ar1")
                type_criterion = str(len(pair_kind)-1)
                
                if float(epsilon_ratio) < 1.1:
                    if type_criterion == "0":
                        kind = "Ar'-Ar'"
                    elif type_criterion == "1":
                        kind = "Ar-Ar'"
                    elif type_criterion == "2":
                        kind = "Ar-Ar"
                else: 
                    if "_Ar1-" in oldfile:
                        kind = "Ar-Ar"
                        type_criterion = "2"
                    elif "-Ar1.txt" in oldfile and "_Ar1-" not in oldfile:
                        kind = "Ar-Ar'"
                        type_criterion = "1"
                    else:
                        kind = "Ar'-Ar'"
                        type_criterion = "0"

                ax.set_title("Radial Distribution Function for " + kind + " at $\epsilon_{1}/\epsilon_{2}$ = " + epsilon_ratio)

                plt.plot(x, y, linestyle='-', linewidth=1.5, marker='o', ms=0, mew=1.5, color='orangered')

                plt.plot()

                name = epsilon_ratio + "_" + type_criterion

                placeholder = "rdf_"+name
                placeholder = placeholder.replace(".", "")
                # print(placeholder)

                y_datas = locals()
                y_datas[placeholder] = [x, y]

                # fig.savefig(name+".png",
                #             dpi=900,
                #             format='png',
                #             transparent=False,
                #             bbox_inches='tight')
                
                # print("saving figure for file " + oldfile)
                
                plt.close()

# Plot with panels for miscibility comparison

# fig, axs = plt.subplots(1, 1, figsize=(10,10), constrained_layout=False)

# axs[0].set_title('Ar-Ar')
# axs[0].plot(rdf_01_2[0], rdf_01_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='silver', label='0.1')
# axs[0].plot(rdf_02_2[0], rdf_02_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='rosybrown', label='0.2')
# axs[0].plot(rdf_03_2[0], rdf_03_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='red', label='0.3')
# axs[0].plot(rdf_04_2[0], rdf_04_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orangered', label='0.4')
# axs[0].plot(rdf_05_2[0], rdf_05_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='peru', label='0.5')
# axs[0].plot(rdf_06_2[0], rdf_06_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orange', label='0.6')
# axs[0].plot(rdf_07_2[0], rdf_07_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkkhaki', label='0.7')
# axs[0].plot(rdf_08_2[0], rdf_08_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='gold', label='0.8')
# axs[0].plot(rdf_09_2[0], rdf_09_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellow', label='0.9')
# axs[0].plot(rdf_10_2[0], rdf_10_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='greenyellow', label='1.0')
# axs[0].plot(rdf_11_2[0], rdf_11_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellowgreen', label='1.1')
# axs[0].plot(rdf_12_2[0], rdf_12_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='olivedrab', label='1.2')
# axs[0].plot(rdf_13_2[0], rdf_13_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='lightgreen', label='1.3')
# axs[0].plot(rdf_14_2[0], rdf_14_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='mediumaquamarine', label='1.4')
# axs[0].plot(rdf_15_2[0], rdf_15_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='teal', label='1.5')
# axs[0].plot(rdf_16_2[0], rdf_16_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='cyan', label='1.6')
# axs[0].plot(rdf_17_2[0], rdf_17_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='deepskyblue', label='1.7')
# axs[0].plot(rdf_18_2[0], rdf_18_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='slategrey', label='1.8')
# axs[0].plot(rdf_19_2[0], rdf_19_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='indigo', label='1.9')
# axs[0].plot(rdf_20_2[0], rdf_20_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkmagenta', label='2.0')

# axs[1].set_title('Ar-Uo')
# axs[1].plot(rdf_01_1[0], rdf_01_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='silver', label='0.1')
# axs[1].plot(rdf_02_1[0], rdf_02_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='rosybrown', label='0.2')
# axs[1].plot(rdf_03_1[0], rdf_03_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='red', label='0.3')
# axs[1].plot(rdf_04_1[0], rdf_04_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orangered', label='0.4')
# axs[1].plot(rdf_05_1[0], rdf_05_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='peru', label='0.5')
# axs[1].plot(rdf_06_1[0], rdf_06_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orange', label='0.6')
# axs[1].plot(rdf_07_1[0], rdf_07_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkkhaki', label='0.7')
# axs[1].plot(rdf_08_1[0], rdf_08_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='gold', label='0.8')
# axs[1].plot(rdf_09_1[0], rdf_09_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellow', label='0.9')
# axs[1].plot(rdf_10_1[0], rdf_10_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='greenyellow', label='1.0')
# axs[1].plot(rdf_11_1[0], rdf_11_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellowgreen', label='1.1')
# axs[1].plot(rdf_12_1[0], rdf_12_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='olivedrab', label='1.2')
# axs[1].plot(rdf_13_1[0], rdf_13_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='lightgreen', label='1.3')
# axs[1].plot(rdf_14_1[0], rdf_14_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='mediumaquamarine', label='1.4')
# axs[1].plot(rdf_15_1[0], rdf_15_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='teal', label='1.5')
# axs[1].plot(rdf_16_1[0], rdf_16_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='cyan', label='1.6')
# axs[1].plot(rdf_17_1[0], rdf_17_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='deepskyblue', label='1.7')
# axs[1].plot(rdf_18_1[0], rdf_18_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='slategrey', label='1.8')
# axs[1].plot(rdf_19_1[0], rdf_19_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='indigo', label='1.9')
# axs[1].plot(rdf_20_1[0], rdf_20_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkmagenta', label='2.0')

# axs[2].set_title('Uo-Uo')
# axs[2].plot(rdf_01_0[0], rdf_01_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='silver', label='0.1')
# axs[2].plot(rdf_02_0[0], rdf_02_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='rosybrown', label='0.2')
# axs[2].plot(rdf_03_0[0], rdf_03_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='red', label='0.3')
# axs[2].plot(rdf_04_0[0], rdf_04_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orangered', label='0.4')
# axs[2].plot(rdf_05_0[0], rdf_05_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='peru', label='0.5')
# axs[2].plot(rdf_06_0[0], rdf_06_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orange', label='0.6')
# axs[2].plot(rdf_07_0[0], rdf_07_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkkhaki', label='0.7')
# axs[2].plot(rdf_08_0[0], rdf_08_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='gold', label='0.8')
# axs[2].plot(rdf_09_0[0], rdf_09_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellow', label='0.9')
# axs[2].plot(rdf_10_0[0], rdf_10_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='greenyellow', label='1.0')
# axs[2].plot(rdf_11_0[0], rdf_11_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellowgreen', label='1.1')
# axs[2].plot(rdf_12_0[0], rdf_12_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='olivedrab', label='1.2')
# axs[2].plot(rdf_13_0[0], rdf_13_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='lightgreen', label='1.3')
# axs[2].plot(rdf_14_0[0], rdf_14_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='mediumaquamarine', label='1.4')
# axs[2].plot(rdf_15_0[0], rdf_15_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='teal', label='1.5')
# axs[2].plot(rdf_16_0[0], rdf_16_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='cyan', label='1.6')
# axs[2].plot(rdf_17_0[0], rdf_17_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='deepskyblue', label='1.7')
# axs[2].plot(rdf_18_0[0], rdf_18_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='slategrey', label='1.8')
# axs[2].plot(rdf_19_0[0], rdf_19_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='indigo', label='1.9')
# axs[2].plot(rdf_20_0[0], rdf_20_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkmagenta', label='2.0')

# for ax in axs.flat:
#     ax.set(xlabel='r (Å)', ylabel='g(r)')

# plt.tight_layout()
# plt.legend(title='Miscibility', loc="lower center", ncol=2)
# plt.plot()

# fig.savefig("miscibility.png",
#             dpi=2000,
#             format='png',
#             transparent=False,
#             bbox_inches='tight')

# plt.close()

# Second attempt this time without panels because it doesn't look good

fig = plt.figure()
ax = fig.add_subplot()

# for Ar-Ar
# ax.set_title('Ar-Ar')

# ax.plot(rdf_01_2[0], rdf_01_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='silver', label='0.1')
# ax.plot(rdf_02_2[0], rdf_02_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='rosybrown', label='0.2')
# ax.plot(rdf_03_2[0], rdf_03_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='red', label='0.3')
# ax.plot(rdf_04_2[0], rdf_04_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orangered', label='0.4')
# ax.plot(rdf_05_2[0], rdf_05_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='peru', label='0.5')
# ax.plot(rdf_06_2[0], rdf_06_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orange', label='0.6')
# ax.plot(rdf_07_2[0], rdf_07_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkkhaki', label='0.7')
# ax.plot(rdf_08_2[0], rdf_08_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='gold', label='0.8')
# ax.plot(rdf_09_2[0], rdf_09_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellow', label='0.9')
# ax.plot(rdf_10_2[0], rdf_10_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='greenyellow', label='1.0')
# ax.plot(rdf_11_2[0], rdf_11_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellowgreen', label='1.1')
# ax.plot(rdf_12_2[0], rdf_12_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='olivedrab', label='1.2')
# ax.plot(rdf_13_2[0], rdf_13_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='lightgreen', label='1.3')
# ax.plot(rdf_14_2[0], rdf_14_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='mediumaquamarine', label='1.4')
# ax.plot(rdf_15_2[0], rdf_15_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='teal', label='1.5')
# ax.plot(rdf_16_2[0], rdf_16_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='cyan', label='1.6')
# ax.plot(rdf_17_2[0], rdf_17_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='deepskyblue', label='1.7')
# ax.plot(rdf_18_2[0], rdf_18_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='slategrey', label='1.8')
# ax.plot(rdf_19_2[0], rdf_19_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='indigo', label='1.9')
# ax.plot(rdf_20_2[0], rdf_20_2[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkmagenta', label='2.0')

# for Ar-Ar'
# ax.set_title("Ar-Ar'")

# ax.plot(rdf_01_1[0], rdf_01_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='silver', label='0.1')
# ax.plot(rdf_02_1[0], rdf_02_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='rosybrown', label='0.2')
# ax.plot(rdf_03_1[0], rdf_03_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='red', label='0.3')
# ax.plot(rdf_04_1[0], rdf_04_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orangered', label='0.4')
# ax.plot(rdf_05_1[0], rdf_05_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='peru', label='0.5')
# ax.plot(rdf_06_1[0], rdf_06_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orange', label='0.6')
# ax.plot(rdf_07_1[0], rdf_07_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkkhaki', label='0.7')
# ax.plot(rdf_08_1[0], rdf_08_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='gold', label='0.8')
# ax.plot(rdf_09_1[0], rdf_09_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellow', label='0.9')
# ax.plot(rdf_10_1[0], rdf_10_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='greenyellow', label='1.0')
# ax.plot(rdf_11_1[0], rdf_11_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellowgreen', label='1.1')
# ax.plot(rdf_12_1[0], rdf_12_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='olivedrab', label='1.2')
# ax.plot(rdf_13_1[0], rdf_13_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='lightgreen', label='1.3')
# ax.plot(rdf_14_1[0], rdf_14_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='mediumaquamarine', label='1.4')
# ax.plot(rdf_15_1[0], rdf_15_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='teal', label='1.5')
# ax.plot(rdf_16_1[0], rdf_16_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='cyan', label='1.6')
# ax.plot(rdf_17_1[0], rdf_17_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='deepskyblue', label='1.7')
# ax.plot(rdf_18_1[0], rdf_18_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='slategrey', label='1.8')
# ax.plot(rdf_19_1[0], rdf_19_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='indigo', label='1.9')
# ax.plot(rdf_20_1[0], rdf_20_1[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkmagenta', label='2.0')

# for Ar'-Ar'
# ax.set_title("Ar'-Ar'")

# ax.plot(rdf_01_0[0], rdf_01_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='silver', label='0.1')
# ax.plot(rdf_02_0[0], rdf_02_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='rosybrown', label='0.2')
# ax.plot(rdf_03_0[0], rdf_03_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='red', label='0.3')
# ax.plot(rdf_04_0[0], rdf_04_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orangered', label='0.4')
# ax.plot(rdf_05_0[0], rdf_05_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='peru', label='0.5')
# ax.plot(rdf_06_0[0], rdf_06_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orange', label='0.6')
# ax.plot(rdf_07_0[0], rdf_07_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkkhaki', label='0.7')
# ax.plot(rdf_08_0[0], rdf_08_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='gold', label='0.8')
# ax.plot(rdf_09_0[0], rdf_09_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellow', label='0.9')
# ax.plot(rdf_10_0[0], rdf_10_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='greenyellow', label='1.0')
# ax.plot(rdf_11_0[0], rdf_11_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellowgreen', label='1.1')
# ax.plot(rdf_12_0[0], rdf_12_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='olivedrab', label='1.2')
# ax.plot(rdf_13_0[0], rdf_13_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='lightgreen', label='1.3')
# ax.plot(rdf_14_0[0], rdf_14_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='mediumaquamarine', label='1.4')
# ax.plot(rdf_15_0[0], rdf_15_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='teal', label='1.5')
# ax.plot(rdf_16_0[0], rdf_16_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='cyan', label='1.6')
# ax.plot(rdf_17_0[0], rdf_17_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='deepskyblue', label='1.7')
# ax.plot(rdf_18_0[0], rdf_18_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='slategrey', label='1.8')
# ax.plot(rdf_19_0[0], rdf_19_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='indigo', label='1.9')
# ax.plot(rdf_20_0[0], rdf_20_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkmagenta', label='2.0')

# fig.subplots_adjust(right=1.5)
# # plt.legend(title='Miscibility', loc="lower right", ncol=1)
# plt.plot()

# for plotting just the legend

ax.plot(rdf_01_0[0], rdf_01_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='silver', label='0.1')
ax.plot(rdf_02_0[0], rdf_02_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='rosybrown', label='0.2')
ax.plot(rdf_03_0[0], rdf_03_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='red', label='0.3')
ax.plot(rdf_04_0[0], rdf_04_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orangered', label='0.4')
ax.plot(rdf_05_0[0], rdf_05_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='peru', label='0.5')
ax.plot(rdf_06_0[0], rdf_06_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='orange', label='0.6')
ax.plot(rdf_07_0[0], rdf_07_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkkhaki', label='0.7')
ax.plot(rdf_08_0[0], rdf_08_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='gold', label='0.8')
ax.plot(rdf_09_0[0], rdf_09_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellow', label='0.9')
ax.plot(rdf_10_0[0], rdf_10_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='greenyellow', label='1.0')
ax.plot(rdf_11_0[0], rdf_11_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='yellowgreen', label='1.1')
ax.plot(rdf_12_0[0], rdf_12_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='olivedrab', label='1.2')
ax.plot(rdf_13_0[0], rdf_13_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='lightgreen', label='1.3')
ax.plot(rdf_14_0[0], rdf_14_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='mediumaquamarine', label='1.4')
ax.plot(rdf_15_0[0], rdf_15_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='teal', label='1.5')
ax.plot(rdf_16_0[0], rdf_16_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='cyan', label='1.6')
ax.plot(rdf_17_0[0], rdf_17_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='deepskyblue', label='1.7')
ax.plot(rdf_18_0[0], rdf_18_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='slategrey', label='1.8')
ax.plot(rdf_19_0[0], rdf_19_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='indigo', label='1.9')
ax.plot(rdf_20_0[0], rdf_20_0[1], linestyle='-', markersize=0.0, linewidth=1.0, color='darkmagenta', label='2.0')

fig.subplots_adjust(right=4.0)
plt.legend(title='Miscibility', loc="lower left", ncol=1)
plt.plot()

fig.savefig("miscibility_legend.png",
            dpi=2000,
            format='png',
            transparent=False,
            bbox_inches='tight')

plt.close()