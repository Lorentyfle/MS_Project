import os

directory = './'

how_many = 300

for root, dirs, files in os.walk(directory):
    
    for filename in files:
        
        file = os.path.join(root, filename)
        
        if "rdf" in file and ".dat" in file and ".lnk" not in file: 

            offset = 1
            number = 200
                
            print("reading data from", file)
            
            with open(file) as f:
                
                data = []
                lines = [line for line in f]

                for index, line in enumerate(lines):
                    
                    if "New_Simulation" in line and index > (len(lines) - (number+offset+1)*how_many):
                        
                        i = 1 + offset
                        while i < number + offset + 1:
                            
                            x = lines[index+i].split()

                            # print(x)

                            r = float(x[0])
                            g = float(x[1])
                            entry = [r,g]
                            data.append(entry)

                            i += 1
                
                # print(data)

                averaged_data = []

                for i in range(number-1):
                    dataslice = data[i::number]

                    # print(dataslice)

                    coordinate = []
                    distribution = []

                    for j in range(len(dataslice)):

                        coordinate.append(dataslice[j][0])
                        distribution.append(dataslice[j][1])

                    averaged_dataslice = [coordinate[0], sum(distribution)/len(distribution)]

                    # print(averaged_dataslice)

                    averaged_data.append(averaged_dataslice)

                temp = file.split('\\')
                # print(temp)
                stem = temp[0].replace('./', '')
                stem = stem.split('_')
                typ = stem[-1]
                # print(typ)
                placeholder = temp[-1]
                pair = placeholder.replace(" ",'')
                pair = pair.replace("rdf","")
                pair = pair.replace(".dat",'')
                # print(pair)
                name = "rdf_" + typ + "_" + pair + ".txt"
                print(name)
                
                # data = list(dict.fromkeys(data))
                
                with open(name, 'w') as d:
                    
                    print("generating extracted data file for", file)
                    for line in averaged_data:
                        
                        d.write(f"{line}\n")
                        