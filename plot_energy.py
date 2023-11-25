from numpy import array, pi, cos, append, log, exp, linspace,sqrt, mean
import matplotlib.pylab as plt
from scipy.optimize import curve_fit


#def F(x,a,b,c,d):
#    return a*cos(b*x + c)*exp(-d*x)

f = open("./Input_output/out_energy.txt","r")
f.readline()
t             = array([])
dt            = array([])
for ligne in f:
    words           = ligne.replace(",",".").split("  ")
    t               = append(t,float(words[0])) # in s
    dt              = append(dt,float(words[1])) # in s
    if (len(t) == 200000):
        break


x = t
y = dt
print(len(x))
print(len(y))
#p0 = [1.5,-7.62,1.5*10**(-3),10.330100000]
#p0 = [20,1,1,1]
#popt,pcov = curve_fit(F,x,y,p0) # curve_fit() can be used for non linear functions

#[ A, B, C, D]=popt
#sA = sqrt(pcov[0,0])
#sB = sqrt(pcov[1,1])
#sC = sqrt(pcov[2,2])
#sD = sqrt(pcov[3,3])

plt.figure(0)

#x_model = linspace(min(x),max(x),256)
#F_model = F(x_model,A,B,C,D)
#plt.plot(x_model,F_model,linestyle='-',color='orange', label='')
#plt.errorbar(x,y,fmt="ob",label=u"Experimental points")
#plt.plot(x,A*cos(B*x + C)*exp(-D*x),"b:",label=u"Fit: $U=A*cos( B \\times t + C )\\times \exp(-D \\times t)$\nwith $A=%.3f$, $B=%.3f$,$C=%.3f$ and $D=%.3f$"%(A,B,C,D))
plt.scatter(x,y)
plt.ylabel("Energy (kJ/mol)")
plt.xlabel("cycle")
plt.title("Energy in function of the cycle")
#plt.legend(loc=4,fontsize=8)
plt.show()
#print("σA =",sA,"| σB =",sB,"| σC =",sC,"| σD =",sD)

#chi2 = sum( (y - F(x,A,B,C,D))**2 )
#Sr = chi2
#St = sum( (y - mean(y))**2 )
#r  = sqrt( (St - Sr) / St)

#print("r = %.4f" %(r))
#with open('Output/output.txt', 'a') as f:
#    f.write("r = ")
#    f.write(str(r))
#    f.write(" | ")
#    f.write(str(p0))
#    f.write('\n')
#print()
#print("chi^2 =", chi2)
#print('A = %.2f +/- %.2f'%(A,sA))
#print('B = %.3e +/- %.3e'%(B,sB))
#print('C = %.3e +/- %.3e'%(C,sC))
#print('D = %.3e +/- %.3e'%(D,sD))