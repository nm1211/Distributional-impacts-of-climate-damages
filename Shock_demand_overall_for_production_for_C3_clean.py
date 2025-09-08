# -*- coding: utf-8 -*-
"""
Created on Wed Mar 19 13:06:44 2025
"""
#Code for approximating the overall production impacts and global share shifts for C3 in the Appendx
# this code considers all adjustments by households after the elasticites were applied and only of the countries considered in the study. It is a feeding back to the GTAP data. 
#It is assumed that governments and industries react as the households in the respective countries do

import numpy as np
from copy import deepcopy
import scipy.linalg

##########################Pfad der Daten
pathy=" path  Y file GTAP 10 2014"
pathz=" path Z file GTAP 10 2014"

path_reg="path region name file for GTAP"
path_sec="path sectoral name file for GTAp"

regio=np.loadtxt(path_reg,delimiter=";",dtype=str)
sectors=np.loadtxt(path_sec,delimiter=";",dtype=str)

path_schablone='file to map the considered 86 regions to GTAP regions, second dimentsion refers to countries, not all countres of GTAP are present in our assessment'
path_impact_data=' file with adjustment in 86 regions considered with the sectors considered (10) of households after elasticities -matched back to GTAP sectors these are in 0.01 for 1% changes or -0.02 for 2% decreases'


schablone=np.loadtxt(path_schablone,delimiter=";",dtype=str)
impact_signal=np.loadtxt(path_impact_data,delimiter=";",dtype=str)
imp_si=impact_signal.reshape(86,10,3)


########################################################

n=141
m=65
####array to link back the sectors to the correspondin GTAP sectors -- use data from econometric analysis
items_cons=[25,18,21,9,24,19,0,23,3,20]
items_cons=np.array(items_cons)
##############################################
#load data
z=np.load(pathz)
y=np.load(pathy)
# y2 is needed to cover canges in final demand -- this container allows to simulate changes which are therafter used to adjust the system
y2=deepcopy(y)

### create container to derive changes in levels 
final_dc_HH=np.ones((n,m))
for k in range(82):
    land=imp_si[k,0,1]
#    print(imp_si[k,0,1])
    for l in range(141):
        #after identifying the country, mapping it to the right GTAP country, which is given in schablone
        if schablone[l,1]==land:
#            print(l, land, regions[l])
            help=0
            ####### help is there to take all floats of imp_si, as long as the items from items_cons are not exhausted
            for s in items_cons:
#                print(s)
                final_dc_HH[l,s]+=np.float(imp_si[k,help,2])
#                print(final_dc_HH[l,s])
#                print(k,s,l)
                help+=1
            break

###########adjust final demand 
for i in range(n):
    for j in range(m):
       y2[:,j,i]*=final_dc_HH[i,j]
       

################################# THe shock in demand is applied to to all final demand in the country, as for sure also governments and industries would adjust the FD in a similar way
            ## and experience price increases... this is to anticipate the approximate lever

  
###############prepare output calcuation
out=z.sum(axis=3).sum(axis=2)+y.sum(axis=2)
#
#
###############and input for VAD
inp=z.sum(axis=1).sum(axis=0)
VAD=out-inp
VAD2=deepcopy(VAD)
#vad 2 will be modified to account for changes in VAD
#

############prepare technology matrix
A=np.copy(z)

for i in range(n):
    for j in range(m):
                A[:,:,i,j]=A[:,:,i,j]/out[i,j]

#
###################prepare Leontief inverse
A=A.reshape(n*m,n*m)
I=np.eye((n*m))
I=I-A
L=scipy.linalg.inv(I)
L2=deepcopy(L)
print(L.shape)
#
#
#
#aggregate y2
y2_reduced=y2.sum(axis=2)
L2=L2.reshape((n,m,n,m))
################## also consider all upstream supply chain effects via IO analysis properties, here also other sectors than the industry sectors can be impacted
for i in range(n):
    for j in range(m):
        L2[:,:,i,j]*=y2_reduced[i,j]
########this is the impact on output
output_per_sector_after=L2.sum(axis=3).sum(axis=2)

## these changes currently only consder changes because of changes in final demand, not any changes in prices.
output_after_w_o_pricein=deepcopy(output_per_sector_after)
path_shock="additionally consider the original price signal to also account for compensation through price increases"
shock=np.loadtxt(path_shock,delimiter=";",dtype=float)
print(np.shape(shock))

###the logic here is to get the relative changes in outpur per sector considering both effects. Therefore the new output per sector
## needs to be compared to the old output (original) and be treated with the price increases for the sectors that trigger the changes... the resulting factor can then be used to anticipate changes in VAD.
for i in range(n):
    for j in range(m):
        output_per_sector_after[i,j]*=shock[i,j]/out[i,j]
        VAD2[i,j]*=output_per_sector_after[i,j]
### consider the initial shock that is handed through and put relations into relation with previous
np.savetxt("relative_changes_per_sector.csv",output_per_sector_after,delimiter=";")
VAD_change_tot=VAD2-VAD
print(VAD.sum(),VAD2.sum(),VAD_change_tot.sum())
np.savetxt("total_value_capture_per_sector_change.csv",VAD_change_tot,delimiter=";")