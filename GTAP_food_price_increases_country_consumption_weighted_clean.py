
"""
"""
###########Code to diffuse the price shocks throught the global supply chain system considering consumption relations //shares
import numpy as np
import scipy.linalg

##########################Loading GTAP MRIO data
pathy="directory for the consumption file y of GTAP 10_2014"
pathz="directory for the inter industry file z of GTAP 10_2014"
path_reg=" path for the name file of regions in our case region.csv"
## the file below considers changes not in %, but as a multiplicative factor, i.e. 1.041 for a 4.1% price increase
path_shocks="path for the initial 'shock' file, where each sector category see the initial price increases as in Calzadilla et al in our case Gtap_10_schablone_world_market_prices.csv"

regions=np.loadtxt(path_reg,delimiter=";",dtype=str)
regio=regions
########################################################
########################################################
#############loading data and defining sectoral and regional dimensionalities
n=141
m=65

#############################################
z=np.load(pathz)
y=np.load(pathy)


###############load price increases and transfer it to relative absolute changes
shock=np.loadtxt(path_shocks,delimiter=";")
#routines to test the shape and overall order of magnituted
#print(shock.shape, shock.sum())
#raise SystemExit
shock=shock-np.ones((n,m))
print(shock.shape, shock.sum())
print(y.shape)
print(z.shape)
##############calculating overal sectoral output to derive A and in theend L
out=z.sum(axis=3).sum(axis=2)+y.sum(axis=2)

############prepare A
A=np.copy(z)

for i in range(n):
    for j in range(m):
                A[:,:,i,j]=A[:,:,i,j]/out[i,j]


###################calculate Leontief-Inverse
A=A.reshape(n*m,n*m)
I=np.eye((n*m))
I=I-A
L=scipy.linalg.inv(I)

##############provide container for systemprice increases
system_price_increase=np.ones((n,m))
## I am working with a copy of L and go through all items that are consumed and have relevant price increases, their origin and the respective price increase
L2=np.copy(L)
L2=L2.reshape((n,m,n,m))
for i2 in range(n):
    for j2 in range(m):
        if shock[i2,j2]>0:
            for i in range(n):
                for j in range(m):
                    system_price_increase[i,j]+=shock[i2,j2]*L2[i2,j2,i,j]
#this could be saved as the price increases for each product for final consumption, but the consumption patterns should be considered as well, i.e. how much of a final good consumed comes from which countries (as origin)
#np.savetxt("increases_in_prices_through_system.csv",system_price_increase,delimiter=";")

#providing an empty container for each country and consumption item -- we use the relative weight of oringin to calculate weighted price increases, so for each good that is consumed in a country, we look at all suppliers and weigth the price increase according to current consumption shares
P_increase_cons_mix_land_av=np.zeros((n,m))
for i2 in range(n):
    for j in range(m):
        for i in range(n):
            P_increase_cons_mix_land_av[i2,j]+=(y[i,j,i2]*system_price_increase[i,j])/y[:,j,i2].sum()
print(P_increase_cons_mix_land_av.shape)
P_increase_cons_mix_land_av=np.array(P_increase_cons_mix_land_av)
np.savetxt("considered_consumptionpatterns_increases_in_prices_through_system_world_market.csv",P_increase_cons_mix_land_av,delimiter=";")

