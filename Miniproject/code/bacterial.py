### importing functions
import pandas as pd
import scipy as sc
import matplotlib.pylab as pl
import seaborn as sns # You might need to install this (e.g., pip install seaborn)

### read and load data
data = pd.read_csv("../data/logistic_growth_data.csv")
print("Loaded {} columns.".format(len(data.columns.values)))
print(data.columns.values)
pd.read_csv("../data/logistic_growth_meta_data.csv")
data.head()
print(data.PopBio_units.unique()) #units of the response variable 
print(data.Time_units.unique()) #units of the independent variable 
data.insert(0, "ID", data.Species + "_" + data.Temp.map(str) + "_" + data.Medium + "_" + data.Citation)
print(data.ID.unique()) #units of the independent variable 
data_subset = data[data['ID']=='Chryseobacterium.balustinum_5_TSB_Bae, Y.M., Zheng, L., Hyun, J.E., Jung, K.S., Heu, S. and Lee, S.Y., 2014. Growth characteristics and biofilm formation of various spoilage bacteria isolated from fresh produce. Journal of food science, 79(10), pp.M2072-M2080.']
data_subset.head()

### plotting
sns.lmplot(x= "Time", y = "PopBio", data = data_subset, fit_reg = False) # will give warning - you can ignore it
pl.show()