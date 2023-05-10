import os
import re
import numpy as np
import pandas as pd
import geopandas as gpd
import numpy as np
import itertools

from scipy import linalg
import sklearn
from sklearn.datasets import make_blobs, make_circles, make_moons

import plotly_express as px

import matplotlib as mpl
from matplotlib import pyplot as plt

from sklearn import metrics
from sklearn.cluster import KMeans
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler

from numpy import random
import seaborn as sb
import matplotlib.pyplot as plt 


import altair as alt
from sklearn.cluster import KMeans
np.random.seed(1)

plt.rcParams['figure.figsize'] = (10,6)

#data path
test_data = "/rds/general/project/hda-22-23/live/TDS/Group6/Data/"
train_data = "/rds/general/project/hda-22-23/live/TDS/Group6/Data/"

## Load data
testo = pd.read_csv(test_data,header=0,index_col=0)
traino = pd.read_csv(train_data,header=0,index_col=None)

#scale the relevant columns
cols_to_scale = ['WBC_count','red_blood_cell_count',
                 'haemoglobin_conc',
                 'platelet_count',
                 'lymphocyte_count',	
                 'monocyte_count',	
                 'neutrophil_count',	
                 'eosinophil_count',	
                 'basophil_count',	
                 'nucleated_rbc_count',	
                 'reticulocyte_count',	
                 'mean_cell_volume']

#scale the columns
testo[cols_to_scale] = StandardScaler().fit_transform(testo[cols_to_scale])
traino[cols_to_scale] = StandardScaler().fit_transform(traino[cols_to_scale])

# Create an elbow plot to determine the optimal number of clusters
inertias = []
for n_clusters in range(2, 11):
    kmeans = KMeans(n_clusters=n_clusters)
    kmeans.fit(traino[cols_to_scale])
    inertias.append(kmeans.inertia_)
    
# Plot the elbow plot
plt.plot(range(2, 11), inertias, marker='o')
plt.title('Elbow plot')
plt.xlabel('Number of clusters')
plt.ylabel('Inertia')
plt.savefig('elbow_plot.png')

#k-means cluster the things
n_clusters = 5 #pick what you like from silhouette

#make the models
kmeans = KMeans(n_clusters)
testok = kmeans.fit(testo[cols_to_scale])
trainok = kmeans.fit(traino[cols_to_scale])

#assign cluster membership to our data
testo_kmeans_cluster = testok.predict(testo[cols_to_scale])
traino_kmeans_cluster = trainok.predict(traino[cols_to_scale])

testo['KMeans_cluster'] = testo_kmeans_cluster
traino['KMeans_cluster'] = traino_kmeans_cluster

