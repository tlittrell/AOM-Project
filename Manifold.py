from sklearn import manifold
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D, axes3d
from matplotlib import cm

X = np.genfromtxt('TimData.csv', delimiter=",")
n_neighbors = 5
n_components = 3
X = X[1:,1:]
rows, cols = np.shape(X)
y = X[:,cols-1]
X = X[:,0:(cols-1)]
print("check 1")
# Change this like for different methods
Y = manifold.TSNE(n_components=n_components,
                  init='pca', random_state=0).fit_transform(X)
print("check 2")
colors = np.array(['k','g','b','r','c','m'])
fig = plt.figure(1)
ax = fig.gca(projection='3d')
ax.scatter(Y[:,0],Y[:,1],Y[:,2], c = colors[y.astype(int)])

plt.show()

np.savetxt("TSNE.csv", Y, delimiter=",")