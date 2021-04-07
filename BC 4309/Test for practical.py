import matplotlib.pyplot as plt
import pandas as pd

dataset= pd.read_csv("listings (1).csv")

df = dataset[(dataset['price'] >= 0) & (dataset['price'] <= 500)]
lat = df['latitude']
long = df['longitude']
# price = dataset[(dataset['price'] >= 99) & (dataset['price'] <= 200)]
price = df['price']
fig,ax = plt.subplots()
numbers = 10000
bins= 100
points = ax.scatter(lat,long,c=price,cmap = "Reds")
plt.colorbar(points)
