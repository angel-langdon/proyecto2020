# -*- coding: utf-8 -*-
import requests
import pandas as pd
import os, json
import pytz
from datetime import datetime
from pyproj import Proj, transform
from pyproj import Transformer
import numpy as np
import matplotlib.pyplot as plt
import math
import ast

def coordsColumnsTransformation(puntos):
    X, Y = [], []
    proj = Transformer.from_crs(3857, 4326, always_xy=True)
    for lista in puntos:
        x1, y1 = lista[0], lista[1]
        x2, y2 = proj.transform(x1, y1)
        x2, y2 = y2, x2
        X.append(x2)
        Y.append(y2)
    return X, Y


def estacionToCsv(dir = '/content/drive/My Drive/Proyecto 2 SENSORES/Ficheros de datos/estaciones/'): # para los que ya tenemos descargados
    fileNames = os.listdir(dir)
    itera = 1
    for fileJson in fileNames: #para cada fichero
        frame=[]
        geom=[]
        try: # para evitar los archivos ocultos 
            with open (dir+fileJson,mode='r',encoding='UTF-8') as f:
                data = json.load(f) # lo procesamos
            for i in range(0, len(data['features'])): #recorremos la rama de features que tiene dos ramas : attributes y geometry(coordenadas a recodificar)
                if i == 0: #las keys se guardan como las primeras filas
                    frame.append(data['features'][i]['attributes'].keys())
                    geom.append(data['features'][i]['geometry'].keys())
                frame.append(data['features'][i]['attributes'].values()) # las siguientes filas de ambas ramas se van escribiendo
                geom.append(data['features'][i]['geometry'].values())
            prueba = pd.DataFrame(frame) # lo pasamos a dataFrame
            geome = pd.DataFrame(geom)
            prueba.columns = prueba.iloc[0] #las primeras filas determinan los nombres de las columnas
            geome.columns = geome.iloc[0]
            prueba = prueba.drop(['parametros','mediciones','gid'],axis=1) #quitamos las columnas que no nos interesan
            prueba = prueba.iloc[1:] #quitamos las primeras filas (ya estan como nombre de las columnas)
            geome = geome.iloc[1:]
            final=prueba.join(geome) #juntamos ambos dataframes en uno

            #ponemos TIMESTAMP como nombre, antes estaban nombrados con la fecha de la descarga desfasada
            valor = final['fecha_carga'][1] # esto es el timestamp referente
            valor = valor/1000
            valor += 3600                
            buen_valor = datetime.fromtimestamp(valor)
            current_time = buen_valor.strftime("%m-%d-%Y_%H-%M-%S")
            current_time = str(current_time)
            nombre='estacion_' + current_time + '.csv' 
            #transformar coordenadas
            lis = []
            it = 0
            for i, j in zip(final['x'], final['y']):
                lis.append('')
                lis[it] = i, j
                it += 1
            final['X'], final['Y'] = coordsColumnsTransformation(lis) #creamos las columnas de latitud y longitud por separado ya TRANSFORMADAS
            final = final.drop(['x', 'y'], axis = 1) # quitamos las columnas antiguas de coordenadas malas 
            final.to_csv('/content/drive/My Drive/Proyecto 2 SENSORES/Ficheros de datos/estacionesCSV/'+ nombre, encoding="UTF-8",sep = ";",index=False)
            print(itera)
            itera += 1

        except UnicodeDecodeError:
            pass

