# -*- coding: utf-8 -*-
import requests
import pandas as pd
import os, json
import pytz
from datetime import datetime
from pyproj import Proj, transform
from pyproj import Transformer
import shutil
import numpy as np
import matplotlib.pyplot as plt
import math
import ast
import gc

def coordsColumnsTransformation(puntos):#recibe una lista de listas con las corrdenadas Y, X.
    X, Y = [], [] #Creamos 2 listas vacías para ir almacenando las coordenadas
    proj = Transformer.from_crs(3857, 4326, always_xy=True) #proj se va a encargar de transformar las coordenadas a un formato conocido
    for lista in puntos:
      x1, y1 = lista[0], lista[1]
      x2, y2 = proj.transform(x1, y1) #se realiza la conversión
      x2, y2 = y2, x2 #Ahora está en latitud, longitud
      X.append(x2)
      Y.append(y2)
    return X, Y #Se devuelven las listas

def downloadJSON():#Esta función descarga los datos y los guarda en csv.
    url = "https://geoportal.valencia.es/arcgis/rest/services/Opendata/MedioAmbiente/MapServer/136/query?f=json&where=1%3D1&returnGeometry=true&spatialRel=esriSpatialRelIntersects&geometry=%7B%22xmin%22%3A-71853.72886058278%2C%22ymin%22%3A4787025.055646416%2C%22xmax%22%3A1525.8182931781994%2C%22ymax%22%3A4802923.957529731%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D&geometryType=esriGeometryEnvelope&inSR=102100&outFields=nombre%2Cdireccion%2Ctipozona%2Ctipoemision%2Cparametros%2Cmediciones%2Cgid%2Cso2%2Cno2%2Co3%2Cco%2Cpm10%2Cpm25%2Cfecha_carga%2Ccalidad_ambiental&orderByFields=gid%20ASC&outSR=102100"
    response = requests.get(url)
    if response.status_code == 200:
        dic = json.loads(response.content)
        
        frame = [] #Como la estructura del json es de dos diccionarios dentro de un valor de una lista, creamos dos listas que nos ayudarán.
        geom = []
        for i in range(0, len(dic['features'])): #recorremos los valores de la lista
            if i == 0:
                frame.append(dic['features'][i]['attributes'].keys())#Guardamos las cabeceras en la primera fila
                geom.append(dic['features'][i]['geometry'].keys())
            frame.append(dic['features'][i]['attributes'].values()) #vamos guardando los valores
            geom.append(dic['features'][i]['geometry'].values())
        prueba = pd.DataFrame(frame)
        geome = pd.DataFrame(geom)
        prueba.columns = prueba.iloc[0] #se pone la primera fila como cabeceras.
        geome.columns = geome.iloc[0]
        prueba = prueba.iloc[1:] #quitamos la primera fila, donde estaban los nombres
        geome = geome.iloc[1:]
        final = prueba.join(geome) #Se juntan los dos dataframes.
        valor = final['fecha_carga'][1]#es el tiempo que ha pasado desde que han creado UNIX, hay que transformarlo
        valor = valor/1000
        valor += 3600                 #le sumamos una hora en segundos para pasarlo a hora espanyola 
        buen_valor = datetime.fromtimestamp(valor)
        current_time = buen_valor.strftime("%m-%d-%Y_%H-%M-%S")
        current_time = str(current_time)
        final = final.drop(['parametros','mediciones','gid', 'fecha_carga'],axis=1) #quitamos las columnas que no necesitamos
        lis = []
        it = 0
        for i, j in zip(final['x'], final['y']): #codificamos los valores para hacer mejor la funcion coordsColumnsTransformation
          lis.append('')
          lis[it] = i, j
          it += 1
        final['X'], final['Y'] = coordsColumnsTransformation(lis)
        final = final.drop(['x', 'y'], axis = 1)
        filename = '/root/proyecto2020/demonioEstacion/estacion/estacion_'+current_time+'.csv'
        with open(filename,"w") as f:
            final.to_csv(f, encoding="UTF-8",sep = ";",index=False)

        
if __name__ == "__main__":
    downloadJSON()
    estacionesPath= "/root/proyecto2020/demonioEstacion/estacion/"
    historicoPath = "/root/proyecto2020/demonioEstacion/historico/"
    ficheros = os.listdir(estacionesPath)
    ficheros = sorted(ficheros)
    if len(ficheros) > 24:
        ficheros_mover = ficheros[:-24]
        for fichero in ficheros_mover:
            shutil.move(estacionesPath+fichero, historicoPath+fichero)
    gc.collect()
    