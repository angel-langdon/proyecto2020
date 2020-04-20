# -*- coding: utf-8 -*-
import requests
import pytz
import json
import os
from datetime import datetime,timezone, timedelta
import time
import pandas as pd
from pyproj import Proj, transform, Transformer
import ast
import numpy as np
import shutil
import gc
import sys

 
def coordsColumnsTransformation(puntos):
    transformer = Transformer.from_crs('epsg:25830','epsg:4326',always_xy=True)
    lista_final=[]
    for lista in puntos:
        lista =  ast.literal_eval(lista)
        lista = [tuple(l) for l in lista]
        lista1 = list(transformer.itransform(lista))
        lista1 = [t[::-1] for t in lista1]
        lista_final.append(lista1)
    coords = np.array(lista_final)
    return coords


def downloadJSON():
    url = "http://apigobiernoabiertortod.valencia.es/apirtod/datos/intensidad_tramos.json?items=1000"
    response = requests.get(url)
    if response.status_code == 200:
        dic = json.loads(response.content)
        tz = pytz.timezone('Europe/Madrid')
        time = datetime.now(tz)
        current_time = time.strftime("%m-%d-%Y_%H-%M-%S")
        current_time = str(current_time)
        filename = '/root/proyecto2020/demonioEspiras/tramos/tramos_'+current_time+'.csv'
        tramos_totales = []
       
        espiras = dic['resources']
        keys = espiras[0].keys()
        data = pd.DataFrame.from_dict(espiras)
        data.drop(['modified','idtramo'],axis=1)
        data['coordinates'] = coordsColumnsTransformation(data['coordinates'])                  #Transformamos las coordenadas a formato latitud, longitud
        #data.to_csv(csvAllDicts, sep=',', encoding='utf-8',index=False)
        data = data.drop(columns='uri')
        data = data[["des_tramo","lectura","coordinates"]]

        data =  pd.DataFrame([(row.des_tramo, row.lectura,sample[0],sample[1]) for row in data.itertuples() for sample in row.coordinates], columns=["des_tramo","lectura","latitud","longitud"])
        with open(filename,"w") as f:
            data.to_csv(f,encoding="UTF-8",sep = ";",index=False)


 
if __name__ == "__main__":
    time.sleep(30)
    downloadJSON()
    tramosPath = "/root/proyecto2020/demonioEspiras/tramos/"
    historicoPath = "/root/proyecto2020/demonioEspiras/historico/"
    ficheros = os.listdir(tramosPath)
    ficheros = sorted(ficheros)
    if len(ficheros) > 96:
        ficheros_mover = ficheros[:-96]
        for fichero in ficheros_mover:
            shutil.move(tramosPath+fichero, historicoPath+fichero)
    gc.collect()

