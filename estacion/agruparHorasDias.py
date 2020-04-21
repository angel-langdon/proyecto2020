# -*- coding: utf-8 -*-
import os
import time
import pandas as pd
import pytz
from datetime import datetime,timedelta
import gc
import math   

def getYesterday():
    tz = pytz.timezone('Europe/Madrid')
    time = datetime.now(tz)
    time = time - timedelta(days=1)
    current_time = time.strftime("%m-%d-%Y-%H-%M-%S")
    current_time = current_time.split("-")
    return current_time
    
def getToday():
    tz = pytz.timezone('Europe/Madrid')
    time = datetime.now(tz)
    current_time = time.strftime("%m-%d-%Y-%H-%M-%S")
    current_time = current_time.split("-")
    return current_time

def procesarListaTiempos(l):
    l = [int(campo) for campo in l]
    return l[0],l[1],l[2],l[3],l[4],l[5]
def procesarNombreFichero(fichero):
    tiempo = fichero[9:-4]
    tiempo = tiempo.replace("_","-")
    tiempo = tiempo.split("-")
    return tiempo
def compareDates(l1,l2,l3):
    monthT,dayT,yearT,hourT,minuteT,secondsT = procesarListaTiempos(l1)
    monthY,dayY,yearY,hourY,minuteY,secondsY = procesarListaTiempos(l2)
    monthF,dayF,yearF,hourF,minuteF,secondsF = procesarListaTiempos(l3)
    if dayT == dayF and monthT==monthF and yearT==yearF and hourF == 0:
        return True
    elif dayY == dayF and monthY==monthF and yearY==yearF and hourF > 0:
        return True
    #else
    return False


def getFileName():
    tz = pytz.timezone('Europe/Madrid')
    time = datetime.now(tz)
    time = time - timedelta(days=1)
    current_time = time.strftime("%Y-%m-%d")
    current_time = str(current_time)
    return '/root/proyecto2020/demonioEstacion/estacionesPorDia/estacion_'+current_time+'.csv'

def unificarFicheros(fichero_final,dataframe):
    keeps = ["so2","no2","o3","co","pm10","pm25"]
    for variable in keeps:
        lectura_actual = list(dataframe[variable])
        lectura_fichero_final = list(fichero_final[variable])
        for j,lectura in enumerate(lectura_actual):
            lectura_fichero_final[j] = (lectura_fichero_final[j]+" "+lectura_actual[j])
        fichero_final[variable] = lectura_fichero_final
    
    lectura_actual = list(dataframe["calidad_ambiental"])
    lectura_fichero_final = list(fichero_final["calidad_ambiental"])
    for j,lectura in enumerate(lectura_actual):
        if len(lectura)>0:
            if not len(lectura_fichero_final[j])>0:
                lectura_fichero_final[j] = (lectura+" "+ lectura_fichero_final[j])
            else:
                lectura_fichero_final[j] = lectura

    fichero_final["calidad_ambiental"] = lectura_fichero_final
    return fichero_final
    
def getMedias(fichero_final):
    keeps = ["so2","no2","o3","co","pm10","pm25"]
    for variable in keeps:
        lectura_fichero_final = list(fichero_final[variable])
        lectura_fichero_final = [str(valor) for valor in lectura_fichero_final]
        for j,valores in enumerate(lectura_fichero_final):
            valores = valores.split()
            if "nan" in valores:
                while "nan" in valores:
                    valores.remove("nan")
            valores = [float(valor) for valor in valores]

            total = sum(valores)
            cantidad = len(valores)
            if cantidad == 0:
                lectura_fichero_final[j] = None
            else:
                lectura_fichero_final[j] = total/cantidad
        fichero_final[variable] = lectura_fichero_final
    
    lectura_fichero_final = list(fichero_final["calidad_ambiental"])
    for j,lectura in enumerate(lectura_fichero_final):
        if len(lectura)>0:
            lecturas = lectura.split()
            most_repeated = max(set(lecturas), key = lecturas.count)
            lectura_fichero_final[j] = most_repeated
    fichero_final["calidad_ambiental"] = lectura_fichero_final
    return fichero_final
        
        
    

def integrationToDay():
    fileName = getFileName()
    tramosPath = "/root/proyecto2020/demonioEstacion/estacion/"
    ficheros = os.listdir(tramosPath)
    tiempo_today = getToday()
    tiempo_yesterday = getYesterday()
    ficheros = sorted(ficheros)
    ficheros.reverse()
    un_fichero = False
    primer_fichero =False
    if len(ficheros)!= 0 :
        for i,fichero in enumerate(ficheros):
            tiempo_fichero = procesarNombreFichero(fichero)
            if compareDates(tiempo_today,tiempo_yesterday,tiempo_fichero):
                if not primer_fichero:
                    primer_fichero =True
                un_fichero = True
                fichero = tramosPath+fichero
                dataframe = pd.read_csv(fichero,sep=";",encoding="UTF-8")
                dataframe = dataframe.sort_values(by=["nombre",'direccion'])
                #dataframe['lectura'] = ["" if x > 6250 else x for x in dataframe['lectura']]
                #dataframe['lectura'] = ["" if x == -1 else x  for x in dataframe['lectura']]
                if primer_fichero:
                    contador_ficheros = 1
                    fichero_final = dataframe
                else:
                    contador_ficheros += 1
                    fichero_final = unificarFicheros(fichero_final,dataframe)
        #print(fichero_final["lectura"])
        #fichero_final["lectura"] = fichero_final["lectura"]//contador_ficheros
        #print(fichero_final["lectura"])
        if un_fichero:
            fichero_final = getMedias(fichero_final,contador_ficheros)
            fichero_final = fichero_final.round({"so2": 2,"no2": 2,"o3": 2,"co": 2,"pm10": 2,"pm25": 2})
            fichero_final.to_csv(fileName,index=False,encoding="UTF-8",sep=";")

if __name__ == "__main__":
    integrationToDay()
    gc.collect()
