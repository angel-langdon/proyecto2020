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
    tiempo = fichero[7:-4]
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
    return '/root/proyecto2020/demonioEspiras/tramosPorDia/tramos_'+current_time+'.csv'

def integrationToDay():
    fileName = getFileName()
    tramosPath = "/root/proyecto2020/demonioEspiras/tramosPorHora/"
    ficheros = os.listdir(tramosPath)
    tiempo_today = getToday()
    tiempo_yesterday = getYesterday()
    ficheros = sorted(ficheros)
    ficheros.reverse()
    primer_fichero_dia = True
    if len(ficheros)!= 0 :
        contador_ficheros = 0
        for i,fichero in enumerate(ficheros):
            tiempo_fichero = procesarNombreFichero(fichero)
            if compareDates(tiempo_today,tiempo_yesterday,tiempo_fichero):
                contador_ficheros += 1
                fichero = tramosPath+fichero
                dataframe = pd.read_csv(fichero,sep=";",encoding="UTF-8")
                dataframe = dataframe.sort_values(by=["des_tramo",'latitud'])
                #dataframe['lectura'] = ["" if x > 6250 else x for x in dataframe['lectura']]
                #dataframe['lectura'] = ["" if x == -1 else x  for x in dataframe['lectura']]
                if primer_fichero_dia:
                    primer_fichero_dia == False
                    fichero_final = dataframe
                else:
                    lectura_actual = list(dataframe['lectura'])
                    lectura_fichero_final = list(fichero_final["lectura"])
                    for j,lectura in enumerate(lectura_actual):
                        if not math.isnan(lectura):
                            if not math.isnan(lectura_fichero_final[j]):
                                lectura_fichero_final[j] = (lectura + lectura_fichero_final[j])
                            else:
                                lectura_fichero_final[j] = lectura
                                
                    fichero_final["lectura"] = lectura_fichero_final
        #print(fichero_final["lectura"])
        #fichero_final["lectura"] = fichero_final["lectura"]//contador_ficheros
        #print(fichero_final["lectura"])
        fichero_final.to_csv(fileName,index=False,encoding="UTF-8",sep=";")

if __name__ == "__main__":
    integrationToDay()
    gc.collect()
