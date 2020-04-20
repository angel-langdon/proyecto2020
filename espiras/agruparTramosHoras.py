# -*- coding: utf-8 -*-
import os
import time
import pandas as pd
import pytz
from datetime import datetime
import shutil
import gc
    
def getCurrentHour():
    tz = pytz.timezone('Europe/Madrid')
    time = datetime.now(tz)
    current_time = time.strftime("%m-%d-%Y-%H-%M-%S")
    current_time = str(current_time)
    current_time = current_time.split("-")
    return current_time
    
def procesarNombreFichero(fichero):
    tiempo = fichero[7:-4]
    tiempo = tiempo.replace("_","-")
    tiempo = tiempo.split("-")
    return tiempo

def procesarListaTiempos(l):
    l = [int(campo) for campo in l]
    return l[0],l[1],l[2],l[3],l[4],l[5]

def compareDates_dmy(l1,l2):
    monthA,dayA,yearA,hourA,minuteA,secondsA = procesarListaTiempos(l1)
    monthF,dayF,yearF,hourF,minuteF,secondsF = procesarListaTiempos(l2)
    if dayA == dayF and monthA==monthF and yearA==yearF:
        return True
    #else
    return False

def compareDates_hm(l1,l2):
    monthA,dayA,yearA,hourA,minuteA,secondsA = procesarListaTiempos(l1)
    monthF,dayF,yearF,hourF,minuteF,secondsF = procesarListaTiempos(l2)
    if hourA == hourF:
        if minuteF==0:
            return True

    elif hourA==hourF+1:
        if minuteF >0:
            return True
    #else
    return False

def getFileName():
    tz = pytz.timezone('Europe/Madrid')
    time = datetime.now(tz)
    current_time = time.strftime("%m-%d-%Y_%H-%M-%S")
    current_time = str(current_time)
    return '/root/proyecto2020/demonioEspiras/tramosPorHora/tramos_'+current_time+'.csv'
    
    
def integrationToOneHour():
    tramosPath = "/root/proyecto2020/demonioEspiras/tramos/"
    ficheros = os.listdir(tramosPath)
    lista_tiempo_actual = getCurrentHour()
    fileName = getFileName()
    ficheros = sorted(ficheros)
    
    if len(ficheros)!= 0 :
        ficheros_posibles = ficheros[-4:]
        ficheros_posibles.reverse() #leemos los ficheros más nuevos primero
        if compareDates_hm(lista_tiempo_actual,procesarNombreFichero(ficheros_posibles[0])) and compareDates_dmy(lista_tiempo_actual,procesarNombreFichero(ficheros_posibles[0])):
            contador_ficheros = 0
            for i,fichero in enumerate(ficheros_posibles):
                lista_tiempo_fichero = procesarNombreFichero(fichero)
                if compareDates_dmy(lista_tiempo_actual,lista_tiempo_fichero): #mismo mes,dia y año
                    if compareDates_hm(lista_tiempo_actual,lista_tiempo_fichero): #misma hora o una hora menos, minutos > 00
                        contador_ficheros += 1
                        fichero = tramosPath+fichero
                        dataframe = pd.read_csv(fichero,sep=";",encoding="UTF-8")
                        dataframe = dataframe.sort_values(by=["des_tramo",'latitud'])
                        dataframe['lectura'] = ["" if x > 6250 else x for x in dataframe['lectura']]
                        dataframe['lectura'] = ["" if x == -1 else x  for x in dataframe['lectura']]
                        if i == 0:
                            fichero_final = dataframe
                        else:
                            lectura_actual = list(dataframe['lectura'])
                            lectura_fichero_final = list(fichero_final["lectura"])
                            for i,lectura in enumerate(lectura_actual):
                                if lectura != "":
                                    if lectura_fichero_final[i] != "":
                                        lectura_fichero_final[i] = (lectura + lectura_fichero_final[i])
                                    else:lectura_fichero_final[i] = lectura
                            fichero_final["lectura"] = lectura_fichero_final
            lista_aux = fichero_final["lectura"]
            fichero_final["lectura"] = [i//contador_ficheros  if len(str(i))>0 else i for i in lista_aux ]
            fichero_final.to_csv(fileName,index=False,encoding="UTF-8",sep=";")
           
    gc.collect()

if __name__ == "__main__":
    integrationToOneHour()
    ficheros = os.listdir("/root/proyecto2020/demonioEspiras/tramosPorHora/")
    ficheros = sorted(ficheros)
    if len(ficheros) > 25:
        tramosHoraPath = "/root/proyecto2020/demonioEspiras/tramosPorHora/"
        historicoPath = "/root/proyecto2020/demonioEspiras/tramosPorHoraHistorico/"
        ficheros_mover = ficheros[:-25]
        for fichero in ficheros_mover:
            shutil.move(tramosHoraPath+fichero, historicoPath+fichero)