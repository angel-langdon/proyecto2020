import requests
import pytz
import json
import os
from datetime import datetime,timezone, timedelta
from zipfile import ZipFile
import time


def zipEspiras():
  dir = '/root/proyecto2020/demonioEspiras/tramos/'
  fileNames = os.listdir(dir)
  with ZipFile('/root/proyecto2020/demonioEspiras/tramos.zip', 'a') as f:
    for fileName in fileNames:
      filePath = '/root/proyecto2020/demonioEspiras/tramos/'+fileName
      f.write(filePath)
  return

def delEspiras():
  dir = '/root/proyecto2020/demonioEspiras/tramos/'
  filenames = os.listdir(dir)
  for archivo in filenames:
    archivo = "/root/proyecto2020/demonioEspiras/tramos/"+archivo
    os.remove(archivo)
  return



if __name__ == '__main__': 
  time.sleep(10)
  time.sleep(5)
  zipEspiras()
  time.sleep(5)
  delEspiras()
