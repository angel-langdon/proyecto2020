import requests
import pytz
import json
import os
from datetime import datetime,timezone, timedelta
from zipfile import ZipFile
import time

def downloadJSON():
  url = "http://apigobiernoabiertortod.valencia.es/apirtod/datos/intensidad_tramos.json?items=1000"
  response = requests.get(url)
  if response.status_code == 200:
    tz = pytz.timezone('Europe/Madrid')
    time = datetime.now(tz)
    current_time = time.strftime("%d-%m-%Y_%H-%M-%S")
    current_time = str(current_time)
    filename = '/root/proyecto2020/demonioEspiras/tramos/tramos_'+current_time+'.json'
    with open(filename,mode='w',encoding='UTF-8') as f:
      json.dump(response.json(),f)
  return

if __name__ == '__main__':
    time.sleep(10)
    downloadJSON()
