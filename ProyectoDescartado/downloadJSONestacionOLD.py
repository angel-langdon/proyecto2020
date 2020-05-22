# -*- coding: utf-8 -*-

import requests
import pytz
import json
import os
from datetime import datetime,timezone, timedelta
from zipfile import ZipFile
import time

def downloadJSON():
  url = "https://geoportal.valencia.es/arcgis/rest/services/Opendata/MedioAmbiente/MapServer/136/query?f=json&where=1%3D1&returnGeometry=true&spatialRel=esriSpatialRelIntersects&geometry=%7B%22xmin%22%3A-71853.72886058278%2C%22ymin%22%3A4787025.055646416%2C%22xmax%22%3A1525.8182931781994%2C%22ymax%22%3A4802923.957529731%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D&geometryType=esriGeometryEnvelope&inSR=102100&outFields=nombre%2Cdireccion%2Ctipozona%2Ctipoemision%2Cparametros%2Cmediciones%2Cgid%2Cso2%2Cno2%2Co3%2Cco%2Cpm10%2Cpm25%2Cfecha_carga%2Ccalidad_ambiental&orderByFields=gid%20ASC&outSR=102100"
  response = requests.get(url)
  if response.status_code == 200:
    tz = pytz.timezone('Europe/Madrid')
    time = datetime.now(tz)
    current_time = time.strftime("%d-%m-%Y_%H-%M-%S")
    current_time = str(current_time)
    filename = '/root/proyecto2020/demonioEstacion/estacion/estacion_'+current_time+'.json'
    with open(filename,mode='w',encoding='UTF-8') as f:
      json.dump(response.json(),f)
  return


if __name__ == '__main__':
  downloadJSON()
