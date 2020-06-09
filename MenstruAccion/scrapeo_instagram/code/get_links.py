# -*- coding: utf-8 -*-
"""
Created on Sun Jun  7 22:20:48 2020

@author: perla
"""

from selenium import webdriver
from bs4 import BeautifulSoup as bs
import time
from selenium.webdriver.chrome.options import Options
import pandas as pd
import datetime

PATH_CHROME_PROFILE = "C:/Users/perla/AppData/Local/Google/Chrome/User Data"
PATH_CHROME_DRIVER = "C:/Users/perla/Desktop/Webdriver/chromedriver.exe"

hashtag='MenstruAccion'

chrome_options = Options()
chrome_options.add_argument("user-data-dir="+PATH_CHROME_PROFILE)
browser = webdriver.Chrome(PATH_CHROME_DRIVER,options=chrome_options)
browser.get('https://www.instagram.com/explore/tags/'+hashtag)

SCROLL_PAUSE_TIME = 6
links=[]


# Get scroll height
last_height = browser.execute_script("return document.body.scrollHeight")

while True:
    # Scroll down to bottom
    browser.execute_script("window.scrollTo(0, document.body.scrollHeight);")

    # Wait to load page
    time.sleep(SCROLL_PAUSE_TIME)
    
    #Extract links from hashtag page
    
    source = browser.page_source
    bsObj=bs(source, 'html.parser')
    
    for a in bsObj.find_all('a', href=True):
        if a['href'].startswith("/p/"):
            links.append('https://instagram.com'+a['href'])
        else:
            pass
        

    # Calculate new scroll height and compare with last scroll height
    new_height = browser.execute_script("return document.body.scrollHeight")
    if new_height == last_height:
        break
    last_height = new_height

links_limpios=list(dict.fromkeys(links))

data = pd.DataFrame({"Links":links_limpios})

currentDT = datetime.datetime.now()
fecha = currentDT.strftime("%Y-%m-%d-%Hhs%Mmins")
archivo = "Instagram_links"+str(fecha) + '.csv'
data.to_csv(archivo, index=False)
