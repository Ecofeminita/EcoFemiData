# -*- coding: utf-8 -*-
"""
Created on Tue Jun  9 12:53:43 2020

@author: perla
"""

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
import pandas as pd
import urllib.request
import datetime
import time

PATH_CHROME_PROFILE = "C:/Users/perla/AppData/Local/Google/Chrome/User Data"
PATH_CHROME_DRIVER = "C:/Users/perla/Desktop/Webdriver/chromedriver.exe"

df1=pd.read_csv("../data/Instagram_links2020-06-08-18hs50mins.csv")
df2=pd.read_csv("../data/Instagram_links2020-06-08-19hs01mins.csv")

links=df1+df2

link=links['Links']

chrome_options = Options()
chrome_options.add_argument("user-data-dir="+PATH_CHROME_PROFILE)
browser = webdriver.Chrome(PATH_CHROME_DRIVER,options=chrome_options)

users=[]
texts=[]
date=[]
data=[]

for x in range(len(link)):
    
    time.sleep(15)

    browser.get(link[x])
    
    user=browser.find_elements_by_xpath("//h2[@class='_6lAjh ']")
    
    for u in user: 
        users.append(u.text)
        
    text=browser.find_elements_by_xpath("//h2[@class='_6lAjh ']//following-sibling::span")
    
    for t in text:
        texts.append(t.text)
        
    fecha=browser.find_elements_by_xpath("//a[@class='c-Yi7']/time")
    
    for f in fecha:
        date.append(f.text)
        
    img = browser.find_element_by_xpath('//div[@class="KL4Bh"]/img')
    
    src = img.get_attribute('src')
    
    # download the image
    archive_name="../data/img/"+str(x)+".png"
    
    urllib.request.urlretrieve(src, archive_name)
    
    data.append({"Link":link,"User":users,"Text":texts,"Date":date})
    
browser.quit()

data=pd.DataFrame(data)

currentDT = datetime.datetime.now()
fecha = currentDT.strftime("%Y-%m-%d-%Hhs%Mmins")
archivo = "../data/Instagram_data"+str(fecha) + '.csv'
data.to_csv(archivo, index=False)

