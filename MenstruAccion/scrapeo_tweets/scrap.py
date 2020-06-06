# -*- coding: utf-8 -*-
"""
Created on Fri Jun  5 13:00:45 2020

@author: perla
"""

import GetOldTweets3 as got
import pandas as pd
import datetime

def scrapeo_twitter(since, until):
    
    tweetCriteria = got.manager.TweetCriteria().setQuerySearch('#Menstruacción OR Menstruacción OR menstruaccion OR #menstruaccion').setSince(since).setUntil(until)
    tweet = got.manager.TweetManager.getTweets(tweetCriteria)
    
    username=[]
    text=[]
    date=[]
    hashtags=[]
    tw_link=[]
    
    for t in tweet:
        username.append(t.username)
        text.append(t.text)
        date.append(t.date)
        hashtags.append(t.hashtags)
        tw_link.append(t.permalink)
        
    data={"Users":username,"Texto":text,"Fecha":date,"Hashtags":hashtags,"Permalinks":tw_link}
    
    df= pd.DataFrame(data)
    
    currentDT = datetime.datetime.now()
    fecha = currentDT.strftime("%Y-%m-%d-%Hhs%Mmins")
    archivo = "Tweets"+str(fecha) + '.csv'
    df.to_csv(archivo, index=False)
    

scrapeo_twitter("2017-02-01","2020-06-04") #nota: la fecha siempre tiene que ir en formato YYYY-MM-DD

    
