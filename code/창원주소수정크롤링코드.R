######################창원 대여소 주소 가져오기############################
#java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-2.jar -port 4445


#install.packages('data.table')
library(data.table)
library(beepr)
library(RSelenium)
library(rvest)
library(dplyr)
library(readxl)
library(rvest) 
library(xml2)
library(stringr)


remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
remDr$open() 
remDr$navigate('https://www.nubija.com/terminal/terminalList.do') #입력주소로 이동
Sys.sleep(5)

url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) 

대여소id=c()
url1=url_item_final %>% html_nodes('#terminal_list')
for(i in 1:280){
  id=url1 %>% html_nodes('.list_terminal_icon') %>% as.character()
  대여소id[i]=as.numeric(str_extract_all(strsplit(id[i],split='/')[[1]], "\\d+")[[5]])  #대여소 id 저장
}
# i=1
# str_extract_all(strsplit(id,split='/')[[1]], "\\d+")[[5]]
# 
# strsplit(id,split='/')[[1]]

# substr(strsplit(id[4],split='/')[[1]][5],1,3)

대여소id
addr=url1 %>% html_nodes('.list_terminal_location') %>% html_text() %>% as.character()
name=url1 %>% html_nodes('.list_terminal_name') %>% html_text() %>% as.character()
name=gsub('\t','',name)
name=gsub('\n','',name)
str(name)

changwon=data.frame(대여소id, name, addr)
str(changwon)
head(changwon)
i=1
lat=c(); long=c()
for(i in 1:279){
  longlat=url1 %>% html_nodes('.list_terminal_normal') %>% as.character()
  lat[i]=strsplit(longlat[2*i],split="'")[[1]][2]
  long[i]=strsplit(longlat[2*i],split="'")[[1]][4]
}

lat #위도
long #경도

changwon=cbind(changwon,lat,long)
str(changwon)
write.csv(changwon,'C:/Users/User/Desktop/인천시빅데이터경진대회/창원시 대여소 정보.csv')
install.packages('revgeo')
library(revgeo)
newAddr=c()
for(i in 1:279){
  newAddr[i]=revgeo(longitude=long[i], latitude=lat[i])
}
head(newAddr)
str(newAddr)
length(newAddr)
strsplit(newAddr[[1]],split=',')
addr.f=c()
for(i in 1:279){
  chlist=strsplit(newAddr[[i]],split=',')[[1]]
  addr.f[i]=paste0(chlist[2], chlist[3], chlist[4], chlist[5])
}
addr.f

#경상남도 창원시 51693
# 
# #창원데이터 full주소 가져오기
# remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
# remDr$open() 
# remDr$navigate('https://map.naver.com/v5/?c=14136701.4616110,4518326.7092716,15,0,0,0,dh') #입력주소로 이동
# Sys.sleep(2)
# i=2
# findButton<-remDr$findElement(using="xpath",value='//*[@id="container"]/div[1]/app-base/search-box/div/div[2]/span')
# findButton$clickElement() ; Sys.sleep(5) 
# findButton$sendKeysToElement(list(name[i]))
# searchButton<-remDr$findElement(using="xpath",value='//*[@id="container"]/div[1]/app-base/search-box/div/div[2]/button')
# searchButton$clickElement() ; Sys.sleep(2) 
# 
# 
# jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
# jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
# jibunAddr$sendKeysToElement(list(doroAddr[d]))
# 
# 
# 
# searchButton2<-remDr$findElement(using="xpath",value='//*[@id="container"]/div[2]/maps-controller/context-menu/div/ul[2]/li/button')
# searchButton2$clickElement() ; Sys.sleep(2)
# 
# 
# IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
# IDButton$clickElement() ; Sys.sleep(2) 
# IDButton$sendKeysToElement(list('ej2747'))
# PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
# PWButton$clickElement() ; Sys.sleep(2) 
# PWButton$sendKeysToElement(list('dmswl0235!'))
# 
# 



