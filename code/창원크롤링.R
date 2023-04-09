##################크롤링 반복문###################
#java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-2.jar -port 4445

library(beepr)
library(RSelenium)
library(rvest)
library(dplyr)
library(data.table)
library(readxl)
library(rvest) 
library(xml2)
library(stringr)

setwd('C:/Users/User/Documents/카카오톡 받은 파일')

df=fread('창원_대여소_공원.csv')
str(df)

doro=c()
for(i in 1:nrow(df)){
  doro[i]=as.character(df[i,'addr'])
}
str(doro)
doro[1]

newAddr=c()
#################지번주소->도로명주소로 변환#######################
for(i in 1:length(doro)){
  remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
  remDr$open() 
  remDr$navigate("https://map.naver.com/") #입력주소로 이동
  Sys.sleep(5)
  #//*[@id="container"]/div[1]/shrinkable-layout/search-layout/search-box/div/div[1]
  #//*[@id="container"]/div[1]/app-base/search-box/div/div[2]
  find1Button<-remDr$findElement(using="xpath",value='//*[@id="container"]/div[1]/app-base/search-box/div/div[2]')
  find1Button$clickElement() ; Sys.sleep(5) 
  #find1Button$clearElement()
  inputButton<-remDr$findElement(using='xpath',value='//*[@id="container"]/div[1]/app-base/search-box/div/div[2]/span')
  inputButton$sendKeysToElement(list(doro[1],key='enter')); Sys.sleep(2)
  
  #//*[@id="container"]/div[1]/shrinkable-layout/search-layout/search-entry/entry-layout/entry-address/div/div/div/div[1]/div[3]/div[1]/div/div/span/a
  url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
  url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
  item_final2<- url_item_final2 %>% html_nodes(".link_end") %>% html_text(); Sys.sleep(2)
  
  newAddr[i]=as.character(item_final2)
}
##############################
geo_code=function(name,n=3){
  
  url='https://map.naver.com/'
  
  remDr$navigate(url)
  
  
  
  search=NULL
  
  while(length(search)==0){
    
    assign('search',remDr$findElement(using='css selector',value='input#searchboxinput.tactile-searchbox-input'))
    
  }
  
  
  
  data=NULL
  
  for(i in 1:length(name)){
    
    search$clearElement()
    
    search$sendKeysToElement(list(name[i],key='enter'))
    
    Sys.sleep(n)
    
    lonlat=as.numeric(str_split(substr(remDr$getCurrentUrl()[[1]],
                                       
                                       regexpr('@',remDr$getCurrentUrl()[[1]])+1,regexpr(',[0-9]+z',remDr$getCurrentUrl()[[1]])-1),',')[[1]])
    
    data=rbind(data,data.frame(name=name[i],lat=lonlat[1],lon=lonlat[2]))
    
  }
  
  return(data)
  
}

geo_code(name)

remDr$close()

