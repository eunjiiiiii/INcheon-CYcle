
##################크롤링 반복문###################
#java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-2.jar -port 4445

# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("dplyr")
# install.packages('data.table')
# install.packages('readxl')
# install.packages('beepr')
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

#(임시)도로명 파일 불러와서doroAddr 벡터 만들기
df=fread('대여소.csv')
str(df)
str(df[2,2])
as.character(df[2,'대여소주소'])

doroAddr=c()
for(i in 1:nrow(df)){
  doroAddr[i]=as.character(df[i,'대여소주소'])
}
str(doroAddr)
head(doroAddr,15)
doroAddr=c()
tnwjd=fread('대여소수정.csv')
str(tnwjd)
for(i in 1:nrow(tnwjd)){
  doroAddr[i]=tnwjd$'대여소주소'[i]
}
str(doroAddr)

df=fread('인천_대여소_공원.csv')
str(df)
doroAddr=c()
for(i in 1:nrow(df)){
  doroAddr[i]=as.character(df[i,'주소'])
}
str(doroAddr)




#doroAddr=fread('doroAddr.csv')
#doroAddr=doroAddr[-1,-1]
#str(doroAddr)
###################################################
######################주소 벡터 생성 완료#############
##################################################

# 
# #빈 벡터 만들기
total_store=c()
food_num=c()
service_num=c()
sale_num=c()
live_num=c()
work_num=c()
move_num=c()
max_gender=c()
AGE1_ratio=c()
AGE2_ratio=c()
AGE3_ratio=c()
AGE4_ratio=c()
AGE5_ratio=c()
AGE6_ratio=c()
income_live=c()
income_work=c()
consum_live=c()
consum_work=c()
public_num=c()
bank_num=c()
hospital_num=c()
school_num=c()
traffic_num=c()

#크롤링한 파일 불러오기
collect=fread("storefinal_최종.csv") ###########각자 크롤링한 파일로 파일이름 수정해주세요!
str(collect)

num=rep(0,nrow(collect))
for (i in 1:nrow(collect)){
  num[i]=i
}
num

collect=cbind(collect,num)

collect=collect[1:540,] ############인덱스 번호 수정!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
na=rep(NA,nrow(collect))

for (i in 1:nrow(collect)){
  if (is.na(collect$AGE1_ratio[i])==TRUE){ ##########AGE1_ratio_seoul를 저장한 변수 아무거나로 수정
    na[i]=collect$num[i]
  }
}

na=na.omit(na)
str(na)
na
#####################크롤링######################################
newi=1
#오류난데 다음부터 다시 돌리려면 아래 코드 주석 해제
#newi=i+1
# try({
#   for(i in newi:length(na)){ 	#d:대여소 개수(1540개)
#     d=na[i]
str(storefinal)
tail(storefinal)

#####################크롤링######################################
newi=36
#오류난데 다음부터 다시 돌리려면 아래 코드 주석 해제
newi=i+1
#head(narows)
try({
  for(i in newi:length(na)){ 	#d:대여소 개수(1540개)
  ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
  d=na[i]
  remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
  remDr$open() 
  remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
  Sys.sleep(5)
  loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
  loginButton$clickElement() ; Sys.sleep(2) 
  IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
  IDButton$clickElement() ; Sys.sleep(2) 
  IDButton$sendKeysToElement(list('ej2747'))
  PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
  PWButton$clickElement() ; Sys.sleep(2) 
  PWButton$sendKeysToElement(list('dmswl0235!'))
  #로그인하기
  findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
  findButton$clickElement(); Sys.sleep(10) #로그인 완료
  
  ###2. 상권분석-1단계 지역선택###
  localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
  localChose$clickElement(); Sys.sleep(2) #지역선택
  localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
  localChose$clickElement(); Sys.sleep(2) #지역선택
  jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
  jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
  jibunAddr$sendKeysToElement(list(doroAddr[d]))
  findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
  findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
  
  #지번명으로 검색이 안될경우
  url_item1<-remDr$getPageSource()[[1]]
  url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
  item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.
  
  item1=gsub('\t','',item1)
  if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
    #도로명으로 검색
    doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
    doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
    
    
    url_item11<-remDr$getPageSource()[[1]]
    url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    
    item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
    item12=gsub('\t','',item12)
    
    item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
    
    item13<- url_item12 %>% html_nodes('.layer-pop-body')  
    
    warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()
    
    if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){ 
      remDr$close
      next
    }else{ #주소 오류 안뜰때
      
      item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()
      
      #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
      if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.
      
      checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
      checkButton$clickElement(); Sys.sleep(5)
    }
  }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)
    
    url_item2<-remDr$getPageSource()[[1]]
    url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
    item2
    addr_num=length(item2)
    
    
    # seoul_i=0
    #주소가 여러 건 검색됐을 경우
    if(addr_num>1){
      #서울특별시가 포함된 주소 버튼 누르기
      # seoul_i=grep("인천광역시",item2)[1]
      # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
      #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
      #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
      #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
      xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
      xpath2=']/a'
      xpath_v=paste0(xpath1,seoul_i+1,xpath2)
      checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
      checkButton$clickElement(); Sys.sleep(5)
    }else{ #주소1건만 검색됐을 경우
      # seoul_i=grep("인천광역시",item2)[1]
      # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
      checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
      checkButton$clickElement(); Sys.sleep(5)
    }
  }
  
  
  ###2단계 영역선택###
  areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
  areaChoose$clickElement(); Sys.sleep(10) #반경선택
  
  #putButton에 들어갈 xpath의 숫자 구하기
  
  #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>
  
  url_item3<-remDr$getPageSource()[[1]]
  url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
  
  item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
 # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
  #//*[@id="OpenLayers.Geometry.Point_656"]
  #//*[@id="OpenLayers.Geometry.Point_655"]
  #숫자만 가져오기
  #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
 # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
  putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
  putButton$clickElement(); Sys.sleep(3)
  okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
  okButton$clickElement(); Sys.sleep(3)
  ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
  ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!
  
  ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
  # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
  # 
  # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
  #   next
  # }
  
  
  ###3단계 업종선택###
  seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
  seleckStore$clickElement(); Sys.sleep(20)
  

  ##음식으로 업종 선택해서 상권분석결과 보기##
  seleckStore=c()
  
  
  xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
  seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
  seleckStore$clickElement(); Sys.sleep(2)
  
  
  
  ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
  ok3Button$clickElement(); Sys.sleep(2)
  
  analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
  analysisButton$clickElement(); Sys.sleep(100)
  
  #****************************************************************#
  #**************************************************************#
  ####마지막. 결과보고서에서 숫자데이터 가져오기####
  url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
  url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
 #57까지 2.지역별 평가지수 추이 표 보여줌.
  
  #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\
  
  #먼저 서울이 아니면 다음 for문으로 넘기기
  
  checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
  if(grepl('서울특별시',checkSeoul==FALSE)){
    next
  }
  
  #서울이 맞으면...
  ##상권평가 탭
  item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
  item_final
  #전체업소수
  total_store[d]=as.numeric(gsub(',','',item_final[1]))
  
  #음식점 수
  food_num[d]=as.numeric(gsub(',','',item_final[2]))
  
  #서비스업소수
  service_num[d]=as.numeric(gsub(',','',item_final[3]))
  
  #도/소매 업소수
  sale_num[d]=as.numeric(gsub(',','',item_final[4]))
  
  #인구수-주거
  live_num[d]=as.numeric(gsub(',','',item_final[8]))
  
  #인구수-직장
  work_num[d]=as.numeric(gsub(',','',item_final[9]))
  
  
  ##인구분석탭
  populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
  populButton$clickElement(); Sys.sleep(2)
  
  ##인구분석탭의 html가져오기
  url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
  url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
  item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
  item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.
  
  #유동인구 - 주말/주중 유동인구 평균
  we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
  mean_we=we/2 #주말 유동인구 평균
  wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
  mean_wd=wd/5 #주중 유동인구 평균
  
  #주말+주중 유동인구 평균
  move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기
  
  #유동인구-성별, 더 많은 성별 factor형
  male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
  female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)

  if(male>female){
    max_gender[d]='M'
  }
  if(male<female){ 
    max_gender[d]='F'
  }

  
  #유동인구-연령별, 연령별 유동인구수 비율
  AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
  AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
  AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
  AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
  AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
  AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율

  
  ##소득/소비 탭
  moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
  moneyButton$clickElement(); Sys.sleep(2)
  
  ##소득/소비탭의 html가져오기
  url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
  url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
  item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
  item_final3
  str(item_final3)
  #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
  #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
  #동이 2개 이상이면..
  length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                        ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)
      
      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                           ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
    }
    if(length(item_final3)>44){ #동이 2개 이상이면
    income_live[d]=inc_live/2
    income_work[d]=inc_work/2
    consum_live[d]=cons_live/2
    consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }
  
  
  
  ##지역분석 탭
  localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
  localAnalysis$clickElement(); Sys.sleep(3)
  
  url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
  url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
  #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
  #item_final4
  
  
  #공공기관 수
  pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()
  
  public_num[d]=as.numeric(gsub(',','',pu[1]))
  
  #금융기관 수
  bank_num[d]=as.numeric(gsub(',','',pu[2]))
  
  #의료/복지 시설 수 
  hospital_num[d]=as.numeric(gsub(',','',pu[3]))
  
  #학교 수
  school_num[d]=as.numeric(gsub(',','',pu[4]))
  
  #교통시설 수
  traffic_num[d]=as.numeric(gsub(',','',pu[8]))

  # ##다시 상권분석화면으로 돌아가기
  # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
  # returnButton$clickElement(); Sys.sleep(2)
  # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
  # return2Button$clickElement(); Sys.sleep(2)
  # 
  remDr$close()
}},silent=FALSE)

beep(5)
#write.csv(storefinal, 'storefinal8.csv') #d=145~
d 

newi=i+1
#head(narows)
try({
  for(i in newi:length(na)){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    d=na[i]
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open() 
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2) 
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2) 
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2) 
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료
    
    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
    
    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.
    
    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
      
      
      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      
      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)
      
      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      
      item13<- url_item12 %>% html_nodes('.layer-pop-body')  
      
      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()
      
      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){ 
        remDr$close
        next
      }else{ #주소 오류 안뜰때
        
        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()
        
        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.
        
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)
      
      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)
      
      
      # seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }
    
    
    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택
    
    #putButton에 들어갈 xpath의 숫자 구하기
    
    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>
    
    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    
    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!
    
    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    # 
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }
    
    
    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)
    
    
    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()
    
    
    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)
    
    
    
    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)
    
    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)
    
    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.
    
    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\
    
    #먼저 서울이 아니면 다음 for문으로 넘기기
    
    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('서울특별시',checkSeoul==FALSE)){
      next
    }
    
    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))
    
    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))
    
    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))
    
    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))
    
    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))
    
    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))
    
    
    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)
    
    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.
    
    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균
    
    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기
    
    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)
    
    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){ 
      max_gender[d]='F'
    }
    
    
    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율
    
    
    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)
    
    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)
      
      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }
    
    
    
    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)
    
    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4
    
    
    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()
    
    public_num[d]=as.numeric(gsub(',','',pu[1]))
    
    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))
    
    #의료/복지 시설 수 
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))
    
    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))
    
    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))
    
    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    # 
    remDr$close()
  }},silent=FALSE)

beep(5)
#write.csv(storefinal, 'storefinal8.csv') #d=145~
d 



newi=i+1
#head(narows)
try({
  for(i in newi:length(na)){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    d=na[i]
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open() 
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2) 
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2) 
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2) 
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료
    
    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
    
    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.
    
    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
      
      
      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      
      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)
      
      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      
      item13<- url_item12 %>% html_nodes('.layer-pop-body')  
      
      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()
      
      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){ 
        remDr$close
        next
      }else{ #주소 오류 안뜰때
        
        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()
        
        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.
        
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)
      
      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)
      
      
      # seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }
    
    
    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택
    
    #putButton에 들어갈 xpath의 숫자 구하기
    
    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>
    
    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    
    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!
    
    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    # 
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }
    
    
    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)
    
    
    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()
    
    
    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)
    
    
    
    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)
    
    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)
    
    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.
    
    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\
    
    #먼저 서울이 아니면 다음 for문으로 넘기기
    
    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('서울특별시',checkSeoul==FALSE)){
      next
    }
    
    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))
    
    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))
    
    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))
    
    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))
    
    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))
    
    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))
    
    
    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)
    
    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.
    
    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균
    
    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기
    
    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)
    
    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){ 
      max_gender[d]='F'
    }
    
    
    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율
    
    
    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)
    
    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)
      
      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }
    
    
    
    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)
    
    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4
    
    
    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()
    
    public_num[d]=as.numeric(gsub(',','',pu[1]))
    
    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))
    
    #의료/복지 시설 수 
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))
    
    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))
    
    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))
    
    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    # 
    remDr$close()
  }},silent=FALSE)

beep(5)
#write.csv(storefinal, 'storefinal8.csv') #d=145~
d 



newi=i+1
#head(narows)
try({
  for(i in newi:length(na)){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    d=na[i]
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open() 
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2) 
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2) 
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2) 
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료
    
    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
    
    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.
    
    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
      
      
      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      
      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)
      
      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      
      item13<- url_item12 %>% html_nodes('.layer-pop-body')  
      
      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()
      
      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){ 
        remDr$close
        next
      }else{ #주소 오류 안뜰때
        
        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()
        
        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.
        
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)
      
      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)
      
      
      # seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }
    
    
    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택
    
    #putButton에 들어갈 xpath의 숫자 구하기
    
    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>
    
    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    
    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!
    
    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    # 
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }
    
    
    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)
    
    
    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()
    
    
    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)
    
    
    
    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)
    
    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)
    
    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.
    
    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\
    
    #먼저 서울이 아니면 다음 for문으로 넘기기
    
    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('서울특별시',checkSeoul==FALSE)){
      next
    }
    
    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))
    
    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))
    
    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))
    
    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))
    
    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))
    
    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))
    
    
    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)
    
    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.
    
    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균
    
    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기
    
    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)
    
    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){ 
      max_gender[d]='F'
    }
    
    
    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율
    
    
    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)
    
    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)
      
      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }
    
    
    
    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)
    
    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4
    
    
    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()
    
    public_num[d]=as.numeric(gsub(',','',pu[1]))
    
    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))
    
    #의료/복지 시설 수 
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))
    
    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))
    
    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))
    
    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    # 
    remDr$close()
  }},silent=FALSE)

beep(5)
#write.csv(storefinal, 'storefinal8.csv') #d=145~
d 

newi=i+1
#head(narows)
try({
  for(i in newi:length(na)){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    d=na[i]
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open() 
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2) 
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2) 
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2) 
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료
    
    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
    
    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.
    
    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료
      
      
      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      
      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)
      
      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      
      item13<- url_item12 %>% html_nodes('.layer-pop-body')  
      
      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()
      
      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){ 
        remDr$close
        next
      }else{ #주소 오류 안뜰때
        
        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()
        
        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.
        
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)
      
      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)
      
      
      # seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("인천광역시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }
    
    
    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택
    
    #putButton에 들어갈 xpath의 숫자 구하기
    
    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>
    
    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    
    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!
    
    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    # 
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }
    
    
    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)
    
    
    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()
    
    
    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)
    
    
    
    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)
    
    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)
    
    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.
    
    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\
    
    #먼저 서울이 아니면 다음 for문으로 넘기기
    
    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('서울특별시',checkSeoul==FALSE)){
      next
    }
    
    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))
    
    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))
    
    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))
    
    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))
    
    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))
    
    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))
    
    
    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)
    
    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.
    
    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균
    
    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기
    
    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)
    
    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){ 
      max_gender[d]='F'
    }
    
    
    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율
    
    
    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)
    
    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)
      
      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2
      
      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2
      
    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }
    
    
    
    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)
    
    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4
    
    
    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()
    
    public_num[d]=as.numeric(gsub(',','',pu[1]))
    
    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))
    
    #의료/복지 시설 수 
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))
    
    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))
    
    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))
    
    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    # 
    remDr$close()
  }},silent=FALSE)

beep(5)
#write.csv(storefinal, 'storefinal8.csv') #d=145~
d 

storefinal=data.frame(total_store,
      food_num,
      service_num,
      sale_num,
      live_num,
      work_num,
      move_num,
      max_gender,
      AGE1_ratio,
      AGE2_ratio,
      AGE3_ratio,
      AGE4_ratio,
      AGE5_ratio,
      AGE6_ratio,
      income_live,
      income_work,
      consum_live,
      consum_work,
      public_num,
      bank_num,
      hospital_num,
      school_num,
      traffic_num)
storefinal

write.csv(storefinal,'storefinal__incheon.csv')
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
# d
# #################################################################newd=d+1
newd=51
endd=86
try({
  for(d in newd:endd){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open()
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2)
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2)
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2)
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료

    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료

    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.

    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료


      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)

      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()

      item13<- url_item12 %>% html_nodes('.layer-pop-body')

      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()

      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){
        remDr$close
        next
      }else{ #주소 오류 안뜰때

        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()

        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.

        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)

      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)


      seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,addr_num+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }


    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택

    #putButton에 들어갈 xpath의 숫자 구하기

    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>

    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!

    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    #
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }


    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)


    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()


    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)



    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)

    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)

    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.

    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\

    #먼저 서울이 아니면 다음 for문으로 넘기기

    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('인천',checkSeoul==FALSE)){
      next
    }

    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))

    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))

    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))

    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))

    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))

    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))


    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)

    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.

    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균

    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기

    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)

    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){
      max_gender[d]='F'
    }


    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율


    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)

    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)

      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }



    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)

    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4


    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()

    public_num[d]=as.numeric(gsub(',','',pu[1]))

    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))

    #의료/복지 시설 수
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))

    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))

    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))

    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    #
    remDr$close()
  }},silent=FALSE)
storefinal=data.frame(total_store,
                      food_num,
                      service_num,
                      sale_num,
                      live_num,
                      work_num,
                      move_num,
                      max_gender,
                      AGE1_ratio,
                      AGE2_ratio,
                      AGE3_ratio,
                      AGE4_ratio,
                      AGE5_ratio,
                      AGE6_ratio,
                      income_live,
                      income_work,
                      consum_live,
                      consum_work,
                      public_num,
                      bank_num,
                      hospital_num,
                      school_num,
                      traffic_num)
str(storefinal)
write.csv(storefinal, 'storefinal_incheon.csv') #d=145~
d

newd=d+1
try({
  for(d in newd:endd){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open()
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2)
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2)
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2)
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료

    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료

    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.

    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료


      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)

      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()

      item13<- url_item12 %>% html_nodes('.layer-pop-body')

      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()

      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){
        remDr$close
        next
      }else{ #주소 오류 안뜰때

        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()

        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.

        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)

      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)


      # seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }


    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택

    #putButton에 들어갈 xpath의 숫자 구하기

    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>

    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!

    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    #
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }


    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)


    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()


    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)



    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)

    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)

    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.

    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\

    #먼저 서울이 아니면 다음 for문으로 넘기기

    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('여수',checkSeoul==FALSE)){
      next
    }

    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))

    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))

    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))

    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))

    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))

    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))


    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)

    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.

    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균

    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기

    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)

    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){
      max_gender[d]='F'
    }


    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율


    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)

    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)

      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }



    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)

    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4


    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()

    public_num[d]=as.numeric(gsub(',','',pu[1]))

    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))

    #의료/복지 시설 수
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))

    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))

    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))

    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    #
    remDr$close()
  }},silent=FALSE)
storefinal=data.frame(total_store,
                      food_num,
                      service_num,
                      sale_num,
                      live_num,
                      work_num,
                      move_num,
                      max_gender,
                      AGE1_ratio,
                      AGE2_ratio,
                      AGE3_ratio,
                      AGE4_ratio,
                      AGE5_ratio,
                      AGE6_ratio,
                      income_live,
                      income_work,
                      consum_live,
                      consum_work,
                      public_num,
                      bank_num,
                      hospital_num,
                      school_num,
                      traffic_num)
str(storefinal)
write.csv(storefinal, 'storefinal_incheon.csv') #d=145~
d


newd=d+1
try({
  for(d in newd:endd){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open()
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2)
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2)
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2)
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료

    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료

    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.

    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료


      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)

      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()

      item13<- url_item12 %>% html_nodes('.layer-pop-body')

      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()

      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){
        remDr$close
        next
      }else{ #주소 오류 안뜰때

        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()

        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.

        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)

      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)


      seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }


    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택

    #putButton에 들어갈 xpath의 숫자 구하기

    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>

    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!

    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    #
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }


    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)


    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()


    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)



    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)

    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)

    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.

    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\

    #먼저 서울이 아니면 다음 for문으로 넘기기

    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('여수',checkSeoul==FALSE)){
      next
    }

    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))

    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))

    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))

    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))

    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))

    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))


    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)

    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.

    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균

    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기

    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)

    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){
      max_gender[d]='F'
    }


    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율


    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)

    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)

      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }



    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)

    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4


    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()

    public_num[d]=as.numeric(gsub(',','',pu[1]))

    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))

    #의료/복지 시설 수
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))

    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))

    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))

    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    #
    remDr$close()
  }},silent=FALSE)
storefinal=data.frame(total_store,
                      food_num,
                      service_num,
                      sale_num,
                      live_num,
                      work_num,
                      move_num,
                      max_gender,
                      AGE1_ratio,
                      AGE2_ratio,
                      AGE3_ratio,
                      AGE4_ratio,
                      AGE5_ratio,
                      AGE6_ratio,
                      income_live,
                      income_work,
                      consum_live,
                      consum_work,
                      public_num,
                      bank_num,
                      hospital_num,
                      school_num,
                      traffic_num)
str(storefinal)
write.csv(storefinal, 'storefinal_incheon.csv') #d=145~
d

# newd=494
# endd=540

newd=d+1
try({
  for(d in newd:endd){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open()
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2)
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2)
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2)
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료

    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료

    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.

    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료


      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)

      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()

      item13<- url_item12 %>% html_nodes('.layer-pop-body')

      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()

      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){
        remDr$close
        next
      }else{ #주소 오류 안뜰때

        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()

        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.

        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)

      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)


      seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        # seoul_i=grep("서울특별시",item2)[1]
        # if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }


    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택

    #putButton에 들어갈 xpath의 숫자 구하기

    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>

    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!

    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    #
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }


    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)


    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()


    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)



    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)

    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)

    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.

    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\

    #먼저 서울이 아니면 다음 for문으로 넘기기

    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('여수',checkSeoul==FALSE)){
      next
    }

    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))

    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))

    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))

    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))

    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))

    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))


    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)

    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.

    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균

    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기

    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)

    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){
      max_gender[d]='F'
    }


    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율


    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)

    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)

      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }



    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)

    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4


    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()

    public_num[d]=as.numeric(gsub(',','',pu[1]))

    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))

    #의료/복지 시설 수
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))

    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))

    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))

    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    #
    remDr$close()
  }},silent=FALSE)
write.csv(storefinal, 'storefinal_incheon.csv') #d=145~
d




newd=d+1
try({
  for(d in newd:endd){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open()
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2)
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2)
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2)
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료

    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료

    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.

    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료


      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)

      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()

      item13<- url_item12 %>% html_nodes('.layer-pop-body')

      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()

      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){
        remDr$close
        next
      }else{ #주소 오류 안뜰때

        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()

        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.

        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)

      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)


      seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        seoul_i=grep("서울특별시",item2)[1]
        if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        seoul_i=grep("서울특별시",item2)[1]
        if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }


    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택

    #putButton에 들어갈 xpath의 숫자 구하기

    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>

    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!

    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    #
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }


    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)


    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()


    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)



    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)

    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)

    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.

    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\

    #먼저 서울이 아니면 다음 for문으로 넘기기

    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('서울특별시',checkSeoul==FALSE)){
      next
    }

    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))

    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))

    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))

    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))

    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))

    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))


    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)

    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.

    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균

    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기

    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)

    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){
      max_gender[d]='F'
    }


    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율


    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)

    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)

      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }



    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)

    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4


    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()

    public_num[d]=as.numeric(gsub(',','',pu[1]))

    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))

    #의료/복지 시설 수
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))

    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))

    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))

    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    #
    remDr$close()
  }},silent=FALSE)
write.csv(storefinal, 'storefinal_incheon.csv') #d=145~
d

newd=d+1
try({
  for(d in newd:endd){ 	#d:대여소 개수(1540개)
    ###1. 소상공인 상권정보시스템 로그인 (한 번만)###
    remDr<-remoteDriver(port=4445L,browserName="chrome") #처음 킬 때 포트지정 및 어떤 플랫폼을 사용할지
    remDr$open()
    remDr$navigate("http://sg.sbiz.or.kr/index.sg?supDev=1#/analy/mainD/") #입력주소로 이동
    Sys.sleep(5)
    loginButton<-remDr$findElement(using="xpath",value='//*[@id="body"]/div/div/div/div/div[3]/div/a')
    loginButton$clickElement() ; Sys.sleep(2)
    IDButton<-remDr$findElement(using="xpath",value='//*[@id="id"]')
    IDButton$clickElement() ; Sys.sleep(2)
    IDButton$sendKeysToElement(list('ej2747'))
    PWButton<-remDr$findElement(using="xpath",value='//*[@id="pass"]')
    PWButton$clickElement() ; Sys.sleep(2)
    PWButton$sendKeysToElement(list('dmswl0235!'))
    #로그인하기
    findButton<-remDr$findElement(using="xpath",value='/html/body/div/div[2]/form/div/button')
    findButton$clickElement(); Sys.sleep(10) #로그인 완료

    ###2. 상권분석-1단계 지역선택###
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    localChose<-remDr$findElement(using="xpath",value='//*[@id="addrSearch"]/div')
    localChose$clickElement(); Sys.sleep(2) #지역선택
    jibunAddr<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/input')
    jibunAddr$clickElement(); Sys.sleep(2) #지번명 버튼 클릭
    jibunAddr$sendKeysToElement(list(doroAddr[d]))
    findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
    findButton$clickElement(); Sys.sleep(5) #주소 검색 완료

    #지번명으로 검색이 안될경우
    url_item1<-remDr$getPageSource()[[1]]
    url_item1<-read_html(url_item1, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
    item1<- url_item1 %>% html_nodes(".addr_list") %>% html_text() #최근 글 목록을 불러온다.

    item1=gsub('\t','',item1)
    if(strsplit(item1,split='\n')[[1]][2]==""){  #지번으로 검색했는데 "검색결과가 없습니다." 뜰 때
      #도로명으로 검색
      doroAddrFind<-remDr$findElement(using="xpath",value='//*[@id="body"]/div[2]/div[2]/div[3]/ng-include/div/div[1]/span[2]/a')
      doroAddrFind$clickElement(); Sys.sleep(5) #도로명 버튼 클릭
      findButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1"]/div[1]/a')
      findButton$clickElement(); Sys.sleep(5) #주소 검색 완료


      url_item11<-remDr$getPageSource()[[1]]
      url_item12<-read_html(url_item11, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

      item12<- url_item12 %>% html_nodes('.addr_list')  %>% as.character()
      item12=gsub('\t','',item12)

      item15<- url_item12 %>% html_nodes('.addr_list') %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()

      item13<- url_item12 %>% html_nodes('.layer-pop-body')

      warn_txt=item13 %>% html_nodes('.text-center') %>% html_text() %>% as.character()

      if(warn_txt[3]=="도로명주소는 띄어쓰기 포함 정확하게 입력하셔야 합니다(도로명+건물번호)"){
        remDr$close
        next
      }else{ #주소 오류 안뜰때

        item14<-url_item12 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text() %>% as.character()

        #도로명으로 검색했는데도 "검색결과가 없습니다." 뜰 때
        if(item14=="검색결과가 없습니다."){next}  #다음 for문으로 넘어가라.

        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }
    }else{ #지번검색이 될 경우 ("검색결과가 없습니다." 안뜰 때)

      url_item2<-remDr$getPageSource()[[1]]
      url_item2<-read_html(url_item2, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.
      item2 <- url_item2 %>% html_nodes('.addr_list')  %>% html_nodes('.ng-scope') %>% html_nodes('.ng-binding') %>% html_text()
      item2
      addr_num=length(item2)


      seoul_i=0
      #주소가 여러 건 검색됐을 경우
      if(addr_num>1){
        #서울특별시가 포함된 주소 버튼 누르기
        seoul_i=grep("서울특별시",item2)[1]
        if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        #selector일경우 : #jsLnbCont1_1 > ul > li:nth-child(2) > a
        #xpath인 경우 : //*[@id="jsLnbCont1_1"]/ul/li[2]/a
        #selector인경우 : #jsLnbCont1_1 > ul > li:nth-child(3) > a
        xpath1='//*[@id="jsLnbCont1_1"]/ul/li['
        xpath2=']/a'
        xpath_v=paste0(xpath1,seoul_i+1,xpath2)
        checkButton<-remDr$findElement(using="xpath",value=xpath_v) #as.character('//*[@id="jsLnbCont1_1"]/ul/li[18]/a')
        checkButton$clickElement(); Sys.sleep(5)
      }else{ #주소1건만 검색됐을 경우
        seoul_i=grep("서울특별시",item2)[1]
        if(seoul_i==0){next} #주소가 서울이 아닐경우 넘기기
        checkButton<-remDr$findElement(using="xpath",value='//*[@id="jsLnbCont1_1"]/ul/li[2]/a') #구냥 그 하나 나온결과 누르깅.ㅎㅎ
        checkButton$clickElement(); Sys.sleep(5)
      }
    }


    ###2단계 영역선택###
    areaChoose<-remDr$findElement(using="xpath",value='//*[@id="btnDrawRadius"]')
    areaChoose$clickElement(); Sys.sleep(10) #반경선택

    #putButton에 들어갈 xpath의 숫자 구하기

    #<g id="OpenLayers.Layer.Vector_32_vroot"><image id="OpenLayers.Geometry.Point_148" cx="359.49999999895226" cy="315.00000000049477" r="1" preserveAspectRatio="none" x="331" y="254" width="56" height="61" xlink:href="/images/new/ico_locate1.png" style="opacity: 1" fill="#000000" fill-opacity="1" stroke="#000000" stroke-opacity="1" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" cursor="pointer"></image></g>

    url_item3<-remDr$getPageSource()[[1]]
    url_item3<-read_html(url_item3, encoding="UTF-8") #url에서 html파일을 읽어오고 저장한다.

    item3<- url_item3 %>% html_nodes('image') %>% html_attr('id') %>% as.character()
    # check=url_item3 %>% html_nodes('.layer-pop.layer-pop-md') %>% html_text()
    #//*[@id="OpenLayers.Geometry.Point_656"]
    #//*[@id="OpenLayers.Geometry.Point_655"]
    #숫자만 가져오기
    #step2_num=strsplit(item2,split="_")[[1]][2] #문자형
    # if(remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')))
    putButton<-remDr$findElement(using="xpath",value=paste0('//*[@id="',item3,'"]')) #//*[@id="OpenLayers.Geometry.Point_509"]
    putButton$clickElement(); Sys.sleep(3)
    okButton<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_radius_ok"]')
    okButton$clickElement(); Sys.sleep(3)
    ok2Button<-remDr$findElement(using="xpath",value='//*[@id="btn_draw_ma_ok"]')
    ok2Button$clickElement(); Sys.sleep(3) #반경선택까지 완료!

    ##영역선택에서 선택된 영역이 없다고 뜨는 오류 처리
    # html_nodes(url_item3,xpath='//*[@id="messagePopup_msg"]') %>% html_text()
    #
    # if(length(url_item3 %>% html_nodes('.layer-pop-body') %>% html_nodes('.text-center'))>=3){
    #   next
    # }


    ###3단계 업종선택###
    seleckStore<-remDr$findElement(using="xpath",value='//*[@id="upjongSelect"]/div/a')
    seleckStore$clickElement(); Sys.sleep(20)


    ##음식으로 업종 선택해서 상권분석결과 보기##
    seleckStore=c()


    xpathValue=paste('//*[@id="up1Tab_Q"]/ul/li[1]/ul/li[1]/label/span')
    seleckStore<-remDr$findElement(using="xpath",value=xpathValue)
    seleckStore$clickElement(); Sys.sleep(2)



    ok3Button<-remDr$findElement(using='xpath',value='//*[@id="LayerPopCategory"]/div[2]/a[1]')
    ok3Button$clickElement(); Sys.sleep(2)

    analysisButton<-remDr$findElement(using='xpath',value='//*[@id="body"]/div[2]/div[2]/div[1]/div[4]/span[2]/a')
    analysisButton$clickElement(); Sys.sleep(100)

    #****************************************************************#
    #**************************************************************#
    ####마지막. 결과보고서에서 숫자데이터 가져오기####
    url_item_final<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final<-read_html(url_item_final, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #57까지 2.지역별 평가지수 추이 표 보여줌.

    #d는 음식점 선택에 따라 바뀌지 않는 값들 저장하는 데이터프레임의 index\

    #먼저 서울이 아니면 다음 for문으로 넘기기

    checkSeoul=html_nodes(url_item_final,xpath='//*[@id="analysisResultinfo"]/div/table[1]/tbody/tr/td[1]') %>% html_text(); Sys.sleep(2)
    if(grepl('서울특별시',checkSeoul==FALSE)){
      next
    }

    #서울이 맞으면...
    ##상권평가 탭
    item_final<- url_item_final %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final
    #전체업소수
    total_store[d]=as.numeric(gsub(',','',item_final[1]))

    #음식점 수
    food_num[d]=as.numeric(gsub(',','',item_final[2]))

    #서비스업소수
    service_num[d]=as.numeric(gsub(',','',item_final[3]))

    #도/소매 업소수
    sale_num[d]=as.numeric(gsub(',','',item_final[4]))

    #인구수-주거
    live_num[d]=as.numeric(gsub(',','',item_final[8]))

    #인구수-직장
    work_num[d]=as.numeric(gsub(',','',item_final[9]))


    ##인구분석탭
    populButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[4]/a')
    populButton$clickElement(); Sys.sleep(2)

    ##인구분석탭의 html가져오기
    url_item_final2<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final2<-read_html(url_item_final2, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final2<- url_item_final2 %>% html_nodes(".text-right") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    item_final2 #5. 주거형태 -단지규모 및 면적별 현황 끝까지 보여줌.

    #유동인구 - 주말/주중 유동인구 평균
    we=as.numeric(gsub(',','',item_final2[113])) #주말 유동인구 총합
    mean_we=we/2 #주말 유동인구 평균
    wd=as.numeric(gsub(',','',item_final2[114])) #주중 유동인구 총합
    mean_wd=wd/5 #주중 유동인구 평균

    #주말+주중 유동인구 평균
    move_num[d]=round(((mean_wd + mean_we) / 2),0) #정수부분까지만 나타내기

    #유동인구-성별, 더 많은 성별 factor형
    male=as.numeric(gsub(',','',item_final2[84])) #남성 유동인구수(하루)
    female=as.numeric(gsub(',','',item_final2[85])) #여성 유동인구수(하루)

    if(male>female){
      max_gender[d]='M'
    }
    if(male<female){
      max_gender[d]='F'
    }


    #유동인구-연령별, 연령별 유동인구수 비율
    AGE1_ratio[d]=as.numeric(gsub('%','',item_final2[94])) #10대 유동인구 비율
    AGE2_ratio[d]=as.numeric(gsub('%','',item_final2[95])) #20대 유동인구 비율
    AGE3_ratio[d]=as.numeric(gsub('%','',item_final2[96])) #30대 유동인구 비율
    AGE4_ratio[d]=as.numeric(gsub('%','',item_final2[97])) #40대 유동인구 비율
    AGE5_ratio[d]=as.numeric(gsub('%','',item_final2[98])) #50대 유동인구 비율
    AGE6_ratio[d]=as.numeric(gsub('%','',item_final2[99])) #60대 이상 유동인구 비율


    ##소득/소비 탭
    moneyButton<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[5]/a')
    moneyButton$clickElement(); Sys.sleep(2)

    ##소득/소비탭의 html가져오기
    url_item_final3<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final3<-read_html(url_item_final3, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    item_final3<- url_item_final3  %>% html_nodes(".bgpoint3") %>% html_text()
    item_final3
    str(item_final3)
    #//*[@id="resultIncome"]/table[4]/tbody/tr[2]/td[1]
    #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[2]
    #동이 2개 이상이면..
    length(item_final3)
    tmp=((length(item_final3)-43)/2+1)
    if(tmp==0){next}
    inc_live=0;inc_work=0;cons_live=0;cons_work=0
    for(i in 1:tmp){
      #소득평균-주거인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[1]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[2]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_live=inc_live+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #(((295+343)/2+(296+344)/2)/2+((301+349)/2+(302+352)/2)/2)

      #//*[@id="resultIncome"]/table[4]/tbody/tr[1]/td[3]
      #소득평균-직장인구 소득 평균
      inc1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[3]')) %>% html_text()
      inc1=gsub('\n','',inc1)
      inc1=gsub('\t','',inc1)
      inc1=gsub(' ','',inc1)
      inc2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[4]/tbody/tr[',i,']/td[4]')) %>% html_text()
      inc2=gsub('\n','',inc2)
      inc2=gsub('\t','',inc2)
      inc2=gsub(' ','',inc2)
      inc_work=inc_work+(((as.numeric(substr(inc1,1,3))+as.numeric(substr(inc1,5,7)))/2)+
                           ((as.numeric(substr(inc2,1,3))+as.numeric(substr(inc2,5,7)))/2))/2

      #//*[@id="resultIncome"]/table[8]/tbody/tr[1]/td[1]
      #소비평균-주거인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[1]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[2]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_live=cons_live+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

      #소비평균-직장인구 소비 평균
      cons1=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[3]')) %>% html_text()
      cons1=gsub('\n','',cons1)
      cons1=gsub('\t','',cons1)
      cons1=gsub(' ','',cons1)
      cons2=html_nodes(url_item_final3,xpath=paste0('//*[@id="resultIncome"]/table[8]/tbody/tr[',i,']/td[4]')) %>% html_text()
      cons2=gsub('\n','',cons2)
      cons2=gsub('\t','',cons2)
      cons2=gsub(' ','',cons2)
      cons_work=cons_work+(((as.numeric(substr(cons1,1,3))+as.numeric(substr(cons1,5,7)))/2)+
                             ((as.numeric(substr(cons2,1,3))+as.numeric(substr(cons2,5,7)))/2))/2

    }
    if(length(item_final3)>44){ #동이 2개 이상이면
      income_live[d]=inc_live/2
      income_work[d]=inc_work/2
      consum_live[d]=cons_live/2
      consum_work[d]=cons_work/2
    }
    if(length(item_final3)<=44){ #동이 1개이면
      income_live[d]=inc_live
      income_work[d]=inc_work
      consum_live[d]=cons_live
      consum_work[d]=cons_work
    }



    ##지역분석 탭
    localAnalysis<-remDr$findElement(using='xpath',value='//*[@id="resultAnalysisForm"]/div/div[3]/div/div/ul[1]/li[6]/a')
    localAnalysis$clickElement(); Sys.sleep(3)

    url_item_final4<-remDr$getPageSource()[[1]]; Sys.sleep(2)
    url_item_final4<-read_html(url_item_final4, encoding="UTF-8"); Sys.sleep(2) #url에서 html파일을 읽어오고 저장한다.
    #item_final4<- url_item_final4 %>% html_nodes(".largenum") %>% html_text(); Sys.sleep(2) #최근 글 목록을 불러온다.
    #item_final4


    #공공기관 수
    pu=html_nodes(url_item_final4,xpath='//*[@id="resultArea"]/table[2]') %>% html_nodes('.largenum') %>% html_text()

    public_num[d]=as.numeric(gsub(',','',pu[1]))

    #금융기관 수
    bank_num[d]=as.numeric(gsub(',','',pu[2]))

    #의료/복지 시설 수
    hospital_num[d]=as.numeric(gsub(',','',pu[3]))

    #학교 수
    school_num[d]=as.numeric(gsub(',','',pu[4]))

    #교통시설 수
    traffic_num[d]=as.numeric(gsub(',','',pu[8]))

    # ##다시 상권분석화면으로 돌아가기
    # returnButton<-remDr$findElement(using='xpath',value='//*[@id="top_menu"]/ul/li[2]/a')
    # returnButton$clickElement(); Sys.sleep(2)
    # return2Button<-remDr$findElement(using='xpath',value='//*[@id="OpenLayers.Layer.Vector_539_svgRoot"]')
    # return2Button$clickElement(); Sys.sleep(2)
    #
    remDr$close()
  }},silent=FALSE)
storefinal=data.frame(total_store,
                      food_num,
                      service_num,
                      sale_num,
                      live_num,
                      work_num,
                      move_num,
                      max_gender,
                      AGE1_ratio,
                      AGE2_ratio,
                      AGE3_ratio,
                      AGE4_ratio,
                      AGE5_ratio,
                      AGE6_ratio,
                      income_live,
                      income_work,
                      consum_live,
                      consum_work,
                      public_num,
                      bank_num,
                      hospital_num,
                      school_num,
                      traffic_num)
str(storefinal)
write.csv(storefinal, 'storefinal_incheon.csv') #d=145~
d



beep(5)
# ?beep
# 
# ########################################################
# #여기까지 돌리기
# #######################################################
# doroAddr[d]
# 
# total_store[d]
# food_num[d]
# service_num[d]
# sale_num[d]
# live_num[d]
# work_num[d]
# move_num[d]
# max_gender[d]
# AGE1_ratio[d]
# AGE2_ratio[d]
# AGE3_ratio[d]
# AGE4_ratio[d]
# AGE5_ratio[d]
# AGE6_ratio[d]
# income_live[d]
# income_work[d]
# consum_live[d]
# consum_work[d]
# public_num[d]
# bank_num[d]
# hospital_num[d]
# school_num[d]
# traffic_num[d]
# 
# d
# #전체 값 확인
# total_store
# food_num
# service_num
# sale_num
# live_num
# work_num
# move_num
# max_gender
# AGE1_ratio
# AGE2_ratio
# AGE3_ratio
# AGE4_ratio
# AGE5_ratio
# AGE6_ratio
# income_live
# income_work
# consum_live
# consum_work
# public_num
# bank_num
# hospital_num
# school_num
# traffic_num
# d
# 
# 
# total_store= total_store[-307]
# food_num= food_num[-307]
# service_num= service_num[-307]
# sale_num= sale_num[-307]
# live_num= live_num[-307]
# work_num= work_num[-307]
# move_num= move_num[-307]
# max_gender= max_gender[-307]
# AGE1_ratio= AGE1_ratio[-307]
# AGE2_ratio= AGE2_ratio[-307]
# AGE3_ratio=AGE3_ratio[-307]
# AGE4_ratio=AGE4_ratio[-307]
# AGE5_ratio=AGE5_ratio[-307]
# AGE6_ratio=AGE6_ratio[-307]
# income_live=income_live[-307]
# income_work=income_work[-307]
# consum_live=consum_live[-307]
# consum_work=consum_work[-307]
# public_num=public_num[-307]
# bank_num=bank_num[-307]
# hospital_num=hospital_num[-307]
# school_num=school_num[-307]
# traffic_num=traffic_num[-307]
# 
# #이 세값들은 핀이 동일한 곳에 찍힘.
# doroAddr[141]
# doroAddr[142]
# doroAddr[143]
# 
# storefinal=data.frame(total_store,
#       food_num,
#       service_num,
#       sale_num,
#       live_num,
#       work_num,
#       move_num,
#       max_gender,
#       AGE1_ratio,
#       AGE2_ratio,
#       AGE3_ratio,
#       AGE4_ratio,
#       AGE5_ratio,
#       AGE6_ratio,
#       income_live,
#       income_work,
#       consum_live,
#       consum_work,
#       public_num,
#       bank_num,
#       hospital_num,
#       school_num,
#       traffic_num)
# str(storefinal)
# tail(storefinal)
# max_gender[which(max_gender=="F")]
# str(c(total_store,
#       food_num,
#       service_num,
#       sale_num,
#       live_num,
#       work_num,
#       move_num,
#       max_gender,
#       AGE1_ratio,
#       AGE2_ratio,
#       AGE3_ratio,
#       AGE4_ratio,
#       AGE5_ratio,
#       AGE6_ratio,
#       income_live,
#       income_work,
#       consum_live,
#       consum_work,
#       public_num,
#       bank_num,
#       hospital_num,
#       school_num,
#       traffic_num))
# 
# 
# 
# # #맨 마지막에 저장
# # write.csv(total_store,'total_store_seoul3.csv')
# # write.csv(food_num,'food_num_seoul3.csv')
# # write.csv(service_num,'service_num_seoul3.csv')
# # write.csv(live_num,'live_num_seoul3.csv')
# # write.csv(work_num,'work_num_seoul3.csv')
# # write.csv(move_num,'move_num_seoul3.csv')
# # write.csv(max_gender,'max_gender_seoul3.csv')
# # write.csv(AGE1_ratio,'AGE1_ratio_seoul3.csv')
# # write.csv(AGE2_ratio,'AGE2_ratio_seoul3.csv')
# # write.csv(AGE3_ratio,'AGE3_ratio_seoul3.csv')
# # write.csv(AGE4_ratio,'AGE4_ratio_seoul3.csv')
# # write.csv(AGE5_ratio,'AGE5_ratio_seoul3.csv')
# # write.csv(AGE6_ratio,'AGE6_ratio_seoul3.csv')
# # write.csv(income_live,'income_live_seoul3.csv')
# # write.csv(income_work,'income_work_seoul3.csv')
# # write.csv(consum_live,'consum_live_seoul3.csv')
# # write.csv(consum_work,'consum_work_seoul3.csv')
# # write.csv(public_num,'public_num_seoul3.csv')
# # write.csv(bank_num,'bank_num_seoul3.csv')
# # write.csv(hospital_num,'hospital_num_seoul3.csv')
# # write.csv(school_num,'school_num_seoul3.csv')
# # write.csv(traffic_num,'traffic_num_seoul3.csv')
# write.csv(storefinal, 'storefinal7.csv') #storefinal2 : d=145~ storefinal4 : d=262~
# d #144 (0720 오후 8시) 262 (0721 오후 12시)
# 
# 
# 
# storefinal=list()
# ##모은 데이터들 불러오기
# for(i in 3:7){
#   storefinal[[i]]=fread(paste0('storefinal',i,'.csv'))
# }
# 
# str(storefinal)
# str(storefinal[[3]])
# rnum=c(); enum=c()
# for(i in 3:7){
#   storefinal[[i]]=na.omit(storefinal[[i]]) #145행부터~
#   sf=storefinal[[i]]
#   rnum[i]=sf[1,1][1] #값 처음 들어간 행 번호추출
#   enum[i]=sf[nrow(sf),1][1]
# }
# 
# rnum
# enum
# 
# final=rbind(storefinal[[3]],storefinal[[4]],storefinal[[6]],storefinal[[7]])
# str(final)
# 
# write.csv(final,'C:/Users/User/Desktop/인천시빅데이터경진대회/final.csv')
# 
# tss=fread('total_store_seoul.csv')
# str(tss)
# sum(is.na(tss)) #68
# tss2=fread('total_store_seoul2.csv') #87부터
# str(tss2)
# #tss2=tss2[87:,'']
# sum(is.na(tss2)) #68
# 
# 
# #파일명
# 
# filename=c('total_store',
#   'food_num',
#   'service_num',
#   'sale_num',
#   'live_num',
#   'work_num',
#   'move_num',
#   'max_gender',
#   'AGE1_ratio',
#   'AGE2_ratio',
#   'AGE3_ratio',
#   'AGE4_ratio',
#   'AGE5_ratio',
#   'AGE6_ratio',
#   'income_live',
#   'income_work',
#   'consum_live',
#   'consum_work',
#   'public_num',
#   'bank_num',
#   'hospital_num',
#   'school_num',
#   'traffic_num')
# filename
# df.final=tss
# str(df.final)
# for(i in 2:length(filename)){
#   c=fread(paste0(filename[i],'_seoul',2,'.csv'))
#   df.final=cbind(df.final,c)
# }
# str(df.final)
# 
# 
# 
