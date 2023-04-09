##데이터 합치기##
install.packages('data.table')
library(data.table)
library(readxl)
setwd('C:/Users/User/Documents/카카오톡 받은 파일')

# data=read_excel('data.xlsx')
# str(data)
data=fread('realreal.csv')
str(data)
sum(is.na(data$sale_num)) #realreal과 real_final구분하는 코드

user_y=fread('bike_여수_대여소정보.csv')
user_c=fread('bike_창원_(2018.06)2.csv')
str(user_y)
str(user_c)

id_c=data[grep('창원',data$address),'STATION_NO']
unique(id_c)
?grepl
i
# ch=data[grep('창원',data$address),]
# str(ch)
for(i in 1:nrow(data)){
  for(j in 1:nrow(user_c)){
    if(grepl('창원',data$address[i])==TRUE & data$STATION_NO[i]== user_c$대여소[j]){
      data$rent[i]=user_c$출발[j]
      data$return[i]=user_c$도착[j]
    }
  }
}

write.csv(data,'final data.csv')

user_y=fread('C:/Users/User/Desktop/인천시빅데이터경진대회/전라남도 여수시_공용자전거 이용현황(위치별, 일별)_20200331.csv')
str(user_y)
head(user_y)

user_y$`대여 Station`= gsub('\\d','',user_y$`대여 Station`)
user_y$`대여 Station`= gsub('[:.:]','',user_y$`대여 Station`)
head(user_y)

user_y$`반납 Station`= gsub('\\d','',user_y$`반납 Station`)
user_y$`반납 Station`= gsub('[:.:]','',user_y$`반납 Station`)
head(user_y)
# user_y$`대여 Station`[1]
# data$STATION_NM[1]

data[which(data$area=='여수'),'rent']=0
data[which(data$area=='여수'),'return']=0
rownums=which(data$area=='여수')
length(rownums)/3 #32행

unique(data[which(data$area=='여수'),'STATION_NM'])

length(unique(user_y$`대여 Station`)) #42행

#user_y 에서 month변수 만들기
str(user_y)
month=c()
for(i in 1:nrow(user_y)){
  month[i]=as.numeric(strsplit(user_y$날짜[i],split='-')[[1]][2])
}
str(month)

user_y=cbind(user_y,month)
str(user_y)

#대여수,반납수 세기
for(i in rownums){
  for(j in 1:nrow(user_y)){
    if(data$STATION_NM[i]==user_y$`대여 Station`[j] & data$month[i]==user_y$month[j] ){
      data$rent[i]=data$rent[i]+user_y$회수[j]
    }
    if(data$STATION_NM[i]==user_y$`반납 Station`[j] & data$month[i]==user_y$month[j] ){
      data$return[i]=data$return[i]+user_y$회수[j]
    }
  }
}


data[which(data$area=="여수" & data$month==2),'rent']

sum(user_y[which(user_y$`대여 Station` =='신동아 파밀리에 삼거리' & user_y$month==1),'회수'])

#user_num update
data$user_num=data$rent+data$return
str(data)

sum(is.na(data$sale_num))

write.csv(data,'realreal수정.csv')







unique(user_y$날짜)

#계절 변수 추가
unique(data$month)

season=rep(NA,nrow(data))
data=cbind(data,season)
str(data)

data[which(data$month %in% c(1,2,12)),'season']='winter'
data[which(data$month %in% c(3,4,5)),'season']='spring'
data[which(data$month %in% c(6,7,8)),'season']='summer'
data[which(data$month %in% c(9,10,11)),'season']='fall'
unique(data$season)
data$season=as.factor(data$season)
str(data)

#이용자수 (user_num) 채우기
data$user_num=data$rent+data$return
str(data)

#서울 1월 2월 데이터 같은지 확인
seoul=data[grep('서울',data$address),]
str(seoul)
m1_seoul=seoul[which(seoul$month==1),]
m2_seoul=seoul[which(seoul$month==2),]

head(m1_seoul[,c('rent','return')])
head(m2_seoul[,c('rent','return')])

#같음


write.csv(data,'final data.csv')

data=fread('final data.csv')
str(data)

park_s=fread('서울시 주요 공원현황.csv')
str(park_s)

group=paste(round(park_s$`Y좌표(WGS84)`,2),'-',round(park_s$`X좌표(WGS84)`,2))
head(group)

park_s=cbind(park_s,group)
str(park_s)

#park_s의 면적변수를 수치형으로 바꾸기!
park_s$면적=gsub('\\W','',park_s$면적)
park_s$면적

#숫자외의 문자도 있는 행들
grep('\\D',park_s$면적) #행번호
park_s[1,'면적']=2896887
park_s[3,'면적']=9132690
park_s[7,'면적']=NA
park_s[9,'면적']=80309
park_s[16,'면적']=61544
park_s[21,'면적']=11302017
park_s[26,'면적']=15000
park_s[51,'면적']=16500
park_s[60,'면적']=219167
park_s[65,'면적']=1447122

unique(park_s$면적)
park_s$면적=as.numeric(park_s$면적)

str(park_s)

park_s=park_s[-which(is.na(park_s$면적)==TRUE),]
str(park_s)

#park_area==NA인 행들 채워넣기!
data[which(is.na(data$park_area)==TRUE),'park_area']=0
unique(data$park_area)
data[which(data$area=="서울"),'park_area']=0
unique(data[which(data$area=="서울"),'park_area'])
unique(data$park_area)

#서울이고, group이 같으면 면적 합하기
for(i in 1:nrow(data)){
  for(j in 1:nrow(park_s)){
    if(data$area[i]=="서울" & data$group_b[i]==park_s$group[j]){
      data$park_area[i]=data$park_area[i]+park_s$면적[j]
    }
  }
}

unique(data[which(data$area=="서울"),'park_area'])
head(data$park_area)

write.csv(data,'final data.csv')
