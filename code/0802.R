library(data.table)

setwd('C:/Users/User/Documents/카카오톡 받은 파일/창원 user/창원 user')

user=list()
month=c(3,4,5,7,8,9,10,11,12)
for(i in month){
  user[[i]]=fread(paste0('창원 이용자수-',i,'월.csv'))
  user[[i]]=cbind(user[[i]],month=rep(i,nrow(user[[i]])))
}

str(user)

user[[3]]


setwd('C:/Users/User/Documents/카카오톡 받은 파일')
data=fread('realreal수정.csv')
str(data)

#user 데이터 프레임으로 합치기
changwon=user[[3]]
month=month[-1]
month
for(i in month){
  changwon=rbind(changwon, user[[i]])
}
str(changwon)
head(changwon)
tail(changwon)
tail(user[[12]])
area=rep("창원",nrow(changwon))
length(area)

changwon=cbind(changwon,area)
str(changwon)

head(data,2)
data=data[,-c(1,5,6,7)]
str(data) 
data[1,28] #28열 : traffic_num

idx=which(as.numeric(changwon[1,'station'])==data$STATION_NO & data$area=="창원")
final=cbind(data[which(as.numeric(changwon[1,'station'])==data$STATION_NO & data$area=="창원"),c(1:28)],changwon[1,c(6,5,2:4)])

for(i in 2:nrow(changwon)){
  tmp=cbind(data[which(as.numeric(changwon[i,'station'])==data$STATION_NO & data$area=="창원"),c(1:28)],changwon[i,c(6,5,2:4)])
  final=rbind(final,tmp)
}
final

head(data)
nrow(data) #17315
data=data[-which(data$area=="여수"),]
nrow(data) #17219 
str(data)
unique(data$area)

str(final)
str(data)

season=rep(' ',nrow(final))
final=cbind(final,season)
str(final)
final=final[,-c(34,35)]

final[which(final$month %in% c(12)),'season']='winter'
final[which(final$month %in% c(3,4,5)),'season']='spring'
final[which(final$month %in% c(7,8)),'season']='summer'
final[which(final$month %in% c(9,10,11)),'season']='fall'
unique(final$season)
head(final)
colnames(final);colnames(data)
data=data[,-1]
data=data[,-c(4,5,6)]
finalfinal=rbind(data,final)
finalfinal
nrow(data)+nrow(final)

setwd('C:/Users/User/Documents/카카오톡 받은 파일')
write.csv(finalfinal,'최종 data set.csv')


######################################
subway=fread('한국철도시설공단_공항철도_주소데이터_20191121.csv')
str(subway)

subway=subway[grep('인천',subway$지번주소),]
doroAddr=as.character(subway$도로명주소)
strsplit(doroAddr
        
for(i in 1:length(doroAddr)){
  geocode()
}
