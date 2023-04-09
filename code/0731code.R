install.packages('ggplot2')
library(ggplot2)
install.packages('data.table')
library(data.table)

setwd('C:/Users/User/Desktop/인천시빅데이터경진대회')
df=fread('한국교통안전공단_지역별 차종별 도로부문 온실가스 배출량_20181231.csv')
str(df)

df$승용=gsub(',','',df$승용)
head(df)
df$승합=gsub(',','',df$승합)
df$화물=gsub(',','',df$화물)
df$특수=gsub(',','',df$특수)
head(df)

df$승용=as.numeric(df$승용)
df$승합=as.numeric(df$승합)
df$화물=as.numeric(df$화물)
df$특수=as.numeric(df$특수)

str(df)

CO2=df$승용+df$승합+df$화물+df$특수
head(CO2)

df$승용[1]+df$승합[1]+df$화물[1]+df$특수[1]

df=cbind(df,CO2)
str(df)

colnames(df)[1]="city"
colnames(df)
df$'구 분'=as.factor(df$'구 분')
windows()
ggplot(df, aes(df$'구 분',df$CO2))+geom_bar()+
  ggtitle('서울, 부산, ')

ggplot(df, aes(x=city, y=CO2, fill=city)) +
  geom_bar(stat="identity", colour="black") +   scale_fill_brewer(palette=1) + +  ggtitle("Bar Chart of Frequency by Car Type & Origin_1")
windows()
ggplot(df, aes(x=df$city,y=df$CO2))+geom_bar()
df=df[order(df$CO2,decreasing=TRUE),]
df
order(df$CO2)
b2 <- ggplot(data=df, aes(reorder(city, -CO2), CO2))
b2 + geom_col() + xlab('city') +  ggtitle("전국 도시별 온실가스 배출량")

df %>%
  ggplot(df, aes(x = city , y = CO2, fill=CO2)) +
  geom_col(width=0.5) +
  ggtitle("전국 도시별 온실가스 배출량")

library(data.table)
setwd('C:/Users/User/Documents/카카오톡 받은 파일')
data=fread('창원_대여소_공원.csv') #대여소
str(data)
df=fread('창원시+무인대여공영자전거+누비자+대여반납이력_2018-09.csv')
str(df)
station=unique(data$대여소id)
station
rent=c()
return=c()
for(i in 1:length(station)){
rent[i]=nrow(df[which(df$출발터미널==station[i]),])
return[i]=nrow(df[which(df$도착터미널==station[i]),])
}
user_num=rent+return
user_c=data.frame(station, rent, return,user_num)
str(user_c)
head(user_c)

write.csv(user_c,'창원 이용자수-9월.csv')