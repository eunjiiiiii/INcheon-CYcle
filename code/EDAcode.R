library(data.table)
library(ggplot2)


setwd('C:/Users/User/Documents/카카오톡 받은 파일')
data=fread('최종 data set.csv')
str(data)

#user_num update
# data$user_num=data$rent+data$return
# str(data)

# #sale_num NA개수 확인
windows()
ggplot(data, aes(is.na(data)))+geom_bar()+
  ggtitle('데이터셋의 결측치 확인')

#month변수 삭제
data=data[,-'month']
str(data)
colnames(data)
# #결측치(NA) 제거
# 
# data2=na.omit(data)
# str(data2)
# dim(data); dim(data2)  #6328행 제거
# str(data2)
# data2$season=as.factor(data2$season)


#1. 계절별 이용자수 boxplot
windows()
ggplot(data, aes(x=data$season, y=data$user_num, fill=data$season)) +
  geom_boxplot() +
  scale_x_discrete(limits = c('spring','summer','fall','winter'))+
  ggtitle('계절별 이용자 수 boxplot') + xlab('계절') + ylab('이용자수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))
#[출처] [R] ggplot2 그래프 커스텀 (customized) 하기|작성자 베어베어스



windows()
ggplot(data, aes(y=data$user_num)) +
  geom_boxplot() +
  ggtitle('real user_num boxplot')

# sum(is.na(data$public_num)) #498 
# sum(is.na(data$public_num) & is.na(data$food_num)) #177
# 
# 941-177 
# 
# 764/nrow(data)*100 #2%



data$max_gender=as.factor(data$max_gender)

#2. 성별변수(max_gender) barplot
windows()
ggplot(data, aes(max_gender))+geom_bar()+
  ggtitle('유동인구수가 더 많은 성별 barplot') + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))+
xlab('대여소 주변 유동인구수가 더 많은 성별')+ylab('대여소개수')

#3. 지역별(범주 3개) 이용자수 boxplot
windows()
ggplot(data, aes(x=data$area, y=data$user_num, fill=data$area)) + xlab('지역') +ylab('이용자수')+
  geom_boxplot() +
  scale_x_discrete(limits = c('서울','창원'))+
  ggtitle('지역별 이용자 수 boxplot')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))

unique(data$area)
data[which(data$area=='여수'),]

#5.지역별 상권수 plot
#1) 전체상권수 boxplot
windows()
ggplot(data, aes(x=data$area, y=data$total_store, fill=data$area)) + xlab('지역') + ylab('대여소 기준 반경 100m의 전체 상권수') +
  geom_boxplot() +
  scale_x_discrete(limits = c('서울','창원'))+
  scale_color_discrete(name="지역") +
  ggtitle('지역별 대여소 기준 반경 100m의 전체 상권수 boxplot')+
theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))

#2) 전체상권수 내에서 음식점수, 서비스업체수, 도소매업체수 비율
food_r=with(data, food_num/total_store)
service_r=with(data,service_num/total_num)
sale_r=with(data,sale_num/total_num)
str(data)
colSums()

#지역별 상권 Type 비율 - 서울
food_s=colSums(data[which(data$area=="서울"),'food_num'],na.rm=TRUE)
service_s=colSums(data[which(data$area=="서울"),'service_num'],na.rm=TRUE)
sale_s=colSums(data[which(data$area=="서울"),'sale_num'],na.rm=TRUE)

install.packages('RColorBrewer')
library(RColorBrewer) # RColorBrewer 불러오기
windows()
RColorBrewer::display.brewer.all() #팔레트 확인
myPal <- RColorBrewer::brewer.pal(n = 5, name = "Pastel1")

sums=c(food_s, service_s, sale_s)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("지역별 상권 Type 비율","서울"),col=myPal[1:3])
# ggplot(data, aes(x = "", y = "", fill = 답)) + 
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y")


#지역별 상권 Type 비율 - 여수
food_y=colSums(data[which(data$area=="여수"),'food_num'],na.rm=TRUE)
service_y=colSums(data[which(data$area=="여수"),'service_num'],na.rm=TRUE)
sale_y=colSums(data[which(data$area=="여수"),'sale_num'],na.rm=TRUE)



sums=c(food_y, service_y, sale_y)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("지역별 상권 Type 비율","여수"),col=myPal[1:3])

#지역별 상권 Type 비율 - 창원
food_c=colSums(data[which(data$area=="창원"),'food_num'],na.rm=TRUE)
service_c=colSums(data[which(data$area=="창원"),'service_num'],na.rm=TRUE)
sale_c=colSums(data[which(data$area=="창원"),'sale_num'],na.rm=TRUE)



sums=c(food_c, service_c, sale_c)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("지역별 상권 Type 비율","창원"),col=myPal[1:3])


#6.결측치 확인
final=na.omit(data)
str(final)
nrow(data)- nrow(final) #1337




#******************************************인천EDA***********************************#
incheon=fread('incheon최종.csv')
str(incheon)

incheon$max_gender=as.factor(incheon$max_gender)
incheon$season=as.factor(incheon$season)
str(incheon)

incheon=incheon[-which(incheon[,'total_store']>600),]
quantile(incheon$total_store)
iqr=18.5
nrow(incheon[which(incheon$total_store>18.5+1.5*iqr),])
#2. 성별변수(max_gender) barplot
unique(incheon$max_gender)

windows()
ggplot(incheon, aes(max_gender))+geom_bar()+
  ggtitle('유동인구수가 더 많은 성별 barplot-인천')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))+
xlab('대여소 주변 유동인구수가 더 많은 성별')+ylab('대여소개수')


#1) 전체상권수 boxplot
windows()
ggplot(incheon, aes(y=incheon$total_store)) + xlab('인천') + ylab('대여소 기준 반경 100m의 전체 상권수') +
  geom_boxplot() +
  ggtitle('인천의 가상 대여소 기준 반경 100m의 전체 상권수 boxplot')+
  scale_y_continuous(breaks=c(100, 200, 300, 400)) +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))

#2) 전체상권수 내에서 음식점수, 서비스업체수, 도소매업체수 비율

#지역별 상권 Type 비율 - 서울
food_s=colSums(incheon[,'food_num'],na.rm=TRUE)
service_s=colSums(incheon[,'service_num'],na.rm=TRUE)
sale_s=colSums(incheon[,'sale_num'],na.rm=TRUE)

#install.packages('RColorBrewer')
library(RColorBrewer) # RColorBrewer 불러오기
windows()
RColorBrewer::display.brewer.all() #팔레트 확인
myPal <- RColorBrewer::brewer.pal(n = 5, name = "Pastel1")

sums=c(food_s, service_s, sale_s)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("인천시 가상대여소 주변 상권 Type 비율"),col=myPal[1:3])


#소득/소비 플랏

colnames(incheon)
windows()
p1=ggplot(incheon, aes(y=incheon$income_live)) + xlab('인천') + ylab('주거인구 소득') +
  geom_boxplot(fill=myPal[3]) 
p2=ggplot(incheon, aes(y=incheon$income_work, fill=green)) + xlab('인천') + ylab('직장인구 소득') +
  geom_boxplot(fill=myPal[3]) 
p3=ggplot(incheon, aes(y=incheon$consum_live, fill=green)) + xlab('인천') + ylab('주거인구 소비') +
  geom_boxplot(fill=myPal[3]) 
p4=ggplot(incheon, aes(y=incheon$consum_work, fill=green)) + xlab('인천') + ylab('직장인구 소비') +
  geom_boxplot(fill=myPal[3]) 

#install.packages("gridExtra")
library(gridExtra)
  grid.arrange(p1, p3,  p2, p4, ncol=2)




######서울, 창원 인구, 면적 비교플랏######
df=data.frame(지역=c('서울','창원'), 인구=c(9270000,1040118), 면적=c(605.5,747.12))
df

df=t(df)
df


windows()
ggplot(df, aes(x=지역,y=인구))+geom_bar()+
  ggtitle('서울과 창원의 인구수 비교')

ggplot(df, aes(x=지역, fill=지역)) + 
     geom_bar(position="dodge", colour="black") + 
     scale_fill_brewer(palette=1) +
     ggtitle("Bar Chart of Frequency by Car Type & Origin")

p<-ggplot(df,aes(x=지역,y=인구,fill=지역))

p1=p+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=salary-se,ymax=salary+se),
                position="dodge",width=0.2)
p1

