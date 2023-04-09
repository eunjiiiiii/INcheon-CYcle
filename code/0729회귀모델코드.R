library(data.table)

setwd('C:/Users/User/Documents/카카오톡 받은 파일')


final=fread('realreal수정.csv')
str(final)

final=final[,-c(1:7,34:36)]
colnames(final)
final$area=as.factor(final$area)
final$season=as.factor(final$season)
final$max_gender=as.factor(final$max_gender)
str(final)
final=final[-which(final$user_num==0),]
str(final)

#결측치 제거
final=na.omit(final)
str(final)
write.csv(final,'final최종.csv')

library(corrplot)
windows()
cfinal=final[,-c('area','season','max_gender')]
str(cfinal)

corrplot(cor(cfinal),method='number')

#일단 food_num, service_num, sale_num
final=final[,-c('food_num','service_num','sale_num')]
lm1=lm(user_num ~ ., data=final)
summary(lm1)
library(car)
vif(lm1)

lm2=step(lm1, direction='both')
summary(lm2)
colnames(final)
windows()
par(mfrow=c(2,2))
plot(lm2)


# park_area + total_store + move_num + 
#   max_gender + AGE1_ratio + AGE2_ratio + AGE3_ratio + AGE4_ratio + 
#   income_live + income_work + consum_live + consum_work + bank_num + 
#   hospital_num + school_num + traffic_num + area + season

#이상치 제거 후 회귀 돌리기

str(final)
windows()
pairs(final[,-c('max_gender','area','season')],main="Scatter plot matrix")


