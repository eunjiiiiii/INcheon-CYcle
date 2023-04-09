##여수와 창원 공원데이터 전처리


#install.packages('data.table')
library(data.table)

setwd('C:/Users/User/Desktop/인천시빅데이터경진대회')
park=fread('전국도시공원정보표준데이터.csv')
str(park)

yeosu=park[grep('여수',park$'제공기관명'),]
str(yeosu)

write.csv(yeosu,'전라남도_여수시_도시공원정보.csv')
