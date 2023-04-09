install.packages("ggmap")
library(ggmap)
install.packages('data.table')
library(data.table)

setwd('C:/Users/User/Desktop/인천시빅데이터경진대회')


register_google(key = "AIzaSyC61ruAz2JiIINqkHHeRbvLuHkoLJVTSwM")
as.numeric(geocode('인천광역시 계양구 다남로 24'))
