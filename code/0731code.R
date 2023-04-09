install.packages('ggplot2')
library(ggplot2)
install.packages('data.table')
library(data.table)

setwd('C:/Users/User/Desktop/��õ�ú����Ͱ�����ȸ')
df=fread('�ѱ������������_������ ������ ���κι� �½ǰ��� ���ⷮ_20181231.csv')
str(df)

df$�¿�=gsub(',','',df$�¿�)
head(df)
df$����=gsub(',','',df$����)
df$ȭ��=gsub(',','',df$ȭ��)
df$Ư��=gsub(',','',df$Ư��)
head(df)

df$�¿�=as.numeric(df$�¿�)
df$����=as.numeric(df$����)
df$ȭ��=as.numeric(df$ȭ��)
df$Ư��=as.numeric(df$Ư��)

str(df)

CO2=df$�¿�+df$����+df$ȭ��+df$Ư��
head(CO2)

df$�¿�[1]+df$����[1]+df$ȭ��[1]+df$Ư��[1]

df=cbind(df,CO2)
str(df)

colnames(df)[1]="city"
colnames(df)
df$'�� ��'=as.factor(df$'�� ��')
windows()
ggplot(df, aes(df$'�� ��',df$CO2))+geom_bar()+
  ggtitle('����, �λ�, ')

ggplot(df, aes(x=city, y=CO2, fill=city)) +
  geom_bar(stat="identity", colour="black") +   scale_fill_brewer(palette=1) + +  ggtitle("Bar Chart of Frequency by Car Type & Origin_1")
windows()
ggplot(df, aes(x=df$city,y=df$CO2))+geom_bar()
df=df[order(df$CO2,decreasing=TRUE),]
df
order(df$CO2)
b2 <- ggplot(data=df, aes(reorder(city, -CO2), CO2))
b2 + geom_col() + xlab('city') +  ggtitle("���� ���ú� �½ǰ��� ���ⷮ")

df %>%
  ggplot(df, aes(x = city , y = CO2, fill=CO2)) +
  geom_col(width=0.5) +
  ggtitle("���� ���ú� �½ǰ��� ���ⷮ")

library(data.table)
setwd('C:/Users/User/Documents/īī���� ���� ����')
data=fread('â��_�뿩��_����.csv') #�뿩��
str(data)
df=fread('â����+���δ뿩����������+������+�뿩�ݳ��̷�_2018-09.csv')
str(df)
station=unique(data$�뿩��id)
station
rent=c()
return=c()
for(i in 1:length(station)){
rent[i]=nrow(df[which(df$����͹̳�==station[i]),])
return[i]=nrow(df[which(df$�����͹̳�==station[i]),])
}
user_num=rent+return
user_c=data.frame(station, rent, return,user_num)
str(user_c)
head(user_c)

write.csv(user_c,'â�� �̿��ڼ�-9��.csv')