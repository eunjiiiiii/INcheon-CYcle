library(data.table)
library(ggplot2)


setwd('C:/Users/User/Documents/īī���� ���� ����')
data=fread('���� data set.csv')
str(data)

#user_num update
# data$user_num=data$rent+data$return
# str(data)

# #sale_num NA���� Ȯ��
windows()
ggplot(data, aes(is.na(data)))+geom_bar()+
  ggtitle('�����ͼ��� ����ġ Ȯ��')

#month���� ����
data=data[,-'month']
str(data)
colnames(data)
# #����ġ(NA) ����
# 
# data2=na.omit(data)
# str(data2)
# dim(data); dim(data2)  #6328�� ����
# str(data2)
# data2$season=as.factor(data2$season)


#1. ������ �̿��ڼ� boxplot
windows()
ggplot(data, aes(x=data$season, y=data$user_num, fill=data$season)) +
  geom_boxplot() +
  scale_x_discrete(limits = c('spring','summer','fall','winter'))+
  ggtitle('������ �̿��� �� boxplot') + xlab('����') + ylab('�̿��ڼ�')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))
#[��ó] [R] ggplot2 �׷��� Ŀ���� (customized) �ϱ�|�ۼ��� ����



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

#2. ��������(max_gender) barplot
windows()
ggplot(data, aes(max_gender))+geom_bar()+
  ggtitle('�����α����� �� ���� ���� barplot') + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))+
xlab('�뿩�� �ֺ� �����α����� �� ���� ����')+ylab('�뿩�Ұ���')

#3. ������(���� 3��) �̿��ڼ� boxplot
windows()
ggplot(data, aes(x=data$area, y=data$user_num, fill=data$area)) + xlab('����') +ylab('�̿��ڼ�')+
  geom_boxplot() +
  scale_x_discrete(limits = c('����','â��'))+
  ggtitle('������ �̿��� �� boxplot')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))

unique(data$area)
data[which(data$area=='����'),]

#5.������ ��Ǽ� plot
#1) ��ü��Ǽ� boxplot
windows()
ggplot(data, aes(x=data$area, y=data$total_store, fill=data$area)) + xlab('����') + ylab('�뿩�� ���� �ݰ� 100m�� ��ü ��Ǽ�') +
  geom_boxplot() +
  scale_x_discrete(limits = c('����','â��'))+
  scale_color_discrete(name="����") +
  ggtitle('������ �뿩�� ���� �ݰ� 100m�� ��ü ��Ǽ� boxplot')+
theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))

#2) ��ü��Ǽ� ������ ��������, ���񽺾�ü��, ���Ҹž�ü�� ����
food_r=with(data, food_num/total_store)
service_r=with(data,service_num/total_num)
sale_r=with(data,sale_num/total_num)
str(data)
colSums()

#������ ��� Type ���� - ����
food_s=colSums(data[which(data$area=="����"),'food_num'],na.rm=TRUE)
service_s=colSums(data[which(data$area=="����"),'service_num'],na.rm=TRUE)
sale_s=colSums(data[which(data$area=="����"),'sale_num'],na.rm=TRUE)

install.packages('RColorBrewer')
library(RColorBrewer) # RColorBrewer �ҷ�����
windows()
RColorBrewer::display.brewer.all() #�ȷ�Ʈ Ȯ��
myPal <- RColorBrewer::brewer.pal(n = 5, name = "Pastel1")

sums=c(food_s, service_s, sale_s)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("������ ��� Type ����","����"),col=myPal[1:3])
# ggplot(data, aes(x = "", y = "", fill = ��)) + 
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y")


#������ ��� Type ���� - ����
food_y=colSums(data[which(data$area=="����"),'food_num'],na.rm=TRUE)
service_y=colSums(data[which(data$area=="����"),'service_num'],na.rm=TRUE)
sale_y=colSums(data[which(data$area=="����"),'sale_num'],na.rm=TRUE)



sums=c(food_y, service_y, sale_y)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("������ ��� Type ����","����"),col=myPal[1:3])

#������ ��� Type ���� - â��
food_c=colSums(data[which(data$area=="â��"),'food_num'],na.rm=TRUE)
service_c=colSums(data[which(data$area=="â��"),'service_num'],na.rm=TRUE)
sale_c=colSums(data[which(data$area=="â��"),'sale_num'],na.rm=TRUE)



sums=c(food_c, service_c, sale_c)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("������ ��� Type ����","â��"),col=myPal[1:3])


#6.����ġ Ȯ��
final=na.omit(data)
str(final)
nrow(data)- nrow(final) #1337




#******************************************��õEDA***********************************#
incheon=fread('incheon����.csv')
str(incheon)

incheon$max_gender=as.factor(incheon$max_gender)
incheon$season=as.factor(incheon$season)
str(incheon)

incheon=incheon[-which(incheon[,'total_store']>600),]
quantile(incheon$total_store)
iqr=18.5
nrow(incheon[which(incheon$total_store>18.5+1.5*iqr),])
#2. ��������(max_gender) barplot
unique(incheon$max_gender)

windows()
ggplot(incheon, aes(max_gender))+geom_bar()+
  ggtitle('�����α����� �� ���� ���� barplot-��õ')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))+
xlab('�뿩�� �ֺ� �����α����� �� ���� ����')+ylab('�뿩�Ұ���')


#1) ��ü��Ǽ� boxplot
windows()
ggplot(incheon, aes(y=incheon$total_store)) + xlab('��õ') + ylab('�뿩�� ���� �ݰ� 100m�� ��ü ��Ǽ�') +
  geom_boxplot() +
  ggtitle('��õ�� ���� �뿩�� ���� �ݰ� 100m�� ��ü ��Ǽ� boxplot')+
  scale_y_continuous(breaks=c(100, 200, 300, 400)) +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black"))

#2) ��ü��Ǽ� ������ ��������, ���񽺾�ü��, ���Ҹž�ü�� ����

#������ ��� Type ���� - ����
food_s=colSums(incheon[,'food_num'],na.rm=TRUE)
service_s=colSums(incheon[,'service_num'],na.rm=TRUE)
sale_s=colSums(incheon[,'sale_num'],na.rm=TRUE)

#install.packages('RColorBrewer')
library(RColorBrewer) # RColorBrewer �ҷ�����
windows()
RColorBrewer::display.brewer.all() #�ȷ�Ʈ Ȯ��
myPal <- RColorBrewer::brewer.pal(n = 5, name = "Pastel1")

sums=c(food_s, service_s, sale_s)
lbls=c('food','service','sale')
pct=round(sums/sum(sums)*100)
lbls=paste(lbls, pct)
lbls=paste0(lbls, '%')
windows()
pie(sums, labels=lbls, main=c("��õ�� ����뿩�� �ֺ� ��� Type ����"),col=myPal[1:3])


#�ҵ�/�Һ� �ö�

colnames(incheon)
windows()
p1=ggplot(incheon, aes(y=incheon$income_live)) + xlab('��õ') + ylab('�ְ��α� �ҵ�') +
  geom_boxplot(fill=myPal[3]) 
p2=ggplot(incheon, aes(y=incheon$income_work, fill=green)) + xlab('��õ') + ylab('�����α� �ҵ�') +
  geom_boxplot(fill=myPal[3]) 
p3=ggplot(incheon, aes(y=incheon$consum_live, fill=green)) + xlab('��õ') + ylab('�ְ��α� �Һ�') +
  geom_boxplot(fill=myPal[3]) 
p4=ggplot(incheon, aes(y=incheon$consum_work, fill=green)) + xlab('��õ') + ylab('�����α� �Һ�') +
  geom_boxplot(fill=myPal[3]) 

#install.packages("gridExtra")
library(gridExtra)
  grid.arrange(p1, p3,  p2, p4, ncol=2)




######����, â�� �α�, ���� ���ö�######
df=data.frame(����=c('����','â��'), �α�=c(9270000,1040118), ����=c(605.5,747.12))
df

df=t(df)
df


windows()
ggplot(df, aes(x=����,y=�α�))+geom_bar()+
  ggtitle('����� â���� �α��� ��')

ggplot(df, aes(x=����, fill=����)) + 
     geom_bar(position="dodge", colour="black") + 
     scale_fill_brewer(palette=1) +
     ggtitle("Bar Chart of Frequency by Car Type & Origin")

p<-ggplot(df,aes(x=����,y=�α�,fill=����))

p1=p+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=salary-se,ymax=salary+se),
                position="dodge",width=0.2)
p1
