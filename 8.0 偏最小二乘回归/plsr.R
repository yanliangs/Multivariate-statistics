#偏最小二乘回归
##2组变量内相关性较强；2组变量典型相关性强； 
library(pls)
library(PerformanceAnalytics)
chart.Correlation(dd1,method="pearson" ,histogram=TRUE)
library(corrplot)
corrplot(cor(da1[,-c(1:4)]))




#例一：研究小麦产量形状的影响因素

d1<-read.table("clipboard",T)
library(corrplot)
corrplot(cor(d1))

library(pls)
x<-scale(d1[,1:3])
y<-scale(d1[,4:6])

#求最优的成分个数，决定选择几个成分
pls1<-plsr(y~x,validation="LOO",jackknife=T)
summary(pls1,what="all")
#选取不同成分时的RMSEP，结合图形判断选取的成分数
validationplot(pls1)

#选择一个成分，误差最小
pls2<-plsr(y~x,ncomp=1,validation="LOO",jackknife=T)

#偏最小二乘回归方程
coefficients(pls2)
#回归参数的显著性检验,jackknife为后面假设检验做准备
jack.test(pls2)

#转化为一般回归系数

#还原到原始变量的模型系数
#回归系数
cx11<-sd(d1$y3)*coefficients(pls2)[7]/sd(d1$x1)
cx21<-sd(d1$y3)*coefficients(pls2)[8]/sd(d1$x2)
cx31<-sd(d1$y3)*coefficients(pls2)[9]/sd(d1$x3)

#计算截距
cint1<-mean(d1$y3)-cx11*mean(d1$x1)-cx21*mean(d1$x2)-cx31*mean(d1$x3)

#汇总回归系数
m=c(cint1,cx11,cx21,cx31)
#同理可计算其他的回归系数

#计算3个因变量的MSE
predict_pls2<-predict(pls2)
m1<-data.frame(predict_pls2)
y_y<-data.frame(y)
mse_y1<-mean((m1$y1.1.comps-y_y$y1)^2)
#同理计算其他2个因变量的MSE，选择MSE小的分析方法

##画出最终模型效果图,预测值和实际值

da2<-data.frame(y,predict(pls2))
names(da2)<-c("y1","y2","y3","compy1","compy2","compy3")

#画出最终模型效果图,预测值和实际值
predplot(pls2)
library(ggplot2)
ggplot(da2,aes(x=y2, y=compy2))+
  geom_point() + 
  geom_smooth(method ='lm')


#例二：利用数据yarn
da3<-read.csv("yarn1.csv")
yy1<-scale(da3[,1])
xx1<-scale(da3[,2:269])
#判断选择几个成分
pls3<-plsr(yy1~xx1,validation="LOO",jackknife=T)
validationplot(pls3)
#选择4个成分，重新做一次回归
pls4<-plsr(yy1~xx1,4,validation="LOO",jackknife=T)

#由于变量个数较多，
#R控制台显示行数的要求已经不能满足，增加语句
options(max.print=(100000000))
coefficients(pls4)
jack.test(pls4)

#画出最终模型效果图
predplot(pls4)

da3<-data.frame(yy1,predict(pls4))
names(da3)<-c("yy1","py1","py2","py3","py4")
library(ggplot2)
ggplot(da3,aes(x=yy1, y=py4))+
  geom_point() + 
  geom_smooth(method ='lm')

#可计算出该问题的R2
sse11<-sum((da3$yy1-da3$py4)^2)
sst11<-sum((da3$yy1-mean(da3$yy1))^2)
r2<-(sst11-sse11)/sst11
r2




#例三：主成分回归，利用以前例子
d2<-read.table("clipboard",T)
d3<-scale(d2)
#确定主成分的个数
d4<-data.frame(d3)
pcr1<- pcr(y~.,scale=TRUE,validation="LOO",data=d4,jackknife=T)
validationplot(pcr1)

#确定成分后重新做一次主成分回归
pcr2<- pcr(y~.,2,scale=TRUE,validation="CV",data=d4,jackknife=T)
coefficients(pcr2)
#显著性检验
jack.test(pcr2)
pcr2$fitted.values
predplot(pcr2)

#计算R2
da4<-data.frame(d4$y,predict(pcr2))
names(da4)<-c("y","py1","py2")
sse1<-sum((da4$y-da4$py2)^2)
sst1<-sum((da4$y-mean(da4$y))^2)
rsquare1<-(sst1-sse1)/sst1



#还原到原始变量的模型系数
#回归系数
cx1<-sd(d2$y)*coefficients(pcr2)[1]/sd(d2$x1)
cx2<-sd(d2$y)*coefficients(pcr2)[2]/sd(d2$x2)
cx3<-sd(d2$y)*coefficients(pcr2)[3]/sd(d2$x3)

#计算截距
cint<-mean(d2$y)-cx1*mean(d2$x1)-cx2*mean(d2$x2)-cx3*mean(d2$x3)

#汇总回归系数
c(cint,cx1,cx2,cx3)




##例三问题若用Plsr分析：
xx2<-d3[,1:3]
yy2<-d3[,4]
pls11<-plsr(yy2~xx2,validation="LOO",jackknife=T)
summary(pls11)
pls12<-plsr(yy2~xx2,2,validation="LOO",jackknife=T)

#回归系数
coefficients(pls12)
#系数显著性检验
jack.test(pls12)
#画出最终模型效果图,预测值和实际值
predplot(pls12)
#同样可以用ggplot2画图，这里略去



#计算R2
#残差平方和总平方和：

da5<-data.frame(yy2,predict(pls12))
names(da5)<-c("yy2","comp1","comp2")

options(digits=6)
sse2<-sum((da5$yy2-da5$comp2)^2)
sst2<-sum((da5$yy2-mean(da5$yy2))^2)
rsquare2<-(sst2-sse2)/sst2


##可比较RMSEP和R2的大小，决定选择的方法

