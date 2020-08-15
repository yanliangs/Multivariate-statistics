#查看工作路径
getwd()
#重置工作路径
setwd("D:/Rdata")
#导入CSV数据,有缺失值也可以导入
da12<-read.csv("c2.csv")

#导出数据da2为要导出的数据集，df1.csv为存放的数据集，不要第一???
write.csv(da2,"df1.csv",row.names=F)


#读入数据(设置工作路径)
na1<-read.csv("sleep.csv")
#没有设置，假设数据存在F???
b1<-read.csv("H:\\da2.csv", row.names = 1,head=T)

#判断是否有缺失值在
da1<-c(1,2,5,7,NA,10)
da2<-c("a","b","c",NA,"e","f")
da3<-data.frame(da1,da2)
is.na(da3)
#numbers of NA
sum(is.na(da3))
#缺失值的位置
which (is.na(da3))
#函数apply批量操作,行和列所在的位置
apply(is.na(da3),1,which)
apply(is.na(da3),2,which)

# 处理办法
#???1,删除
da4<-na.omit(da3)
#???2：如果数据波动不大，可以用变量其余值的均值替???
#用均值、给定值替???

da3$da1[is.na(da3$da1)]<-mean(da3$da1,na.rm=T)
install.packages("Hmisc")
library(Hmisc)
x1<-c(1,2,3,0,5,6,2,NA)
y3<-impute(x1,3)#???3替换缺失???

x2<-c(NA,4,5,7,NA,5,6,2)
x<-data.frame(x1,x2)
y1<-impute(x$x1,mean)
y2<-impute(x$x2,mean)

x3<-data.frame(y1,y2)




#???3：多重插???,这里利用数据sleep

install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)
#利用mice包自带的数据sleep

str(sleep)
#缺失变量可视???
md.pattern(sleep)
aggr(sleep,prop=F,number=T)

#利用 mice函数，完成插补的信息放在对象data1的imp???,默认产生5???

data1<-mice(sleep,method='pmm',maxit=100,seed=1234)


#显示整体插补数据
data1$imp
#显示其中某变量Dream的插补数???
data1$imp$Dream
#显示整体数据
result2<-complete(data1)
#判断是否还有缺失???
anyNA(result2)

#判断插补效果
#0代表原始数据???1-5代表5次插补的数据
stripplot(data1,pch=19,cex=0.5,alpha=.3)

#模型判断，Dream与Span，Gest线性关???
model1<-with(data1,lm(Dream~Span+Gest))
pooled1<-pool(model1)
summary(pooled1)

#和删除缺失值之后的模型比较???
model2 <-lm(Dream~Span+Gest,data = sleep,na.action=na.omit)
summary(model2)
#完整数据集和缺失数据集进行线性回归后???
#参数估计和P值基本一致，
#缺失值是完全随机产生的，





#为了最小化模拟误差，我们可以使用更多的插补，例如m=50
data2<- mice(sleep,m=50,seed = 23109)
result2=complete(data2)

#采用同样的方法比???
fit2 <- with(data2,lm(Dream ~ Span+Gest))
pooled2<-pool(fit2)
summary(pooled2)


#随机森林插补
install.packages("randomForest")
library(randomForest)
daimp<-mice(sleep,method="rf",maxit=100,seed=1234)
complete( daimp)
#验证的方法和上面一???

#???4：回归预???
#查看不同列之间的相关性，挑出和缺失值列最相关的列，将含有缺失值列作为
#因变量列，与之最相关的列作为自变量进行回归，然后根据回归方程进行预测填补

mod1<-lm(x1~x3,data=na1)
testdata1<-read.table("clipboard",T)
predict(mod1,newdata=testdata1)
plot(x3,x1)