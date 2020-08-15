#相关分析
#1.求相关系数
  
d1<-read.csv("LungCap.csv",T)
#两个连续变量的person相关检验,利用数据LungCap
cor.test(Age,LungCap)
cor(da1)

#等级变量间的相关。Smoke是字符串，首先变为因子再转化为数值
m<-as.factor(Smoke)
n<-as.numeric(m)
#两个变量的等级相关检验
cor.test(LungCap,n,method="spearman",exact=F)
cor.test(LungCap,n,method="kendall",exact=F)

#3.多个变量相关性检验
#2个变量见显著性检验
cor.test(Age,LungCap)

#3.多个变量相关性检验
library(psych)
corr.test(da1)
#较全面的可视化,分布、相关系数、散点图及显著性
library(PerformanceAnalytics)
chart.Correlation(da1,method="pearson" ,histogram=TRUE)

#4.偏相关分析
library(ggm)
#以lungca数据集为例，将第三个变量的影响除去
pcor(c(1,2,3),var(da2[,1:3]))
#在数据集marks中，控制第4,5，求2,3间
data(marks)
pcor(c(2,3,4,5), var(marks))
pcor(c(2,3,1), var(marks))
pcor.test(pcor(c(2,3,1), var(marks)), 1, n=88)#控制一个变量
pcor.test(pcor(c(2,3,4,5), var(marks)), 2, n=88)#控制一个变量


#典型相关分析
library(candisc)
library(corrplot)
library(heplots)
#wheat数据集
da1<-read.table("clipboard",T)
head(da1,2)


#相关性可视化,corelation graph
#法一：利用函数corrplot
corrplot(cor(da1),method="ellipse",addCoef.col = T)
corrplot(cor(da1),type="lower",diag=F,tl.col="black",tl.srt=45)

#法三：利用函数chart.Correlation
library(PerformanceAnalytics)
chart.Correlation(da1,method="pearson" ,histogram=TRUE)

#法二：利用GGally中的函数ggpairs
#显示单变量间的相关系数，数据框为原数据 
library(GGally)
ggpairs(da1)


#典型相关分析
library(candisc)
x<-da1[,1:3]
y<-da1[,4:6]
x1<-scale(x)
y1<-scale(y)
can1<-cancor(x1,y1,set.names = c("x","y"))

#第一对典型变量得分图
plot(can1,smooth=T)
#典型相关系数及检验
can1
#标准化典型系数,由此写出典型变量
can1$coef

#典型变量得分
can1$scores


#典型变量散点图，观察其相关关系
par(mfrow=c(1,2))
plot(can1,which=1,smooth=T)
plot(can1,which=2,smooth=T)
plot(can1,which=3,smooth=T)
par(mfrow=c(1,1))

#典型相关系数显著性检验
corcoef.test<-function(r,n,p,q,alpha=0.001){
  m<-length(r);Q<-rep(0,m);lambda<-1
   for (k in m:1){
    lambda<-lambda*(1-r[k]^2);
    Q[k]<- -log(lambda)
     }
   s<-0;i<-m
   for (k in 1:m){
    Q[k]<-(n-k+1-1/2*(p+q+3)+s)*Q[k]
    chi<-1-pchisq(Q[k],(p-k+1)*(q-k+1))
    if (chi>alpha){
       i<-k-1;break
      }
     s<-s+1/r[k]^2
    }
     i
  }
 corcoef.test(r=can1$cancor,n=27,p=3,q=3)

 d2<-read.csv("Full.AAUP.csv",T)




#疫情分布可视化图
library(treemap)
cov1<-read.table("clipboard",T)
treemap(cov1,title="covID",index="country1",type="index",vSize = "confirm",vColor="death",aspRatio=2,palette= "RdYlGn",inflate.labels = T)
treemap(cov1,title="covID",index="country1",type="index",vSize = "death",vColor="confirm",aspRatio=2,palette= "RdYlGn",inflate.labels = T)

