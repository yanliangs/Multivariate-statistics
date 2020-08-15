#距离判别分析
install.packages("WMDB")
library(WMDB)
data1=read.table('clipboard',T)
W2=function(x, mu1, mu2, S){(mahalanobis(x,mu2, S) - mahalanobis(x,mu1, S))} 
mu1=c(3, 10)
mu2=c(4, 7)
S=matrix(c(0.8,-0.8,-0.8,3.2),nrow=2)
x=c(2, 8)
W2(x, mu1,mu2, S) 


#利用著名的鸢尾花数据集
X<-iris[,1:4]
G<-gl(3,50)
#产生3个因子，每个因子重复数50
wmd(X,G)
#X为测试集，G为因子 

#假设每类选取的测试样品不同
set.seed(2021)
#设定随机种子数
m1<-c(sample(1:50,7),sample(51:100,9),sample(101:150,8))
test1<-iris[m1,1:4]
#每组随机选取7,9,8个，共24个样品构成测试集
head(test1)
查看前6行
train<-iris[-m1,1:4]
#去掉测试样本，剩下的作为训练样本
n<-c(1,2,3)
n1<-rep(n,c(43,41,42))
n2<-as.factor(n1)
wmd(TrnX=train,TrnG=n2,TstX=test1)#费歇尔判别分析

# 利用著名的鸢尾花例子
set.seed(2020)
#设定随机种子数
m1<-c(sample(1:50,10),sample(51:100,10),sample(101:150,10))
test1<-iris[m1,1:4]
#每组随机选取5个，共15个构成测试集
head(test1)
#查看前6行
train<-iris[-m1,1:4]
#去掉测试样本，剩下的作为训练样本
n1<-gl(3,40)
wmd(train,n1)
wmd(TrnX=train,TrnG=n1,TstX=test1)
#TrnX训练集，TrnG训练集分类，TstX要预测的数据
wmd(TrnX=train,TrnG=n1)
#如果不给出测试集，就把当前的训练集当做测试集

#mosquito种类判别
data2<-read.table("clipboard",head=T)
n1<-c(1,2)
n2<-rep(n1,c(6,8))
n2
n3<-as.factor(n2)
n4<-factor(n2)
wmd(data2,n3)
test1<-data.frame(c(1.72,2.13),c(1.24,1.45))
wmd(TrnX=data2,TrnG=n3)
test1<-read.table("clipboard",T)
wmd(TrnX=data2,TrnG=n3,TstX=test1)



#fisher 线性判别
library(MASS)
b1<-read.table("clipboard",T)
b3=read.table("clipboard",head=T,sep='\t',row.names=1)


ld1<-lda(G~.,data=b1)
#输出线性判别函数的系数
ld1
#对训练样品回判
z=predict(ld1)
#回判分类结果
newG=z$class
#样品的Fisher得分
z$x
b=cbind(b1$G,newG,z$x)
b
data.frame(b1$G,newG)#看原分类和回判结果
tab1<-table(b1$G,z$class) #看原分类和回判结果汇总
tab1
sum(diag(prop.table(tab1))) #正确分类率

#输入待判样品
testdata<-read.table("clipboard",T)
#待判样品判断
predict(ld1,testdata)
#用plot画出费歇尔得分的领域图
plot(ld1,cex=0.8)
#用ggplot画出费歇尔得分的分类图 
library(ggplot2)
place=rownames(b1)
s=cbind(b1,as.data.frame(z$x))
p=ggplot(cbind(b1,as.data.frame(z$x)) ,aes(x=LD1,y=LD2))
p+geom_point(aes(color=G),alpha=4,size=3)
p+geom_point(aes(color=G),alpha=4,size=3)+geom_text(label=place,cex=3,nudge_y=-0.25)
p+geom_point(aes(color=G),alpha=4,size=4)+geom_text(label=place,cex=4,nudge_y=-0.25)+annotate("text",x=1.56,y=-0.4,label="天津")+annotate("text",x=-0.424,y=-1.94,label="陕西")+labs(title="费歇尔判别结果")+xlim(-4,8)+ylim(-4,4)

set.seed(2021)
m1<-c(sample(1:50,10),sample(51:100,10),sample(101:150,10))
train<-iris[-m1,]
test=iris[m1,]
FisherModel<-lda(Species~.,data=train)
predict(FisherModel)
testpre<-predict(FisherModel,newdata=test[,1:4])
table(test$Species,testpre$class)
plot(FisherModel, col = rep(c('red', 'green', 'blue'), summary(train$Species)), dimen = 2)
ld<-predict(FisherModel)$x
library(ggplot2)
p<-ggplot(cbind(train,as.data.frame(ld)) ,aes(x=LD1,y=LD2))
p+geom_point(aes(colour=Species),alpha=2,size=4)
p+geom_point(aes(colour=Species),alpha=2,size=4)+geom_point(aes(shape = factor(Species)))

#-----------------------------------------------------------------
#如果Fisher判别函数只有一个,分析思路类似蚊子分类
#第一步：查看原始数据情况
library(ggplot2)
c1<-read.table("clipboard",T)
plot(c1$x1,c1$x2)
text(c1$x1,c1$x2,c1$G,adj=0.8,cex=0.5)
ggplot(c1,aes(x1,x2,color=factor(G)))+geom_point(aes(shape=factor(G)),size=3)
#第二步：输出判别函数和回判结果
library(MASS)
ld2<-lda(G~.,data=c1)#如果是部分函数，需要把点换成x1+x2+x3
ld2
z0<-predict(ld2)
z0
#第三步：预测，输出待判别样品的类别和费歇尔函数值
c2<-read.table("clipboard",T)
z<-predict(ld2,c2)
z
tab0=table(c1$G,z0$class) #c1是原数据集，z0是回判结果
tab0
sum(diag(prop.table(tab0)))

#作图
z1<-cbind(c1,z0$x)
library(ggplot2)
#一个变量只能做柱状图，二维多维可以做散点图
##法一盒须图##linetype=5意思是一条虚线，geom_hline水平线，geom_vline是水平线
p<-ggplot(z1,aes(factor(G),LD1))
p+geom_boxplot(color=c("red","blue"))+
  geom_hline(yintercept= 0,linetype=5,color="red")+
  geom_vline(xintercept=1.5,linetype=5,color="red")+
  theme_bw()+ggtitle("my result")
#theme_bw()修改背景
##法二柱状图(横坐标是LD1费舍得分,纵坐标是频数,histogram(直方图),bins就是用来指定分组数目（格子）)
plot(ld2) #plot是单个因子就作图一次,分类作图
ggplot(z1,aes(LD1,fill=factor(G)))+geom_histogram(bins=20)


#---------------------------------------------------------
#Byes判别法
#首先检验协方差矩阵是否相等
install.packages("heplots")
library(heplots)
library(car)
d<-read.table("clipboard",T)
boxM(d[,-5],d$G)

#单变量正态性检验
shapiro.test(iris$Sepal.Width)
qqnorm(iris$Sepal.Width)
ggplot(iris,aes(Sepal.Width,fill=factor(Species)))+geom_density(alpha=0.8)
##单变量正态性检验
###shapiro.test(iris[51:100,]$Sepal.Length)
##qqnorm(iris$Sepal.Width)
##ggplot(iris,aes(Sepal.Width,fill=factor(Species)))+geom_density(alpha=0.8)

#画概率密度曲线

#多元正态检验
library(mvnormtest)
d<-read.table("clipboard",T)
mshapiro.test(t(d[1:50,-5]))
mshapiro.test(t(d[51:150,-5]))
mshapiro.test(t(d[150:255,-5]))
#数据集一定要转换为矩阵，且数据集里面的待验证变量一定要是数值型


#若协方差矩阵相等采用线性函数
m1<-da(factor(G)~.,prior=c(1/3,2/3),data =byes1)
predict(m1)
predict(m1,testdata)

#若协方差矩阵不相等，采用二次判别法
m1<-qda(G~.,prior=c(1/2,0.25,0.25),data =d)
predict(m1)
predict(m1,testdata)


#利用ggplot函数作图
#1.散点图
ggplot(b1,aes(x1,x2))
ggplot(b1,aes(x1,x2))+geom_point()
ggplot(b1,aes(x1,x2))+geom_line()+geom_point(color="red",shape=1,size=3)
ggplot(b1,aes(x1,x2))+geom_line(color="red")+geom_point(color="blue",shape=1,size=2)
                                                        
#2.柱状图
ggplot(mtcars,aes(cyl))+geom_bar()
ggplot(mtcars,aes(factor(cyl)))+geom_bar()
#cyl转换为因子
ggplot(mtcars,aes(factor(cyl),fill=factor(am)))+geom_bar()
#堆积柱状图  
ggplot(mtcars,aes(factor(cyl),fill=factor(am)))+geom_bar(position="dodge")
#柱状图并排放置
table(mtcars$cyl)
# 3.直方图,一个变量
ggplot(mtcars,aes(mpg))+geom_histogram()
ggplot(mtcars,aes(mpg,fill=mpg))+geom_histogram(color="red")
#4.概率密度图
ggplot(mtcars,aes(mpg))+geom_density()
ggplot(mtcars,aes(mpg,color=factor(vs)))+geom_density()
ggplot(mtcars,aes(mpg,fill=factor(vs)))+geom_density(alpha=0.5)
#透明度alpha=0.5

#5. 箱线图
ggplot(mtcars,aes(factor(vs),mpg))+geom_boxplot()
ggplot(mtcars,aes(wt,mpg,color=factor(vs)))+geom_point() 

ggplot(mtcars,aes(wt,mpg))+geom_point(color="blue")
ggplot(mtcars,aes(wt,mpg,color=factor(vs)))+geom_point(shape=2,size=2)
+annotate("text",x=3,y=20,label="yes")+labs(title="hello",x="xxx",y="yyy")
+geom_vline(xintercept=3.5)+geom_hline(yintercept=18)+xlim(3,4)+ylim(15,30)


#生成多元正态数据
library(MASS)

set.seed(1234);
mean <- c(230.7,146.7,3.6)
sigma <- matrix(c(15360.8,6721.2,-47.1,6721.2,4700.9,-16.5,-47.1,-16.5,0.3),nrow=3,ncol=3);
mydata1 <- mvrnorm(n=500,mean,sigma)
#生成的mydata数据集是矩阵的形式，如果检验生成变量的正态性，应该先变成数据框形式
rnorm(n, mean = 0, sd = 1)
#生成均值为0，标准差为1个数为n的正态数据列