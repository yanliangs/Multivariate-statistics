install.packages("psych")
install.packages("scatterplot3d")

library(scatterplot3d)
library(psych)
#过一段会有人更新,输入下面命令即可
Update.packages()
#1.创建一个向量
x1=c(171,175,159,155,152,158,154,164,168,166,159,164)
x2=c(57,64,41,38,35,44,41,51,57,49,47,46)
x3=c("good","better","best")
x4=c("好","较好","最好")
#长度及数据类型
length(x1)
mode(x2)
mode(x3)
mode(x4)
#2.创建一个矩阵（二维数组）
#按行合并，生成二维数组
rbind(x1,x2)
#按列合并，生成二维数组
cbind(x1,x2)
#生成矩阵
#利用数据x1创建矩阵
 matrix(x1,nrow=3,ncol=4)#系统默认按列

A=matrix(x1,nrow=6,ncol=2,byrow=T)#按行排列
 #矩阵转置
A
t(A)
#矩阵相乘
C=t(A)%*%A
C
#矩阵求逆
solve(C)
# 求特征值特征向量
v=eigen(C)
v$values
v$vectors
#数据框data.frame，各列数据类型可以不相同
y=data.frame(x1,x2)
#产生由x1,x2构造的数据框
y1=data.frame('身高'=x1,'体重'=x2)
y1


#数据的描述统计分析
#ch1 ppt example
m1<-c(42,52,48,58)
m2<-c(4,5,4,3) 
m=data.frame(m1,m2)
cov(m)
cor(m)
apply(m,2,mean)
#column's mean
library(psych)
describe(m)

#famous Fisher's Iris data
describe(iris[,-5])#除第5个变量
S<-round(cov(iris[,1:4]),2) #keep 2 decimals of the covariance matrix
S
R<-round(cor(iris[,1:4]),2) #keep 2 decimals of the correlation matrix
R
cov(iris[,1:2])
round(cov(iris[,1:2]),2)  
#第1，2两变量的协方差阵
round(cov(iris[,3:4]),2)  
#第3，4两变量的协方差阵
round(cov(iris[,1:2],iris[,3:4]),2)  
#第1，2列与第3，4列
#上述cov换为cor，即得相应变量间的相关系数阵


#数据的可视化
plot(x1,x2)
#散点图
plot(x1,x2,main="biaoti",pch=19,col="red",cex=0.9)
#变量所在的列，标题，形状，颜色，大小
#famous Fisher's Iris data
hist(iris$Sepal.Length)#直方图，纵轴代表的是频数
boxplot(iris$Sepal.Length)#柱状图
bp=boxplot(iris$Sepal.Width)
bp$out#找到异常点

#正态性
qqnorm(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width)


plot(iris$Petal.Length,iris$Petal.width)
plot(iris$Petal.Length,iris$Petal.width,type="b",pch=15,lty=2,col="red",main="relation",ylab ="w",xlab="Length",)
#线段的类型，线不过点，点形状，虚实，颜色，纵坐标范围，主题，横坐标纵坐标轴的名称
#多元数据的两两散点图
pairs(iris[,1:4],main="Scatterplot Matirx For Fisher's Iris Data")
pairs(iris[,-5],gap=0)
plot(iris[,1:4],gap=0)
#盒须图
boxplot(iris[1:4],col=c("yellow","red","green","blue"),notch=F)
boxplot(iris[1:4],col=c("yellow","red","green","blue"),notch=T)

#绘制三维散点图
library(scatterplot3d)
scatterplot3d(iris[,2:4])

#读取数据
# import data，三种方式 打开数据
#方式一：在excel中选择数据区域直接复制，然后输入：
b1<-read.table("clipboard",head=T)
b2=read.table("clipboard",head=T,sep='\t')
b3<-read.table("clipboard",head=T,row.names = 1)

#方式二：将数据另存为csv格式，然后选择下面方式打开
b4<-read.csv("D:\\pca1.csv", head=T)
#方式三：将数据另存为纯文本文件，然后输入下面命令

b5<-read.delim("D:\\pca1.txt",head=T)

#export data
write.table(c,file="D:/c2.csv",sep=",",row.names = F)

#清屏
Control+l
#把当前的作图结果清空
dev.off()
#若退出，不保存工作空间映像，直接输入
q(save="no")

