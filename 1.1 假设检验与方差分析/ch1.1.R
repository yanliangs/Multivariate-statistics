install.packages("gplots")
install.packages("agricolae")
install.packages("DescTools")
install.packages("HH")


#单因素分析
a1<-read.table("clipboard",T)
attach(a1)
#绑定数据
table(年龄)
table(感冒类型)
table(年龄,感冒类型)

hist(体重)
table(年龄,组别)
#二维列联表
barplot(table(年龄,组别),beside=T,col=1:4)
# 以组别分组的年龄条图
table(组别,年龄)
barplot(table(组别,年龄),beside=T,col=1:2)
# 以年龄分组的组别条图
barplot(table(组别,年龄),beside=T,col=1:2,legend.text = c("对照组","治疗组"),args.legend=list(x=3.5,y=5))

ftable(感冒类型,效果,组别)
#三因素分析，通常用函数ftable
detach(a1)
#解除绑定

#列联表问题的检验
#卡方的比例适合性检验
x <- c(315, 101, 108, 32) 
#输入样本资料
#输入待检验的比例
chisq.test(x, p = c(9/16, 3/16, 3/16, 1/16))

#列联表的独立性检验,收入和职业的满意度调查
b1<-read.table("clipboard",T)
table(b1$满意度)
table(b1$年收入)
x<-table(b1$满意度,b1$年收入)
mybar<-barplot(x,beside=T,col=c("lightblue","yellow","red","green"),legend.text = c("比较满意","很不满意","很满意","有些不满"),args.legend=list(x=8,y=180))
text(mybar,x/2,x,cex=0.8)
#图形上标注数字，第一个x是数字所在的位置，第二个是真正的数字,cex表字的大小

x1<-read.table("clipboard",T)
chisq.test(x1[,-1])
#列联表的独立性检验

c1<-read.table("clipboard",T)
t.test(c1,alternative="greater",mu=225)
#单样本t检验,右侧检验
t.test(c1,alternative="two.sided",mu=225)
#双侧检验
t.test(c1,alternative="less",mu=225)
#左侧检验
#两个样本均值检验


#单因素分析
a1<-read.table("clipboard",T)
attach(a1)
#绑定数据
table(年龄)
table(感冒类型)
table(年龄,感冒类型)

hist(体重)
table(年龄,组别)
#二维列联表
barplot(table(年龄,组别),beside=T,col=1:4)
#以组别分组的年龄条图
table(组别,年龄)
barplot(table(组别,年龄),beside=T,col=1:2)
#以年龄分组的组别条图
barplot(table(组别,年龄),beside=T,col=1:2,legend.tex=C("对照组","治疗组"),arg.legend=list(x=3,y=5))
ftable(感冒类型,效果,组别) 

#三因素分析，通常用函数ftable
detach(a1)
#解除绑定

#列联表问题的检验
#卡方的比例适合性检验
x <- c(315, 101, 108, 32) 
#输入样本资料
#输入待检验的比例
chisq.test(x, p = c(9/16, 3/16, 3/16, 1/16))

#列联表的独立性检验,收入和职业的满意度调查
b1<-read.table("clipboard",T)
table(b1$满意度)
table(b1$年收入)
x<-table(b1$满意度,b1$年收入)
mybar<-barplot(x,beside=T,col=c("lightblue","yellow","red","green"),legend.text = c("比较满意","很不满意","很满意","有些不满"),args.legend=list(x=8,y=180))
text(mybar,x/2,x,cex=0.8)
#图形上标注数字，第一个x是数字所在的位置，第二个是真正的数字,cex表字的大小

x1<-read.table("clipboard",T)
chisq.test(x1)
#列联表的独立性检验

c1<-read.table("clipboard",T)
t.test(c1,alternative="greater",mu=225)
#单样本t检验,右侧检验
t.test(c1,alternative="two.sided",mu=225)
#双侧检验
t.test(c1,alternative="less",mu=225)
#左侧检验
#两个样本均值检验


#方差齐性检验
m1<-read.table("clipboard",T)
with(m1,var.test(old,new))
var.test(m1$old,m1$new,alternative="two.sided")
#上两种方式都可以m1为数据集，old，new为两列数
t.test(m1$old,m1$new,var.equal=TRUE)
#方差齐性，2样本均值检验

m2<-read.table("clipboard",T)
t.test(m2$left,m2$right,var.equal=F)
#方差不齐性，2样本均值检验
#配对法t检验
t.test(m2$left,m2$right,paired=T)
#比例问题
prop.test(400,10000,p=0.02)
#近似检验
binom.test(400,10000,p=0.02)
#精确检验

#相关性检验
cor.test(iris[,1],iris[,2])
#只能做两个变量间的检验，默认双侧皮尔逊检验
library(psych)
corr.test(iris[,1:4])
#利用psych包中的corr.test函数，可以做多个变量间的相关性检验

#一元方差分析
#单因素
s1<-read.table("clipboard",T)
attach(s1)
#绘制三个品种产量的箱线图
boxplot(产量~品种,col="gold",main="hexutu",xlab="pinzhong",ylab="chanliang")
#三个品种产量的方差分析
mod1<-aov(产量~品种)   #使用aov进行方差分析，自变量~因变量
summary(mod1)#则拒绝原假设，原假设H0：μ1=μ2=μ3
#绘制三个品种产量的均值图
library(gplots)
plotmeans(产量~品种)
#三个品种产量的多重比较
library(agricolae)
lsd<-LSD.test(mod1,"品种") #模型，模型的比较量：品种间的均值产量差异
lsd
library(DescTools)
PostHocTest(mod1,method="lsd")
#两种LSD结果输出的侧重点不同
TukeyHSD(mod1)
bartlett.test(产量~ 品种,data=s1)
#方差齐性检验
detach(s1)


#两因素方差分析，考虑因素间有无交互作用
s2=read.table("clipboard",T)
attach(s2)
boxplot(产量~ 品种+施肥方式,col=c("gold","green","red"),ylab="产量",xlab="品种与施肥方式")
summary(mod2)#交互作用品种：施肥方式不显著
library(HH)
interaction2wt(产量~ 品种+施肥方式)
#画交互作用图
#如果交互作用显著，不考虑主效应；如果交互作用不显著，研究主效应
#品种和施肥方式之间没有交互作用

mod2=aov(产量~ 品种+施肥方式)
summary(mod2)
mod2=aov(产量~ 品种+施肥方式+品种:施肥方式)
PostHocTest(mod2,method="lsd")
#该问题交互作用不显著，进行多重比较
TukeyHSD(mod2)
detach(s2)
