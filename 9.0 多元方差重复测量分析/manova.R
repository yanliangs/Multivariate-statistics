#多元均值检验
da0<-read.table("clipboard",T)
mean.vect<-apply(da0,2,mean)
cov.matrix<-cov(da0)
n<-dim(da0)[1]
p<-dim(da0)[2]
mu.0<-c(6212.01,32.87,2972,9.5,15.78)

#T2统计量
T.sq<-n*t(mean.vect-mu.0)%*%solve(cov.matrix)%*%(mean.vect-mu.0)
#转化为F统计量
F.value<-T.sq*(n-p)/(n-1)/p

#小于F.value的概率
pf(F.value,p, n-p)

#犯第一类错误的概率
p.value<-1-pf(F.value,p, n-p)

#正态分布的概率
##p(X<40)
pnorm(40,,mean=50,sd=10)
##p(X<1.96),默认正态分布
pnorm(1.96,mean=0,sd=1)

#正态分布的分位数,
#p(X<c)=0.975,反求C
qnorm(0.975,mean=0,sd=1)

#求卡方和t分布的概率
##自由度为19，卡方小于5的概率
pchisq(5,df=19)

##自由度为11，t值小于5的概率
pt(-3,df=11)



#多元方差分析
library(MASS)
da1<-read.table("clipboard",T)
#查看数据基本信息
str(da1)
attach(da1)
y<-cbind(y1,y2)
#求均值fun换成sd，求标准差
aggregate(y,by=list(gender),FUN=mean) 	 #求各类均值
aggregate(y,by=list(drug),FUN=mean) 	 #求各类均值
aggregate(y,by=list(drug),FUN=sd)

#多元方差分析
fit1<-manova(y~drug*gender)
#或
fit1<-manova(y~drug+gender+drug:gender)
summary(fit1)

#去掉交互作用
fit2<-manova(y~drug+gender)
summary(fit2)

#对y1为例做一元方差分析，y2方法类似，

#全模型
fit3<-aov(y1~drug*gender)
summary(fit3)

#交互作用可视化
interaction.plot(drug,gender,y1)

#去掉交互
fit4<-aov(y1~drug+gender)
summary(fit4)

#交互作用可视化另一种方法
library(HH)
interaction2wt(y1~drug*gender)

#多重比较
library(DescTools)
PostHocTest(fit4,method="lsd")
PostHocTest(fit4,method="bonferroni")

###一元方差分析，交互作用显著时，找最优组合

da2<-read.table("clipboard",T)

fit5<-aov(Weight_change~Diet*Country)

###由于交互项显著
library(emmeans)
marginal<-emmeans(fit5,pairwise~Country:Diet,adjust="tukey")
summary(marginal)

library(multcompView)
CLD(marginal$emmeans,alpha=0.05,Letters=letters,adjust="tukey")
