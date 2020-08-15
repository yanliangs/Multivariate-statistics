#回归分析

library(car)
#例1：利用 数据Lungcap做多元线性回归分析，自变量有分类变量
da1<-read.csv("Lungcap.csv",T)
attach(da1)

lm.full<-lm(LungCap~Age+Height+Smoke+Gender+Caesarean,data=da1)
#回归中有字符串分类变量时，自动转为哑变量，有k个levels,
#就有 k-1个哑变量并以低一级的作为参照
#整个模型的显著性,至少一个变量显著

#step regression
step(lm.full)
#最终模型再运行一下
lm.final<-lm(LungCap~.,data=da1)
summary(lm.final)
# 进行回归诊断
plot(lm.final)

#a,提取模型的残差，进行正态性检验H0:服从正态分布
shapiro.test(residuals(lm.final))

#b,残差的独立性，H0:数据相互独立
durbinWatsonTest(lm.final)

#c,方差齐性检验H0:方差齐性
ncvTest(lm.final)

#d,共线性检验,不一定减去vif最大的变量，根据研究兴趣保留变量
vif(lm.final)

#对线性模型假设的综合检验,如果不满足，可对照前面一一检验
##并依次对偏斜度、峰度、链接函数、异方差性进行检验
library(gvlma)
gvmodel<-gvlma(lm.final)
summary(gvmodel)
##该问题分析结束

###上面的分析，age是作为一个连续型变量。假定该问题想对年龄分组
age.cut<-cut(Age,c(3,9,12,20))
#此时age.cut为factor

lm2<-lm(LungCap~age.cut+Height+Smoke+Caesarean,data=da1)
# 得到的结果是某一类别作为基数进行比较
summary(lm2)
#age.cut也可转化为数值型，as.numeric(age.cut) 


#例二：多项式回归：一个自变量 
##R自带的数据集women，15个年纪30~39岁女性的身高和体重
data(women)
lm3<-lm(weight~height,data=women)
summary(lm3)
plot(lm3,which=1)

#残差图可以看出两者可能存在非线性关系
#加入二次项后，观察残差图
lm4<-lm(weight~height+I(height^2),data=women)
summary(lm4)
#诊断
plot(lm4,which=1 )

#嵌套模型：一个项包含在另一个中，不显著时选择简单的
anova(lm3,lm4)

#不是嵌套模型，比较2个模型
AIC(lm3,lm4)

#如果效果不好可加入三次项，一般来说，n-1个弯需要n次方拟合。
#高次方解释不直观，但用于预测很好
lm5<-lm(weight~height+I(height^2)+I(height^3),data=women)
summary(lm5)
plot(lm5,1 )
anova(lm4,lm5)
#画出身高和体重的散点图及三次回归曲线
library(car)
scatterplot(weight~height,data=women,pch=5,cex=2)
#曲线拟合的结果要好于直线拟合结果

#例三：开始一个模型，建议从一个full model
data("trees")
tree<-data.frame(trees)
attach(tree)
fit2<-lm(Volume~Girth*Height+I(Girth^2)+I(Height^2))
summary(fit2)
step(fit2) 

fit3<-lm(Volume~Girth + Height + I(Girth^2))
summary(fit3)
anova(fit3,fit2)
#选择一个简单模型

#例四：非线性回归，利用朱鹮数据
#首先画图，大致看关系，然后拟合
D<-read.table( "clipboard",T)
plot(D$year,D$Nests)
out<-nls(Nests~exp(b1*(year+b0)),data=D,start=list(b0=-1981,b1=1),trace=T)
summary(out)
R2<-1-sum((D$Nests-fitted(out))^2)/sum((D$Nests-mean(D$Nests))^2)
plot(D$year,D$Nests)
lines(D$year,fitted(out),col=2) 



