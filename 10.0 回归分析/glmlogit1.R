#logistic回归
library(AER)
data("Affairs")
str(Affairs)#查看变量数据类型
table(Affairs$affairs)

#新建一列变量叫ynaffairs，如果affairs是0，则该变量为0，否则为1，原数据多一列
Affairs$ynaffairs<-ifelse(Affairs$affairs==0,0,1)

#equals to the following code
Affairs$ynaffairs[Affairs$affairs>0]<-1
Affairs$ynaffairs[Affairs$affairs==0]<-0

##定义该变量为因子
Affairs$ynaffair <- factor(Affairs$ynaffairs,levels=c(0,1),labels=c('no','yes'))

# full.model
model1<-glm(ynaffair~gender + age + yearsmarried + children + religiousness + education + occupation + rating, family = binomial(),data = Affairs)
summary(model1)
step(model1)
AIC(model1)

#step选择的结果，重新运行一次
model2<- glm(ynaffair~gender+age + yearsmarried + religiousness + rating,family=binomial(),data=Affairs)
summary(model2)
 #logistic回归比较用Chisq,两个模型是否有显著差异，如果p>0.05,模型无显著差异，用简单模型
anova(model1,model2,test="Chisq")

#求or值
exp(model2$coefficients)
#or的置信区间
exp(confint(model2))

# 整个模型显著性检验，Hosmer-Lemeshow Goodness-of-Fit Test,p>0.05,模型拟合合适
library(performance)
performance_hosmer(model2)

#注意两者的区别
testpre<-predict(model2)
testpre1<-predict(model2,type="response")

#预测
testda1<-data.frame(rating=c(1,2,3,4,5),gender=c("male","male","male","male","male"),
                    age=mean(Affairs$age),
                    yearsmarried=mean(Affairs$yearsmarried),
                    religiousness=mean(Affairs$religiousness))
# 其他是均值，年纪是等差数列，公差为10，age=seq(17,57）
testda2<-data.frame(rating=mean(Affairs$rating),gender=c("female","female","female","female","female"),
                    age=seq(17,57,10),
                    yearsmarried=mean(Affairs$yearsmarried),
                    religiousness=mean(Affairs$religiousness))                               
testda1$prob<-predict(model2,newdata=testda1,type="response")
testda2$prob<-predict(model2,newdata=testda2,type="response")


#预测效果，画ROC曲线,计算曲线下面积AUC的值

library(pROC)
library(ggplot2)
pre <- predict(model2,type='response')
modelroc<-roc(Affairs$ynaffairs,pre,smoothed = TRUE,ci=TRUE,ci.alpha=0.95,plot=TRUE,auc.polygon=TRUE, max.auc.polygon=TRUE,grid=TRUE,print.auc=TRUE)
plot(modelroc,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=T,auc.polygon.col="skyblue",print.thres=T)


#画ROC曲线的另一个包
library(ROCR)
pred <- prediction(pre,Affairs$ynaffairs)
perf<-performance(pred,"tpr","fpr")
 ouc<-performance(pred,"auc")
 ouc=unlist(slot(ouc,"y.values"))
 plot(perf,xlim=c(0,1),ylim=c(0,1),col="red",main=paste("ROC curve (","AUC=",ouc,")"),lwd=2)
 abline(0,1)
 
 #泊松回归
 a1<-read.table("clipboard",T)
 attach(a1)
 log<-glm(times~.,family=poisson(link=log),data=a1)
 summary(log)
 
