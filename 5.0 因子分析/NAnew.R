#使用聚类方法进行缺失值填补

library(DMwR2)
library(DMwR)
library(VIM)
set.seed(100)
newdata<-iris
#随机定义缺失值
newdata[sample(1:nrow(newdata),20),"Sepal.Width"] <- NA
anyNA(newdata)
sum(is.na(newdata))
#缺失数据可视化
aggr(newdata,prop=F,number=T)


#聚类方法寻找与缺失样本相似
#的10个相近的样品的加权平均来填补缺失值
library(DMwR)
knnout<-knnImputation(newdata,k=10)
anyNA(knnout)

#插补效果
#取出原数据集，缺失数据对应的变量值
original<-iris
actuals <- original$Sepal.Width[is.na(newdata$Sepal.Width)]
#取出插补后变量对应的值
predicteds <- knnout$Sepal.Width[is.na(newdata$Sepal.Width)]
#实际值和插补值比较，和实际值差距越小越好，选择mape小的
regr.eval(actuals, predicteds)

#和多重差补法结果比较
library(mice)
library(randomForest)
miceout<-mice(newdata,method="rf",maxit=100,seed=1234)
#得到插补数据
miceoutput<-complete( miceout)
anyNA(miceoutput)

predicteds2 <- miceoutput$Sepal.Width[is.na(newdata$Sepal.Width)]
#实际值和插补值比较，和实际值差距越小越好，选择mape小的
regr.eval(actuals, predicteds2)
#插补效果不如聚类结果

#均值插补
newdata1<-newdata
sum(is.na(newdata1))
newdata1$Sepal.Width[is.na(newdata1$Sepal.Width)]<-mean(newdata1$Sepal.Width,na.rm=T)
predicted3<-newdata1$Sepal.Width[is.na(newdata$Sepal.Width)]
regr.eval(actuals, predicted3)

#该实例显示knn插补的效果好一些