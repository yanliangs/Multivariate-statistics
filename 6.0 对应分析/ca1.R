#对应分析例1

library(ca)
library(factoextra)
x1<-read.table("clipboard",T)
options(digits=3) 
#独立性检验
chisq.test(x1)
#对应分析
ca1<-ca(x1)
#崖底碎石图不应超过max(1/(行数-1)，1/(列数-1))
fviz_screeplot(ca1)+geom_hline(yintercept=25,linetype=2,color="red")
#输出分析结果
ca1
#选择2个维度可解释原始信息的88.39%，inertial列则给出了惯量在行变量中的分解情况
#数值越大，说明该类别对惯量的贡献越大，Dim为二维图上的坐标


#行或列分析图
fviz_ca_row(ca1)
fviz_ca_col(ca1)
#总的结果图（带箭头)
fviz_ca_biplot(ca1,arrow=c(T,T))
fviz_ca_biplot(ca1,arrow=c(F,T))
#调整颜色
fviz_ca_biplot(ca1,col.col = "#FF6600",col.row = "#336699",arrow=c(T,T))
fviz_ca_biplot(ca1,col.col = "#FF6600",col.row = "#336699",arrow=c(F,F))
plot(ca1)

#分别取出行和列变量分析结果，行列变量对各维度的贡献（百分比）
row<-get_ca_row(ca1)
col<-get_ca_col(ca1)
#颜色深、圆形大的对各维度的影响大
library(corrplot)
corrplot(row$contrib,is.corr=F)
corrplot(col$contrib,is.corr=F)

#对应分析例2
#利用数据集incomeCA,分析对应分析和多重对应分析
da2<-read.csv("IncomeCA.csv")
da3<-table(da2[,-3])
chisq.test(da3)
ca2<-ca(da3)
fviz_ca_biplot(ca2,col.col = "#FF6600",col.row = "#336699",arrow=c(T,T))


#对应分析例3
library(ca)
brand<-data.frame(low=c(2,49,4,4,15,1),
                  medium=c(7,7,5,49,2,7),high=c(16,3,23,5,5,14))
rownames(brand)=c("A","B","C","D","E","F")
chisq.test(brand)
options(digits=3) 
brand_ca<-ca(brand) 
brand_ca
fviz_ca_biplot(brand_ca,col.col = "#FF6600",col.row = "#336699",arrow=c(T,T))





#例4：多重对应分析
library(ca)
da2<-read.csv("incomeCA.csv")
mca1<-mjca(da2,lambda="Burt")
plot(mca1)
plot(mca1,map = "rowprincipal",arrows = c(TRUE, TRUE))


# MASS中的函数corresp做对应分析
ca4<-corresp(x,nf=2)

#1.简单随机抽样
index<-sample(1:nrow(df2),5)
#按照随机序号index,从数据集中抽取相应的样品
sampledata<-df2[index,]

#2.利用sampling包中的函数，进行分层抽样
install.packages("sampling")
library(sampling)
# 利用strata函数得到一个抽样的序号
strata_data<-strata(df3,stratanames="G",size=c(3,4,3),method="srswor")
strata_data
sampledata2<-df2[strata_data$G,]




#获得工作路径
getwd()
#重置工作路径
setwd("D:/Rdata")
#CSV数据存到该数据集中
da12<-read.csv("c2.csv")
#有缺失值也可以导入
d13<-read.csv("c2.csv")
#导出数据da1为要导出的数据集，df1.csv为存放的数据集，不要第一列
write.csv(da1,"df1.csv",row.names=F)


