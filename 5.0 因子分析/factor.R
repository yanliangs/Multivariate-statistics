#因子分析
library(psych)
library(corrplot)
a1=cbind(c(1,0.2,-0.2),c(0.2,1,-0.4),c(-0.2,-0.4,1))
a2=eigen(a1)
round(a2$values,3) #小数点后保留3位有效数字
round(a2$vectors,3)#


a1=read.table("clipboard",head=T)
a2=cor(a1) #求相关系数
#相关关系可视化
install.packages("corrplot")
library(corrplot)
corrplot(a2)
corrplot(a2,add=T,method="number",type="lower")

#因子分析适合性检验
cortest.bartlett(cor(a1),n=52)  
KMO(cor(a1))


#因子分析
library(psych)
#第一步：因子个数的选择，a2为相关系数阵
fa.parallel(a2,n.obs=52,fa="fa") 
#psych包中两个因子分析函数principal和fa
#法一：principal函数主成分法因子分析,a1可以是原始数据也可以是相关阵，但相关阵不能输出因子得分
fa1<-principal(a1,nfactors=2,rotate="none",
              scores=TRUE,method="regression") 
#显示载荷，共通度特殊因子和公因子贡献.loading按行获取主成分因子分析系数
fa1$loadings
#因子得分系数
fa1$scores
#得分系数
fa1$weights
#显示从哪些变量中提取的因子及其载荷
fa1$communality
#共同度
fa.diagram(fa1,simple=F)

#因子载荷图，rownames为行名称

factor.plot(fa1$loadings,labels=rownames(fa1$loadings))

# 法二：利用fa函数，提取公因子的方法有极大似然 ml、主轴迭代法pa等
fa2<-fa(a1,nfactors=2,fm="pa",rotate="none",scores="regression")
#比较两个函数的结果，一般根据贡献率决定采用提取公因子的方法
fa2$loadings
fa.diagram(fa2,simple=F)


#因子旋转，a1为原数据集
fa3=principal(a1,nfactors=2,rotate="varimax",scores=T,method="regression")
#varimax为方差极大旋转
#同样可得上面的内容，不赘述
factor.plot(fa3$loadings,labels=rownames(fa3$loadings))

#假设旋转后，利用ggplot2画因子载荷图

library(ggplot2)
load<-fa3$loadings
factor_loadings1<-data.frame(varible_name=colnames(a1),factor1=load[,1],factor2=load[,2])
ggplot(data=factor_loadings1, aes(x=factor1, y=factor2, label=varible_name))+
  geom_point(shape=1,size=3)+
  geom_text(nudge_y=-0.03)+
  geom_vline(xintercept=0,linetype=5,color="red")+
  geom_hline(yintercept=0,linetype=5,color="red")+
  theme_bw() +ggtitle("My Factor Graph") 

#因子得分图
score<-data.frame(sample=rownames(a1),factor1=fa1$scores[,1],factor2=fa1$scores[,2]) 
ggplot(data=score, aes(x=factor1,y=factor2,label=sample))+
geom_text(size=3,nudge_y=-0.1) +geom_point(shape=1,size=1.5)+
  geom_vline(xintercept=0,linetype=5,color="red")+
  geom_hline(yintercept=0,linetype=5,color="red")+
  theme_bw() +ggtitle("My Factor Graph") 




利用plot画因子载荷图
plot(fa1)
load1=fa1$loadings #提取载荷矩阵
plot(load1[,1:2], xlim=c(-0.8,0.9), ylim=c(0,0.7),col = "red",pch = 19) 
text(load1[,1], load1[,2], adj=c(-0.5, -0.5))   #为散点标号
abline(h=0);  abline(v=0) 


fa.diagram(fa1,simple=F)




#数据首先存为csv或纯文本格式，可利用下面打开
x2<-read.table("D:\pca11.csv", head=T)
b5=read.delim("D:\\pca12.txt",head=T)
Control+l
#把当前的作图结果清空
dev.off()
dev.new()
#若退出，不保存工作空间映像，直接输入
q(save="no")



