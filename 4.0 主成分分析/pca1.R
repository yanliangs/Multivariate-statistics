#求特征值和特征向量
a1=cbind(c(16,2,30),c(2,1,4),c(30,4,100))
a2=eigen(a1)
round(a2$values,3) #小数点后保留3位有效数字
round(a2$vectors,3)#

#例一
da1<-read.table("clipboard", header=T)#读入数据
pca1 <- prcomp(da1, scale=TRUE) 
#prcomp可以分析变量个数大于观测个数，而princomp不可以
#主成分载荷
pca1$rotation
#主成分得分
pca1$x
#各主成分方差贡献率
summary(pca1)
#碎石图
screeplot(pca1,type="line")
## make a scree plot
pca.var <- pca1$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

#利用主成分得分，样品的分类情况
library(ggplot2)

pca.data1 <- data.frame(Sample=rownames(pca1$x),X=pca$x[,1],Y=pca$x[,2],G=gl(2,5))
pca.data1                       
ggplot(data=pca.data1,aes(x=X,y=Y,label=Sample))+geom_point(aes(color=G))+geom_text(cex=4,nudge_y=-0.25)+
  xlab(paste("PC1 - ", pca.var.per[1], "%",sep=""))+
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep=""))+
  theme_bw() +
  ggtitle("My PCA Graph")
#利用主成分得分，做热图
library(pheatmap)
pheatmap(pca.data1[,2:3],show_rownames=T,scale='none',col=colorRampPalette(c("green","black","red"))(100))
#利用原数据做热图
pheatmap(da2,show_rownames=T,scale='none',col=colorRampPalette(c("green","black","red"))(100))

#get the name of the top 10 measurements (genes) that contribute most to pc1.
loading_scores <- pca1$rotation[,1]
#取出第一主成分载荷,取绝对值
gene_scores <- abs(loading_scores)
#降序排列
ggene_score_ranked <- sort(gene_scores, decreasing=TRUE)
#取出第一主成分载荷排序中前10个基因
top_10_genes <- names(gene_score_ranked[1:10])
#显示具体的基因名字
top_10_genes
#显示前10名，具体的载荷系数
pca$rotation[top_10_genes,1]


#主成分分析例题二
d1<-read.table("clipboard", header=T)#读入数据
R=round(cor(d1), 3)#求相关系数矩阵
library(psych)
cortest.bartlett(cor(d1),n=30)#主成分分析适合性检验  
KMO(cor(d1))#主成分分析适合性

#主成分分析,函数prcomp和princomp都可以做
pca2<-prcomp(d1,scale=T)
#主成分方差贡献率和主成分载荷
summary(pca2)
#变量的主成分载荷
pca2$rotation
#样品的主成分得分
pca2$x
#画碎石图
screeplot (pca2,type="line")

#画变量载荷图
pca.data<-data.frame(variabe_names=rownames(pca2$rotation),X=pca2$rotation[,1],Y=pca2$rotation[,2])
pca.data
library(ggplot2)
pca.var1<-pca2$sdev^2
pca.var.per1 <- round(pca.var1/sum(pca.var1)*100, 1)
ggplot(data=pca.data,aes(x=X, y=Y,label=variabe_names))+geom_text()+xlab(paste("PC1-",pca.var.per1[1], "%",sep=""))+ylab(paste("PC2-",pca.var.per1[2],"%",sep=""))+theme_bw() +ggtitle("My PCA Graph")

#画样品得分图
pca.data2<-data.frame(Sample=rownames(d1),X=pca2$x[,1],Y=pca2$x[,2])
pca.data2
ggplot(data=pca.data2, aes(x=X,y=Y,label=Sample))+
  geom_text() +
  xlab(paste("PC1-",pca.var.per1[1], "%", sep=""))+
  ylab(paste("PC2-", pca.var.per1[2],"%", sep=""))+
  geom_vline(xintercept=0,linetype=5,color="red")+
  geom_hline(yintercept=0,linetype=5,color="red")+
  theme_bw() +
  ggtitle("My PCA Graph")




#主成分分析例二
b3=read.table("clipboard",head=T,sep='\t')
b4=princomp(~x1+x2+x3+x4+x5+x6+x7+x8,data=b3,cor=T)
summary(b4)
b4$loadings
#输出载荷
b5=b4$scores[,1]
#第一主成分得分放到对象b5中
rank(b5)
#排序，默认降序排列
b6=data.frame(b3[,1],b5,rank(b5))
#若要分类，就取两个主成分，画主成分得分图
plot(b4$scores[,1:2]);
text(b4$scores[,1], b4$scores[,2], adj=c(0.5, -0.5)) ;
abline(h=0)
abline(v=0)
#利用ggplot2画图
#主成分得分图
temp2<-data.frame(predict(pcca1))
score<-data.frame(b3,pc1=temp2$PC1,pc2=temp2$PC2) 
ggplot(score,aes(x=pc1,y=pc2,shape=G))+geom_point(size=3,color="red")+stat_ellipse(aes(color=G))+geom_vline(xintercept =0,linetype="dashed")+geom_hline(yintercept=0,linetype="dashed")+xlab("pc1(60.92%)")+ylab("pc2(10.92%)") 
#因子载荷图
pca.data2<-data.frame(Sample=rownames(x13$rotation),X=x13$rotation[,1],Y=x13$rotation[,2])
pca.data2
ggplot(data=pca.data2, aes(x=X, y=Y, label=Sample))+geom_text() +xlab(paste("PC1-", pca.var.per[1], "%", sep=""))+ylab(paste("PC2-", pca.var.per[2], "%", sep=""))+theme_bw() +ggtitle("My PCA Graph")


