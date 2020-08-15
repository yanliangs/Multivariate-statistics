#系统聚类法,在基础包stats中

d=read.table("clipboard",T)
d0=dist(d,method="euclidean",diag=T,upper=F,p=2)
hc1=hclust(d0,method="complete")
#d0是距离矩阵，complete系统默认的最大距离法，single为最小距离法，average为类平均法
plot(hc1)
plot(hc1,hang=-1,cex=0.6)
#默认树形画谱系图,比较2种画法结果
#分成的3类并进行加框处理
rect.hclust(hc1,3,border="red")
rect.hclust(hc1,2,border="blue")
cutree(hc1,10:1)#确认分类结果
cutree(hc1,4:1)
cutree(hc1,3) #分成三类
#分2类

data.frame(hc1$merge,hc1$height)
#显示哪两类合并，合并距离是多少;



install.packages("ape")
library(ape)
#首先生成层次聚类图
plot(hc1)
plot(as.phylo(hc1),  direction = "leftwards", align.tip.label = TRUE,cex=0.6)
# as.phylo将对象转换为“phylo”类树，type默认为树状图，Leftwards可以改成rightwards ;
plot(as.phylo(hc1),  direction = "rightwards", align.tip.label = TRUE)
plot(as.phylo(hc1), type = "fan")
# 画圆形聚类图

#热图
c1=as.matrix(d)#
#数据框转化为矩阵，d是原始数据
heatmap(c1,scale="column")
install.packages ("pheatmap")
library(pheatmap)
pheatmap(d,show_rownames=T,scale='none',col=colorRampPalette(c("green","black","red"))(100))
pheatmap(d,show_rownames=T,scale='column',col=colorRampPalette(c("green","lightblue","red"))(100))


#动态聚类,在自带的基础包stats中
install.packages(c("cluster","factoextra","ggfortify","ape"))
install.packages("ggplot2")
library(cluster)
library(factoextra)
library(ggfortify)
library(ggplot2)
d<-read.table("clipboard",T)
fviz_nbclust(scale(d),kmeans,method="wss")
#判断一下分几类合适
km1=kmeans(d,3,nstart=20,algorithm="Hartigan-Wong") 
#d为原始数据，聚为3类,nstart是随机集合的个数
sort(km1$cluster)
#对分类结果进行排序并查看分类结果
autoplot(km1, data =d, label=TRUE, label.size = 3,frame = TRUE)
#画出分类图