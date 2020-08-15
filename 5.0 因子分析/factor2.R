#因子载荷图
library(psych)
library(ggplot2)
#student scores
fa1<-principal(pd1,nfactors=2,rotate="none",
               scores=TRUE,method="regression")
fa1$values
#fa1和fa2的比例
fa.var<-fa1$values
fa.var.per <- round(fa.var/sum(fa.var)*100, 1)
xlab <- paste("fa1-",fa.var.per[1], "%",sep="")
ylab <- paste("fa2-",fa.var.per[2], "%",sep="")
x<-"fa1"
y<-"fa2"
# 载荷数据
x2<-fa1$loadings
data_x <- data.frame(varinames=colnames(pd1),fa1=x2[,1],fa2=x2[,2])
ggplot(data_x,aes(x=fa1,y=fa2,label=colnames(pd1)))+
  geom_point(aes(color=varinames),shape=1,size=2)+
  geom_text(nudge_y=-0.02)+
  geom_segment(aes(x=0,y=0,xend=fa1,yend=fa2),arrow=arrow(length=unit(0.2,"cm")),alpha=0.75)+
  xlab(xlab)+ylab(ylab)+theme_bw()
#+ coord_equal(ratio=1)将X轴和y轴比例设置为一样的 


