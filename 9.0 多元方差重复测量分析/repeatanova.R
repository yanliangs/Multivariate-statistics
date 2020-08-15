#重复测量方差分析
data1<-read.table("clipboard",T)
attach(data1)
#转化为因子
data1$obs<-as.factor(data1$obs)
model1<-aov(shengwuliang~group*time+Error(obs/(time)),data1)
summary(model1)

# 交互作用图
with(model1,interaction.plot(time,group,shengwuliang,type="o",col=c("red","dark green"),pch=c(16,18),lwd=c("2","2"),main="Interaction Plot"))           

