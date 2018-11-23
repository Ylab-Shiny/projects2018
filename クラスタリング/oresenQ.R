library(reshape2)
library(ggplot2)
#dev.new()# yo use garyo vane graph chai naya window ma plot huncha
kmean.y$centers
tr <- t(kmean.y$centers)
me <- melt(tr)
#me   Var1 Var2    value
#1   00:00    1 130.0625
#2   01:00    1 131.7188
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line()+xlab("time")
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+xlab("time")#to change the thickness of line
ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+
  xlab("time")+theme(axis.text.x = element_text(angle = 90, hjust = 1))#to rotate the x-axis by 90 degree
#different colour ko lines athawa points haru plot garna cha vane yeso garne
mu<-mutate(me,cluster=paste0("cluster",me$Var2))
qqq <- ggplot(mu,aes(x=Var1,y=value,group=Var2,color=cluster))+geom_line(size=1.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
