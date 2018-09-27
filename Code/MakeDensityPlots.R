set.seed(1)
################################################################## Heading Angle
df2 <- data.frame(BestFit=rbeta(100000, fitdistr(S.A[which(!is.na(S.A) & Patch==0,arr.ind=TRUE)],start=list(shape1=1,shape2=1),"beta")$estimate[1],
                                        fitdistr(S.A[which(!is.na(S.A) & Patch==0,arr.ind=TRUE)],start=list(shape1=1,shape2=1),"beta")$estimate[2]),
                  Data=sample(S.A[which(!is.na(S.A),arr.ind=TRUE)],100000,replace=TRUE),
                  DataIn=sample(S.A[which(!is.na(S.A) & Patch==1,arr.ind=TRUE)],100000,replace=TRUE),
                  DataOut=sample(S.A[which(!is.na(S.A) & Patch==0,arr.ind=TRUE)],100000,replace=TRUE), 
                  Uniform=rbeta(100000,1,1))
    df2s <- stack(df2) 
      colnames(df2s)<-c("TurningAngle","Distribution") 
  
df2s$TurningAngle  <- df2s$TurningAngle  * (rbinom(length(df2s$TurningAngle),prob=0.5,size=1)*2-1)
df2s$Iter<- c(rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000))

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$TurningAngle[which(df2s$Distribution=="Uniform" & df2s$Iter==i)])
zz <- c(zz, zz+2,zz-2)
densityX[,i] <- density(zz,bw=0.05)$x
densityY[,i] <- density(zz,bw=0.05)$y
id[,i] <- rep(i,512)
               }
               
DFa <-data.frame(Heading=c(densityX),Density=c(densityY),id=c(id),Distribution="Uniform")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$TurningAngle[which(df2s$Distribution=="Data" & df2s$Iter==i)])
zz <- c(zz, zz+2,zz-2)
densityX[,i] <- density(zz,bw=0.05)$x
densityY[,i] <- density(zz,bw=0.05)$y
id[,i] <- rep(i,512)
               }
               
DFb <-data.frame(Heading=c(densityX),Density=c(densityY),id=c(id),Distribution="Data")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$TurningAngle[which(df2s$Distribution=="DataIn" & df2s$Iter==i)])
zz <- c(zz, zz+2,zz-2)
densityX[,i] <- density(zz,bw=0.05)$x
densityY[,i] <- density(zz,bw=0.05)$y
id[,i] <- rep(i,512)
               }
               
DFc <-data.frame(Heading=c(densityX),Density=c(densityY),id=c(id),Distribution="Data, In Patch")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$TurningAngle[which(df2s$Distribution=="DataOut" & df2s$Iter==i)])
zz <- c(zz, zz+2,zz-2)
densityX[,i] <- density(zz,bw=0.05)$x
densityY[,i] <- density(zz,bw=0.05)$y
id[,i] <- rep(i,512)
               }
               
DFd <-data.frame(Heading=c(densityX),Density=c(densityY),id=c(id),Distribution="Data, Out of Patch")


DF<-rbind(DFa,DFc,DFd)
DF$group2<-paste0(DF$id,DF$Distribution)

#ggplot object
lab <- data.frame(lab="A")
g = ggplot(DF,aes(x=Heading,y=Density))
#Density function
g = g + geom_line(aes(group=group2,color=Distribution),alpha=I(0.15))  +  geom_text(data=lab, x=-0.75, y=0.98, hjust=0, vjust=0, aes(label=lab))
g = g + ylim(0,max(DF$Density))    + theme(legend.position = "none")
g = g + xlim(-1,1) + xlab("Turning Angle") + scale_colour_manual(values = c("black", terrain.colors(10)[2], terrain.colors(10)[7]))
#polar coordinates
g1 = g + coord_polar()   + theme(legend.text=element_text(size=8)) + theme(text = element_text(size=12)) + guides(colour = guide_legend(override.aes = list(alpha=1,size=2.5)))
      
          
 ggsave("DensityAngle.pdf",g1,width=4.5,height=4.5)
 
##################################################################### Stepsize
df2 <- data.frame(Data=sample(S.D[which(!is.na(S.D),arr.ind=TRUE)],100000,replace=TRUE),
                  DataIn=sample(S.D[which(!is.na(S.D) & Patch==1,arr.ind=TRUE)],100000,replace=TRUE),
                  DataOut=sample(S.D[which(!is.na(S.D) & Patch==0,arr.ind=TRUE)],100000,replace=TRUE), 
                  LogNormal=exp(rnorm(100000,fitdistr(log(S.D[which(!is.na(S.D),arr.ind=TRUE)]),"normal")$estimate[1],fitdistr(log(S.D[which(!is.na(S.D),arr.ind=TRUE)]),"normal")$estimate[2])))
                  
df2s <- stack(df2) 
colnames(df2s)<-c("StepSize","Distribution") 
  

df2s$Iter<- c(rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000))

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="LogNormal" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=0.5)$x
densityY[,i] <- density(zz,bw=0.5)$y
id[,i] <- rep(i,512)
               }
               
DFa <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Log Normal")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="Data" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=0.5)$x
densityY[,i] <- density(zz,bw=0.5)$y
id[,i] <- rep(i,512)
               }
               
DFb <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Data")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="DataIn" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=0.5)$x
densityY[,i] <- density(zz,bw=0.5)$y
id[,i] <- rep(i,512)
               }
               
DFc <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Data, In Patch")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="DataOut" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=0.5)$x
densityY[,i] <- density(zz,bw=0.5)$y
id[,i] <- rep(i,512)
               }
               
DFd <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Data, Out of Patch")


DF<-rbind(DFa,DFc,DFd)
DF$group2<-paste0(DF$id,DF$Distribution)

#ggplot object

g = ggplot(DF,aes(x=StepSize,y=Density))
#Density function
g = g + geom_line(aes(group=group2,color=Distribution),alpha=I(0.15))
g = g + ylim(0,max(DF$Density))  + annotation_compass('B','NE')  + theme(legend.position = "none")
g = g + xlim(0,19) + xlab("Step Size") + scale_colour_manual(values = c("black", terrain.colors(10)[2], terrain.colors(10)[7])) + theme(aspect.ratio=1)
#polar coordinates
g2 = g  + theme(legend.text=element_text(size=8)) + theme(text = element_text(size=12)) + guides(colour = guide_legend(override.aes = list(alpha=1,size=2.5)))
 

ggsave("DensityStep.pdf",g2,width=4.5,height=4.5)

################################################################################
 parsPareto <- c(mean(extract(mPt, pars=c("A"))$A),mean(extract(mPt, pars=c("B"))$B),mean(extract(mPt, pars=c("U"))$U))   
 
df2 <- data.frame(Data=sample(S.D[which(!is.na(S.D),arr.ind=TRUE)],100000,replace=TRUE),
                  DataIn=sample(S.D[which(!is.na(S.D) & Patch==1,arr.ind=TRUE)],100000,replace=TRUE),
                  DataOut=sample(S.D[which(!is.na(S.D) & Patch==0,arr.ind=TRUE)],100000,replace=TRUE), 
                  LogNormal=exp(rnorm(100000,fitdistr(log(S.D[which(!is.na(S.D),arr.ind=TRUE)]),"normal")$estimate[1],fitdistr(log(S.D[which(!is.na(S.D),arr.ind=TRUE)]),"normal")$estimate[2])),
                  Pareto=rPareto(100000, t=parsPareto[1], alpha=parsPareto[2], truncation = parsPareto[3]))
                  
df2s <- stack(df2) 
colnames(df2s)<-c("StepSize","Distribution") 
  

df2s$Iter<- c(rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000),rep(1:100,each=1000))

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="LogNormal" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=1.0)$x
densityY[,i] <- density(zz,bw=1.0)$y
id[,i] <- rep(i,512)
               }
               
DFa <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Log Normal")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="Pareto" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=1.0)$x
densityY[,i] <- density(zz,bw=1.0)$y
id[,i] <- rep(i,512)
               }
               
DFb <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Pareto")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="DataIn" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=1.0)$x
densityY[,i] <- density(zz,bw=1.0)$y
id[,i] <- rep(i,512)
               }
               
DFc <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Data, In Patch")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=100)

for(i in 1:100){
zz<-na.omit(df2s$StepSize[which(df2s$Distribution=="DataOut" & df2s$Iter==i)])
densityX[,i] <- density(zz,bw=1.0)$x
densityY[,i] <- density(zz,bw=1.0)$y
id[,i] <- rep(i,512)
               }
               
DFd <-data.frame(StepSize=c(densityX),Density=c(densityY),id=c(id),Distribution="Data, Out of Patch")


DF<-rbind(DFa,DFb,DFc,DFd)
DF$group2<-paste0(DF$id,DF$Distribution)

#ggplot object

g = ggplot(DF,aes(x=log(StepSize),y=log(Density)))
#Density function
g = g + geom_line(aes(group=group2,color=Distribution),alpha=I(0.15)) + annotation_compass('C','NE')
g = g + ylim(-10,log(max(DF$Density)))   + theme(legend.position = "none")
g = g + xlim(0,log(17)) + xlab("log(Step Size)")  + scale_colour_manual(values = c("black","blue", terrain.colors(10)[2], terrain.colors(10)[7]))  + theme(aspect.ratio=1)
#polar coordinates
g3 = g  + theme(legend.text=element_text(size=8)) + theme(text = element_text(size=12)) + guides(colour = guide_legend(override.aes = list(alpha=1,size=2.5)))    
       
    
 ggsave("DensityLogStep.pdf",g3,width=4.5,height=4.5)
 
 

 
DF<-rbind(DFa,DFb,DFc,DFd)
DF$group2<-paste0(DF$id,DF$Distribution)


levels(DF$Distribution)[levels(DF$Distribution)=="Log Normal"] <- "(A) Uniform, (B) Log Normal"

#ggplot object

g = ggplot(DF,aes(x=log(StepSize),y=log(Density)))
#Density function
g = g + geom_line(aes(group=group2,color=Distribution),alpha=I(0.15)) + annotation_compass('C','NE')
g = g + ylim(-10,log(max(DF$Density)))  
g = g + xlim(0,log(19)) + xlab("log(Step Size)")  + scale_colour_manual(values = c("black","blue", terrain.colors(10)[2], terrain.colors(10)[7]))  + theme(aspect.ratio=1)
#polar coordinates
g4 = g  + theme(legend.text=element_text(size=14),legend.title=element_text(size=16)) + theme(text = element_text(size=12)) + guides(colour = guide_legend(override.aes = list(alpha=1,size=2.5)))    
       
legend <- g_legend(g4) 
 
 ggsave("DensityLegend.pdf",legend,width=4.5,height=4.5) 
 
 

 
  