############################################################# Plot main results
# Make caterpillar plot
m3a<-rstan::extract(m3,pars="BetaAngle")
sample_eff<-apply(m3a$BetaAngle[,1,],2,quantile,probs=c(0.05,0.5,0.95))
df_angle<-data.frame(Lags=c(1:90),Group="Heading Change",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
m3d<-rstan::extract(m3,pars="BetaDist")
sample_eff<-apply(m3d$BetaDist[,1,],2,quantile,probs=c(0.05,0.5,0.95))
df_dist<-data.frame(Lags=c(1:90),Group="Step-Size",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 df_all<-rbind(df_angle,df_dist)

g1 <- ggplot(df_angle,aes(x=Lags,y=Median))+geom_point()+ annotation_compass('A','NE') +
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
 geom_vline(aes(xintercept=60.5),color="darkred",linetype="dotted")+
 labs(y="Effect Size, \U03C8") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
g2 <- ggplot(df_dist,aes(x=Lags,y=Median))+geom_point()+   annotation_compass('B','NE') +
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
 geom_vline(aes(xintercept=50.5),color="darkred",linetype="dotted")+
 labs(y="Effect Size, \U03D5") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
CairoPDF("EmberaPars1-SE1.pdf",width=4.5,height=4.5)
print(g1)
dev.off()

CairoPDF("EmberaPars2-SE1.pdf",width=4.5,height=4.5)
print(g2)
dev.off()
                 
  

####################################################### Make caterpillar plot  2
m3a<-rstan::extract(m3,pars="BetaAngle")
sample_eff<-apply(m3a$BetaAngle[,2,],2,quantile,probs=c(0.05,0.5,0.95))
df_angle<-data.frame(Lags=c(1:90),Group="Heading Change",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
m3d<-rstan::extract(m3,pars="BetaDist")
sample_eff<-apply(m3d$BetaDist[,2,],2,quantile,probs=c(0.05,0.5,0.95))
df_dist<-data.frame(Lags=c(1:90),Group="Step-Size",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
df_all<-rbind(df_angle,df_dist)

g1 <- ggplot(df_angle,aes(x=Lags,y=Median))+geom_point()+ annotation_compass('A','NE') +
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
 geom_vline(aes(xintercept=37.5),color="darkred",linetype="dotted")+
 labs(y="Effect Size, \U03C8") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
g2 <- ggplot(df_dist,aes(x=Lags,y=Median))+geom_point()+   annotation_compass('B','NE') +
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
 geom_vline(aes(xintercept=42.5),color="darkred",linetype="dotted")+
 labs(y="Effect Size, \U03D5") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
CairoPDF("EmberaPars1-SE2.pdf",width=4.5,height=4.5)
print(g1)
dev.off()

CairoPDF("EmberaPars2-SE2.pdf",width=4.5,height=4.5)
print(g2)
dev.off()
                 
