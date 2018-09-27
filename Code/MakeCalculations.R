############################################################ Prepare Data
set.seed(1)
 Trip <- as.numeric(as.character(d$code))
   
 MaxTicks<-max(table(Trip))
    
 Yield <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 Recovery <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 Distance <-matrix(NA,ncol=max(Trip),nrow=MaxTicks) 
 Ang <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 AngDiff <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)

 N<-c()
 for(j in 1:max(Trip)){
 X <- as.numeric(as.character(d$x[which(Trip==j)] ))
 Y <- as.numeric(as.character(d$y[which(Trip==j)] ))
 N[j] <- length(X)
 Yieldq<-ifelse(as.numeric(as.character(d$hit[which(Trip==j)]))==1,1,0)
 Yieldq[is.na(Yieldq)]<-0
 Yield[1:N[j],j]<-Yieldq     
 
 Recoveryq<-ifelse(d$hit[which(Trip==j)]==2,1,0)
 Recoveryq[is.na(Recoveryq)]<-0
 Recovery[1:N[j],j]<-Recoveryq    
 
 Dist<-c()  
 ang1<-c()
 angdif<-c()
  
 for(i in 2:N[j]){
 Dist[i]<- dist(X[i],X[i-1],Y[i],Y[i-1]);
 ang1[i]<- ang(X[i],X[i-1],Y[i],Y[i-1])
 if(i>3){
 angdif[i]<- ang.dif(ang1[i],ang1[i-1])
         }}    
         
 Distance[1:N[j],j]<-Dist
 Ang[1:N[j],j]<-ang1       
 AngDiff[1:N[j],j]<-angdif
 }

 Distance<-Distance*100000
 #fitdistr(c(na.omit(c(AngDiff[,]/181))), "beta",start=list(shape1=1,shape2=1))

 for(i in 1:MaxTicks){
 for(j in 1:max(Trip)){
  Distance[i,j] <- ifelse(Distance[i,j]==0,runif(1,0,0.1),Distance[i,j])
      }}

 Distance[is.na(Distance)] <- 999999

 Yield <- ifelse(Yield>=1,1,0)
 Yield[is.na(Yield)] <- 999999

 Recovery <- ifelse(Recovery>=1,1,0)
 Recovery[is.na(Recovery)] <- 999999

 EmptyAngle <-  fitdistr( ScaleBeta(c(na.omit(c(AngDiff)))),start=list(shape1=1,shape2=1),"beta")$estimate

 for(i in 1:MaxTicks){
 for(j in 1:max(Trip)){
  AngDiff[i,j] <- ifelse(is.nan(AngDiff[i,j]), rbeta(1,EmptyAngle[1], EmptyAngle[2])*pi,AngDiff[i,j])
      }}
      
 AngDiff <- ScaleBeta(AngDiff)
 AngDiff[is.na(AngDiff)] <- 999999
 Lags<-90

 AngDiff <- AngDiff[c(-1,-2,-3),]
 Distance <- Distance[c(-1,-2,-3),]
 Yield <- Yield[c(-1,-2,-3),]
 Recovery <- Recovery[c(-1,-2,-3),]
 N <- N-3
 MaxTicks<-MaxTicks-3

########################################################################### Supp. Analysis
 s.A <- AngDiff
 s.D <- Distance
 s.Y <- Yield

 s.A[s.A==999999] <- NA
 s.D[s.D==999999] <- NA
 s.Y[s.Y==999999] <- NA

 S.Y <- s.Y[-1,]      # Shift arrays to be of same length, but with Y[i+1] aligned with A and D at time i
 S.A <- s.A[-1636,]
 S.D <- s.D[-1636,]

 Patch <- S.Y
 for(k in 1:6){
  Patch[1:50,k]<-rep(0,50)
   for(i in 51:(sum(!is.na(S.Y[,k])))){
    Patch[i,k] <- ifelse(sum(S.Y[((i-50):(i)),k])==0,0,1)
                               }
                               }
                               
sink('Calculations.txt')
print("Predict Yield from Angle")
print(                                 
precis(glm(c(S.Y)~c(S.A),family="binomial"),prob=0.9)
)
print("Predict Yield from StepSize")
print(  
precis(glm(c(S.Y)~c(S.D),family="binomial"),prob=0.9)
)

print("Predict Angle in Patch")
print(  
precis(lm(c(S.A)~c(Patch)),prob=0.9)
)
print("Predict StepSize in Patch")
print(  
precis(lm(c(S.D)~c(Patch)),prob=0.9)
)
sink()




