 d <- read.csv("./Data/DataForRossAndWinterhalder2017.csv")
 
######################################################################## Table 1
 NHunts<-6
 Tab1 <- matrix(NA,ncol=9,nrow=NHunts)
 colnames(Tab1) <- c("GPS Points", "Distance", "Time", "Average Speed", "Elevation Change", "Encounters", "Shots", "Hits", "Recovered")

 for(i in 1:NHunts){
 Tab1[i,] <- c( length(d$elevation[which(d$code==i)]),
                round(sum(as.numeric(substr(d$extensions[which(d$code==i)],1,7))*10)/1000,2),
                round((length(d$elevation[which(d$code==i)])*10)/(60*60),2),
                round(mean(as.numeric(substr(d$extensions[which(d$code==i)],1,7)))*3.6,2),
                (max(as.numeric(as.character(d$elevation[which(d$code==i)])))-min(as.numeric(as.character(d$elevation[which(d$code==i)])))),
                sum(d$encountered[which(d$code==i)]==1,na.rm=TRUE),
                sum(d$shot[which(d$code==i)]==2,na.rm=TRUE),
                sum(d$hit[which(d$code==i)]==2,na.rm=TRUE),
                sum(d$recovered[which(d$code==i)]==2,na.rm=TRUE)
                                            )
                                            }
  Tab2 <- matrix(NA,ncol=9,nrow=8)
 colnames(Tab2) <- c("GPS Points", "Distance", "Time", "Average Speed", "Elevation Change", "Encounters", "Shots", "Hits", "Recovered")
 Tab2[1:6,] <- Tab1
  Tab2[7,] <- colSums(Tab1,na.rm=TRUE)
  Tab2[8,] <- colMeans(Tab1,na.rm=TRUE)
  
  X<-xtable(Tab2, digits=c(0,0,2,2,2,1,0,0,0,0))         # Last two rows need to be rounded by hand, sorry ;)
  
  writeLines(print(X),"Table1.txt")
