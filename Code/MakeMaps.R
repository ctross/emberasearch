   
###################################################################### Plot maps                              
d$Hunt<-factor(d$code)
d$Latitude<-(d$y)
d$Longitude<-(d$x)

d$encountered2<-d$encountered
d$encountered2[is.na(d$encountered2)]<-0

for( j in 1:6){
d$encountered2[min(which(d$Hunt==j))] <-1
}


d$timesince <- NA
d$timesince[1] <- 0
for( i in 1:length(d$timesince)){
d$timesince[i] <- ifelse(d$encountered2[i]==1,0,d$timesince[i-1]+1)
}

d$timesince <- (10*ifelse(as.numeric(d$timesince)>60,60, as.numeric(d$timesince)))/60

for( j in 1:6){
d$encountered2[min(which(d$Hunt==j))] <-0
}

d2<-d[which(d$encountered2==1),]

for( j in 1:6){
 i<-0
 while(i>=0){
if(d$timesince[min(which(d$Hunt==j))+i]<10 & d$encountered2[min(which(d$Hunt==j))+i]==0){
   d$timesince[min(which(d$Hunt==j))+i]<-10
   i<-i+1}  else{ 
i<- -1}
   }}


     
xx<- 0.018
yy<- 0.0048
(p1 <- ggplot() +
 geom_path(data=d, aes(x=Latitude, y=Longitude, group=Hunt, colour=timesince),size=1)  +  
 geom_point(data=d2,aes(x=Latitude, y=Longitude), size=0.5 )+  scale_colour_gradientn(name="Minutes since \n last encounter",
 colours = terrain.colors(10)[1:8],breaks=c(0,2.5,5,7.5,10),labels=c(0,2.5,5,7.5,"\u2265 10"))  +
 theme( axis.text.x=element_blank(),
 axis.title.x=element_blank(),
 axis.ticks.x=element_blank(),
 axis.text.y=element_blank(),
 axis.title.y=element_blank(),
 axis.ticks.y=element_blank())  +    annotation_compass('A','NE') +
 geom_rect(aes(xmin=-77.18772-yy, xmax=-77.18622-yy, ymin=4.32123-xx,ymax=4.324505-xx), alpha=0.3, fill="indianred") +
 scaleBar(lat = 4.29858, lon = -77.19022, distanceLon = 0.5, distanceLat = 0.1, distanceLegend = 0.2, dist.unit = "km", orientation = FALSE)     )
                                                  
(p2 <- ggplot() +
 geom_path(data=d, aes(x=Latitude, y=Longitude, group=Hunt, colour=timesince),size=0.5) +
 ylim(4.32078-xx, 4.32483-xx) + xlim(-77.18772-yy,-77.18622-yy)+
 geom_point(data=d2,aes(x=Latitude, y=Longitude),size=0.25 )+   scale_colour_gradientn(name="Minutes since \n last encounter",
 colours = terrain.colors(10)[1:8],breaks=c(0,2.5,5,7.5,10),labels=c(0,2.5,5,7.5,"\u2265 10"))  +
 theme(axis.text.x=element_blank(),
 axis.title.x=element_blank(),
 axis.ticks.x=element_blank(),
 axis.text.y=element_blank(),
 axis.title.y=element_blank(),
 axis.ticks.y=element_blank())+       annotation_compass('B','NE') +
 scaleBar(lat = 4.32078-xx, lon = -77.18722-yy, distanceLon = 0.05, distanceLat = 0.015, distanceLegend = 0.03, dist.unit = "km", orientation = FALSE)   )

CairoPDF("EmberaMap1.pdf",width=4.5,height=4.5)
print(p1)
dev.off()

CairoPDF("EmberaMap2.pdf",width=4.5,height=4.5)
print(p2)
dev.off()

