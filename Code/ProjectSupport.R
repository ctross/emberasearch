
ScaleBeta <- function(X){
 a<-0
 b<-pi
 y<-X
 Samp<-50

 y2 <- (y-a)/(b-a)
 y3<-(y2*(Samp - 1) + 0.5)/Samp
 y3}

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}
ang.dif <- function(x,y) {min((2 * pi) - abs(x - y), abs(x - y))}

dist<-function(x2a,x1a,y2a,y1a){
   return(sqrt((x2a-x1a)^2 + (y2a-y1a)^2))}

ang<-function(x2,x1,y2,y1){
  Theta<-seq(0,2*pi,length.out=100)
  DB<-cbind(cos(Theta),sin(Theta))
   r <- dist(x2,x1,y2,y1)
   x0<-(x2-x1)/r
   y0<-(y2-y1)/r

   (atan2(y0,x0) + pi )
   }

createScaleBar <- function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units = "km"){
	# First rectangle
	bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")

	topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
	rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
	lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
	rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)

	# Second rectangle t right of the first rectangle
	bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
	rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"], bottomRight[1,"long"]),
	lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
	rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)

	# Now let's deal with the text
	onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
	onTop2 <- onTop3 <- onTop
	onTop2[1,"long"] <- bottomRight[1,"long"]
	onTop3[1,"long"] <- bottomRight2[1,"long"]

	legend <- rbind(onTop, onTop2, onTop3)
	legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
	return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}
           scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
	laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
	# First rectangle
	rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)

	# Second rectangle
	rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)

	# Legend
	scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size, colour = legend.colour)

	res <- list(rectangle1, rectangle2, scaleBarLegend)

	if(orientation){# Add an arrow pointing North
		coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
		arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
		res <- c(res, arrow)
	}
	return(res)
}


annotation_compass <- function(label,
                               position = c('N','NE','E','SE','S','SW','W','NW'),
                               padding = grid::unit(c(0.5,0.5),"line"), ...){
  position <- match.arg(position)
  x <- switch (position,
    N = 0.5,
    NE = 1,
    E = 1,
    SE = 1,
    S = 0.5, 
    SW = 0,
    W = 0, 
    NW = 0
  )
  y <- switch (position,
               N = 1,
               NE = 1,
               E = 0.5,
               SE = 0,
               S = 0, 
               SW = 0,
               W = 0.5, 
               NW = 1
  )
  hjust <- switch (position,
               N = 0.5,
               NE = 1,
               E = 1,
               SE = 1,
               S = 0.5, 
               SW = 0,
               W = 0, 
               NW = 0
  )
  vjust <- switch (position,
               N = 1,
               NE = 1,
               E = 0.5,
               SE = 0,
               S = 0, 
               SW = 0,
               W = 0.5, 
               NW = 1
  )
  f1 <- switch (position,
                   N = 0,
                   NE = -1,
                   E = -1,
                   SE = -1,
                   S = 0, 
                   SW = 1,
                   W = 1, 
                   NW = 1
  )
  f2 <- switch (position,
                   N = -1,
                   NE = -1,
                   E = 0,
                   SE = 1,
                   S = 1, 
                   SW = 1,
                   W = 0, 
                   NW = -1
  )
  annotation_custom(grid::textGrob(label, 
                                   x=grid::unit(x,"npc") + f1*padding[1] , 
                                   y=grid::unit(y,"npc") + f2*padding[2],
                                   hjust=hjust,vjust=vjust, ...))
}

# WAIC function
waic <- function(stanfit){
  log_lik <- extract (stanfit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
    p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
    pointwise=pointwise, total=total, se=se))
}

# colVars function needed by waic function
colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
                     sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

rPareto <- function(n, t, alpha, truncation = NULL) {
  FinvPareto <- function(x,t,alpha) {
    return(t/(1-x)^(1/alpha))
  }
  u <- 0
  o <- 1
  if (!is.null(truncation)) {
    if (is.numeric(truncation)) {
      if (truncation > t) {
        o <- 1 - (t / truncation)^alpha
      }
    }
  }

  return(FinvPareto(runif(n, u, o),t,alpha))
}
