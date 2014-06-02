## Gg map practice session




ggplot(Data_Merged_2011, 
       aes(x=long, y=lat, group=group, fill=variable.value)) + 
    #    coord_map("polyconic") + 
    geom_polygon(colour=NA) + scale_fill_gradient(low="black", high="red")




ggplot(DataSS, 
       aes(x=long, y=lat, group=group, fill=variable.value)) + 
    #    coord_map("polyconic") + 
    geom_polygon(colour="black",alpha=0.5) + scale_fill_gradient(low="black", high="red")



#CA_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/CA_2011_EoR_Scotland.shp")
#DC_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/DC_2011_EoR_Scotland.shp")
#IZ_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/IZ_2011_EoR_Scotland.shp")
#LC_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/LC_2011_EoR_Scotland.shp")
#WD_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/WD_2011_EoR_Scotland.shp")


#CA_map <- fortify(CA_shp)
#DC_map <- fortify(DC_shp)
#IZ_map <- fortify(IZ_shp)
#LC_map <- fortify(LC_shp)
#OA_map <- fortify(OA_shp)
#WD_map <- fortify(WD_shp)





# ggplot(CA_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(DC_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(IZ_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(LC_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(OA_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(WD_map, aes(x=long, y=lat, group=group)) + geom_path()

## Not run:
## extents and legends
##################################################
hdf <- get_map()

ggmap(hdf, extent = "normal")
ggmap(hdf) # extent = 'panel', note qmap defaults to extent = 'device'
ggmap(hdf, extent = "device")
require(MASS)
mu <- c(-95.3632715, 29.7632836); nDataSets <- sample(4:10,1)
chkpts <- NULL
for(k in 1:nDataSets){
    a <- rnorm(2); b <- rnorm(2); si <- 1/3000 * (outer(a,a) + outer(b,b))
    chkpts <- rbind(chkpts, cbind(mvrnorm(rpois(1,50), jitter(mu, .01), si), k))
}
chkpts <- data.frame(chkpts)
names(chkpts) <- c("lon", "lat","class")
chkpts$class <- factor(chkpts$class)
qplot(lon, lat, data = chkpts, colour = class)
ggmap(hdf, extent = "normal") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
ggmap(hdf) +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
ggmap(hdf, extent = "device") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
theme_set(theme_bw())
ggmap(hdf, extent = "device") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
ggmap(hdf, extent = "device", legend = "topleft") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)





##################### Extracting centroid info from polygons in SP format



DZ_shp[1,]@polygons -> x
x[[1]] -> x2
x2
x2@Polygons -> x3
x3[[1]] -> x4
x4@coords


polygon(Z[,1], Z[,2], type="l")
Z <- as.data.frame(Z)
names(Z) <- c("x", "y")
p <- ggplot(Z, aex(x=x, y=y)) + geom_polygon(aes(fill="grey"))
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill="grey"))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill=grey))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill='grey'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill='grey30'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey30'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey', linetype=NULL))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey', line=NULL))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey', linetype=0))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey'))
p
qplot(x, y, data=Z)
qplot(x, y, data=Z) + geom_polygon()
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon()
p
?ComputePolyCentroids
?calcCentroid
require(maptools)
?calcCentroid
require(PBSmapping)
install.packages(PBSmapping)
install.packages("PBSmapping")
require("PBSmapping")
?calcCentroid
Z2 <- as.PolySet(Z)
require(rgeos)
install.packages("rgeos")
require(rgeos)
gArea(x)
gArea(x2)
gArea(x3)
gArea(x4)
gArea(Z)
class(x)
class(DZ_shp)
gArea(DZ_shp)
gArea(DZ_shp, byid=T)
plot(density(gArea(DZ_shp, byid=T)))
plot(density(log(gArea(DZ_shp, byid=T))))
plot(density(log(gArea(DZ_shp[1,], byid=T))))
gArea(DZ_shp[1,], byid=T)
gCentroid(DZ_shp[1,])
gCentroid(DZ_shp[1,]) -> centroid
p
p + geom_point(aes(x=x, y=y, data=centroid))
centroid
centroid$x
as.data.frame(centroid)
p + geom_point(aes(x=x, y=y, data=as.data.frame(centroid)))
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon()
p <- p + geom_point(aes(x=centroid$x, y=centroid$y))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, col="red"))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, fill="red"))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, col=red))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, col='red'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon()
p
p + layer(data=centroid, geom="point", geom_params=list(color="red"))
p + layer(data=as.data.frame(centroid), geom="point", geom_params=list(color="red"))
