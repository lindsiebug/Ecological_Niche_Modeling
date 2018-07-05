library(sp)
library(rgeos)
library(raster)
library(rworldmap)

box <- readWKT("POLYGON((-180 90, 180 90, 180 -90, -180 -90, -180 90))")
proj4string(box) <- CRS("+proj=cea +datum=WGS84")
set.seed(1)
pts <- spsample(box, n=2000, type="random")
pols <- gBuffer(pts, byid=TRUE, width=50) # create circle polys around each point
merge = sample(1:40, 100, replace = T) # create vector of 100 rand #s between 0-40 to merge pols on

Sp.df1 <- gUnionCascaded(pols, id = merge) # combine polygons with the same 'merge' value
# create SPDF using polygons and randomly assigning 1 or 2 to each in the @data df
Sp.df <- SpatialPolygonsDataFrame(Sp.df1,
                                  data.frame(z = factor(sample(1:2, length(Sp.df1), replace = TRUE)),
                                             row.names= unique(merge)))
Sp.df <- crop(Sp.df, box)
colors <- c(rgb(r=0, g=0, blue=220, alpha=50, max=255), rgb(r=220, g=0, b=0, alpha=50, max=255))

land <- getMap()

overlay.map <- spplot(Sp.df, zcol = "z", col.regions = colors,
                      col = NA, alpha = 0.5, breaks=c(0,1),
                      sp.layout = list("sp.polygons", land, fill = "transparent",
                                       col = "grey50"))
overlay.map


# find the count of polygons below each grid cell
GT <- GridTopology(c(-179.5, -89.5), c(1, 1), c(360, 180))
SG <- SpatialGrid(GT)
proj4string(SG) <- CRS("+proj=cea +datum=WGS84")


o <- over(SG, Sp.df1, returnList=TRUE)
ct <- sapply(o, length)
summary(ct)
SGDF <- SpatialGridDataFrame(SG, data=data.frame(ct=ct))
spplot(SGDF, "ct", col.regions=bpy.colors(20))
