### test_spcv.r
# quick test to see every thing is working
require(tidyr)
require(dplyr)
require(sp)
require(stringr)
require(ggplot2)

# load functions associated with spcv
devtools::load_all(".")

m3 <- readRDS("data-raw/matrix3.rds")

m3.grid <- m3 %>% dplyr::select(x,y)

# create a high-resolution grid for vizualization purposes
hr_grid <- expand.grid(seq(1,20, by =.5), seq(1,20, by = .5))
names(hr_grid) <- c("x","y")

# convert data.frames to SpatialPointsDataFrames
coordinates(m3.grid) = ~x+y
coordinates(m3)      = ~x+y
coordinates(hr_grid) = ~x+y

set.seed(1)
#m3_sample <- m3[sample(nrow(m3), 100, replace = F),]

m3_sample <- readRDS( "data-raw/spdf_sample1.rds")
# saveRDS(m3, "data-raw/spdf_pop1.rds")
# saveRDS(hr_grid, "hr_grid.rds")








spplot(m3_sample)
m3.idw = gstat::idw(conc~1, m3_sample, hr_grid, idp = 4)
k = convert_spdf_for_ggplot2(m3.idw, "var1.pred")
ggplot(k, aes(x,y,fill =z )) + geom_tile() + theme_bw()
orig.idw = gstat::idw(conc~1, m3, hr_grid, idp = 4)
ok = convert_spdf_for_ggplot2(orig.idw, "var1.pred")
ggplot(ok, aes(x,y,fill =z )) + geom_tile() + theme_bw()

#  
ggplot(k, aes(x,y,fill =z )) + geom_tile() + theme_basic() + ggtitle("IDP")

cv.1 <- spcv(df.sp = m.sample, my_idp = 1, var_name = "conc", )

# Environmental data analysis often involves estimating the concentration of analytes
# in an entire study area, based on a small set of discrete, non-randomly collected, field samples. 
# 
# Interpolation methods are often employed to estimate concentrations through a study area. For regulatory decision making, EPA often defaults to interpolation via a method known as Natural Neighbor (NatN) interpolation, based on weighted Veronoi Polygons. EPA's reliance on this method stems in atleast in part because -- unlike other interpolation techniques (e.g., kriging, inverse-distance weighting), the NatN approach
# doesn't involve analyst discretion in developing the interpolation model.
# 
# 
