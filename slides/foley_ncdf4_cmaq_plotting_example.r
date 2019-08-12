# Download CMAQ data from the CMAS data warehouse:
# https://drive.google.com/drive/folders/1icGCTT8kFhEzbzIBWtwufeXoJRAHOPWT
# Download file: DAILY_ACONC_LST_CMAQv5.0.2_cb05tucl_WRF3.4_EMP2002af_2002.nc
# Meta data for 2002 CMAQ output:  https://doi.org/10.15139/S3/IF4U6D

library(ncdf4)
library(fields)
library(maps)
library(M3)
library(raster)
library(leaflet)

#> Set location where you have downloaded the CMAQ netcdf file.  
setwd("/work/MOD3EVAL/fib/data_warehouse/")
cctm.file <- "DAILY_ACONC_LST_CMAQv5.0.2_cb05tucl_WRF3.4_EMP2002af_2002.nc"

#> Open the CCTM file.
cctm.in <- nc_open(cctm.file)

#> Print information about a netCDF file, including the variables and dimensions it contains.
print(cctm.in)

#> Create a list of all model variables in cctm.file. 
#> CMAQ netcdf files are formatted following I/O API conventions (https://www.cmascenter.org/ioapi/).  I/O API is a data storage library designed specifically for CMAQ data. 
#> A variable called “TFLAG” will always be the first variable listed in a CMAQ I/O API file, so remove the first element of the list.
all.mod.names <- unlist(lapply(cctm.in$var, function(var)var$name))[-1]

#> Create a list units for all the model variables in the cctm.file. 
#> A variable called “TFLAG” will always be the first variable listed in an I/O API file, so remove the first element of the list.
#> Use gsub to strip out extra spaces.
all.mod.units <- gsub(" ","",unlist(lapply(cctm.in$var, function(var)var$units))[-1])

#> Pull out the time steps and the grid coordinates associated with the data.
#> These functions from the M3 library are wrappers for functions in the ncdf4 package.
date.seq <- get.datetime.seq(cctm.file)
format.date.seq <- format.Date(date.seq,"%m/%d/%Y")

#> Lambert projected coordinates of the grid cell CENTERS (unit=km).
#> These are the unique x, y coordinates of the grid cell centers -- NOT the coordinates for every grid cell, since the data are on a regular grid.
#> You can also use the get.coord.for.dimension() function to extract the grid cell edges by changing the “position” argument.
x.proj.coord <- get.coord.for.dimension(cctm.file,"col")$coords
length(x.proj.coord)
#[1] 459
y.proj.coord <- get.coord.for.dimension(cctm.file,"row")$coords
length(y.proj.coord)
#[1] 299

#> Also get the grid cell centers of all of the grid cell with units=meters.  We will use this later when we convert the data to an object raster.
xy.proj.coord.meters <- expand.grid(x.proj.coord*1000,y.proj.coord*1000)
dim(xy.proj.coord.meters)
#[1] 137241      2

#> Projection information character string that can be used by the R package rgdal.
proj.string <- get.proj.info.M3(cctm.file)
proj.string
#[1] "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

#> US, Canada, Mexico map lines in Lambert projected coordinates.
map.lines <- get.map.lines.M3.proj(cctm.file,database="canusamex")$coords

#> Extract PMIJ_AVG (daily 24hr LST averaged PM25 concentrations for 1/1/2002 - 12/31/2002.
mod.name <- "PMIJ_AVG"
mod.unit <- all.mod.units[all.mod.names==mod.name]
mod.array <- ncvar_get(cctm.in,var=mod.name) 
dim (mod.array)
#[1] 459 299 365

#> Create annual average.
mod.annual.avg <- apply(mod.array,c(1,2),mean)
dim(mod.annual.avg)
#[1] 459 299


#> A 'nice' color palette to try for mapping. 
my.color.palette <- colorRampPalette(c("#E6E6E6","#999999","#56B4E9","#0072B2","#009E73","#F0E442","#E69F00","#D55E00","#A52A2A"))
my.range <- c(0,25)
my.color.bins <- seq(0,25,by=1)
n.colors <- length(my.color.bins)-1
my.colors <- my.color.palette(n.colors)
#> Spatial map of annual average PM25 for 2002.
image.plot(x.proj.coord,y.proj.coord,mod.annual.avg,col=my.colors,breaks=my.color.bins,legend.args=list(text=paste(mod.unit)),xlab="",ylab="",axes=F,main=paste("Annual Average",mod.name)) 
#> Add US, Canada, Mexico map lines
lines(map.lines)

#> Create raster object using projection information extracted from the I/O API file. 
xyz <- data.frame(x=xy.proj.coord.meters[,1],y=xy.proj.coord.meters[,2],z=matrix(mod.annual.avg))
mod.raster <- rasterFromXYZ(xyz,crs=proj.string)
  
#> Now make a lovely leaflet map using the raster object.
my.pal <- colorNumeric(
  palette =  my.color.palette(n.colors),
  domain = my.range
)

mapStates <- map("state",fill=T,plot=F)
leaflet(data=mapStates) %>% 
                addTiles(options=tileOptions(opacity=1)) %>% addProviderTiles("OpenStreetMap.Mapnik")  %>% 
                addRasterImage(mod.raster,colors=my.pal,opacity=.5) %>% 
              addLegend("bottomright", pal = my.pal, values = my.range,title = paste("Annual Average",mod.name),opacity = 1)
