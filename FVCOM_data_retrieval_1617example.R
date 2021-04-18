### This code sources FVCOM data for the years 2016 and 2017 using the new catalog system.
### You can access the full new catalog here: http://www.smast.umassd.edu:8080/thredds/catalog/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/catalog.html
###It is important to note that 2016 uses the GOM3 mesh and 2017 data uses the GOM5 mesh.
### This code will source data for 2016 and 2017 for the months of April and September, as an example.
### This code extracts data from every FVCOM location in the GOM everyday within the month at 12pm noon 
### and averages all daily values to get a monthy average value at each FVCOM location.
### This code Gets FVCOM data, then uses the raster and akima packages to both interpolate and snap the unstructured FVCOM data to a structured grid.
### I do this because it makes it easier for me to just snap all interpolated variables to the same grid, so that it is easy to match up variable values and locations
###

### Load in needed packages ####
library(readxl)
library(sdm)
library(raster)
library(sf)
library(ncdf4) # package for netcdf manipulation
library(maptools)
library(rgdal)
library(maps)
library(mapdata)
library(dplyr)
library(viridis)
library(sp)
library(rgeos)
library(writexl)
library(SDMTools)
library(RColorBrewer)
library(classInt)
library(akima)
library(chron)
##### 
setwd("D:/MENHtrawl")
survey_data<- read_excel("D:/MENHtrawl/Cir data/DATAREDO2.xlsx")
#### Edit dates for what Days you want to retrive data for ####

dates <- format(c(seq(as.Date("4/1/2016", "%m/%d/%Y"), by=1, len=30), # April 2016. "by=" how often you want days... e.g by=2 means you will get data every 2  days 4/1/2016, 4/3/2016, 4/5/2016, etc... I recommend this if you are getting data for many months/years because I have had R crash because it was just too much for it to handle, getting daily data. 
                  seq(as.Date("9/1/2016", "%m/%d/%Y"), by=1, len=30),  # September 2016
                  seq(as.Date("4/1/2017", "%m/%d/%Y"), by=1, len=30), # April 2017
                  seq(as.Date("9/1/2017", "%m/%d/%Y"), by=1, len=30)  # September 2017
),
format="%m/%d/%Y")
dates <- as.data.frame(dates)
colnames(dates) <- "date"
dates$y <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[3]))
dates$m <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[1]))
dates$d <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[2]))
dates$modified_julian_date <- julian(as.numeric(dates$m), as.numeric(dates$d), as.numeric(dates$y),c(month = 11, day = 17, year = 1858)) + 0.5 ###convert time to Julian. "+0.5" means I want data from only the noon hour to be retrieved

#### Download FVCOM time ID ####
library(dplyr)
##April 2016
fvcom_time <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc?Itime[0:1:720]")
fvcom_time<-ncvar_get(fvcom_time)
fvcom_time=data.frame((fvcom_time))
fvcom_time=distinct(fvcom_time)
colnames(fvcom_time) = c("modified_julian_date")
fvcom_time$modified_julian_date<-as.numeric(fvcom_time$modified_julian_date)+0.5
##September 2016
fvcom_data <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201609.nc?Itime[0:1:720]")
fvcom_data<-ncvar_get(fvcom_data)
fvcom_data=data.frame((fvcom_data))
fvcom_data=distinct(fvcom_data)
colnames(fvcom_data) = c("modified_julian_date")
fvcom_data$modified_julian_date<-as.numeric(fvcom_data$modified_julian_date)+0.5
fvcom_time<-rbind(fvcom_time, fvcom_data)

##April 2017
fvcom_data <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc?Itime[0:1:720]")
fvcom_data<-ncvar_get(fvcom_data)
fvcom_data=data.frame((fvcom_data))
fvcom_data=distinct(fvcom_data)
colnames(fvcom_data) = c("modified_julian_date")
fvcom_data$modified_julian_date<-as.numeric(fvcom_data$modified_julian_date)+0.5
fvcom_time<-rbind(fvcom_time, fvcom_data)
##September 2017
fvcom_data <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201709.nc?Itime[0:1:720]")
fvcom_data<-ncvar_get(fvcom_data)
fvcom_data=data.frame((fvcom_data))
fvcom_data=distinct(fvcom_data)
colnames(fvcom_data) = c("modified_julian_date")
fvcom_data$modified_julian_date<-as.numeric(fvcom_data$modified_julian_date)+0.5
fvcom_time<-rbind(fvcom_time, fvcom_data)

fvcom_time<- distinct(fvcom_time)
fvcom_time$modified_julian_date <- as.numeric(as.character(fvcom_time$modified_julian_date))
fvcom_time$id <- 0:(nrow(fvcom_time)-1)

#### Match time ####

time_id <- c()
for(i in 1:nrow(dates)){
  temp <- fvcom_time$id[which(round(fvcom_time$modified_julian_date,1)==round(dates$modified_julian_date[i],1))]
  if(length(temp)==0) time_id[i] <- NA
  else time_id[i] <- temp
}


#### Download FVCOM location data ####
lon2016 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?lon[0:1:48450]"))
lat2016 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?lat[0:1:48450]"))
names(lat2016) <- "lat"
names(lon2016) <- "lon"

latitude2016 <- lat2016$lat[5:nrow(lat2016)]
longitude2016 <- lon2016$lon[5:nrow(lon2016)]
latitude2016 <- as.numeric(as.character(latitude2016))
longitude2016 <- as.numeric(as.character(longitude2016))

lon2017 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?lon[0:1:136431]"))
lat2017 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?lat[0:1:136431]"))
names(lat2017) <- "lat"
names(lon2017) <- "lon"

latitude2017 <- lat2017$lat[5:nrow(lat2017)]
longitude2017 <- lon2017$lon[5:nrow(lon2017)]
latitude2017 <- as.numeric(as.character(latitude2017))
longitude2017 <- as.numeric(as.character(longitude2017))

#### Grid location####

start_x <- floor(range(survey_data$Longitude)[1])
end_x <- ceiling(range(survey_data$Longitude)[2])
start_y <- floor(range(survey_data$Latitude)[1])
end_y <- ceiling(range(survey_data$Latitude)[2])
my_mesh=expand.grid(seq(start_x, end_x, by=0.01), seq(start_y, end_y, by=0.01))
coordinates(my_mesh) <- ~Var1 + Var2
grid_data <-as.data.frame(my_mesh@coords)
colnames(grid_data) <-c("lon", "lat")
grid_data <- grid_data[which(grid_data$lat>42.85369),]

#### Download depth data ####
library(SDMTools)
#
#
#
#### depth_grid_plot is the grid i use to snap all fvcom data to.
depth_grid_plot <- read.csv("D://MENHtrawl/grid_depth_data.csv")

####Download FVCOM Depth Data####
###Even though depth data would be the same from 2016-2017, you still need to download seperate depth data for each year because they use different meshes

FVcom_depth2016 <- data.frame(ncvar_get(nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc?h[0:1:48450]")))
names(FVcom_depth2016) <- "FVcom_depth"
FVcom_depth2016 <- cbind(longitude2016, latitude2016, FVcom_depth2016)
colnames(FVcom_depth2016) <- c("lon", "lat", "FVcom_depth")

FVcom_depth2016.list <- list()
coordinates(depth_grid_plot) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- as.data.frame(FVcom_depth2016)
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, FVcom_depth, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FVcom_depth2016.list[[i]] <- raster::extract(rast, depth_grid_plot)
}
save(FVcom_depth2016.list, file="D://MENHtrawl/FVCOM_csv/FVcom_depth2016.list.RData")

depth_grid_plot <- read.csv("D://MENHtrawl/grid_depth_data.csv")
FVdepth2016<-cbind(depth_grid_plot[2:3],FVcom_depth2016.list[[1]])
names(FVdepth2016)[3]<-"AvgDepth"
save(FVdepth2016, file="D://MENHtrawl/FVCOM_csv/FVdepth2016.RData")

FVcom_depth2017<- data.frame(ncvar_get(nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc?h[0:1:136431]")))
names(FVcom_depth2017) <- "FVcom_depth"
FVcom_depth2017 <- cbind(longitude2017, latitude2017, FVcom_depth2017)
colnames(FVcom_depth2017) <- c("lon", "lat", "FVcom_depth")

FVcom_depth2017.list <- list()
coordinates(depth_grid_plot) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- as.data.frame(FVcom_depth2017)
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, FVcom_depth, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FVcom_depth2017.list[[i]] <- raster::extract(rast, depth_grid_plot)
}
save(FVcom_depth2017.list, file="D://MENHtrawl/FVCOM_csv/FVcom_depth2017.list.RData")

depth_grid_plot <- read.csv("D://MENHtrawl/grid_depth_data.csv")
FVdepth2017<-cbind(depth_grid_plot[2:3],FVcom_depth2017.list[[1]])
names(FVdepth2017)[3]<-"AvgDepth"
save(FVdepth2017, file="D://MENHtrawl/FVCOM_csv/FVdepth2017.RData")

#### Download temperature data ####
# April 2016
aprilfvcomtemp2016<-list()
for (i in 1:30){ #1:30 because there are 30 days in the month of April
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep="")) #[0:1:48450] is used in GOM3 mesh and [0:1:136431] is used in GOM5 (2017) mesh. make sure you have these correct when you are trying to coource links because if not, it would give you a "400 Bad Request" or cannot open the connection type error
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(aprilfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(aprilfvcomtemp2016, file="D://MENHtrawl/FVCOM_csv/aprilfvcomtemp2016.RData")
# September 2016
septemberfvcomtemp2016<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201609.nc.ascii?temp[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep=""))#time_id is [i+30]because we want to find temp values for in the time_id df starting at row 31 a.k.a september 2016
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  septemberfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(septemberfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(septemberfvcomtemp2016, file="D://MENHtrawl/FVCOM_csv/septemberfvcomtemp2016.RData")

# April 2017
aprilfvcomtemp2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?temp[", time_id[i+60], ":1:", time_id[i+60], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(aprilfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(aprilfvcomtemp2017, file="D://MENHtrawl/FVCOM_csv/aprilfvcomtemp2017.RData")

# September 2017
septemberfvcomtemp2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201709.nc.ascii?temp[", time_id[i+90], ":1:", time_id[i+90], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  septemberfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(septemberfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(septemberfvcomtemp2017, file="D://MENHtrawl/FVCOM_csv/septemberfvcomtemp2017.RData")

#Append monthly data together by season, so you could eventually average seasonal data together
# Since I only took data from one month per season/year, I'm not running this section, but if you had data for april, may, and june for example, you would append them here if you wanted the average of those months to represent your spring average data
#SP17temp<-append(aprilfvcomtemp2017,mayfvcomtemp2017,after=30)
#SP17temp<-append(SP17temp,junefvcomtemp,after=61) #If you had more months, you could continue to append them in this fashion, just keep track of the after= bc in this case it is 61, because if you had May data, May is 31 days long. 30+31=61
#do it for the fall months too
#FL17temp<-append(septemberfvcomtemp,octoberfvcomtemp,after=30)
#FL17temp<-append(FL17temp,novemberfvcomtemp,after=61)

#since I didnt run that append section, I am just going to rename the april 2017 month data "SP17", etc. so that the code will continue to run.
SP16temp<- aprilfvcomtemp2016
FL16temp<-septemberfvcomtemp2016
SP17temp<- aprilfvcomtemp2017
FL17temp<-septemberfvcomtemp2017
#but if you did run the append section, you wouldn't need to run this rename section above^
#####Take temperature data and interpolate and snap it to "grid_data" grid that was made####
###Spring2016
TRD_SP16 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:30){ #Again, pay  attention to the length of this forloop, it should be as long as your "SP16temp" list
  print(i)
  temp_data <- as.data.frame(SP16temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP16[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_SP16, file="D://MENHtrawl/FVCOM_csv/TRD_SP16.RData")
###Fall2016
TRD_FL16 <- list()
for(i in 1:30){
  print(i)
  temp_data <- as.data.frame(FL16temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_FL16[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_FL16, file="D://MENHtrawl/FVCOM_csv/TRD_FL16.RData")

###Spring2017
TRD_SP17 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:30){
  print(i)
  temp_data <- as.data.frame(SP17temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP17[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_SP17, file="D://MENHtrawl/FVCOM_csv/TRD_SP17.RData")
###Fall2017
TRD_FL17 <- list()
for(i in 1:30){
  print(i)
  temp_data <- as.data.frame(FL17temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_FL17[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_FL17, file="D://MENHtrawl/FVCOM_csv/TRD_FL17.RData")

#### Plot depth grid map ####
depth_grid_plot <- read.csv("D://MENHtrawl/grid_depth_data.csv")
plot_data <- as.data.frame(cbind(depth_grid_plot$lon, depth_grid_plot$lat, -depth_grid_plot$fathom))
colnames(plot_data) <- c("Longitude", "Latitude", "Y")
depth_odd_id <- which(plot_data$Y<=0)
summary(plot_data)
plot_data <- na.omit(plot_data)
plot_data <- plot_data[which(plot_data$Y>0),]
summary(plot_data)

plotvar <- plot_data$Y
nclr=8
plotclr <- brewer.pal(nclr+1,"Blues")[2:length(brewer.pal(nclr+1,"Blues"))]
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(survey_data$Longitude)[1]
end_x <- range(survey_data$Longitude)[2]
start_y <- range(survey_data$Latitude)[1]
end_y <- range(survey_data$Latitude)[2]
#jpeg(filename = paste("D://MENHtrawl/Plots/grid_depth_mapNEW.jpeg", sep=""), width=100, height=50, units = "mm", res = 600)
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(plot_data$Longitude, plot_data$Latitude, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
box()
degAxis(1)
degAxis(2)
par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.75, bty="n", title="Depth (Fathom)")
dev.off()
#### Plot temperature grid map ####
library(popbio)
load("D://MENHtrawl/FVCOM_csv/TRD_SP16.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_FL16.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_SP17.RData")
load("D://MENHtrawl/FVCOM_csv/TRD_FL17.RData")

meantemprastersp16<- list(Reduce(`+`, TRD_SP16) / length(TRD_SP16)) ###taking averages of each day duringthe season at each location
meantemprasterfl16<- list(Reduce(`+`, TRD_FL16) / length(TRD_FL16))
meantemprastersp17<- list(Reduce(`+`, TRD_SP17) / length(TRD_SP17)) ###taking averages of each day duringthe season at each location
meantemprasterfl17<- list(Reduce(`+`, TRD_FL17) / length(TRD_FL17))


plottemp<-function(rasterdata, month,year){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_temperature_mapNEW.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
  #par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks= 
                              if(month=="April") c(2.65,4.7,5.25,6,6.5,7,7.5,8.5,9.5,10.5,11.85)
                           else c(1.83,7,8,8.5,9,9.5,10,10.5,11.5,13,16.15))
    colcode <- findColours(class, plotclr)
    
    start_x <- range(survey_data$Longitude)[1]
    end_x <- range(survey_data$Longitude)[2]
    start_y <- range(survey_data$Latitude)[1]
    end_y <- range(survey_data$Latitude)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.2)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=1.0,bg="white",
           bty = "o", title="Temperature (C)")
    axis(1, cex=0.6)
    axis(2, cex=0.6)
  }
  #dev.off()
}
par(mar=c(2,2,0,0), mfrow=c(2,2))
plottemp(meantemprastersp16,"April","2016")
plottemp(meantemprasterfl16,"September","2016")
plottemp(meantemprastersp17,"April","2017")
plottemp(meantemprasterfl17,"September","2017")

#### Download salinity data ####
# April 2016
aprilfvcomsal2016<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(aprilfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(aprilfvcomsal2016, file="D://MENHtrawl/FVCOM_csv/aprilfvcomsal2016.RData")

# September 2016
septemberfvcomsal2016<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201609.nc.ascii?salinity[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  septemberfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(septemberfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(septemberfvcomsal2016, file="D://MENHtrawl/FVCOM_csv/septemberfvcomsal2016.RData")
# April 2017
aprilfvcomsal2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?salinity[", time_id[i+60], ":1:", time_id[i+60], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(aprilfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(aprilfvcomsal2017, file="D://MENHtrawl/FVCOM_csv/aprilfvcomsal2017.RData")

# May 2017 leaving this here as how to include more months, if you need those, but I wont run it in this example because it takes a while to run this code, the more months you include. Would also have to go back and changes "dates" dataframe to properly include this.
#mayfvcomsal2017<-list()
#for (i in 1:31){
#  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201705.nc.ascii?salinity[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:136431]", sep=""))
#  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
#  mayfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
#  colnames(mayfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
#  print(i)
#}
#save(mayfvcomtemp2017, file="D://MENHtrawl/FVCOM_csv/mayfvcomtemp2017.RData")

# September 2017
septemberfvcomsal2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201709.nc.ascii?salinity[", time_id[i+90], ":1:", time_id[i+90], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  septemberfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(septemberfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(septemberfvcomsal2017, file="D://MENHtrawl/FVCOM_csv/septemberfvcomsal2017.RData")


#SP17sal<-append(aprilfvcomsal,mayfvcomsal,after=30) #again, do this section if you have multiple months to represent a season
#SP17sal<-append(SP17sal,junefvcomsal,after=61)

#FL17sal<-append(septemberfvcomsal,octoberfvcomsal,after=30)
#FL17sal<-append(FL17sal,novemberfvcomsal,after=61)

SP16sal<-aprilfvcomsal2016 #again, don'trun these lines (447-450) if you ran the last section (441-445)
FL16sal<- septemberfvcomsal2016
SP17sal<-aprilfvcomsal2017
FL17sal<- septemberfvcomsal2017

#####Take salinity data and interpolate and snap it to "grid_data" grid that was made####
###Spring2016
SRD_SP16 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:30){
  print(i)
  temp_data <- as.data.frame(SP16sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP16[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_SP16, file="D://MENHtrawl/FVCOM_csv/SRD_SP16.RData")
###Fall2016
SRD_FL16 <- list()
for(i in 1:30){
  print(i)
  temp_data <- as.data.frame(FL16sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FL16[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_FL16, file="D://MENHtrawl/FVCOM_csv/SRD_FL16.RData")


###Spring2017
SRD_SP17 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:30){
  print(i)
  temp_data <- as.data.frame(SP17sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP17[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_SP17, file="D://MENHtrawl/FVCOM_csv/SRD_SP17.RData")
###Fall2017
SRD_FL17 <- list()
for(i in 1:30){
  print(i)
  temp_data <- as.data.frame(FL17sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FL17[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_FL17, file="D://MENHtrawl/FVCOM_csv/SRD_FL17.RData")




#### Plot salinity grid map ####
load("D://MENHtrawl/FVCOM_csv/SRD_SP16.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_FL16.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_SP17.RData")
load("D://MENHtrawl/FVCOM_csv/SRD_FL17.RData")

library(popbio)

meansalrastersp16<- list(Reduce(`+`, SRD_SP16) / length(SRD_SP16)) ###taking averages of each day during the season at each location
meansalrasterfl16<- list(Reduce(`+`, SRD_FL16) / length(SRD_FL16))
meansalrastersp17<- list(Reduce(`+`, SRD_SP17) / length(SRD_SP17)) ###taking averages of each day during the season at each location
meansalrasterfl17<- list(Reduce(`+`, SRD_FL17) / length(SRD_FL17))


plotsalinity<-function(rasterdata, month,year){
  #jpeg(filename = paste("D://MENHtrawl/Plots/grid_salinity_mapNEW.jpeg", sep=""), width=140, height=120, units = "mm", res = 600)
 # par(mar=c(2,2,0,0), mfrow=c(1,1))
  plotvar=unlist(rasterdata)
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="quantile")
  fix_break<-round(class$brks, digits = 2)
  for(i in 1:length(rasterdata)){
    print(i)
    plotvar <- rasterdata[[i]]
    nclr=10
    plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
    class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks= 
                              c(0.00,26.0,31.0,31.5,32.0,32.5,33.0,33.5,34.0,34.5,35.1))
    colcode <- findColours(class, plotclr)
    
    start_x <- range(survey_data$Longitude)[1]
    end_x <- range(survey_data$Longitude)[2]
    start_y <- range(survey_data$Latitude)[1]
    end_y <- range(survey_data$Latitude)[2]
    plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
    map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
    #plot(sa511_513, add=T)
    box()
    legend("topleft", paste(year, "-", month, sep=""), bty="n", cex=1.2)
    legend("bottomright", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=1.0,bg="white",
           bty = "o", title="Salinity (psu)")
    axis(1, cex=0.6)
    axis(2, cex=0.6)
  }
  #dev.off()
}
par(mar=c(2,2,0,0), mfrow=c(2,2))
plotsalinity(meansalrastersp16,"April","2016")
plotsalinity(meansalrasterfl16,"September","2016")
plotsalinity(meansalrastersp17,"April","2017")
plotsalinity(meansalrasterfl17,"September","2017")
