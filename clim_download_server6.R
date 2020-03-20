# Global clims_download_server6  v0.6
# the purpose of this code is to download soil and climate data for GBIF/global  data
# contact: ebrahim.jahanshiri@cffresearch.org, e.jahanshiri@gmail.com
# How to do:
# this code is run by your R client to extract the climate and soil data from where your climate and soil data are organised
# please see Worldclim and ISRIC websites to download your copy of the data 

list.of.packages <- c("raster", "sp")
pckList <- lapply(list.of.packages, require, character.only = TRUE)

#get the raster of the area for temperature and rainfall

data <- as.data.frame(read.csv("alldatabg.csv"))

#names(data)

spdata <- data
coordinates(spdata) <- ~ decimalLongitude + decimalLatitude
proj4string(spdata) <- CRS("+proj=longlat +datum=WGS84")

# fill the empty stacks with extracted data
raindata = stack()
tempdata = stack()
for (i in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) {
  temprast = raster(paste("/data/sci_data/worldClimData/WC2/wc2.0_30s_tavg_",i,".tif",sep=""));
  rainrast = raster(paste("/data/sci_data/worldClimData/WC2/wc2.0_30s_prec_",i,".tif",sep=""));
  crs(temprast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
  crs(rainrast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
  raindata = addLayer(raindata, rainrast);
  tempdata = addLayer(tempdata, temprast);
}
names(tempdata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
names(raindata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")

# extract data based on the acesssion coordinates
temp = data.frame()
rain = data.frame()

for (i in 1:12){
  temp = rbind(temp, as.numeric(extract(tempdata[[i]], spdata)))
  rain = rbind(rain, as.numeric(extract(raindata[[i]], spdata)))
}

row.names(temp) = c("tJan", "tFeb", "tMar", "tApr", "tMay", "tJun", "tJul", "tAug", "tSep",  "tOct", "tNov", "tDec")
row.names(rain) = c("rJan", "rFeb", "rMar", "rApr", "rMay", "run", "rJul", "rAug", "rSep",  "rOct", "rNov", "rDec")

## getting the soil

phdata = stack()
sandata = stack()
claydata = stack()

ph = NULL
clay = NULL
sand = NULL 


for (i in 1:7) phdata = addLayer(phdata, raster(paste("/data/sci_data/soilgrids/PHIHOX_M_sl",  i, "_250m.tif", sep = "")))
for (i in 1:7) claydata = addLayer(claydata, raster(paste("/data/sci_data/soilgrids/CLYPPT_M_sl",  i, "_250m.tif", sep = "")))
for (i in 1:7) sandata = addLayer(sandata, raster(paste("/data/sci_data/soilgrids/SNDPPT_M_sl",  i, "_250m.tif", sep = "")))
depthdata = raster(paste("/data/sci_data/soilgrids/BDRICM_M_250m.tif", sep = ""))
# combine soil data
for (i in 1:7){
  ph = cbind(ph, as.numeric(extract(phdata[[i]], spdata)))
  clay = cbind(clay, as.numeric(extract(claydata[[i]], spdata)))
  sand = cbind(sand, as.numeric(extract(sandata[[i]], spdata)))
}
BDRICM.BDRICM_M = as.numeric(extract(depthdata, spdata))

colnames(ph) <- c("PHIHOX.M.sl1","PHIHOX.M.sl2","PHIHOX.M.sl3","PHIHOX.M.sl4","PHIHOX.M.sl5","PHIHOX.M.sl6", "PHIHOX.M.sl7")
colnames(clay) <- c("CLYPPT.M.sl1","CLYPPT.M.sl2","CLYPPT.M.sl3","CLYPPT.M.sl4","CLYPPT.M.sl5","CLYPPT.M.sl6", "CLYPPT.M.sl7")
colnames(sand) <- c("SNDPPT.M.sl1","SNDPPT.M.sl2","SNDPPT.M.sl3","SNDPPT.M.sl4","SNDPPT.M.sl5","SNDPPT.M.sl6", "SNDPPT.M.sl7")
#names(BDRICM.M) <- "BDRICM.BDRICM_M"

# write the data out
saveRDS(data.frame(ph, clay, sand, BDRICM.BDRICM_M,cbind(t(temp), t(rain),coordinates(spdata))), file = paste("alldatabg_extracted.rds",sep=""))
write.csv(data.frame(ph, clay, sand, BDRICM.BDRICM_M, cbind(t(temp), t(rain),coordinates(spdata))), paste("alldatabg_extracted.csv",sep=""))
