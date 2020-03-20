# Results.R  v6
# Creating figures for the manuscript
# contact: ebrahim.jahanshiri@cffresearch.org, e.jahanshiri@gmail.com

library(tidyverse)
library(reshape2)
library(ggplot2)
library(data.table)
library(maps)
library(ggthemes)
setwd("/Users/ej/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx2/")

# Getting simple statistics for each crop 
alldata <- readRDS('./data/output/Oct19Combined/alldata.rds')
alldata$crop <- as.factor(alldata$crop)
names(alldata) <- c("point",                            "crop",                             "latitude",
"longitude" ,                       "tempdata" ,                        "raindata",
"phdata",                           "depthdata" ,                       "sanddata",
"claydata" ,                        "tempsuit"  ,                       "rainsuit",
"phsuit"  ,                         "depthsuit"  ,                      "texturesuit",
"climprod"  ,                       "climave"     ,                     "soilprod",
"soilave"   ,                       "soilaveweighted",                  "climprod_soilprod_allprod",
"climprod_soilprod_allave"  ,       "climprod_soilave_allprod" ,        "climprod_soilave_allave",
"climprod_soilaveweighted_allprod", "climprod_soilaveweighted_allave",  "climave_soilprod_allprod",
"climave_soilprod_allave"  ,        "climave_soilave_allprod"  ,        "climave_soilave_allave",
"climave_soilaveweighted_allprod",  "climave_soilaveweighted_allave",   "norainـsoilprodـallprod",
"norainـsoilprodـallave" ,          "norainـsoilaveـallprod" ,          "norainـsoilaveـallave",
"norainـsoilaveweightedـallprod",   "norainـsoilaveweightedـallave")

yearmax <- as_tibble(readRDS('./data/Output/Oct19Combined/yearmax.rds'))
# removing records below 30 so that mean and SD are correct representative
yearmax = yearmax[-which(yearmax$crop == "Bitter Bean"),] #46263
yearmax = yearmax[-which(yearmax$crop == "Sesbania"),] #47121 47122 47123 47124
yearmax = yearmax[-which(yearmax$crop == "Adzuki Bean"),] #76679 76680 76681 76682 76683 76684 76685
yearmax = yearmax[-which(yearmax$crop == "Long Beans"),] #46264 46265 46266 46267 46268 46269 46270 46271 46272
yearmax = yearmax[-which(yearmax$crop == "Ambarella"),] #1  2  3  4  5  6  7  8  9 10 11
yearmax = yearmax[-which(yearmax$crop == "Galangal"),] #82347 82348 82349 82350 82351 82352 82353 82354 82355 82356
yearmax = yearmax[-which(yearmax$crop == "Turmeric"),] #82357 82358 82359 82360 82361 82362 82363 82364 82365 82366 82367 82368 82369 82370 82371 82372
yearmax = yearmax[-which(yearmax$crop == "Sesame"),] #265 records
yearmax = yearmax[-which(yearmax$crop == "African Yam Bean"),] #76686 76687 76688 76689 76690 76691 76692 76693 76694 76695 76696 76697 76698 76699 76700 76701 76702
yearmax = yearmax[-which(yearmax$crop == "Moth-bean"),] #82373 82374 82375 82376 82377 82378 82379 82380 82381 82382 82383 82384 82385 82386 82387 82388 82389 82390 82391 82392 82393 82394 82395 82396
yearmax = yearmax[-which(yearmax$crop == "Okra"),]
yearmax = yearmax[-which(yearmax$crop == "Rapeseed"),]
yearmax = yearmax[-which(yearmax$crop == "Lima Bean"),] #659 records
yearmax = yearmax[-which(yearmax$crop == "Groundnut"),]
yearmax1 <- yearmax[,-c(1,3:10)]
yearmax1.m <- melt(yearmax1, id.vars = "crop")
# further aggregating data for presentation
assign("tempsuit", group_by(yearmax1, crop) %>% summarise(mean = mean(tempsuit)))
assign("rainsuit", group_by(yearmax1, crop) %>% summarise(mean = mean(rainsuit)))
assign("phsuit", group_by(yearmax1, crop) %>% summarise(mean = mean(phsuit)))
assign("depthsuit", group_by(yearmax1, crop) %>% summarise(mean = mean(depthsuit)))
assign("texturesuit", group_by(yearmax1, crop) %>% summarise(mean = mean(texturesuit)))

assign("climprod", group_by(yearmax1, crop) %>% summarise(mean = mean(climprod)))
assign("climave", group_by(yearmax1, crop) %>% summarise(mean = mean(climave)))
assign("soilprod", group_by(yearmax1, crop) %>% summarise(mean = mean(soilprod)))
assign("soilave", group_by(yearmax1, crop) %>% summarise(mean = mean( soilave)))
assign("soilaveweighted", group_by(yearmax1, crop) %>% summarise(mean = mean( soilaveweighted)))

assign("climprod_soilprod_allprod", group_by(yearmax1, crop) %>% summarise(mean = mean( climprod_soilprod_allprod)))
assign("climprod_soilprod_allave", group_by(yearmax1, crop) %>% summarise(mean = mean( climprod_soilprod_allave)))
assign("climprod_soilave_allprod", group_by(yearmax1, crop) %>% summarise(mean = mean( climprod_soilave_allprod)))
assign("climprod_soilave_allave", group_by(yearmax1, crop) %>% summarise(mean = mean( climprod_soilave_allave)))
assign("climprod_soilaveweighted_allprod", group_by(yearmax1, crop) %>% summarise(mean = mean( climprod_soilaveweighted_allprod)))
assign("climprod_soilaveweighted_allave", group_by(yearmax1, crop) %>% summarise(mean = mean( climprod_soilaveweighted_allave)))
assign("climave_soilprod_allprod", group_by(yearmax1, crop) %>% summarise(mean = mean( climave_soilprod_allprod)))
assign("climave_soilprod_allave", group_by(yearmax1, crop) %>% summarise(mean = mean( climave_soilprod_allave)))
assign("climave_soilave_allprod", group_by(yearmax1, crop) %>% summarise(mean = mean( climave_soilave_allprod)))
assign("climave_soilave_allave", group_by(yearmax1, crop) %>% summarise(mean = mean( climave_soilave_allave)))
assign("climave_soilaveweighted_allprod", group_by(yearmax1, crop) %>% summarise(mean = mean( climave_soilaveweighted_allprod)))
assign("climave_soilaveweighted_allave", group_by(yearmax1, crop) %>% summarise(mean = mean( climave_soilaveweighted_allave)))
assign("norainـsoilprodـallprod", group_by(yearmax1, crop) %>% summarise(mean = mean( norainـsoilprodـallprod)))
assign("norainـsoilprodـallave", group_by(yearmax1, crop) %>% summarise(mean = mean( norainـsoilprodـallave)))
assign("norainـsoilaveـallprod", group_by(yearmax1, crop) %>% summarise(mean = mean( norainـsoilaveـallprod)))
assign("norainـsoilaveـallave", group_by(yearmax1, crop) %>% summarise(mean = mean( norainـsoilaveـallave)))
assign("norainـsoilaveweightedـallprod", group_by(yearmax1, crop) %>% summarise(mean = mean( norainـsoilaveweightedـallprod)))
assign("norainـsoilaveweightedـallave", group_by(yearmax1, crop) %>% summarise(mean = mean( norainـsoilaveweightedـallave)))
num.sample = group_by(yearmax1, crop) %>% summarise(count = n())
write_csv(num.sample, "./data/Output/Oct19Combined/num_sample.csv")

indices = cbind(crop = tempsuit[,1],
      no.sampl = num.sample[,2],          
      tempsuit = tempsuit[,2],                          
      rainsuit= rainsuit[,2], 
      phsuit= phsuit[,2], 
      depthsuit = depthsuit[,2], 
      texturesuit = texturesuit[,2], 
      
      climprod = climprod[,2], 
      climave = climave[,2], 
      soilprod =  soilprod[,2], 
      soilave = soilave[,2], 
      soilaveweighted = soilaveweighted[,2], 
      
      climprod_soilprod_allprod =  climprod_soilprod_allprod[,2], 
      climprod_soilprod_allave = climprod_soilprod_allave[,2], 
      climprod_soilave_allprod =  climprod_soilave_allprod[,2], 
      climprod_soilave_allave = climprod_soilave_allave[,2], 
      climprod_soilaveweighted_allprod = climprod_soilaveweighted_allprod[,2], 
      climprod_soilaveweighted_allave = climprod_soilaveweighted_allave[,2], 
      
      climave_soilprod_allprod =  climave_soilprod_allprod[,2], 
      climave_soilprod_allave = climave_soilprod_allave[,2], 
      climave_soilave_allprod = climave_soilave_allprod[,2], 
      climave_soilave_allave =  climave_soilave_allave[,2], 
      climave_soilaveweighted_allprod = climave_soilaveweighted_allprod[,2], 
      climave_soilaveweighted_allave =  climave_soilaveweighted_allave[,2], 
      
      norainـsoilprodـallprod = norainـsoilprodـallprod[,2], 
      norainـsoilprodـallave = norainـsoilprodـallave[,2], 
      norainـsoilaveـallprod = norainـsoilaveـallprod[,2], 
      norainـsoilaveـallave = norainـsoilaveـallave[,2], 
      norainـsoilaveweightedـallprod = norainـsoilaveweightedـallprod[,2], 
      norainـsoilaveweightedـallave = norainـsoilaveweightedـallave[,2])
names(indices) = c( "crop",
                    "count",
                    "Thermal",
                    "Rainfall",
                    "pH",
                    "Depth",
                    "Texture",
                    
                    "Thermal x Rainfall",                         
                    "Average(Thermal, Rainfall)",
                    "pH x Texture x Depth",
                    "Average(pH, Texture, Depth)",
                    "Average(0.6xpH, 0.2xTexture, 0.2xDepth)",
                    
                    "Thermal x Rainfall x pH x Texture x Depth",
                    "Average((Thermal x Rainfall), (pH x Texture x Depth))",
                    "(Thermal x Rainfall)x Average(pH, Texture, Depth)", 
                    "Average((Thermal x Rainfall), Average(pH, Texture, Depth))",
                    "(Thermal x Rainfall) x Average(0.6xpH, 0.2xTexture, 0.2xDepth)", 
                    "Average((Thermal x Rainfall), Average(0.6xpH, 0.2xTexture, 0.2xDepth))", 
                    
                    "Average(Thermal, Rainfall) x pH x Texture x Depth",
                    "Average((Thermal, Rainfall), (pH x Texture x Depth))",          
                    "Average(Thermal, Rainfall) x Average(pH, Texture, Depth)",          
                    "Average((Thermal, Rainfall) , Average(pH, Texture, Depth))",
                    "Average(Thermal, Rainfall) x Average(0.6xpH, 0.2xTexture, 0.2xDepth)",  
                    "Average((Thermal, Rainfall) x Average(0.6xpH, 0.2xTexture, 0.2xDepth))", 
                    
                    "Thermal x pH x Texture x Depth",
                    "Average(Thermal x pH x Texture x Depth)",           
                    "Thermal x Average(pH, Texture, Depth)",           
                    "Average(Thermal, Average(pH, Texture, Depth))",
                    "Thermal x Average(0.6xpH, 0.2xTexture, 0.2xDepth)",   
                    "Average(Thermal, Average(0.6xpH, 0.2xTexture, 0.2xDepth))")
# getting the results per index
res <- lapply( indices[,-c(1,2)] , function(x) rbind( mean = mean(x) ,
                                         sd = sd(x) ,
                                         median = median(x) ,
                                         minimum = min(x) ,
                                         maximum = max(x) ,
                                         s.size = length(x) ) )

as.data.frame( res,check.names = F)
write.csv(res, "./Analysis/sixth/results_perindex.csv")
write_csv(as.data.frame( res,check.names = F ), "./Analysis/sixth/results_perindex1.csv")

res2 <- as.data.frame(t(as.data.frame(res,check.names = F )))
              
## creating the dataset per crop groups
tindices = t(cbind(indices[,-c(1,2)], num.sample[,2]))
colnames(tindices) <- c(as.character(indices[,1]))

CEREALS <- yearmax1.m[yearmax1.m$crop %in% c( "Fonio", 
                                             "Finger Millet", 
                                             "Foxtail Millet", 
                                             "Proso Millet", 
                                             "Pearl Millet", 
                                             "Teff", 
                                             "Paddy",
                                             "Barley",
                                             "Common Wheat",
                                             "Sugarcane",
                                             "Quinoa"), ]
  
LEGUMES  <- yearmax1.m[yearmax1.m$crop %in% c("Black Gram",
                                              "Bambara Groundnut",
                                              "Cowpea",
                                              "Egyptian sesban",
                                              "Leucaena", 
                                              "Hyacinth Bean",
                                              "Pigeon pea",
                                              "Soybean",
                                              "Velvet Bean", 
                                              "White Pea", 
                                              "Winged Bean", 
                                              "Tepary Beans ",
                                              "Moringa"), ]

TUBER_ROOT <- yearmax1.m[yearmax1.m$crop %in% c("Taro (Cocoyam)",
                                           "Yautia",
                                           "Mashua",
                                           "Water Yam",
                                           "Cassava",
                                           "Oca"), ]

FRUITS <- yearmax1.m[yearmax1.m$crop %in% c("Soursop",
                                           "Carob",
                                           "Breadfruit",
                                           "Guava",
                                           "Indian Mulberry",
                                           "Pomelo",
                                           "Avocado",
                                           "Java-plum",
                                           "Pomegranate",
                                           "Akee"), ]

lbl = c("Thermal",
   "Rainfall",
   "pH",
   "Depth",
   "Texture",
   
   "Thermal x Rainfall",                         
   "Average(Thermal, Rainfall)",
   "pH x Texture x Depth",
   "Average(pH, Texture, Depth)",
   "Average(0.6xpH, 0.2xTexture, 0.2xDepth)",
   
   "Thermal x Rainfall x pH x Texture x Depth",
   "Average((Thermal x Rainfall), (pH x Texture x Depth))",
   "(Thermal x Rainfall)x Average(pH, Texture, Depth)", 
   "Average((Thermal x Rainfall), Average(pH, Texture, Depth))",
   "(Thermal x Rainfall) x Average(0.6xpH, 0.2xTexture, 0.2xDepth)", 
   "Average((Thermal x Rainfall), Average(0.6xpH, 0.2xTexture, 0.2xDepth))", 
   
   "Average(Thermal, Rainfall) x pH x Texture x Depth",
   "Average((Thermal, Rainfall), (pH x Texture x Depth))",          
   "Average(Thermal, Rainfall) x Average(pH, Texture, Depth)",          
   "Average((Thermal, Rainfall) , Average(pH, Texture, Depth))",
   "Average(Thermal, Rainfall) x Average(0.6xpH, 0.2xTexture, 0.2xDepth)",  
   "Average((Thermal, Rainfall) x Average(0.6xpH, 0.2xTexture, 0.2xDepth))", 
   
   "Thermal x pH x Texture x Depth",
   "Average(Thermal x pH x Texture x Depth)",           
   "Thermal x Average(pH, Texture, Depth)",           
   "Average(Thermal, Average(pH, Texture, Depth))",
   "Thermal x Average(0.6xpH, 0.2xTexture, 0.2xDepth)",   
   "Average(Thermal, Average(0.6xpH, 0.2xTexture, 0.2xDepth))")

##  figure 1
              
alldata.sp <- as.data.frame(alldata)
library(sp)
coordinates(alldata.sp) <- ~ longitude + latitude

proj4string(news) <- CRS("+init=epsg:4326")
library(sf)
sfalldata <- st_as_sf(alldata.sp)


world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()


map <- world +
  geom_point(aes(x = longitude, y = latitude, colour = crop),
             data = yearmax, alpha = .5) 

map <- world +
  geom_point(aes(x = Longitude, y = Latitude, colour = Country.of.origin),
             data = as.data.frame(bgdata), alpha = .5) 
png("./Analysis/sixth/Figure1.png", width = 3500, heigh = 2200,res = 300)
map + theme(legend.direction = "horizontal", legend.box.background = element_rect())
dev.off()              
              
              
## figure 2

for (i in c("CEREALS", "LEGUMES", "TUBER_ROOT", "FRUITS")){

  data = get(i)

  #for (i in levels(yearmax$crop)){
  # data <- yearmax1.m[yearmax1.m$crop %in% i, ]
  png(file=paste('./Analysis/sixth/combined.png', sep =""), width = 7000, height = 5500, res = 600)
  p1 <-
    ggplot(data = CEREALS, aes(x=variable, y=value)) +
    geom_boxplot(outlier.shape = NA)+ coord_flip()+
    #geom_text(aes(label=..count..), y=-1, stat='count', colour="red", size=4)+
    ylab("CEREALS") +
    xlab("Index type")+ scale_x_discrete(labels=lbl)#+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

  p2 <-
    ggplot(data = LEGUMES, aes(x=variable, y=value)) +
    geom_boxplot(outlier.shape = NA)+ coord_flip()+
    #geom_text(aes(label=..count..), y=-1, stat='count', colour="red", size=4)+
    ylab("LEGUMES") +
    xlab("Index type")+ scale_x_discrete(labels=lbl)#+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

  p3 <-
    ggplot(data = TUBER_ROOT, aes(x=variable, y=value)) +
    geom_boxplot(outlier.shape = NA)+ coord_flip()+
    #geom_text(aes(label=..count..), y=-1, stat='count', colour="red", size=4)+
    ylab("TUBER_ROOT") +
    xlab("Index type")+ scale_x_discrete(labels=lbl) #+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

  p4 <-
    ggplot(data = FRUITS, aes(x=variable, y=value)) +
    geom_boxplot(outlier.shape = NA)+ coord_flip()+
    #geom_text(aes(label=..count..), y=-1, stat='count', colour="red", size=4)+
    ylab("FRUITS") +
    xlab("Index type")+ scale_x_discrete(labels=lbl)#+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
  library(gridExtra)
  p = grid.arrange(p1, p2, p3, p4, nrow = 2)

  #print(p + facet_grid(rows = vars(crop)))
  print(p)

  dev.off()


    
## figure 3 and 4      
      
bgdata <- readRDS(file = "./data/extract/old/third/BGIITA_AllHasNA.rds")


png("./Analysis/sixth/bg_depth.png", width = 7000, height = 5500, res = 600)
plot(density(bgdata$BDRICM.M,na.rm = T), xlab = "Soil depth (cm)", main = "", xlim=c(45,205))
abline(v=50, col = "red", lty = 2)
abline(v=150)
abline(v=175)
abline(v=200, col = "red", lty = 2)
dev.off()

densityDepth <- data.frame(Depth=density(bgdata$BDRICM.M,na.rm = T)$x, 
                       densityDepth = density(bgdata$BDRICM.M,na.rm = T)$y)
write_csv(densityDepth, "./Analysis/sixth/bg_density_data_BDRICM.csv")


png("./Analysis/sixth/bg_pH.png", width = 7000, height = 5500, res = 600)
#par(mfrow=c(2,2)) 
# for pH
plot(density(bgdata$PHIHOX.M.sl1/10,na.rm = T), xlab = "pH", main = "")
lines(density(bgdata$PHIHOX.M.sl2/10,na.rm = T), col = "blue")
lines(density(bgdata$PHIHOX.M.sl3/10,na.rm = T), col = "red")
lines(density(bgdata$PHIHOX.M.sl4/10,na.rm = T), col = "green")
lines(density(bgdata$PHIHOX.M.sl5/10,na.rm = T), col = "yellow")

legend(7.5, 1.2, c("0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm"), col = c("black", "blue", "red", "green", "yellow"),
         lwd = c(3, 3, 3, 3, 3))

abline(v=4.3, col = "red", lty = 2)
abline(v=5.0)
abline(v=6.5)
abline(v=7.0, col = "red", lty = 2)
dev.off()

# for sandclay
png("./Analysis/fifth/bg_sandclay.png", width = 7000, height = 5500, res = 600)
plot(density(bgdata$SNDPPT.M.sl1,na.rm = T), xlab = "Avg. Clay% (left curves) and Sand% (right curves)", main = "", xlim=c(0,94), ylim = c(0,0.085))
lines(density(bgdata$SNDPPT.M.sl2,na.rm = T), col = "blue")
lines(density(bgdata$SNDPPT.M.sl3,na.rm = T), col = "red")
lines(density(bgdata$SNDPPT.M.sl4,na.rm = T), col = "green")
lines(density(bgdata$SNDPPT.M.sl5,na.rm = T), col = "yellow")

lines(density(bgdata$CLYPPT.M.sl1,na.rm = T), col = "black")
lines(density(bgdata$CLYPPT.M.sl2,na.rm = T), col = "blue")
lines(density(bgdata$CLYPPT.M.sl3,na.rm = T), col = "red")
lines(density(bgdata$CLYPPT.M.sl4,na.rm = T), col = "green")
lines(density(bgdata$CLYPPT.M.sl5,na.rm = T), col = "yellow")
legend(35,0.086, c("0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm"), col = c("black", "blue", "red", "green", "yellow"),
       lwd = c(3, 3, 3, 3, 3))


#abline(v=15, col = "red", lty = 2)
abline(v=0, col = "red", lty = 2)
abline(v=10)
abline(v=15)

abline(v=70, col = "red", lty = 2)
abline(v=86)
abline(v=100, col = "red", lty = 2)

dev.off()

# for clay only
png("./Analysis/sixth/bg_clay.png", width = 7000, height = 5500, res = 600)
plot(density(bgdata$CLYPPT.M.sl1,na.rm = T), xlab = "%Clay", main = "", xlim=c(0,48), ylim = c(0,0.085))
lines(density(bgdata$CLYPPT.M.sl2,na.rm = T), col = "blue")
lines(density(bgdata$CLYPPT.M.sl3,na.rm = T), col = "red")
lines(density(bgdata$CLYPPT.M.sl4,na.rm = T), col = "green")
lines(density(bgdata$CLYPPT.M.sl5,na.rm = T), col = "yellow")

legend(39,0.087, c("0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm"), col = c("black", "blue", "red", "green", "yellow"),
       lwd = c(3, 3, 3, 3, 3))

abline(v=10)
abline(v=20)

dev.off()


png("./Analysis/sixth/bg_temp_rain.png",  width = 7000, height = 11000, res = 600)
par(mfrow=c(6,4)) 
plot(density(bgdata$tavgJan,na.rm = T), xlab = "Avg. Temperature °C (Jan)", main = "", col = "grey")
lines(density(bgdata$tminJan,na.rm = T), col = "blue")
lines(density(bgdata$tmaxJan,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgFeb,na.rm = T), xlab = "Avg. Temperature °C (Feb)", main = "", col = "grey")
lines(density(bgdata$tminFeb,na.rm = T), col = "blue")
lines(density(bgdata$tmaxFeb,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgMar,na.rm = T), xlab = "Avg. Temperature °C (Mar)", main = "", col = "grey")
lines(density(bgdata$tminMar,na.rm = T), col = "blue")
lines(density(bgdata$tmaxMar,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgApr,na.rm = T), xlab = "Avg. Temperature °C (Apr)", main = "", col = "grey")
lines(density(bgdata$tminApr,na.rm = T), col = "blue")
lines(density(bgdata$tmaxApr,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgMay,na.rm = T), xlab = "Avg. Temperature °C (May)", main = "", col = "grey")
lines(density(bgdata$tminMay,na.rm = T), col = "blue")
lines(density(bgdata$tmaxMay,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgJun,na.rm = T), xlab = "Avg. Temperature °C (Jun)", main = "", col = "grey")
lines(density(bgdata$tminJun,na.rm = T), col = "blue")
lines(density(bgdata$tmaxJun,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgJul,na.rm = T), xlab = "Avg. Temperature °C (Jul)", main = "", col = "grey")
lines(density(bgdata$tminJul,na.rm = T), col = "blue")
lines(density(bgdata$tmaxJul,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgAug,na.rm = T), xlab = "Avg. Temperature °C (Aug)", main = "", col = "grey")
lines(density(bgdata$tminAug,na.rm = T), col = "blue")
lines(density(bgdata$tmaxAug,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgSep,na.rm = T), xlab = "Avg. Temperature °C (Sep)", main = "", col = "grey")
lines(density(bgdata$tminSep,na.rm = T), col = "blue")
lines(density(bgdata$tmaxSep,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgOct,na.rm = T), xlab = "Avg. Temperature °C (Oct)", main = "", col = "grey")
lines(density(bgdata$tminOct,na.rm = T), col = "blue")
lines(density(bgdata$tmaxOct,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgNov,na.rm = T), xlab = "Avg. Temperature °C (Nov)", main = "", col = "grey")
lines(density(bgdata$tminNov,na.rm = T), col = "blue")
lines(density(bgdata$tmaxNov,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)

plot(density(bgdata$tavgDec,na.rm = T), xlab = "Avg. Temperature °C (Dec)", main = "", col = "grey")
lines(density(bgdata$tminDec,na.rm = T), col = "blue")
lines(density(bgdata$tmaxDec,na.rm = T), col = "red")
abline(v=16, col = "red", lty = 2)
abline(v=19)
abline(v=30)
abline(v=38, col = "red", lty = 2)
dev.off()

# rains
      
plot(density(rainaggregs$RMS_Jan ,na.rm = T), xlab = "Rainfall mm (Jan-Apr Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Feb,na.rm = T), xlab = "Rainfall mm (Feb-May Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Mar,na.rm = T), xlab = "Rainfall mm (Mar-Jun Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Apr,na.rm = T), xlab = "Rainfall mm (Apr-Jul Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_May,na.rm = T), xlab = "Rainfall mm (May-Aug Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$JRMS_un,na.rm = T), xlab = "Rainfall mm (Jun-Sep Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Jul,na.rm = T), xlab = "Rainfall mm (Jul-Oct Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Aug,na.rm = T), xlab = "Rainfall mm (Aug-Nov Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Sep,na.rm = T), xlab = "Rainfall mm (Sep-Dec Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Oct,na.rm = T), xlab = "Rainfall mm (Oct-Jan Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Nov,na.rm = T), xlab = "Rainfall mm (Nov-Feb Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
plot(density(rainaggregs$RMS_Dec,na.rm = T), xlab = "Rainfall mm (Dec-Mar Season)", main = "")
abline(v=300, col = "red", lty = 2)
abline(v=750)
abline(v=1400)
abline(v=3000, col = "red", lty = 2)
dev.off()


