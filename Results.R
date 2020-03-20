
# library(dplyr)
# library(ggplot2)
# library(sp)
# library(rgdal)
# library(leaflet)
# library(maptools)
# library(maps)
# library(sf)
setwd("/Users/ej/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx2/")
library(tidyverse)
library(reshape2)
library(ggplot2)

# Getting simple statistics for each crop 
alldata <- readRDS('./data/output/Oct19Combined/alldata.rds')
# alldata5bg <- readRDS('./data/output/fifth/BGIITA_alldata.rds')
# alldata <- bind_rows(alldata5, alldata5bg)
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

#yearmax5bg <- as.tibble(readRDS('./data/Output/fifth/BGIITA_yearmax.rds'))
yearmax <- as_tibble(readRDS('./data/Output/Oct19Combined/yearmax.rds'))
#yearmaxbg <-  as.tibble(readRDS('./data/Output/third/BG_IITA_yearmax.rds'))
# the error belongs to the factor level 
#yearmax <- bind_rows(yearmax4, yearmax5, yearmaxbg)
# correcting the error
#yearmax$crop = as_factor(yearmax$crop)
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
# 79795 records remain
# writing ecological data out
ecology <- readRDS("./data/ecology/new_ecology.rds")
#write.csv(ecology[ecology$name_var_lndrce %in% levels(yearmax$crop),], "./data/ecology/ecologywithtexture.csv")

# out for testing thea accuracy
# yearmax.eval <- yearmax[sample(nrow(yearmax), 0.01*nrow(yearmax)), ]
# ecols = data.frame(matrix(nrow = 0, ncol=ncol(ecology)))
# ecols = NULL
# ns = NULL
# for (i in 1:749){
#   # ecols = rbind(ecols, ecology[which(ecology$name_var_lndrce == as.character(yearmax.eval$crop)[i]),])
#   # print(ecology$name_var_lndrce == as.character(yearmax.eval$crop[i]))
#   n = which(ecology$name_var_lndrce == as.character(yearmax.eval$crop)[i])
#   #print(cbind(ecology$name_var_lndrce[n], as.character(yearmax.eval$crop)[i]))
#   ecols = bind_rows(list(ecols, as.list(ecology[n[1],])))
#   #ns = append(ns, n)
#   #print(n)
#   }
library(data.table)
#write.csv(bind_cols(ecols,yearmax.eval), "./data/output/third/OnePercentTestData.csv")

yearmax1 <- yearmax[,-c(1,3:10)]
yearmax1.m <- melt(yearmax1, id.vars = "crop")

# yearmax1.m.m <- melt(yearmax1, id.vars = "value")
# 
# print(which(is.na(yearmax1.m[,c("value")])))
# names(yearmax)
# [1] "point"                            "crop"                             "latitude"                        
# [4] "longitude"                        "tempdata"                         "raindata"                        
# [7] "phdata"                           "depthdata"                        "sanddata"                        
# [10] "claydata"                         "tempsuit"                         "rainsuit"                        
# [13] "phsuit"                           "depthsuit"                        "texturesuit"                     
# [16] "climprod"                         "climave"                          "soilprod"                        
# [19] "soilave"                          "soilaveweighted"                  "climprod_soilprod_allprod"       
# [22] "climprod_soilprod_allave"         "climprod_soilave_allprod"         "climprod_soilave_allave"         
# [25] "climprod_soilaveweighted_allprod" "climprod_soilaveweighted_allave"  "climave_soilprod_allprod"        
# [28] "climave_soilprod_allave"          "climave_soilave_allprod"          "climave_soilave_allave"          
# [31] "climave_soilaveweighted_allprod"  "climave_soilaveweighted_allave"   "norainـsoilprodـallprod"         
# [34] "norainـsoilprodـallave"           "norainـsoilaveـallprod"           "norainـsoilaveـallave"           
# [37] "norainـsoilaveweightedـallprod"   "norainـsoilaveweightedـallave" 

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
# printing out the results of mean in a box plot
png(file="./Analysis/sixth/boxplot_perindex_mean.png", width = 2500, height = 1500, res = 200)
mean <- 
  ggplot(data = res2, aes(x= row.names(res2), y = mean)) +
  geom_boxplot(outlier.shape = NA)+ coord_flip()+  
  geom_text(aes(label=..count..), y=-1, stat='count', colour="red", size=4)+
  ylab("Mean of predictions") +
  xlab("Index type")
print(mean)
dev.off()

png(file="./Analysis/sixth/boxplot_perindex_sd.png", width = 2500, height = 1500, res = 200)
sd <- 
  ggplot(data = res2, aes(x= row.names(res2), y = sd)) +
  geom_boxplot(outlier.shape = NA)+ coord_flip()+  
  geom_text(aes(label=..count..), y=-1, stat='count', colour="red", size=4)+
  ylab("SD of predictions") +
  xlab("Index type")
print(sd)
dev.off()


# print the geo_line as well:

# creating the dataset per crop groups
tindices = t(cbind(indices[,-c(1,2)], num.sample[,2]))
colnames(tindices) <- c(as.character(indices[,1]))
#write.csv(as.data.frame(tindices, check.names=F), "./Analysis/fifth/results_percrop.csv")

#write.csv(cbind(levels(yearmax$crop), levels(as.factor(alldata$sci_name)), num.sample), "./Analysis/fifth/crops_count.csv") #### didnt

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

# cerealveg2 <- yearmax1.m[yearmax1.m$crop %in% c("Fonio",
#                                               "Quinoa",
#                                               "Okra",
#                                               "Sesame"), ]

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

# cereal1 <- yearmax1.m[yearmax1.m$crop %in% c("Paddy",
#                                              "Sugarcane",
#                                              "Common Wheat"
#                                               ), ]
# 
# legume1 <- yearmax1.m[yearmax1.m$crop %in% c("Cowpea",
#                                             "Hyacinth Bean",
#                                             "Leucaena",
#                                             "Pigeon pea",
#                                             "Soybean"), ]
# 
# legume2 <- yearmax1.m[yearmax1.m$crop %in% c("Winged Bean",
#                                             "Bitter Bean",
#                                             "Long Beans",
#                                             "Black Gram"), ]
# 
# legume3 <- yearmax1.m[yearmax1.m$crop %in% c("Sesbania",
#                                             "Egyptian sesban",
#                                             "Velvet Bean",
#                                             "White Pea"), ]
# 
# legume4 <- yearmax1.m[yearmax1.m$crop %in% c("adzuki bean",
#                                             "African yam bean"), ]
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
}
#}

# for (i in levels(yearmax1.m$variable)){
#   data <- yearmax1.m[yearmax1.m$variable %in% i, ]
#   png(file=paste('./data/output/third/', i, ".png", sep =""), width = 2500, height = 1500, res = 200)
#   p <- 
#     ggplot(data = data, aes(x=variable, y=value)) +
#     geom_boxplot(outlier.shape = NA)+ coord_flip()+  
#     geom_text(aes(label=..count..), y=-1, stat='count', colour="red", size=4)+
#     ylab("Suitability index") +
#     xlab("Index type")
#   print(p + facet_grid(rows = vars(crop)))
#   dev.off()
# }

## ggmap tutorial
alldata.sp <- as.data.frame(alldata)
library(sp)
coordinates(alldata.sp) <- ~ longitude + latitude

proj4string(news) <- CRS("+init=epsg:4326")
library(sf)
sfalldata <- st_as_sf(alldata.sp)

### Use GGPLOTLY https://www.r-bloggers.com/how-to-deal-with-ggplotly-huge-maps/
library(ggplot2)
library(maps)
library(ggthemes)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()


# map <- world +
#   geom_point(aes(x = longitude, y = latitude,
#                  text = paste('crop: ', crop,
#                               'tempsuit : ', tempsuit), size = norainـsoilaveweightedـallave),
#              data = yearmax, colour = 'purple', alpha = .5) +
#   scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000)) +
#   labs(size = 'Followers')


map <- world +
  geom_point(aes(x = longitude, y = latitude, colour = crop),
             data = yearmax, alpha = .5) 

map <- world +
  geom_point(aes(x = Longitude, y = Latitude, colour = Country.of.origin),
             data = as.data.frame(bgdata), alpha = .5) 
png("./Analysis/sixth/Figure1.png", width = 3500, heigh = 2200,res = 300)
map + theme(legend.direction = "horizontal", legend.box.background = element_rect())
dev.off()


library(htmlwidgets)
saveWidget(map, "./Analysis/sixth/test.html")
# This is the static map that I’m animating using ggplotly, with the following code:

# library(plotly)

# ggplotly(map, tooltip = c('text', 'size'))


# using dplyr for summarising the value
# all.indices <- group_by(yearmax1.m, variable) %>% summarise(mean = mean(value))
# library(glue)
# 
# for (i in names(yearmax[,c(12:38)])){
#   assign(i, group_by(yearmax1, crop) %>% summarise(mean = mean(i)))
# }



# library(tidyverse)
# p <- ggplot(mpg, aes(displ, cty)) + geom_point()
# 
# # Use vars() to supply variables from the dataset:
# p + facet_grid(rows = vars(drv))
# 
# 
# p <- ggplot(yearmax1, aes(climave)) + geom_boxplot()
# 
# # Use vars() to supply variables from the dataset:
# p + facet_grid(rows = vars(crop))
# 
# p <- ggplot(data = yearmax1.m, aes(x=variable, y=value)) + geom_boxplot()
# p + facet_grid(rows = vars(crop))


plot(yearmax$climprod_soilaveweighted_allave)




## statistics ------------------










# pearlmilmcal <- read.csv("Calcspearlmilm.csv")
# #pearlmilmcal <- pearlmilmcal %>% mutate(norain = temp * ph * texture * depth* 0.000001)
# 
# 
# 
# 
#     p1 = ggplot(pearlmilmcal, aes(temp))+geom_histogram(bins = 5) + 
#   xlab("Temperature suitability index") +
# theme(axis.text=element_text(size=12),
#       axis.title=element_text(size=14,face="bold"))
# 
#    p2 = ggplot(pearlmilmcal, aes(rain))+geom_histogram(bins = 5) + 
#   xlab("Rainfall suitability index") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))
# 
#    p3 = ggplot(pearlmilmcal, aes(ph))+geom_histogram(bins = 5) + 
#   xlab("PH suitability index") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))
# 
#    p4 = ggplot(pearlmilmcal, aes(depth))+geom_histogram(bins = 5) + 
#   xlab("Depth suitability index") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))
# 
#   p5 = ggplot(pearlmilmcal, aes(texture))+geom_histogram(bins = 5) + 
#   xlab("Texture suitability index") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))
# 
#   p6 = ggplot(pearlmilmcal, aes(total))+geom_histogram(bins = 5) + 
#   xlab("Total suitability index") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))
# 
#    p7 = ggplot(pearlmilmcal, aes(norain))+geom_histogram(bins = 5) + 
#   xlab("Total suitability index (excluding Rainfall index)") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))
# 
#    multiplot(p1,p2,p3,p4,p5,p6,p7, cols=2)
# 
#    write.csv(round(stat.desc(pearlmilmcal[,c("temp", "rain", "ph", "depth","texture","total","norain")]),0), "pearlmilmcal_stats.csv")
#    
#    
#    #pearlmilmts <- read.csv("TSpearlmilm.csv")
#    #names(pearlmilmts)
#    
#    #ggplot(pearlmilmts, aes(Jan)) + geom_boxplot()
#    
# # for finger millet
#    fingermilmcal <- read.csv("Calcsfingermilm.csv")
#    fingermilmcal <- fingermilmcal %>% mutate(norain = temp * ph * texture * depth* 0.000001)
#    
#    p1 = ggplot(fingermilmcal, aes(temp))+geom_histogram(bins = 5) + 
#      xlab("Temperature suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p2 = ggplot(fingermilmcal, aes(rain))+geom_histogram(bins = 5) + 
#      xlab("Rainfall suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p3 = ggplot(fingermilmcal, aes(ph))+geom_histogram(bins = 5) + 
#      xlab("PH suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p4 = ggplot(fingermilmcal, aes(depth))+geom_histogram(bins = 5) + 
#      xlab("Depth suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p5 = ggplot(fingermilmcal, aes(texture))+geom_histogram(bins = 5) + 
#      xlab("Texture suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p6 = ggplot(fingermilmcal, aes(total))+geom_histogram(bins = 5) + 
#      xlab("Total suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p7 = ggplot(fingermilmcal, aes(norain))+geom_histogram(bins = 5) + 
#      xlab("Total suitability index (excluding Rainfall index)") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    multiplot(p1,p2,p3,p4,p5,p6,p7, cols=2)
#    
#    write.csv(round(stat.desc(fingermilmcal[,c("temp", "rain", "ph", "depth","texture","total","norain")]),0), "fingermilmcal_stats.csv")
#    
#    
# ## for pigeon pea   
#    p1 = ggplot(pigeonmcal, aes(temp))+geom_histogram(bins = 5) + 
#      xlab("Temperature suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p2 = ggplot(pigeonmcal, aes(rain))+geom_histogram(bins = 5) + 
#      xlab("Rainfall suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p3 = ggplot(pigeonmcal, aes(ph))+geom_histogram(bins = 5) + 
#      xlab("PH suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p4 = ggplot(pigeonmcal, aes(depth))+geom_histogram(bins = 5) + 
#      xlab("Depth suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p5 = ggplot(pigeonmcal, aes(texture))+geom_histogram(bins = 5) + 
#      xlab("Texture suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p6 = ggplot(pigeonmcal, aes(total))+geom_histogram(bins = 5) + 
#      xlab("Total suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p7 = ggplot(pigeonmcal, aes(norain))+geom_histogram(bins = 5) + 
#      xlab("Total suitability index (excluding Rainfall index)") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    multiplot(p1,p2,p3,p4,p5,p6,p7, cols=2)
#    
#    write.csv(round(stat.desc(pigeonmcal[,c("temp", "rain", "ph", "depth","texture","total","norain")]),0), "pigeonmcal_stats.csv")
#    
# ## for breadfruit
#    readfmcal <- read.csv("Calcsbreadfm.csv")
#    breadfmcal <- breadfmcal %>% mutate(norain = temp * ph * texture * depth* 0.000001)
#    
#    p1 = ggplot(breadfmcal, aes(temp))+geom_histogram(bins = 5) + 
#      xlab("Temperature suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p2 = ggplot(breadfmcal, aes(rain))+geom_histogram(bins = 5) + 
#      xlab("Rainfall suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p3 = ggplot(breadfmcal, aes(ph))+geom_histogram(bins = 5) + 
#      xlab("PH suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p4 = ggplot(breadfmcal, aes(depth))+geom_histogram(bins = 5) + 
#      xlab("Depth suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p5 = ggplot(breadfmcal, aes(texture))+geom_histogram(bins = 5) + 
#      xlab("Texture suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p6 = ggplot(breadfmcal, aes(total))+geom_histogram(bins = 5) + 
#      xlab("Total suitability index") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    p7 = ggplot(breadfmcal, aes(norain))+geom_histogram(bins = 5) + 
#      xlab("Total suitability index (excluding Rainfall index)") +
#      theme(axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold"))
#    
#    multiplot(p1,p2,p3,p4,p5,p6,p7, cols=2)
#    
#    write.csv(round(stat.desc(breadfmcal[,c("temp", "rain", "ph", "depth","texture","total","norain")]),0), "breadfmcal_stats.csv")
#    
#    
# # Multiple plot function
# #
# # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# # - cols:   Number of columns in layout
# # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# #
# # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# # then plot 1 will go in the upper left, 2 will go in the upper right, and
# # 3 will go all the way across the bottom.
# #
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ## mapping-----------------------
# 
# mapbreadf <- read.csv("yearlycalc_breadfm.csv" )
# mapfingermil <- read.csv("yearlycalc_fingermilm.csv" )
# mappearlmil <- read.csv("yearlycalc_pearlmilm.csv" )
# mappigeon <- read.csv("yearlycalc_pigeonm.csv" )
# mapyam <- read.csv("yearlycalc_yamm.csv" )
# 
# mapmoringaf <- read.csv("yearlycalc_Moringa.csv")
# 
# 
# # mapbreadf <- read.csv("yearlycalc_breadfm_accession.csv" )
# # mapfingermil <- read.csv("yearlycalc_fingermilm_accession.csv" )
# # mappearlmil <- read.csv("yearlycalc_pearlmilm_accession.csv" )
# # mappigeon <- read.csv("yearlycalc_pigeonm_accession.csv" )
# # mapyam <- read.csv("yearlycalc_yamm_accession.csv" )
# 
# coordinates(mapbreadf) <- ~ lon+lat
# coordinates(mapfingermil) <- ~ lon+lat
# coordinates(mappearlmil) <- ~ lon+lat
# coordinates(mappigeon) <- ~ lon+lat
# coordinates(mapyam) <- ~ lon+lat
# coordinates(mapmoringaf) <- ~ lon+lat
# 
# 
# proj4string(mapbreadf) = CRS("+init=epsg:4326")
# proj4string(mapfingermil) = CRS("+init=epsg:4326")
# proj4string(mappearlmil) = CRS("+init=epsg:4326")
# proj4string(mappigeon) = CRS("+init=epsg:4326")
# proj4string(mapyam) = CRS("+init=epsg:4326")
# 
# proj4string(mapmoringaf) = CRS("+init=epsg:4326")
# 
# sfbreadf= st_as_sf(mapbreadf)
# sffingermil= st_as_sf(mapfingermil)
# sfpearlmil= st_as_sf(mappearlmil)
# sfpigeon= st_as_sf(mappigeon)
# sfyam= st_as_sf(mapyam)
# sfmoringa = st_as_sf(mapmoringaf)
# 
# sfyam= st_as_sf(mapmoringaf)
# 
# st_write(sfbreadf, dsn = "mapbreadf.shp", layer = "mapbreadf.shp", driver = "ESRI Shapefile")
# st_write(sfmoringa, dsn = "mapbreadf.shp", layer = "mapbreadf.shp", driver = "ESRI Shapefile")
# 
# 
# #writeOGR(mapfingermil, "mapfingermil.shp", driver = "ESRI Shapefile", "SUBTAUTHOR") #shitty function
# writePointsShape(mapbreadf,"mapbreadf") #dont use it is deprecated
# writePointsShape(mapfingermil,"mapfingermils")
# writePointsShape(mappearlmil,"mappearlmil")
# writePointsShape(mappigeon,"mappigeon")
# writePointsShape(mapyam,"mapyam")
# 
# 
# ## off to QGIS for creating maps
# map("world")
# plot(mapbreadf, add=T)
# plot(mapfingermil, add=T, col="green")
# 
# ## plotting histogram ---------
# 
# breadf <- read.csv("yearlycalc_breadfm.csv" )
# fingermil <- read.csv("yearlycalc_fingermilm.csv" )
# pearlmil <- read.csv("yearlycalc_pearlmilm.csv" )
# pigeon <- read.csv("yearlycalc_pigeonm.csv" )
# yam <- read.csv("yearlycalc_yamm.csv" )
# 
# gg <- ggplot(fingermil, aes(allave))
# gg <- gg + geom_histogram(binwidth = 5)
# gg
# 
# library(reshape2)
# library(ggplot2)
# d <- melt(fingermil[,-c(1,12,14)])
# ggplot(d,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free_x") + 
#   geom_histogram(binwidth = 20)
# 
# d <- melt(pearlmil[,-c(1,12,14)])
# ggplot(d,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free_x") + 
#   geom_histogram(binwidth = 20)
# 
# d <- melt(pigeon[,-c(1,12,14)])
# ggplot(d,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free_x") + 
#   geom_histogram(binwidth = 20)
# 
# 
# d <- melt(yam[,-c(1,12,14)])
# ggplot(d,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free_x") + 
#   geom_histogram(binwidth = 20)
# 
# 
# 
# 
# 
# 
# ## ranking -----------
# breadf <- read.csv("yearlycalcbreadfm.csv" )
# fingermil <- read.csv("yearlycalcfingermilm.csv" )
# pearlmil <- read.csv("yearlycalcpearlmilm.csv" )
# pigeon <- read.csv("yearlycalcpigeonm.csv" )
# yam <- read.csv("yearlycalcyamm.csv" )
# 
# bfrannks <- numeric()
# 
# for (i in 2:9){
#   bfrannks = append(bfrannks, (length(which(breadf[,i]>=25))/nrow(breadf))*100)
# }
# 
# fmrannks <- numeric()
# 
# for (i in 2:9){
#   fmrannks = append(fmrannks, (length(which(fingermil[,i]>=25))/nrow(fingermil))*100)
# }
# 
# pmrannks <- numeric()
# 
# for (i in 2:9){
#   pmrannks = append(pmrannks, (length(which(pearlmil[,i]>=25))/nrow(pearlmil))*100)
# }
# 
# pigrannks <- numeric()
# 
# for (i in 2:9){
#   pigrannks = append(pigrannks, (length(which(pigeon[,i]>=25))/nrow(pigeon))*100)
# }
# 
# yamrannks <- numeric()
# 
# for (i in 2:9){
#   yamrannks = append(yamrannks, (length(which(yam[,i]>=25))/nrow(yam))*100)
# }
# 
# allranks = data.frame()
# allranks = as.data.frame(rbind(bfrannks,fmrannks,pmrannks,pigrannks,yamrannks))
# names(allranks) = names(breadf)[2:9]
# round(allranks)
# write.csv(allranks, "allranks25.csv")
# 
# 
# 
# bfrannks <- numeric()
# 
# for (i in 2:9){
#   bfrannks = append(bfrannks, (length(which(breadf[,i]>=50))/nrow(breadf))*100)
# }
# 
# fmrannks <- numeric()
# 
# for (i in 2:9){
#   fmrannks = append(fmrannks, (length(which(fingermil[,i]>=50))/nrow(fingermil))*100)
# }
# 
# pmrannks <- numeric()
# 
# for (i in 2:9){
#   pmrannks = append(pmrannks, (length(which(pearlmil[,i]>=50))/nrow(pearlmil))*100)
# }
# 
# pigrannks <- numeric()
# 
# for (i in 2:9){
#   pigrannks = append(pigrannks, (length(which(pigeon[,i]>=50))/nrow(pigeon))*100)
# }
# 
# yamrannks <- numeric()
# 
# for (i in 2:9){
#   yamrannks = append(yamrannks, (length(which(yam[,i]>=50))/nrow(yam))*100)
# }
# 
# allranks = data.frame()
# allranks = as.data.frame(rbind(bfrannks,fmrannks,pmrannks,pigrannks,yamrannks))
# names(allranks) = names(breadf)[2:9]
# round(allranks)
# write.csv(allranks, "allranks50.csv")
# 
# ### CV -------------------
# 
# breadf <- read.csv("yearlycalc_breadfm_accession.csv" )
# fingermil <- read.csv("yearlycalc_fingermilm_accession.csv" )
# pearlmil <- read.csv("yearlycalc_pearlmilm_accession.csv" )
# pigeon <- read.csv("yearlycalc_pigeonm_accession.csv" )
# yam <- read.csv("yearlycalc_yamm_accession.csv" )
# 
# 
# CV <- function(mean, sd){
#   (sd/mean)*100
# }
# 
# 
# bfcv <- numeric()
# 
# for (i in 2:9){
#   bfcv = append(bfcv, CV(mean(breadf[,i]), sd(breadf[,i]))) 
# }
# 
# fmcv <- numeric()
# 
# for (i in 2:9){
#   fmcv = append(fmcv, CV(mean(fingermil[,i]), sd(fingermil[,i])))
# }
# 
# pmcv <- numeric()
# 
# for (i in 2:9){
#   pmcv = append(pmcv, CV(mean(pearlmil[,i]), sd(pearlmil[,i])))
# }
# 
# pigcv <- numeric()
# 
# for (i in 2:9){
#   pigcv = append(pigcv, CV(mean(pigeon[,i]), sd(pigeon[,i])))
# }
# 
# yamcv <- numeric()
# 
# for (i in 2:9){
#   yamcv = append(yamcv, CV(mean(yam[,i]), sd(yam[,i])))
# }
# 
# allcv = data.frame()
# allcv = as.data.frame(rbind(bfcv,fmcv,pmcv,pigcv,yamcv))
# names(allcv) = names(breadf)[2:9]
# round(allcv)
# write.csv(allcv, "allcv.csv")
# 
# 
# ## Correlation
# 
