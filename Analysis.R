# Analysis.R
# the purpose of this code is to calculate suitability indices 
# contact: ebrahim.jahanshiri@cffresearch.org, e.jahanshiri@gmail.com
library(data.table)
setwd("~/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx2/")

# reading ecological niche descriptors and combining the extracted data with presence/accession data
ecology <- readRDS("./data/ecology/new_ecology.rds")
alldatabg_extracted <- readRDS("./data/extract/Oct19Combined/alldatabg_extracted.rds") 
alldatabg_raw <- readRDS("./data/extract/Oct19Combined/alldatabg_raw.rds") 
alldata <- cbind(alldatabg_raw, alldatabg_extracted) 

# making sure all the cases have available soil, 
# climate and accession data and remote crops with very low records
# assigning separate data frames for climate variables
cropdata <- alldata[complete.cases(alldata[,]),] 
row.names(cropdata) <- 1:nrow(cropdata)
cropdata <- cropdata[-which(cropdata$sci_name == "Pachyrhizus erosus (L.) Urb"),]
cropdata <- cropdata[-which(cropdata$sci_name == "Ipomoea batatas (L.) Lam"),]
tempdata = cropdata[,28:39]
raindata = cropdata[,40:51]
#----------------------------
## Climate suitability functions
##---------------------------
tmonsuits = data.frame(matrix(nrow = 0, ncol=12))
tcropsuits = data.frame(matrix(nrow = 0, ncol=12))
rainaggregs = data.frame(matrix(nrow = 0, ncol=12))
rainsuits = data.frame(matrix(nrow = 0, ncol=12))
phsuits = NULL  
depthsuits = NULL
texturesuits = NULL
phdata = NULL
sandata = NULL
claydata = NULL
clayagg= NA
depthdata = NULL
raindataggregs = list()
crps = data.frame(matrix(nrow = 0, ncol=20))

# control that cropID is present in the data
crps = integer()
for (i in 1:nrow(cropdata)){
  crps = append(crps, which(ecology$cropid == as.numeric(cropdata$CFFcropid[i])))
}
if (any(crps == 0)){
  print("Season is 0")
  
}
# main loop for the calculation of indices

for (i in 1:nrow(cropdata)){
  tmonsuit = numeric()    
  tcropsuit = numeric()
  tcropsuitpren = numeric()
  raindataggreg = numeric()
  raindatapren = numeric()
  rainsuit = numeric()  
  phsuit = numeric()  
  depthsuit = numeric()
  texturesuit = numeric()
# for each record, get the season in months  
  crp = which(ecology$cropid == as.numeric(cropdata$CFFcropid[i]))[1]
  if (is.na(crp))
  cropname = ecology[crp, "name_var_lndrce"]
  season = round ((ecology[crp,"period_between_harvest_min"] + ecology[crp,"period_between_harvest_max" ])/60)
  cropid = cropdata$CFFcropid[i]

# functions 
  
tempsuitfun = function(x) {
  y=0
  tabs_min=ecology[crp,"temperature_absolute_min"] 
  topt_min=ecology[crp,"temperature_optimal_min"] 
  topt_max=ecology[crp,"temperature_optimal_max"] 
  tabs_max=ecology[crp,"temperature_absolute_max"]
  if (any(x > tabs_min) & any(x < topt_min))  {y = round(((x - tabs_min)/(topt_min - tabs_min))*100)}
  if (any(x > topt_min) & any(x < topt_max)) {y  = 100}
  if (any(x > topt_max) & any(x < tabs_max))  {y  = round((1-( (x - topt_max)/(tabs_max - topt_max)))*100)}
  if (any(x < tabs_min) | any(x > tabs_max))  {y = 0}
  return(y)
}  
rainsuitfun = function(x) {
  y=0
  rabs_min=ecology[crp,"rainfall_absolute_min"] 
  ropt_min=ecology[crp,"rainfall_optimal_min"] 
  ropt_max=ecology[crp,"rainfall_optimal_max"] 
  rabs_max=ecology[crp,"rainfall_absolute_max" ]
  if (any(x > rabs_min) & any(x < ropt_min))  {y = round(((x - rabs_min)/(ropt_min - rabs_min))*100)}
  if (any(x > ropt_min) & any(x < ropt_max)) {y  = 100}
  if (any(x > ropt_max) & any(x < rabs_max))  {y  = round((1-( (x - ropt_max)/(rabs_max - ropt_max)))*100)}
  if (any(x < rabs_min) | any(x > rabs_max))  {y = 0}
  return(y)
}  

phsuitfun = function(x) {
  y=0
  phabs_min=ecology[crp,"soil_ph_absolute_min"] 
  phopt_min=ecology[crp,"soil_ph_optimal_min"] 
  phopt_max=ecology[crp,"soil_ph_optimal_max"] 
  phabs_max=ecology[crp,"soil_ph_absolute_max"]
  if (any(x > phabs_min) & any(x < phopt_min))  {y = round(((x - phabs_min)/(phopt_min - phabs_min))*100)}
  if (any(x > phopt_min) & any(x < phopt_max)) {y  = 100}
  if (any(x > phopt_max) & any(x < phabs_max))  {y  = round((1-( (x - phopt_max)/(phabs_max - phopt_max)))*100)}
  if (any(x < phabs_min) | any(x > phabs_max))  {y = 0}
  return(y)
}  

depthsuitfun = function(x) {
  optz = 10
  if ((x >=0) & (x< 50)) {
    if (ecology[crp,"soil_depth_optimal_low"] == 1) { #change ecology3 to cropdata with cropdata
      optz = 100
    } else {
      optz = 10 
    }
  }
  if ((x >= 50) & (x< 150)) {
    if (ecology[crp, "soil_depth_optimal_medium"] == 1) {
      optz = 100
    } else {
      optz = 10
    }
  }
  if (x >= 150) {
    if (ecology[crp,"soil_depth_optimal_deep"] == 1) {
      optz = 100
    } else {
      optz = 10
    }
  }
  return(optz)
}  


#---------------
# calculations
#---------------

# calculate suitability based for each month based on each average
  for (j in 1:12){
    tempcalc =   tempdata[i,j]
# in case you are using WorldClim v1 make sure you include this line
    #tempcalc <- get(paste("tempaverage",i, sep=""))[j]/10 
    #tempcalc[is.na(tempcalc)] <- -9999 # to get rid of NA's
    #print(c(i=i,j=j))
    tempcalc <- lapply(tempcalc, tempsuitfun )
    tmonsuit= append(tmonsuit, as.numeric(tempcalc))
    #tempsuit = addLayer(tempsuit, tempcalc)
  }
# then derive temp suitability based on all possible seasons in a year
# in the meantime aggregate the rainfall data per season
  if (season <= 1) { 
    for (j in 1:12){
      tcropsuit = append(tcropsuit, tmonsuit[j])
    }
    raindataggreg = raindata[i,]
  }
  
  if ((season <= 12) & (season > 1)) {
    for (j in 1:(12-(season-1))){
      tcropsuit = append(tcropsuit, min(tmonsuit[j:(j+(season-1))]))
      raindataggreg = append(raindataggreg, sum(as.numeric(raindata[i,j:(j+(season-1))])))
    }
    
    #creating a stack for the months that fall over Dec
    tcropsuitpren = c(tmonsuit[(12-(season-2)):12], tmonsuit[1:(season-1)])
    raindatapren = c(raindata[i,(12-(season-2)):12], raindata[i,1:(season-1)])
    
    # adding the aggreages for the rest of the year
    for (j in 1:(length(tcropsuitpren)-(season-1))){
      tcropsuit = append(tcropsuit, min(tcropsuitpren[j:(j+(season-1))]))
      raindataggreg = append(raindataggreg, sum(as.numeric(raindatapren[j:(j+(season-1))])))
      
    }
  }
  if (season > 12) {
    # to calcuate for the prennials all layers are the same
    for (j in 1:12){
      tcropsuit = append(tcropsuit, min(tmonsuit))
      raindataggreg = append(raindataggreg, sum(as.numeric(raindata[i,1:12])))
      
    }
  }
  
raindataggregs <- rbindlist(list(raindataggregs,as.list (raindataggreg)))

# calcualte rain suitability


  for (j in 1:12){
# in case you are using WorldClim v1 make sure you include this line    
    #raincalc <- get(paste("rainaggreg",i, sep=""))[j] 
    #raincalc[is.na(raincalc)] <- -9999 # to get rid of NA's
    
    raincalc <- lapply(raindataggreg[j], rainsuitfun )
    
    #raincalc = mask(crop(raincalc, extent(boundary)), boundary) #masking zeros if needed
    
    rainsuit= append(rainsuit, as.numeric(raincalc))
    
  }

# depth standardise data for pH
  if (ecology[crp,"soil_depth_optimal_low"] == 1){
    phagg = (1/1200)*(5*(cropdata[i,"PHIHOX.M.sl1"]+cropdata[i,"PHIHOX.M.sl2"])+
                    10*(cropdata[i,"PHIHOX.M.sl2"]+cropdata[i,"PHIHOX.M.sl3"])+
                    15*(cropdata[i,"PHIHOX.M.sl3"]+cropdata[i,"PHIHOX.M.sl4"])+
                    30*(cropdata[i,"PHIHOX.M.sl4"]+cropdata[i,"PHIHOX.M.sl5"]))
  }else {
    if (ecology[crp, "soil_depth_optimal_medium"] == 1) {
      phagg = (1/2000)*(5*(cropdata[i,"PHIHOX.M.sl1"]+cropdata[i,"PHIHOX.M.sl2"])+
                    10*(cropdata[i,"PHIHOX.M.sl2"]+cropdata[i,"PHIHOX.M.sl3"])+
                    15*(cropdata[i,"PHIHOX.M.sl3"]+cropdata[i,"PHIHOX.M.sl4"])+
                    30*(cropdata[i,"PHIHOX.M.sl4"]+cropdata[i,"PHIHOX.M.sl5"])+
                    40*(cropdata[i,"PHIHOX.M.sl5"]+cropdata[i,"PHIHOX.M.sl6"]))
    }else {
      phagg = (1/4000)*(5*(cropdata[i,"PHIHOX.M.sl1"]+cropdata[i,"PHIHOX.M.sl2"])+
                          10*(cropdata[i,"PHIHOX.M.sl2"]+cropdata[i,"PHIHOX.M.sl3"])+
                          15*(cropdata[i,"PHIHOX.M.sl3"]+cropdata[i,"PHIHOX.M.sl4"])+
                          30*(cropdata[i,"PHIHOX.M.sl4"]+cropdata[i,"PHIHOX.M.sl5"])+
                          40*(cropdata[i,"PHIHOX.M.sl5"]+cropdata[i,"PHIHOX.M.sl6"])+
                          100*(cropdata[i,"PHIHOX.M.sl6"]+cropdata[i,"PHIHOX.M.sl7"]))
      
    }
  }

  phdata = append(phdata, phagg)

# calculate pH suitability  
  phsuit <- lapply(phagg, phsuitfun)
  

# calcualte depth suitability

  depthagg = cropdata[i,"BDRICM.BDRICM_M"]
  depthdata = append(depthdata, depthagg)
  
  depthsuit <- lapply(depthagg, depthsuitfun )
  
# depth standardise the texture data
# and then calculate the soil texture suitability 
  
sandagg = NA
   if (ecology[crp,"soil_texture_optimal_heavy"] == 1){   # warning change ecology3 to cropdata
     if (ecology[crp,"soil_depth_optimal_low"] == 1){
       sandagg = (1/120)*(5*(cropdata[i,"SNDPPT.M.sl1"]+cropdata[i,"SNDPPT.M.sl2"])+
                             10*(cropdata[i,"SNDPPT.M.sl2"]+cropdata[i,"SNDPPT.M.sl3"])+
                             15*(cropdata[i,"SNDPPT.M.sl3"]+cropdata[i,"SNDPPT.M.sl4"])+
                             30*(cropdata[i,"SNDPPT.M.sl4"]+cropdata[i,"SNDPPT.M.sl5"]))
     }else {
       if (ecology[crp, "soil_depth_optimal_medium"] == 1) {
         sandagg = (1/200)*(5*(cropdata[i,"SNDPPT.M.sl1"]+cropdata[i,"SNDPPT.M.sl2"])+
                               10*(cropdata[i,"SNDPPT.M.sl2"]+cropdata[i,"SNDPPT.M.sl3"])+
                               15*(cropdata[i,"SNDPPT.M.sl3"]+cropdata[i,"SNDPPT.M.sl4"])+
                               30*(cropdata[i,"SNDPPT.M.sl4"]+cropdata[i,"SNDPPT.M.sl5"])+
                               40*(cropdata[i,"SNDPPT.M.sl5"]+cropdata[i,"SNDPPT.M.sl6"]))
       }else {
         sandagg = (1/400)*(5*(cropdata[i,"SNDPPT.M.sl1"]+cropdata[i,"SNDPPT.M.sl2"])+
                               10*(cropdata[i,"SNDPPT.M.sl2"]+cropdata[i,"SNDPPT.M.sl3"])+
                               15*(cropdata[i,"SNDPPT.M.sl3"]+cropdata[i,"SNDPPT.M.sl4"])+
                               30*(cropdata[i,"SNDPPT.M.sl4"]+cropdata[i,"SNDPPT.M.sl5"])+
                               40*(cropdata[i,"SNDPPT.M.sl5"]+cropdata[i,"SNDPPT.M.sl6"])+
                               100*(cropdata[i,"SNDPPT.M.sl6"]+cropdata[i,"SNDPPT.M.sl7"]))
         
       }
     }
     
    if (sandagg >= 65){
      texturesuit = 25
    } else {
      texturesuit = 100
    }
  
}
  if (ecology[crp,"soil_texture_optimal_light"] == 1){
    if (ecology[crp,"soil_depth_optimal_low"] == 1){
      clayagg = (1/120)*(5*(cropdata[i,"CLYPPT.M.sl1"]+cropdata[i,"CLYPPT.M.sl2"])+
                            10*(cropdata[i,"CLYPPT.M.sl2"]+cropdata[i,"CLYPPT.M.sl3"])+
                            15*(cropdata[i,"CLYPPT.M.sl3"]+cropdata[i,"CLYPPT.M.sl4"])+
                            30*(cropdata[i,"CLYPPT.M.sl4"]+cropdata[i,"CLYPPT.M.sl5"]))
    }else {
      if (ecology[crp, "soil_depth_optimal_medium"] == 1) {
        clayagg = (1/200)*(5*(cropdata[i,"CLYPPT.M.sl1"]+cropdata[i,"CLYPPT.M.sl2"])+
                              10*(cropdata[i,"CLYPPT.M.sl2"]+cropdata[i,"CLYPPT.M.sl3"])+
                              15*(cropdata[i,"CLYPPT.M.sl3"]+cropdata[i,"CLYPPT.M.sl4"])+
                              30*(cropdata[i,"CLYPPT.M.sl4"]+cropdata[i,"CLYPPT.M.sl5"])+
                              40*(cropdata[i,"CLYPPT.M.sl5"]+cropdata[i,"CLYPPT.M.sl6"]))
      }else {
        clayagg = (1/400)*(5*(cropdata[i,"CLYPPT.M.sl1"]+cropdata[i,"CLYPPT.M.sl2"])+
                              10*(cropdata[i,"CLYPPT.M.sl2"]+cropdata[i,"CLYPPT.M.sl3"])+
                              15*(cropdata[i,"CLYPPT.M.sl3"]+cropdata[i,"CLYPPT.M.sl4"])+
                              30*(cropdata[i,"CLYPPT.M.sl4"]+cropdata[i,"CLYPPT.M.sl5"])+
                              40*(cropdata[i,"CLYPPT.M.sl5"]+cropdata[i,"CLYPPT.M.sl6"])+
                              100*(cropdata[i,"CLYPPT.M.sl6"]+cropdata[i,"CLYPPT.M.sl7"]))
        
      }
    }
    
    if (clayagg >= 15){
      texturesuit = 25
    } else {
      texturesuit = 100
    }
}
  if (ecology[crp,"soil_texture_optimal_medium"] == 1){
    if (ecology[crp,"soil_depth_optimal_low"] == 1){
      sandagg = (1/120)*(5*(cropdata[i,"SNDPPT.M.sl1"]+cropdata[i,"SNDPPT.M.sl2"])+
                            10*(cropdata[i,"SNDPPT.M.sl2"]+cropdata[i,"SNDPPT.M.sl3"])+
                            15*(cropdata[i,"SNDPPT.M.sl3"]+cropdata[i,"SNDPPT.M.sl4"])+
                            30*(cropdata[i,"SNDPPT.M.sl4"]+cropdata[i,"SNDPPT.M.sl5"]))
      
      clayagg = (1/120)*(5*(cropdata[i,"CLYPPT.M.sl1"]+cropdata[i,"CLYPPT.M.sl2"])+
                            10*(cropdata[i,"CLYPPT.M.sl2"]+cropdata[i,"CLYPPT.M.sl3"])+
                            15*(cropdata[i,"CLYPPT.M.sl3"]+cropdata[i,"CLYPPT.M.sl4"])+
                            30*(cropdata[i,"CLYPPT.M.sl4"]+cropdata[i,"CLYPPT.M.sl5"]))
    }else {
      if (ecology[crp, "soil_depth_optimal_medium"] == 1) {
        sandagg = (1/200)*(5*(cropdata[i,"SNDPPT.M.sl1"]+cropdata[i,"SNDPPT.M.sl2"])+
                              10*(cropdata[i,"SNDPPT.M.sl2"]+cropdata[i,"SNDPPT.M.sl3"])+
                              15*(cropdata[i,"SNDPPT.M.sl3"]+cropdata[i,"SNDPPT.M.sl4"])+
                              30*(cropdata[i,"SNDPPT.M.sl4"]+cropdata[i,"SNDPPT.M.sl5"])+
                              40*(cropdata[i,"SNDPPT.M.sl5"]+cropdata[i,"SNDPPT.M.sl6"]))
        
        clayagg = (1/200)*(5*(cropdata[i,"CLYPPT.M.sl1"]+cropdata[i,"CLYPPT.M.sl2"])+
                              10*(cropdata[i,"CLYPPT.M.sl2"]+cropdata[i,"CLYPPT.M.sl3"])+
                              15*(cropdata[i,"CLYPPT.M.sl3"]+cropdata[i,"CLYPPT.M.sl4"])+
                              30*(cropdata[i,"CLYPPT.M.sl4"]+cropdata[i,"CLYPPT.M.sl5"])+
                              40*(cropdata[i,"CLYPPT.M.sl5"]+cropdata[i,"CLYPPT.M.sl6"]))
      }else {
        sandagg = (1/400)*(5*(cropdata[i,"SNDPPT.M.sl1"]+cropdata[i,"SNDPPT.M.sl2"])+
                              10*(cropdata[i,"SNDPPT.M.sl2"]+cropdata[i,"SNDPPT.M.sl3"])+
                              15*(cropdata[i,"SNDPPT.M.sl3"]+cropdata[i,"SNDPPT.M.sl4"])+
                              30*(cropdata[i,"SNDPPT.M.sl4"]+cropdata[i,"SNDPPT.M.sl5"])+
                              40*(cropdata[i,"SNDPPT.M.sl5"]+cropdata[i,"SNDPPT.M.sl6"])+
                              100*(cropdata[i,"SNDPPT.M.sl6"]+cropdata[i,"SNDPPT.M.sl7"]))
        
        clayagg = (1/400)*(5*(cropdata[i,"CLYPPT.M.sl1"]+cropdata[i,"CLYPPT.M.sl2"])+
                              10*(cropdata[i,"CLYPPT.M.sl2"]+cropdata[i,"CLYPPT.M.sl3"])+
                              15*(cropdata[i,"CLYPPT.M.sl3"]+cropdata[i,"CLYPPT.M.sl4"])+
                              30*(cropdata[i,"CLYPPT.M.sl4"]+cropdata[i,"CLYPPT.M.sl5"])+
                              40*(cropdata[i,"CLYPPT.M.sl5"]+cropdata[i,"CLYPPT.M.sl6"])+
                              100*(cropdata[i,"CLYPPT.M.sl6"]+cropdata[i,"CLYPPT.M.sl7"]))
        
      }
    }
    
    
    if ((sandagg >= 52) | (clayagg > 27)) {
      texturesuit = 25
    } else {
      texturesuit = 100
    }

  } 
  sandata = append(sandata, sandagg)
  claydata = append(claydata, clayagg)
# create data frames for ouput data
  tmonsuits[i,]= tmonsuit
  tcropsuits[i,]= tcropsuit
  #rainaggregs[i,] = raindataggreg
  rainsuits[i,] = rainsuit
  phsuits = append(phsuits, as.numeric(phsuit))
  depthsuits= append(depthsuits, as.numeric(depthsuit))
  texturesuits = append(texturesuits, as.numeric(texturesuit)) 


}
# make sure all layers are named
names(tmonsuits) <- c("TMS_Jan", "TMS_Feb", "TMS_Mar", "TMS_Apr", "TMS_May", "TMS_Jun", "TMS_Jul", "TMS_Aug", "TMS_Sep",  "TMS_Oct", "TMS_Nov", "TMS_Dec")
names(tcropsuits) <- c("TCS_Jan", "TCS_Feb", "TCS_Mar", "TCS_Apr", "TCS_May", "TCS_Jun", "TCS_Jul", "TCS_Aug", "TCS_Sep",  "TCS_Oct", "TCS_Nov", "TCS_Dec")

names(rainaggregs) <- c("RMS_Jan", "RMS_Feb", "RMS_Mar", "RMS_Apr", "RMS_May", "RMS_Jun", "RMS_Jul", "RMS_Aug", "RMS_Sep",  "RMS_Oct", "RMS_Nov", "RMS_Dec")
names(rainsuits) <- c("RCS_Jan", "RCS_Feb", "RCS_Mar", "RCS_Apr", "RCS_May", "RCS_Jun", "RCS_Jul", "RCS_Aug", "RCS_Sep",  "RCS_Oct", "RCS_Nov", "RCS_Dec")


for (i in colnames(rainsuits)){
  print(which(is.na(rainsuits[,i])))
}

#save them into the disk
saveRDS(phdata, "./data/output/Oct19Combined/phdata.rds")
saveRDS(sandata, "./data/output/Oct19Combined/sandata.rds")
saveRDS(claydata, "./data/output/Oct19Combined/claydata.rds")

saveRDS(tmonsuits, "./data/output/Oct19Combined/tmonsuit.rds")
saveRDS(tcropsuits, "./data/output/Oct19Combined/tcropsuit.rds")
saveRDS(rainaggregs, "./data/output/Oct19Combined/raindataggreg.rds")
saveRDS(rainsuits, "./data/output/Oct19Combined/rainsuit.rds")
saveRDS(phsuits, "./data/output/Oct19Combined/phsuit.rds")
saveRDS(depthsuits, "./data/output/Oct19Combined/depthsuit.rds")
saveRDS(texturesuits, "./data/output/Oct19Combined/texturesuit.rds") 

# calculate all 28 indices
library(data.table)
alldata = NULL
yearmax = NULL

#res <- function (n){ 
for (k in 1:nrow(cropdata)){
  
  #ptm <- proc.time()
# create empty lists  
  cropname = as.character(ecology[which(ecology$cropid == cropdata$CFFcropid[k]), "name_var_lndrce"])[1]
  
  climprod = list()
  soilprod = list()
  
  climave = list()
  soilave = list()
  soilaveweighted = list()
  
  climprod_soilprod_allprod = list()
  climprod_soilprod_allave =  list()
  climprod_soilave_allprod = list()
  climprod_soilave_allave = list()
  climprod_soilaveweighted_allprod = list()
  climprod_soilaveweighted_allave = list()
  
  climave_soilprod_allprod = list()
  climave_soilprod_allave = list()
  climave_soilave_allprod = list()
  climave_soilave_allave = list()
  climave_soilaveweighted_allprod = list()
  climave_soilaveweighted_allave = list()
  
  norainـsoilprodـallprod = list() 
  norainـsoilprodـallave = list()
  norainـsoilaveـallprod = list()
  norainـsoilaveـallave = list() 
  norainـsoilaveweightedـallprod = list()
  norainـsoilaveweightedـallave = list()
  
  temptotcal = as.numeric(tmonsuits[k,])
  raintotcal = as.numeric(rainsuits[k,])
  phtotcal = as.numeric(phsuits[k])
  depthtotcal = as.numeric(depthsuits[k])
  txturtotcal = as.numeric(texturesuits[k])
  
  soilprod = as.list(0.0001* (phtotcal * depthtotcal * txturtotcal))
  soilave = as.list(mean(c(phtotcal , depthtotcal , txturtotcal)))
  soilaveweighted = as.list(sum(0.6*phtotcal , 0.2*depthtotcal , 0.2*txturtotcal)) 

# calculate indices                  
  for (i in 1:12){
    climprod <- rbindlist(list(climprod,as.list (0.01* (temptotcal[[i]][1]* raintotcal[[i]][1]))))
    climave <- rbindlist(list(climave, as.list(mean(c(temptotcal[[i]][1], raintotcal[[i]][1])))))
    
    climprod_soilprod_allprod <- rbindlist(list(climprod_soilprod_allprod, as.list(0.01*(climprod[[1]][i] * soilprod[[1]]))))
    climprod_soilprod_allave <- rbindlist(list(climprod_soilprod_allave,  as.list(mean(c(climprod[[1]][i],soilprod[[1]])))))
    climprod_soilave_allprod <- rbindlist(list(climprod_soilave_allprod, as.list(0.01*(climprod[[1]][i] * soilave[[1]]))))
    climprod_soilave_allave <- rbindlist(list(climprod_soilave_allave, as.list(mean(c(climprod[[1]][i], soilave[[1]])))))
    climprod_soilaveweighted_allprod <- rbindlist(list(climprod_soilaveweighted_allprod, as.list(0.01*(climprod[[1]][i] * soilaveweighted[[1]]))))
    climprod_soilaveweighted_allave <- rbindlist(list(climprod_soilaveweighted_allave, as.list(mean(c(climprod[[1]][i], soilaveweighted[[1]])))))
    
    climave_soilprod_allprod <- rbindlist(list(climave_soilprod_allprod, as.list(0.01*(climave[[1]][i] * soilprod[[1]]))))
    climave_soilprod_allave <- rbindlist(list(climave_soilprod_allave, as.list(mean(c(climave[[1]][i],soilprod[[1]])))))
    climave_soilave_allprod <- rbindlist(list(climave_soilave_allprod, as.list(0.01*(climave[[1]][i] * soilave[[1]]))))
    climave_soilave_allave <- rbindlist(list(climave_soilave_allave, as.list(mean(c(climave[[1]][i],soilave[[1]])))))
    climave_soilaveweighted_allprod <- rbindlist(list(climave_soilaveweighted_allprod, as.list(0.01*(climave[[1]][i] * soilaveweighted[[1]]))))
    climave_soilaveweighted_allave <- rbindlist(list(climave_soilaveweighted_allave, as.list(mean(c(climave[[1]][i], soilaveweighted[[1]])))))
    
    norainـsoilprodـallprod <- rbindlist(list(norainـsoilprodـallprod, as.list(0.01*(temptotcal[i] * soilprod[[1]]))))
    norainـsoilprodـallave <- rbindlist(list(norainـsoilprodـallave, as.list(mean(c(temptotcal[i],soilprod[[1]])))))
    norainـsoilaveـallprod <- rbindlist(list(norainـsoilaveـallprod, as.list(0.01*(temptotcal[i] * soilave[[1]]))))
    norainـsoilaveـallave <- rbindlist(list(norainـsoilaveـallave, as.list(mean(c(temptotcal[i],soilave[[1]])))))
    norainـsoilaveweightedـallprod <- rbindlist(list(norainـsoilaveweightedـallprod, as.list(0.01*(temptotcal[i] * soilaveweighted[[1]]))))
    norainـsoilaveweightedـallave <- rbindlist(list(norainـsoilaveweightedـallave, as.list(mean(c(temptotcal[i],soilaveweighted[[1]])))))
  }
# create a big tibble for all data includes all 28 indices for all records for all crops
  alldata <- rbindlist(list(alldata, data.frame(point = rep(k, 12),
                                             crop = rep(cropname, 12),
                                             latitude = as.numeric(rep(cropdata$decimalLatitude[k], 12)),
                                             longitude = as.numeric(rep(cropdata$decimalLongitude[k], 12)),
                                             tempdata = as.numeric(tempdata[k,]),
                                             raindata = as.numeric(raindata[k,]),
                                             phdata = as.numeric(rep(phdata[k], 12)),
                                             depthdata = as.numeric(rep(depthdata[k], 12)),
                                             sanddata = as.numeric(rep(sandata[k], 12)), 
                                             claydata = as.numeric(rep(claydata[k], 12)),
                                             
                                             tempsuit = temptotcal, 
                                             rainsuit = raintotcal ,
                                             phsuit =   as.numeric(rep(phtotcal,12)) ,
                                             depthsuit = as.numeric(rep(depthtotcal,12)),
                                             texturesuit = as.numeric(rep(txturtotcal,12)),
                                             
                                             climprod = climprod,
                                             climave = climave,
                                             soilprod = as.numeric(rep(soilprod, 12)),
                                             soilave = as.numeric(rep(soilave, 12)),
                                             soilaveweighted = as.numeric(rep(soilaveweighted, 12)),
                                             
                                             climprod_soilprod_allprod = climprod_soilprod_allprod,
                                             climprod_soilprod_allave =  climprod_soilprod_allave,
                                             climprod_soilave_allprod = climprod_soilave_allprod,
                                             climprod_soilave_allave = climprod_soilave_allave,
                                             climprod_soilaveweighted_allprod = climprod_soilaveweighted_allprod,
                                             climprod_soilaveweighted_allave = climprod_soilaveweighted_allave,
                                             
                                             climave_soilprod_allprod = climave_soilprod_allprod,
                                             climave_soilprod_allave = climave_soilprod_allave,
                                             climave_soilave_allprod = climave_soilave_allprod,
                                             climave_soilave_allave = climave_soilave_allave,
                                             climave_soilaveweighted_allprod = climave_soilaveweighted_allprod,
                                             climave_soilaveweighted_allave = climave_soilaveweighted_allave,
                                             
                                             norainـsoilprodـallprod = norainـsoilprodـallprod, 
                                             norainـsoilprodـallave = norainـsoilprodـallave,
                                             norainـsoilaveـallprod = norainـsoilaveـallprod,
                                             norainـsoilaveـallave = norainـsoilaveـallave,
                                             norainـsoilaveweightedـallprod = norainـsoilaveweightedـallprod,
                                             norainـsoilaveweightedـallave = norainـsoilaveweightedـallave, row.names = 1:12)))
  
  # tidyup per year
  
  yearmax <- rbindlist(list(yearmax, data.frame(point = k,
                                             crop = cropname,
                                             latitude = as.numeric(cropdata$decimalLatitude[k]),
                                             longitude = as.numeric(cropdata$decimalLongitude[k]),
                                             
                                             tempdata = mean(as.numeric(tempdata[k,])),
                                             raindata = mean(as.numeric(raindata[k,])),
                                             phdata = as.numeric(phdata[k]),
                                             depthdata = as.numeric(depthdata[k]),
                                             sanddata = as.numeric(sandata[k]), 
                                             claydata = as.numeric(claydata[k]),
                                             
                                             tempsuit = min(temptotcal), 
                                             rainsuit = max(raintotcal) ,
                                             phsuit =   as.numeric(phtotcal) ,
                                             depthsuit = as.numeric(depthtotcal),
                                             texturesuit = as.numeric(txturtotcal),
                                             
                                             climprod = max(climprod$V1),
                                             climave = mean(climave$V1),
                                             soilprod = as.numeric(soilprod),
                                             soilave = as.numeric(soilave),
                                             soilaveweighted = as.numeric(soilaveweighted),
                                                           
                                             climprod_soilprod_allprod = max(climprod_soilprod_allprod$V1),
                                             climprod_soilprod_allave =  mean(climprod_soilprod_allave$V1),
                                             climprod_soilave_allprod = max(climprod_soilave_allprod$V1),
                                             climprod_soilave_allave = max(climprod_soilave_allave$V1),
                                             climprod_soilaveweighted_allprod = max(climprod_soilaveweighted_allprod$V1),
                                             climprod_soilaveweighted_allave = max(climprod_soilaveweighted_allave$V1),
                                                           
                                             climave_soilprod_allprod = max(climave_soilprod_allprod$V1),
                                             climave_soilprod_allave = max(climave_soilprod_allave$V1),
                                             climave_soilave_allprod = max(climave_soilave_allprod$V1),
                                             climave_soilave_allave = max(climave_soilave_allave$V1),
                                             climave_soilaveweighted_allprod = max(climave_soilaveweighted_allprod$V1),
                                             climave_soilaveweighted_allave = max(climave_soilaveweighted_allave$V1),
                                                           
                                             norainـsoilprodـallprod = max(norainـsoilprodـallprod$V1), 
                                             norainـsoilprodـallave = max(norainـsoilprodـallave$V1),
                                             norainـsoilaveـallprod = max(norainـsoilaveـallprod$V1),
                                             norainـsoilaveـallave = max(norainـsoilaveـallave$V1),
                                             norainـsoilaveweightedـallprod = max(norainـsoilaveweightedـallprod$V1),
                                             norainـsoilaveweightedـallave = max(norainـsoilaveweightedـallave$V1))))
                                             
  #proc.time() - ptm
}
# save data
saveRDS(alldata, './data/output/Oct19Combined/alldata.rds')
saveRDS(yearmax, './data/output/Oct19Combined/yearmax.rds')
# check for consistency
for (k in names(yearmax)){
  print(which(is.na(yearmax[,c(k)])))
}
