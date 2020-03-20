# DataCleanFilter.R
# the purpose of this code is to calculate suitability indices 
# contact: ebrahim.jahanshiri@cffresearch.org, e.jahanshiri@gmail.com

list.of.packages <- c("ssh", "sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


setwd("/Users/ej/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx2/")
# data cleaning  dataset from GBIF for data in .CSV files
# cropid refers to the id in CFFRC database
list = list.files("./data/accessions/GBIF/Occurencedata/third", full.names = T)
cropids = NULL
sci_names = NULL
alldata = NULL
metadata = NULL
for (i in 1: length(list)){
  #cropids = append(cropids, strsplit(strsplit(i, "/")[[1]][7], " ")[[1]][1])
  a = strsplit(strsplit(list[i], "/")[[1]][7], " ")[[1]]
  sci_name = strsplit(a, " ")[2:(length(a)-1)]
  data = read.csv(list[i])
  data$CFFcropid = a[1]
  #sci_name = paste(sci_name, collapse = ' ')
  data$sci_name = paste(sci_name, collapse = ' ')
  data.coor <- data[-which(is.na(data$decimalLongitude)),]
  data.basisofrecs <- data.coor[which(data.coor$basisOfRecord == "HUMAN_OBSERVATION" | data.coor$basisOfRecord == "MACHINE_OBSERVATION" | data.coor$basisOfRecord == "OBSERVATION" | data.coor$basisOfRecord == "LITERATURE" | data.coor$basisOfRecord == "LIVING_SPECIMEN") ,]
  metadata = rbind(metadata, data.frame(sci_name =paste(sci_name, collapse = ' '), cropid = a[1], ndown = nrow(data), ncoor = nrow(data.coor), nbasisofRecs = nrow(data.basisofrecs)))
  #write.csv(data.basisofrecs, paste("./data/accessions/GBIF/processed/", sci_name, ".csv", sep = ""), row.names = F)
  alldata = rbind(alldata, data.frame(data.basisofrecs, stringsAsFactors = FALSE))
}
which(is.na(alldata$decimalLongitude))
# writing out the cleaned data
write.csv(metadata, "./data/accessions/GBIF/processed/third/metadata.csv")
saveRDS(as.data.frame(alldata), file = "./data/accessions/GBIF/processed/third/GBIF_AllHasNA.rds")
saveRDS(as.data.frame(alldata[,c("gbifID","sci_name","CFFcropid", "decimalLatitude", "decimalLongitude")]), file = "./data/accessions/GBIF/processed/third/GBIF_AllCoordsHasNA.rds")

