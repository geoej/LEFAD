# GBIF climsoildownllader v0.1
# the purpose of this code is to download soil and climate data for GBIF  data
# contact: ebrahim.jahanshiri@cffresearch.org, e.jahanshiri@gmail.com
# How to do:
# 1- organise GBIF data in a csv file and name it GBIF.csv
# 2- The file should contain two numeric non-empty columns called "decimalLongitude" and  "decimalLatitude"
# 3- Run the code below, it might take a long time to download data so it might be good 
#    idea to berak data into chunks
# 4- The final ouput is saved "GBIFclimsoil.csv"

# connect through SSH to get the climate data 
library(ssh)
session <- ssh_connect("USER_NAME@IP_ADDRESS", passwd = "PASSWORD") # i added password so can run in batch mode
print(session)
# Upload a file to the server
# data_path <- "GBIF.csv"
data_path <- "./data/extract/fifth/glb_coords.rds"
code_path <- "./code/clim_download_server6.R"

scp_upload(session, data_path)
scp_upload(session, code_path)


#out <- ssh_exec_internal(session, "R -e 'list.files()'")
ssh_exec_wait(session, command = c(
  'R CMD BATCH clim_download_server6.R log.txt &'
))

scp_download(session, "log.txt", to = ".")

scp_download(session, "WCLIMextract_GBIF.rds", to = ".")

scp_download(session, "ISRICextract_GBIF.rds", to = ".")


# combining data

clim <- readRDS("WCLIMextract_GBIF.rds")
soil <- readRDS("ISRICextract_GBIF.rds")
climsoil <- cbind (soil,clim)
row.names(climsoil) = 1:nrow(climsoil)
write.csv(climsoil, "GBIFclimsoil.csv")
