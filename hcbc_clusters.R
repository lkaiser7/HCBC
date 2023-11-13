# HCBC Data Exploration #
# Hawaii Coral Bleaching Collaborative #

### GIT VERSION CONTROL PROJECT ###
# library(usethis)
# use_git_config()
# create_github_token()
# gitcreds::gitcreds_set()

### SET UP ###

hcbcDir<-"C:/Users/lkais/Dropbox/PacIOOS/Projects/DAR_HCBC/"
setwd(hcbcDir)

# load packages
# install.packages("stringr")
library(sf)
library(readr)
# library(leaflet)
# library(maps)
# library(stringr)
# library(tidygeocoder)
# library(openxlsx)

### DATA ###

# map data
hi_map<-read_sf(paste0(hcbcDir, "map_data/"), "Coastline")
plot(hi_map$geometry)

# include NW Hawaiian Islands 
nwhi_map<-read_sf(paste0(hcbcDir, "map_data/"), "Coastline_NWHI")
plot(nwhi_map$geometry)

# hcbc zones for Main Hawaiian Island only
hcbc_zones<-read_sf(paste0(hcbcDir, "HCBC_zones/"), "HCBC_Zones")
plot(hcbc_zones)
table(hcbc_zones$ZoneName)
# mhi_zones<-read_sf(paste0(hcbcDir, "HCBC_zones/"), "HCBC_Zones_MHI")
# plot(mhi_zones$geometry)
# table(mhi_zones$ZoneName) # does not include Maui_SE

list.files(paste0(hcbcDir, "2019_data/"))
list.files(paste0(hcbcDir, "2015_data/"))

# 2019 bleaching event data (Main and NW Hawaiian Islands)
clust_2019<-read_csv(paste0(hcbcDir, "2019_data/HCBC_2019_ClusteredData.csv"))
table(clust_2019$ZoneName)

# 2015 bleaching event data (Hawaii, Maui, Lanai, Oahu only)
clust_2015<-read_csv(paste0(hcbcDir, "2015_data/HCBC_2015_ClusterData_Observations.csv"))
table(clust_2015$Island_Name)

# check data set names
dim(clust_2019); names(clust_2019)
dim(clust_2015); names(clust_2015)

# plot points
plot(hi_map$geometry)
plot(hcbc_zones[3], add = T)
points(clust_2019$Longitude_DD_mn, clust_2019$Latitude_DD_mn, col = "white", bg = "blue", pch = 21)
points(clust_2015$Longitude_mn, clust_2015$Latitude_mn, col = "white", bg = "red", pch = 21)

head(as.data.frame(clust_2019))
summary(clust_2019)
head(as.data.frame(clust_2015))
summary(clust_2015)

### 2105 DATA EDITS ###
# add counter to 2015 data
clust_2015$obs_n<-1:dim(clust_2015)[1]

# convert to spatial points data frame
sp_2015<-st_as_sf(clust_2015, coords = c("Longitude_mn", "Latitude_mn"),
                  crs = st_crs(hcbc_zones))
# find intersection of polygon and points
zones_2015<-st_intersection(hcbc_zones, sp_2015)
# dim(zones_2015) # 33 points not within hcbc_zones
plot(hcbc_zones$geometry[which(hcbc_zones$ISLAND == "Hawaii")])
plot(hcbc_zones$geometry[which(hcbc_zones$ISLAND == "Maui")])
plot(hcbc_zones$geometry[which(hcbc_zones$ISLAND == "Lanai")])
plot(hcbc_zones$geometry[which(hcbc_zones$ISLAND == "Oahu")])
points(clust_2015$Longitude_mn, clust_2015$Latitude_mn, col = "yellow", pch = 20)
plot(sp_2015[1], col = 'red', pch = 20, add = T)
plot(zones_2015$geometry, col = "blue", pch = 20, add = T)

# # check mhi_zones
# sp_2015<-st_as_sf(clust_2015, coords = c("Longitude_mn", "Latitude_mn"),
#                   crs = st_crs(mhi_2015))
# mhi_2015<-st_intersection(mhi_zones, sp_2015)
# dim(mhi_zones)

# add zone name to 2015 cluster data
clust_2015$ZoneName<-NA
clust_2015$ZoneName<-zones_2015$ZoneName[match(clust_2015$obs_n, zones_2015$obs_n)]
table(clust_2015$ZoneName, useNA = "ifany")

# save file
write_csv(clust_2015, paste0(hcbcDir, "2015_data/HCBC_2015_ClusteredData.csv"))

### DATA MERGE ###
# create combinded dataset for AGOL

# 2019 subset
agol_2019<-clust_2019[,c(20, 1:2, 5:6, 4, 7, 10:18)]
head(agol_2019); names(agol_2019)
# convert depth to meters
agol_2019$Depth_ft_mn<-agol_2019$Depth_ft_mn/3.281
names(agol_2019)[6]<-"Depth_m_mn"
summary(agol_2019)

# 2015 subset
agol_2015<-cbind(rep(2015, dim(clust_2015)[1]), 
                 clust_2015[,c(1, 39, 5:6, 4, 22, 26, 29, 27:28, 31, 34, 32:33, 35)])
head(agol_2015); names(agol_2015)
summary(agol_2015)
names(agol_2015)<-names(agol_2019)

# combine datasets
agol_clust<-rbind(agol_2019, agol_2015)
agol_clust[515:520,]
summary(agol_clust)

# add bins

# depth
summary(agol_clust$Depth_m_mn)
agol_clust$depth_bin<-NA
# Depth_bin (IN M!): Shallow = 6, Mid = 6-18, Deep > 18
# loop through and fill gaps
for(d in 1:dim(agol_clust)[1]){  # d = 1
  # get observation depth in meters
  depth_m<-agol_clust$Depth_m_mn[d]
  
  # fill with bin
  if(depth_m < 6){
    agol_clust$depth_bin[d]<-"Shallow"
  }else if(depth_m >= 6 & depth_m <= 18){
    agol_clust$depth_bin[d]<-"Mid"
  }else if(depth_m > 18){
    agol_clust$depth_bin[d]<-"Deep"
  }else{
    agol_clust$depth_bin[d]<-NA
  }
  # run next entry 
}
table(agol_clust$depth_bin, useNA = "ifany")

# bleached coral
summary(agol_clust$CoralBleached_Perc_mn)
# %CoralBleached_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%
agol_clust$bleach_bin<-NA

# loop through and fill gaps 
for(d in 1:dim(agol_clust)[1]){  # d = 1
  # get affected coral percentage of observation
  pct_aff<-agol_clust$CoralBleached_Perc_mn[d]
  # fill with bin
  if(pct_aff == 0){
    agol_clust$bleach_bin[d]<-0
  }else if(pct_aff > 0 & pct_aff <= 10){
    agol_clust$bleach_bin[d]<-1
  }else if(pct_aff > 10 & pct_aff <= 30){
    agol_clust$bleach_bin[d]<-2
  }else if(pct_aff > 30 & pct_aff <= 50){
    agol_clust$bleach_bin[d]<-3
  }else if(pct_aff > 50 & pct_aff <= 75){
    agol_clust$bleach_bin[d]<-4
  }else if(pct_aff > 75){
    agol_clust$bleach_bin[d]<-5
  }else{
    agol_clust$bleach_bin[d]<-NA
  }
  # run next entry 
}
table(agol_clust$bleach_bin, useNA = "ifany")

# # add long zone names
# table(agol_clust$ZoneName, useNA = "ifany")

# save file
head(data.frame(agol_clust))
write_csv(agol_clust, paste0(hcbcDir, "HCBC_ClusteredData.csv"))

### END ###