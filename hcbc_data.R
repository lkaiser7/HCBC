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
library(leaflet)
library(maps)
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

list.files(paste0(hcbcDir, "2019_data/"))
list.files(paste0(hcbcDir, "2015_data/"))

# 2019 bleaching event data (Main and NW Hawaiian Islands)
hcbc_2019<-read_csv(paste0(hcbcDir, "2019_data/HCBC_2019_Observations_Update_3.9.21.csv"))
hcbc_2019
# problems(hcbc_2019)

# 2015 bleaching event data (Hawaii, Maui, Lanai, Oahu only)
hcbc_2015<-read_csv(paste0(hcbcDir, "2015_data/HCBC_2015_Observations.csv"))
hcbc_2015
# drop empty columns at end of dataset
hcbc_2015<-hcbc_2015[,-29:-35]

# check data set names
dim(hcbc_2019); names(hcbc_2019)
dim(hcbc_2015); names(hcbc_2015)

# plot points
plot(hi_map$geometry)
points(hcbc_2019$Longitude_DD, hcbc_2019$Latitude_DD, col = "white", bg = "blue", pch = 21)
points(hcbc_2015$Longitude_DD, hcbc_2015$Latitude_DD, col = "white", bg = "red", pch = 21)

### 2019 DATA EDITS ###
# gap fill bin number based on data

# depth
summary(hcbc_2019$Depth_ft)
table(hcbc_2019$Depth_bin, useNA = "ifany")
# Depth_bin (IN M!): Shallow = 6, Mid = 6-18, Deep > 18
# loop through and fill gaps
for(d in 1:dim(hcbc_2019)[1]){  # d = 1
  # if depth bin is missing
  if(is.na(hcbc_2019$Depth_bin[d])){
    # fill with bin (convert feet to meters!)
    d_ft<-hcbc_2019$Depth_ft[d]/3.281
    if(d_ft < 6){
      hcbc_2019$Depth_bin[d]<-"Shallow"
    }else if(d_ft >= 6 & d_ft <= 18){
      hcbc_2019$Depth_bin[d]<-"Mid"
    }else if(d_ft > 18){
      hcbc_2019$Depth_bin[d]<-"Deep"
    }else{
      hcbc_2019$Depth_bin[d]<-NA
    }
  }
  # run next missing entry 
}
table(hcbc_2019$Depth_bin, useNA = "ifany")

# live coral
summary(hcbc_2019$`%LiveCoralCover`)
table(hcbc_2019$`%LiveCoralCover_Bin`, useNA = "ifany")
# %LiveCoralCover_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%

# loop through and fill gaps 
for(d in 1:dim(hcbc_2019)[1]){  # d = 1
  # if depth bin is missing
  if(is.na(hcbc_2019$`%LiveCoralCover_Bin`[d])){
    # fill with bin
    LC_cover<-hcbc_2019$`%LiveCoralCover`[d]
    if(LC_cover == 0){
      hcbc_2019$`%LiveCoralCover_Bin`[d]<-0
    }else if(LC_cover > 0 & LC_cover <= 10){
      hcbc_2019$`%LiveCoralCover_Bin`[d]<-1
    }else if(LC_cover > 10 & LC_cover <= 30){
      hcbc_2019$`%LiveCoralCover_Bin`[d]<-2
    }else if(LC_cover > 30 & LC_cover <= 50){
      hcbc_2019$`%LiveCoralCover_Bin`[d]<-3
    }else if(LC_cover > 50 & LC_cover <= 75){
      hcbc_2019$`%LiveCoralCover_Bin`[d]<-4
    }else if(LC_cover > 75){
      hcbc_2019$`%LiveCoralCover_Bin`[d]<-5
    }else{
      hcbc_2019$`%LiveCoralCover_Bin`[d]<-NA
    }
  }
  # run next missing entry 
}
table(hcbc_2019$`%LiveCoralCover_Bin`, useNA = "ifany")

# bleached coral
summary(hcbc_2019$`%CoralBleached`)
table(hcbc_2019$`%CoralBleached_Bin`, useNA = "ifany")
# %CoralBleached_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%

# loop through and fill gaps 
for(d in 1:dim(hcbc_2019)[1]){  # d = 1
  # if depth bin is missing
  if(is.na(hcbc_2019$`%CoralBleached_Bin`[d])){
    # fill with bin
    LC_bleach<-hcbc_2019$`%CoralBleached`[d]
    if(LC_cover == 0){
      hcbc_2019$`%CoralBleached_Bin`[d]<-0
    }else if(LC_cover > 0 & LC_cover <= 10){
      hcbc_2019$`%CoralBleached_Bin`[d]<-1
    }else if(LC_cover > 10 & LC_cover <= 30){
      hcbc_2019$`%CoralBleached_Bin`[d]<-2
    }else if(LC_cover > 30 & LC_cover <= 50){
      hcbc_2019$`%CoralBleached_Bin`[d]<-3
    }else if(LC_cover > 50 & LC_cover <= 75){
      hcbc_2019$`%CoralBleached_Bin`[d]<-4
    }else if(LC_cover > 75){
      hcbc_2019$`%CoralBleached_Bin`[d]<-5
    }else{
      hcbc_2019$`%CoralBleached_Bin`[d]<-NA
    }
  }
  # run next missing entry 
}
table(hcbc_2019$`%CoralBleached_Bin`, useNA = "ifany")

# NOT USED: severity 
# Average severity of bleaching 
# 0 = no bleaching, 1 = slight paling, 2 = significant loss of pigmentation, 
# 3 = almost or completely stark white, 4 = recently dead from bleaching 
# If recorded, CoralBleached is also recorded. Not required.

# table(hcbc_2019$BleachingSeverity, useNA = "ifany")
# %LiveCoralCover_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%
# %CoralBleached_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%
# Severty_Bin: 1 = pale, 2 = pigmentation loss, 3 = stark white, 4 = dead

# # save new bin data
# write_csv(hcbc_2019, paste0(hcbcDir, "2019_data/HCBC_2019_Observations_Bin.csv"))
names(hcbc_2019)[1:25]

### 2015 DATA EDITS ###
# create columns to match 2019 dataset
names(hcbc_2015)
head(as.data.frame(hcbc_2015))

# depth
summary(hcbc_2015$Depth_m)
hcbc_2015$Depth_bin<-NA
# Depth_bin (IN M!): Shallow = 6, Mid = 6-18, Deep > 18
# loop through and add bin
for(d in 1:dim(hcbc_2015)[1]){  # d = 1
  # get observation depth in meters
  depth_m<-hcbc_2015$Depth_m[d]

    # fill with bin
    if(depth_m < 6){
      hcbc_2015$Depth_bin[d]<-"Shallow"
    }else if(depth_m >= 6 & depth_m <= 18){
      hcbc_2015$Depth_bin[d]<-"Mid"
    }else if(depth_m > 18){
      hcbc_2015$Depth_bin[d]<-"Deep"
    }else{
      hcbc_2015$Depth_bin[d]<-NA
    }
  # run next entry 
}
table(hcbc_2015$Depth_bin, useNA = "ifany")

# live coral
summary(hcbc_2015$PercentLiveCoralCover)
summary(hcbc_2015$PctCoralUnbleached) # use if Percent Live in NA
hcbc_2015$`%LiveCoralCover_Bin`<-NA
# %LiveCoralCover_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%

# loop through and fill gaps 
for(d in 1:dim(hcbc_2015)[1]){  # d = 1
  # get live coral percentage of observation
  pct_live<-hcbc_2015$PercentLiveCoralCover[d]
  pct_unb<-hcbc_2015$PctCoralUnbleached[d]

  # fill with bin if not NA
  if(is.na(pct_live)){
    # fill with unbleached bin
    if(pct_unb == 0){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-0
    }else if(pct_unb > 0 & pct_unb <= 10){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-1
    }else if(pct_unb > 10 & pct_unb <= 30){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-2
    }else if(pct_unb > 30 & pct_unb <= 50){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-3
    }else if(pct_unb > 50 & pct_unb <= 75){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-4
    }else if(pct_unb > 75){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-5
    }
  }else{
    # fill with live bin
    if(pct_live == 0){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-0
    }else if(pct_live > 0 & pct_live <= 10){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-1
    }else if(pct_live > 10 & pct_live <= 30){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-2
    }else if(pct_live > 30 & pct_live <= 50){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-3
    }else if(pct_live > 50 & pct_live <= 75){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-4
    }else if(pct_live > 75){
      hcbc_2015$`%LiveCoralCover_Bin`[d]<-5
    }
  }
  # run next entry 
}
table(hcbc_2015$`%LiveCoralCover_Bin`, useNA = "ifany")

# bleached coral
summary(hcbc_2015$PctCoralPartialBleached)
summary(hcbc_2015$PctCoralFullyBleached)
summary(hcbc_2015$PctAffected) # sum of partial and fully bleached columns
# 2019: percent of living coral cover that was partially or fully bleached
# %CoralBleached_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%
hcbc_2015$`%CoralBleached_Bin`<-NA

# loop through and fill gaps 
for(d in 1:dim(hcbc_2015)[1]){  # d = 1
  # get affected coral percentage of observation
  pct_aff<-hcbc_2015$PctAffected[d]
    # fill with bin
    if(pct_aff == 0){
      hcbc_2015$`%CoralBleached_Bin`[d]<-0
    }else if(pct_aff > 0 & pct_aff <= 10){
      hcbc_2015$`%CoralBleached_Bin`[d]<-1
    }else if(pct_aff > 10 & pct_aff <= 30){
      hcbc_2015$`%CoralBleached_Bin`[d]<-2
    }else if(pct_aff > 30 & pct_aff <= 50){
      hcbc_2015$`%CoralBleached_Bin`[d]<-3
    }else if(pct_aff > 50 & pct_aff <= 75){
      hcbc_2015$`%CoralBleached_Bin`[d]<-4
    }else if(pct_aff > 75){
      hcbc_2015$`%CoralBleached_Bin`[d]<-5
    }else{
      hcbc_2015$`%CoralBleached_Bin`[d]<-NA
    }
  # run next entry 
}
table(hcbc_2015$`%CoralBleached_Bin`, useNA = "ifany")

# # save new bin data
# write_csv(hcbc_2015, paste0(hcbcDir, "2015_data/HCBC_2015_Observations_Bin.csv"))
names(hcbc_2015)

### 2019 VS 2015 OBSERVATIONS ###

# select columns for comparison 
bin_2019<-hcbc_2019[, c(1, 3, 5, 11:13, 17, 19 )]
bin_2015<-hcbc_2015[, c(1:2, 4, 10:11, 29:31)]
# same sample sites
bleach_events<-merge(bin_2015, bin_2019, by = c("Latitude_DD", "Longitude_DD"))
# Hawaii Island only
plot(hi_map$geometry)
points(bleach_events$Longitude_DD, bleach_events$Latitude_DD, col = "white", bg = "green", pch = 21)

### HCBC LEAFLET ###

# map legend colors
live_pal<-colorNumeric(palette = "YlGn", domain = hcbc_2019$`%LiveCoralCover_Bin`)
bleach_pal<-colorNumeric(palette = "YlOrRd", domain = hcbc_2019$`%CoralBleached_Bin`)

# 2019 leaflet map of coral
leaflet(hcbc_2019, options = leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addControl("2019 Bleaching Event", position = "topright") %>%
  addCircleMarkers(lng = ~Longitude_DD, lat = ~Latitude_DD, group = "Live Coral",
                   radius = (hcbc_2019$`%LiveCoralCover_Bin`+1)*2, 
                   color = ~live_pal(`%LiveCoralCover_Bin`),
                   stroke = F, fillOpacity = 0.5,
                   label = ~Depth_bin,
                   labelOptions = labelOptions(textOnly = TRUE),
                   popup = ~Organization_Name) %>%
  addLegend("bottomleft", pal = live_pal, values = ~`%LiveCoralCover_Bin`,
            title = "%Live Coral", opacity = 1, group = "Live Coral") %>%
            addCircleMarkers(lng = ~Longitude_DD, lat = ~Latitude_DD, group = "Bleached Coral",
                             radius = (hcbc_2019$`%CoralBleached_Bin`+1)*2, 
                             color = ~bleach_pal(`%CoralBleached_Bin`),
                             stroke = F, fillOpacity = 0.5,
                             label = ~Depth_bin,
                             labelOptions = labelOptions(textOnly = TRUE),
                             popup = ~Organization_Name) %>%
              addLegend("bottomleft", pal = bleach_pal, values = ~`%CoralBleached_Bin`,
                        title = "%Bleached Coral", opacity = 1, group = "Bleached Coral") %>%
                        addLayersControl(overlayGroups = c("Live Coral", "Bleached Coral"),
                                         options = layersControlOptions(collapsed = F)) %>%
                          hideGroup("Live Coral")

# 2015 leaflet map of coral
leaflet(hcbc_2015, options = leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addControl("2015 Bleaching Event", position = "topright") %>%
  addCircleMarkers(lng = ~Longitude_DD, lat = ~Latitude_DD, group = "Live Coral",
                   radius = (hcbc_2015$`%LiveCoralCover_Bin`+1)*2, 
                   color = ~live_pal(`%LiveCoralCover_Bin`),
                   stroke = F, fillOpacity = 0.5,
                   label = ~Depth_bin,
                   labelOptions = labelOptions(textOnly = TRUE),
                   popup = ~Organization_Name) %>%
  addLegend("bottomleft", pal = live_pal, values = ~`%LiveCoralCover_Bin`,
            title = "%Live Coral", opacity = 1, group = "Live Coral") %>%
  addCircleMarkers(lng = ~Longitude_DD, lat = ~Latitude_DD, group = "Bleached Coral",
                   radius = (hcbc_2015$`%CoralBleached_Bin`+1)*2, 
                   color = ~bleach_pal(`%CoralBleached_Bin`),
                   stroke = F, fillOpacity = 0.5,
                   label = ~Depth_bin,
                   labelOptions = labelOptions(textOnly = TRUE),
                   popup = ~Organization_Name) %>%
  addLegend("bottomleft", pal = bleach_pal, values = ~`%CoralBleached_Bin`,
            title = "%Bleached Coral", opacity = 1, group = "Bleached Coral") %>%
  addLayersControl(overlayGroups = c("Live Coral", "Bleached Coral"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup("Live Coral")

### END ###