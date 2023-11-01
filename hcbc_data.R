# HCBC Data Exploration #
# Hawaii Coral Bleaching Collaborative #

### GIT VERSION CONTROL PROJECT ###
# library(usethis)
# use_git_config(user.name = "Lauren R Kaiser", user.email = "lkaiser7@hawaii.edu")
# create_github_token()
# gitcreds::gitcreds_set()
# # 

### SET UP ###

hcbcDir<-"C:/Users/lkais/Dropbox/PacIOOS/Projects/HCBC/"
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

# 2019 bleaching event data
hcbc_2019<-read_csv(paste0(hcbcDir, "2019_data/HCBC_2019_Observations_Update_3.9.21.csv"))
hcbc_2019
# problems(hcbc_2019)

# plot points
points(hcbc_2019$Longitude_DD, hcbc_2019$Latitude_DD, col = "white", bg = "blue", pch = 21)

### GAP FILL ###
# fill bin number based on data

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

# save new bin data
write_csv(hcbc_2019, paste0(hcbcDir, "2019_data/HCBC_2019_Observations_Bin.csv"))

### HCBC LEAFLET ###

# map legend colors
live_pal<-colorNumeric(palette = "YlGn", domain = hcbc_2019$`%LiveCoralCover_Bin`)
bleach_pal<-colorNumeric(palette = "YlOrRd", domain = hcbc_2019$`%CoralBleached_Bin`)

# leaflet map of coral
leaflet(hcbc_2019, options = leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(lng = ~Longitude_DD, lat = ~Latitude_DD, group = "Live Coral",
                   radius = (hcbc_2019$`%LiveCoralCover_Bin`+1)*2, 
                   color = ~live_pal(`%LiveCoralCover_Bin`),
                   stroke = F, fillOpacity = 0.5,
                   label = ~Depth_bin,
                   labelOptions = labelOptions(textOnly = TRUE),
                   popup = ~Organization_Name) %>%
  addLegend("bottomleft", pal = live_pal, values = ~`%LiveCoralCover_Bin`,
            labels = c("0%", "1-10%", "11-30%", "31-50%", "51-75%", "76-100%"),
            title = "%Live Coral", opacity = 1, group = "Live Coral") %>%
            addCircleMarkers(lng = ~Longitude_DD, lat = ~Latitude_DD, group = "Bleached Coral",
                             radius = (hcbc_2019$`%CoralBleached_Bin`+1)*2, 
                             color = ~bleach_pal(`%CoralBleached_Bin`),
                             stroke = F, fillOpacity = 0.5,
                             label = ~Depth_bin,
                             labelOptions = labelOptions(textOnly = TRUE),
                             popup = ~Organization_Name) %>%
              addLegend("bottomleft", pal = bleach_pal, values = ~`%CoralBleached_Bin`,
                        labels = c("0%", "1-10%", "11-30%", "31-50%", "51-75%", "76-100%"),
                        title = "%Bleached Coral", opacity = 1, group = "Bleached Coral") %>%
                        addLayersControl(overlayGroups = c("Live Coral", "Bleached Coral"),
                                         options = layersControlOptions(collapsed = F)) %>%
                          hideGroup("Bleached Coral")

### WORK IN PROGRESS ###

# severity 
# Average severity of bleaching 
# 0 = no bleaching, 1 = slight paling, 2 = significant loss of pigmentation, 
# 3 = almost or completely stark white, 4 = recently dead from bleaching 
# If recorded, CoralBleached is also recorded. Not required.

table(hcbc_2019$BleachingSeverity, useNA = "ifany")

n_severe<-hcbc_2019[which(hcbc_2019$BleachingSeverity != "NA"),]
print(n_severe[, 16:20], n = 10)

n_severe$live_bleach<-abs(n_severe$`%LiveCoralCover_Bin` - n_severe$`%CoralBleached_Bin`)
n_severe$severity_delta<-n_severe$BleachingSeverity - n_severe$live_bleach

print(n_severe[, c(16:20, 63:64)], n = 10)
tail(n_severe[, c(16:20, 63:64)], n = 10)
table(n_severe$severity_delta)

# %LiveCoralCover_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%
# %CoralBleached_Bin: 0 = 0%, 1 = 1-10%, 2 = 11-30%, 3 = 31-50%, 4 = 51-75%, 5 = 76-100%
# Severty_Bin: 1 = pale, 2 = pigmentation loss, 3 = stark white, 4 = dead

pct_coral<-hcbc_2019[which(hcbc_2019$`%LiveCoralCover` != "NA"),]
pct_coral$live_ratio<-pct_coral$`%LiveCoralCover_Bin`/pct_coral$`%CoralBleached_Bin`
pct_coral$bleach_ratio<-pct_coral$`%CoralBleached_Bin`/pct_coral$`%LiveCoralCover_Bin`
print(pct_coral[,c(16:20, 63:64)])

# live to bleach ratio
table(hcbc_2019$`%LiveCoralCover_Bin`, hcbc_2019$`%CoralBleached_Bin`)
table(round(pct_coral$live_ratio, 2))
table(round(pct_coral$bleach_ratio, 2))

table(pct_coral$`%LiveCoralCover_Bin` > pct_coral$`%CoralBleached_Bin`)
table(pct_coral$`%LiveCoralCover` > pct_coral$`%CoralBleached`)
