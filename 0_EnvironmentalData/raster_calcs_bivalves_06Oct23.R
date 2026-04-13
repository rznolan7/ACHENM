# Fernando Machado Stredel 06 Oct 2023
# Minimum, maximum, and range raster calculation for a temporal bin

# install.packages("terra", dependencies = T)
library(terra)

setwd("E:/Pleist_Clim")

# make sure you have the "formatted_data" folder in your directory
"formatted_data" %in% dir()

# 4k times' directories
dirs <- list.dirs("formatted_data/", full.names = F)[-1]

# table with info on names and ages of each time
time_names <- read.csv("exp_names_ages.csv", header = T)

# let's say we want the rasters from a period of 100ky (1808 to 1908)
from <- 0
to <- 11

time_names[time_names$Age_Sim_k == from, "Exp_Name"] # tEVTK
time_names[time_names$Age_Sim_k == to, "Exp_Name"] #tEVT9

# indices to get the directories' names by their age
indx <- which(time_names$Age_Sim_k >= from & 
                time_names$Age_Sim_k <= to)

# see:
# time_names$Age_Sim_k[indx]
# time_names$Exp_Name[indx]

times_bin <- time_names$Exp_Name[indx] # times within the bin of interest

months <- substr(tolower(month.name), 1, 3) # months 3-letter names

# create lists for rasters for the bin
sss_min_ls <- list()
sst_min_ls <- list()

sss_max_ls <- list()
sst_max_ls <- list()

#
# loop to populate the lists
for (t in 1:length(times_bin)) {
  
  # get the 12 month rasters' names
  tmp_M_nms <- list.files(path = paste0("formatted_data/", times_bin[t]),
                          pattern = paste0(".pfcl", paste(months, collapse = "|"), "*"), full.names = T)
  
  # lists of monthly rasters of each 4k time
  sss_M_ls <- list()
  sst_M_ls <- list()
  
  # get all monthly rasters for each 4k time
  for (m in 1:length(tmp_M_nms)) {
    # each monthly stack (temporary) within a 4k time
    tmp_M_r <- rast(tmp_M_nms[m])
    # get sss & sst rasters at 5m depth
    sss_M_ls[[m]] <- tmp_M_r[[grep(pattern = "salinity.*=5_t*", names(tmp_M_r))]]  #changed from tmp_r
    sst_M_ls[[m]] <- tmp_M_r[[grep(pattern = "temp.*=5_t*", names(tmp_M_r))]]      #changed from tmp_r
  }
  
  # transform lists to stacks (12 months)
  sss_M_stack <- rast(sss_M_ls)
  sst_M_stack <- rast(sst_M_ls)
  
  # get min. value raster among all months for each variable at each 4k time
  sss_min_t <- app(sss_M_stack, min, na.rm = T)
  names(sss_min_t)[1] <- paste0("min_salinity_5m_", times_bin[t])
  sst_min_t <- app(sst_M_stack, min, na.rm = T)
  names(sst_min_t)[1] <- paste0("min_temperature_5m_", times_bin[t])
  
  # get max. value raster among all months for each variable at each 4k time
  sss_max_t <- app(sss_M_stack, max, na.rm = T)
  names(sss_max_t)[1] <- paste0("max_salinity_5m_", times_bin[t])
  sst_max_t <- app(sst_M_stack, max, na.rm = T)
  names(sst_max_t)[1] <- paste0("max_temperature_5m_", times_bin[t])
  
  # add to list of min. value rasters (includes all 4k times)
  sss_min_ls[[t]] <- sss_min_t
  sst_min_ls[[t]] <- sst_min_t 
  
  # add to list of max. value rasters (includes all 4k times)
  sss_max_ls[[t]] <- sss_max_t
  sst_max_ls[[t]] <- sst_max_t

}

# transform lists to stacks (all 4k times)
sss_min_s <- rast(sss_min_ls)
sst_min_s <- rast(sst_min_ls)

sss_max_s <- rast(sss_max_ls)
sst_max_s <- rast(sst_max_ls)

#
# calculate (overall) min. value raster for the bin stack of each variable
sss_min_bin <- app(sss_min_s, min, na.rm = T)
names(sss_min_bin)[1] <- paste0("min_salinity_5m_", from, "k_to_", to, "k")

sst_min_bin <- app(sst_min_s, min, na.rm = T)
names(sst_min_bin)[1] <- paste0("min_temperature_5m_", from, "k_to_", to, "k")

#
# calculate (overall) max. value raster for the bin stack of each variable
sss_max_bin <- app(sss_max_s, max, na.rm = T)
names(sss_max_bin)[1] <- paste0("max_salinity_5m_", from, "k_to_", to, "k")

sst_max_bin <- app(sst_max_s, max, na.rm = T)
names(sst_max_bin)[1] <- paste0("max_temperature_5m_", from, "k_to_", to, "k")

#
# calculate (overall) range [max - min] raster for the bin stack of each variable
sss_rng_bin <- sss_max_bin - sss_min_bin
names(sss_rng_bin)[1] <- paste0("range_salinity_5m_", from, "k_to_", to, "k")

sst_rng_bin <- sst_max_bin - sst_min_bin
names(sst_rng_bin)[1] <- paste0("range_temperature_5m_", from, "k_to_", to, "k")

# plots (optional)
# jpeg(filename = "summary_salinity_5m_1808k_to_1908k.jpeg", width = 1000, height = 1250, res = 200)
# par(mfrow = c(3,1))
# plot(sss_min_bin, main = names(sss_min_bin)[1])
# plot(sss_max_bin, main = names(sss_max_bin)[1])
# plot(sss_rng_bin, main = names(sss_rng_bin)[1])
# dev.off()
# 
# jpeg(filename = "summary_temperature_5m_1808k_to_1908k.jpeg", width = 1000, height = 1250, res = 200)
# par(mfrow = c(3,1))
# plot(sst_min_bin, main = names(sst_min_bin)[1])
# plot(sst_max_bin, main = names(sst_max_bin)[1])
# plot(sst_rng_bin, main = names(sst_rng_bin)[1])
# dev.off()

setwd("D:/202508_MaxentModels/0_EnvironmentalData/Holocene")

# save rasters
writeRaster(sss_min_bin, filename = paste0(names(sss_min_bin)[1], ".tif"))
writeRaster(sss_max_bin, filename = paste0(names(sss_max_bin)[1], ".tif"))
writeRaster(sss_rng_bin, filename = paste0(names(sss_rng_bin)[1], ".tif"))

writeRaster(sst_min_bin, filename = paste0(names(sst_min_bin)[1], ".tif"))
writeRaster(sst_max_bin, filename = paste0(names(sst_max_bin)[1], ".tif"))
writeRaster(sst_rng_bin, filename = paste0(names(sst_rng_bin)[1], ".tif"))
#
###