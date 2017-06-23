library(dplyr)

# X1950_2012_torn <- read_csv("~/Desktop/ResearchRepo/1950-2012_torn.csv", col_names = FALSE)

# write.csv(torn, file = "torn.csv")
# write.csv(torn_vr, file = "torn_vr.csv")

# torn <- X1950_2012_torn
# 
# torn <- select(torn, fscale = X11, injury = X12, fatality = X13, loss = X14, stlat = X16, endlat = X18, stlon = X17, endlon = X19)
# 
# torn <- mutate(torn, lat = (stlat + endlat)/2,long = (stlon + endlon)/2) %>% filter(long < -67 & long > -130, lat > 20 & lat < 52)
# 
# torn_vr <- select(torn, hit = fscale, px = long, pz = lat) %>% filter(hit >= 0)

torn_vr <- read_csv("torn_vr.csv")

cutoff <- 50; dataset <- torn_vr

tornado_vr <- varyres(torn_vr, 50)
mapit(tornado_vr[[8]])
