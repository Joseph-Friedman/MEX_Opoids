# Setup
#Get travel time from google
library(tidyverse)
library(sp)
library(rgeos)
library(devtools)
#install_github("DerekYves/placement")
library(placement)
#register_google("") #Insert Google API here

#Load ageb.est
ageb.est <- readOGR("/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/Scripts/AGEB_EST/ageb.est.shp")
names(ageb.est@data) <- c("CVE_AGEB", "CVEGEO", "CVE_ENT", "CVE_MUN", "CVE_LOC", "POB_2010", 
                          "IND1", "IND2", "IND3", "IND4", "IND5", "IND6", "IND7", "IND8",
                          "IND9", "IND10", "CVE_SUN", "NOM_SUN", "IMU2010", "GMU2010", 
                          "LUGAR_EDO", "LUGAR_NAC", "total_n", "SES", "localGvalues", 
                          "localGvalues.est.num", "KNN", "dist_num.sy", "time_mins.sy", "dist_num.otay",      
                          "time_mins.otay", "dist_num.west", "time_mins.west", "dist_num.east", "time_mins.east", 
                          "dist_num", "time_mins")
ageb.est@data$SES <- factor(ageb.est@data$SES, levels = c("Very high", "High", "Medium", "Low", "Very low"))

# Get distance to Border by AGEB, eval=FALSE, include=FALSE}
#Get centroind lat/lon
AGEB.centers <- SpatialPointsDataFrame(gCentroid(ageb.est, byid=TRUE), 
                                       ageb.est@data, match.ID=FALSE)
df.AGEB.centers <- as.data.frame(AGEB.centers)

#Filter 3 border cities
df.AGEB.centers.tj <- df.AGEB.centers %>% filter(NOM_SUN == "Tijuana")
df.AGEB.centers.tj$y <- as.numeric(df.AGEB.centers.tj$y)
df.AGEB.centers.tj$x <- as.numeric(df.AGEB.centers.tj$x)
df.AGEB.centers.tj$latlon <- paste0(df.AGEB.centers.tj$y,",",df.AGEB.centers.tj$x)
df.AGEB.centers.tj$latlon <- stri_trans_general(str = df.AGEB.centers.tj$latlon, id = "Latin-ASCII")
df.AGEB.centers.mxl <- df.AGEB.centers %>% filter(NOM_SUN == "Mexicali")
df.AGEB.centers.mxl$latlon <- paste0(df.AGEB.centers.mxl$y,",",df.AGEB.centers.mxl$x)
df.AGEB.centers.mxl$latlon <- stri_trans_general(str = df.AGEB.centers.mxl$latlon, id = "Latin-ASCII")
df.AGEB.centers.juarez <- df.AGEB.centers %>% filter(NOM_SUN == "Juárez")
df.AGEB.centers.juarez$latlon <- paste0(df.AGEB.centers.juarez$y,",",df.AGEB.centers.juarez$x)
df.AGEB.centers.juarez$latlon <- stri_trans_general(str = df.AGEB.centers.juarez$latlon, id = "Latin-ASCII")

#Get travel time from google
df.AGEB.centers.tj$dest.otay <- "32.549901, -116.938391" #Otay Mesa
df.AGEB.centers.tj$dest.sy <- "32.542349, -117.029078" #San Ysidro
tj.travel.time.otay <- drive_time(address = df.AGEB.centers.tj$latlon, 
                                  dest = df.AGEB.centers.tj$dest.otay, #Otay Mesa
                                  auth = "standard_api", privkey = "AIzaSyBPbtUynG9T9MUo59nQXqRID9OWf9t1lSk",
                                  clientid = NULL, clean = "TRUE", travel_mode = "driving",
                                  units = "metric", verbose = FALSE, add_date = "none",
                                  language = "en-EN", messages = TRUE, small = FALSE)
tj.travel.time.sy <- drive_time(address = df.AGEB.centers.tj$latlon, 
                                dest = df.AGEB.centers.tj$dest.sy, #San Ysidro
                                auth = "standard_api", privkey = "AIzaSyBPbtUynG9T9MUo59nQXqRID9OWf9t1lSk",
                                clientid = NULL, clean = "TRUE", travel_mode = "driving",
                                units = "metric", verbose = FALSE, add_date = "none",
                                language = "en-EN", messages = TRUE, small = FALSE)

df.AGEB.centers.mxl$dest.west <- "32.664679, -115.498365" #Calexico West 
df.AGEB.centers.mxl$dest.east <- "32.672920, -115.387876" #Calexico East 	
mxl.travel.time.west <- drive_time(address = df.AGEB.centers.mxl$latlon, dest = df.AGEB.centers.mxl$dest.west,
                                   auth = "standard_api", privkey = "AIzaSyBPbtUynG9T9MUo59nQXqRID9OWf9t1lSk",
                                   clientid = NULL, clean = "TRUE", travel_mode = "driving",
                                   units = "metric", verbose = FALSE, add_date = "none",
                                   language = "en-EN", messages = TRUE, small = FALSE)
mxl.travel.time.east <- drive_time(address = df.AGEB.centers.mxl$latlon, dest = df.AGEB.centers.mxl$dest.east,
                                   auth = "standard_api", privkey = "AIzaSyBPbtUynG9T9MUo59nQXqRID9OWf9t1lSk",
                                   clientid = NULL, clean = "TRUE", travel_mode = "driving",
                                   units = "metric", verbose = FALSE, add_date = "none",
                                   language = "en-EN", messages = TRUE, small = FALSE)

df.AGEB.centers.juarez$dest <- "31.764207, -106.451286" #Bridge of the Americas 	
juarez.travel.time <- drive_time(address = df.AGEB.centers.juarez$latlon, dest = df.AGEB.centers.juarez$dest,
                                 auth = "standard_api", privkey = "AIzaSyBPbtUynG9T9MUo59nQXqRID9OWf9t1lSk",
                                 clientid = NULL, clean = "TRUE", travel_mode = "driving",
                                 units = "metric", verbose = FALSE, add_date = "none",
                                 language = "en-EN", messages = TRUE, small = FALSE)

#Merge travel times/distance back to AGEB df
df.AGEB.centers.tj$dist_num.sy <- tj.travel.time.sy$dist_num
df.AGEB.centers.tj$time_mins.sy <- tj.travel.time.sy$time_mins
df.AGEB.centers.tj$dist_num.otay <- tj.travel.time.otay$dist_num
df.AGEB.centers.tj$time_mins.otay <- tj.travel.time.otay$time_mins
df.AGEB.centers.mxl$dist_num.west <- mxl.travel.time.west$dist_num
df.AGEB.centers.mxl$time_mins.west <- mxl.travel.time.west$time_mins
df.AGEB.centers.mxl$dist_num.east <- mxl.travel.time.west$dist_num
df.AGEB.centers.mxl$time_mins.east <- mxl.travel.time.west$time_mins
df.AGEB.centers.juarez$dist_num <- juarez.travel.time$dist_num
df.AGEB.centers.juarez$time_mins <- juarez.travel.time$time_mins

write.csv(df.AGEB.centers.tj, 
          "/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/scripts/df_AGEB_centers_tj.csv")
write.csv(df.AGEB.centers.mxl, 
          "/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/scripts/df_AGEB_centers_mxl.csv")
write.csv(df.AGEB.centers.juarez, 
          "/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/scripts/df_AGEB_centers_juarez.csv")

#Get df for large metro areas (Monterrey, CDMX, Guadalajara)
df.AGEB.centers.cdmx <- df.AGEB.centers %>% filter(NOM_SUN == "Valle de México")
df.AGEB.centers.gdl <- df.AGEB.centers %>% filter(NOM_SUN == "Guadalajara")
df.AGEB.centers.mty <- df.AGEB.centers %>% filter(NOM_SUN == "Monterrey")

write.csv(df.AGEB.centers.cdmx, 
          "/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/scripts/df_AGEB_centers_cdmx.csv")
write.csv(df.AGEB.centers.gdl, 
          "/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/scripts/df_AGEB_centers_gdl.csv")
write.csv(df.AGEB.centers.mty, 
          "/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/scripts/df_AGEB_centers_mty.csv")
