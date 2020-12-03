rm(list = ls())
pacman::p_load(data.table, tidyverse,stringr,stringi,DT,expss,reshape,reshape2,readxl,hablar,skimr,fuzzyjoin,ggmap,spdep,maptools,rgdal)

#Don not run without google api first
register_google(key = "[your key]")

df <- read_excel('anexo-15118-19.xlsx', 
                 sheet = 3)
colnames(df)[4] <- "Year"
colnames(df)[5] <- "Month"
colnames(df)[6] <- "n"

#Clean up text
df$Establecimiento <- stri_trans_general(str = df$Establecimiento, id = "Latin-ASCII")
df$Establecimiento <- str_to_upper(df$Establecimiento)
df$Estado <- stri_trans_general(str = df$Estado, id = "Latin-ASCII")
df$Estado <- str_to_upper(df$Estado)
df$Municipio <- stri_trans_general(str = df$Municipio, id = "Latin-ASCII")
df$Municipio <- sub('[^A-Za-z]+', ' ', df$Municipio)
df$Municipio <- str_to_upper(df$Municipio)

df.establecimiento <- df %>% group_by(Establecimiento, Municipio, Estado) %>% summarise(total_n=sum(n)) %>% ungroup
df.establecimientos <- df.establecimiento

#Get pharmacy coordinates
df.establecimientos$Municipio <- stri_trans_general(str = df.establecimientos$Municipio, id = "Latin-ASCII")
df.establecimientos$Municipio <- sub('[^A-Za-z]+', ' ', df.establecimientos$Municipio)
df.establecimientos$Estado <- stri_trans_general(str = df.establecimientos$Estado, id = "Latin-ASCII")
df.establecimientos$Estado <- str_to_upper(df.establecimientos$Estado)
df.establecimientos$place <- paste(df.establecimientos$Establecimiento,",", 
                                   df.establecimientos$Municipio,",", 
                                   df.establecimientos$Estado)
df.establecimientos$coords <- geocode(df.establecimientos$place, ext = ".com.mx")
df.establecimientos$lon <- df.establecimientos$coords$lon
df.establecimientos$lat <- df.establecimientos$coords$lat
df.establecimientos <- df.establecimientos %>% select(!coords, !place)

write.csv(df.establecimientos, "establecimientos_geo.csv")
#df.establecimientos <- read.csv("establecimientos_geo.csv")
##864 pharmacies - 4 not geocoded - Missing 0.5%

#Get methadone clinic coordinates
methadone <- read.csv("cofepris_methadone.csv")
methadone$Municipio <- sub('[^A-Za-z]+', '', methadone$Municipio)
methadone$Establecimiento <- str_replace_all(methadone$Establecimiento, "[\r\n]" , " ")
methadone$place <- paste(methadone$Establecimiento,",",
                         methadone$Calle,",",
                         methadone$Colonia,",",
                         methadone$Codigo.Postal,",",
                         methadone$Municipio,",", 
                         methadone$Estado)

methadone$coords <- geocode(methadone$place)
methadone$lon <- methadone$coords$lon
methadone$lat <- methadone$coords$lat
#write.csv(methadone, "cofepris_methadone.csv")

#Get Mexican cities coordinates
municipios <- paste0(df.establecimientos$Municipio,", ", 
                     df.establecimientos$Estado)
municipios <- as.data.frame(municipios) 
municipios <- municipios %>% group_by(municipios) %>% summarize()
municipios$municipios <- as.character(municipios$municipios)
municipios$coords <- geocode(municipios$municipios)
municipios$lon <- municipios$coords$lon
municipios$lat <- municipios$coords$lat
municipios$lat <- municipios$lat
municipios$lat <- municipios$lat - 0.05
municipios$lat <- municipios$lat - 0.025
municipios$lon <- municipios$lon - 0.025
municipios$municipios <- str_to_title(municipios$municipios)
municipios$municipios2 <- gsub(pattern=', ', ',\n', municipios$municipios)

municipios$lon[206] <- -117.15825 #Move Tijuana
municipios$lat[119] <- 32.556283 #Move Mexicali
municipios$lon[119] <- -115.520588 #Move Mexicali
municipios$lat[97] <- 31.681285 #Move Juarez
municipios$lon[97] <- -106.514774 #Move Juarez

write.csv(municipios, "municipios_geo.csv")

#Get US border cities coordinates
us.border <- c("San Diego, California", "Calexico, California", "San Luis, Arizona", "Nogales, Arizona",
               "El Paso, Texas", "Eagle Pass, Texas", "Hidalgo, Texas", "Brownsville, Texas", "Laredo, Texas")
us.border <- as.data.frame(us.border)
colnames(us.border)[1] <- "City"
us.border$City <- as.character(us.border$City)
us.border$coords <- geocode(us.border$City)
us.border$lon <- us.border$coords$lon
us.border$lat <- us.border$coords$lat
us.border$lat <- us.border$lat + 0.05
us.border$lat <- us.border$lat + 0.025
us.border$lon <- us.border$lon + 0.025 

us.border$lat[1] <- 32.575 #Change San Diego to show on map
us.border$lon[1] <- -117.15825 #Change San Diego to show on map
us.border$lat[2] <- 32.689704 #Change Calexico to show on map
us.border$lon[2] <- -115.450635 #Change Calexico to show on map


us.border$City2 <- gsub(pattern=', ', ',\n', us.border$City)

write.csv(us.border, "us_border.csv")

#Get border crossing geoms
crossings <- c("San Ysidro Port of Entry", "Otay Mesa Port of Entry", 
               "Calexico East Port of Entry", "Calexico West Port of Entry",
               "San Luis Rio Colorado Port of Entry",
               "Nogales Port of Entry",
               "El Paso Bridge of the Americas Port of Entry", "El Paso-PDN Port of Entry", 
               "El Paso Ysleta Port of Entry",
               "Eagle Pass Port of Entry", "Eagle Pass II Port of Entry",
               "Laredo-Juarez/Lincoln Port of Entry", "Laredo Bridge 1",
               "Hidalgo Port of Entry", 
               "Brownsville - Gateway Port of Entry", "Brownsville - Veterans", "Brownsville - B&M Port of Entry")
crossings <- as.data.frame(crossings)
colnames(crossings)[1] <- "Port"
crossings$Port <- as.character(crossings$Port)
crossings$coords <- geocode(crossings$Port)
crossings$lon <- crossings$coords$lon
crossings$lat <- crossings$coords$lat
write.csv(crossings, "crossings.csv")



p.cdmx <- ggmap(get_map(location = "cdmx",
                        zoom = 10, scale = 3,
                        source="stamen", maptype='terrain-lines', #http://maps.stamen.com/#watercolor/12/37.7706/-122.3782
                        color = 'bw', crop = T, legend='bottom'))

p.gdl <- ggmap(get_map(location = "guadalajara",
                       zoom = 11, scale = 3,
                       source="stamen", maptype='terrain-lines', #http://maps.stamen.com/#watercolor/12/37.7706/-122.3782
                       color = 'bw', crop = T, legend='bottom'))

p.mty <- ggmap(get_map(location = "monterrey",
                       zoom = 11, scale = 3,
                       source="stamen", maptype='terrain-lines', #http://maps.stamen.com/#watercolor/12/37.7706/-122.3782
                       color = 'bw', crop = T, legend='bottom'))

p.tj <- ggmap(get_map(location = "tijuana",
                      zoom = 11, scale = 3,
                      source="stamen", maptype='terrain-lines', #http://maps.stamen.com/#watercolor/12/37.7706/-122.3782
                      color = 'bw', crop = T, legend='bottom'))

p.mxl <- ggmap(get_map(location = "mexicali",
                       zoom = 11, scale = 3,
                       source="stamen", maptype='terrain-lines', #http://maps.stamen.com/#watercolor/12/37.7706/-122.3782
                       color = 'bw', crop = T, legend='bottom'))

p.juarez <- ggmap(get_map(location = "Juarez",
                          zoom = 11, scale = 3,
                          source="stamen", maptype='terrain-lines', #http://maps.stamen.com/#watercolor/12/37.7706/-122.3782
                          color = 'bw', crop = T, legend='bottom'))

#Save maps
save(p.cdmx, file = "Images/p_cdmx.RData")
save(p.gdl, file = "Images/p_gdl.RData")
save(p.mty, file = "Images/p_mty.RData")
save(p.tj, file = "Images/p_tj.RData")
save(p.mxl, file = "Images/p_mxl.RData")
save(p.juarez, file = "Images/p_juarez.RData")


#Load df with centroids/distance
df.AGEB.centers.tj <- read.csv( 
          "df_AGEB_centers_tj.csv")
df.AGEB.centers.mxl <- read.csv(  
          "df_AGEB_centers_mxl.csv")
df.AGEB.centers.juarez <- read.csv( 
          "df_AGEB_centers_juarez.csv")
df.AGEB.centers.cdmx <- read.csv(  
          "df_AGEB_centers_cdmx.csv")
df.AGEB.centers.gdl <- read.csv( 
          "df_AGEB_centers_gdl.csv")
df.AGEB.centers.mty <- read.csv( 
          "df_AGEB_centers_mty.csv")

ageb.shp <- readOGR("IMU_2010/IMU_2010.shp")
ageb.shp.epsg <- spTransform(ageb.shp, CRS("+proj=longlat +datum=WGS84")) #switch to long/lat as was projected

#Shp pharmacy coordinates
df.filter.na <- df.establecimientos %>% drop_na(lon)
coordinates(df.filter.na) <- ~lon+lat
dat <- SpatialPointsDataFrame(df.filter.na,data.frame(id=1:length(df.filter.na)))
proj4string(dat) <- CRS("+proj=longlat +ellps=WGS84") 
proj4string(ageb.shp.epsg) <- CRS("+proj=longlat +ellps=WGS84") 
res <- data.table(over(dat, ageb.shp.epsg))
res <- cbind(res, df.filter.na@data)
res <- res %>% group_by(CVEGEO) %>% summarise(sum(total_n))
colnames(res)[2] <- "total_n"
ageb.est <- merge(x=ageb.shp.epsg, y=res, by="CVEGEO", all.x=T)

#Change GMU2010 to SES
ageb.est@data$SES <- recode_factor(ageb.est@data$GMU2010,  
              `Muy alto` = "Very low", Alto = "Low", Medio = "Medium", 
              Bajo = "High", `Muy bajo` = "Very high")
ageb.est@data$SES <- factor(ageb.est@data$SES, 
                            levels = c("Very low", "Low", "Medium", "High", "Very high"))

#Change GMU2010 to English
ageb.est@data$GMU2010 <- recode_factor(ageb.est@data$GMU2010,  
              `Muy alto` = "Very high", Alto = "High", Medio = "Medium", 
              Bajo = "Low", `Muy bajo` = "Very low")

#https://pudding.cool/process/regional_smoothing/
#https://walkerke.github.io/2016/07/spatial-neighbors-in-r---an-interactive-illustration/
set.seed(1)

#Get coordinates from shp file
coords <- coordinates(ageb.est)
IDs<-row.names(as(ageb.est, "data.frame"))

#Run KNN
knn.obj <- knearneigh(coords, k = 20)
knn <- knn2nb(knearneigh(coords, k = 20), row.names = IDs)
knn <- include.self(knn)

#localG statistic for each AGEB for number of dispensed opioids
ageb.est.df <- as(ageb.est, "data.frame")
ageb.est.df[is.na(ageb.est.df)] <-0
localGvalues <- localG(x = as.numeric(ageb.est.df$total_n), 
                       listw = nb2listw(knn, style = "B"), zero.policy = FALSE)
localGvalues <- round(localGvalues,3)

#localG statistic for each AGEB for number of establishments
localGvalues.est.num <- localG(x = as.numeric(ageb.est.df$total_n), 
                       listw = nb2listw(knn, style = "B"), zero.policy = FALSE)
localGvalues.est.num <- round(localGvalues,3)

#Merge localGvalues (smoothing)
ageb.est@data$localGvalues <- localGvalues
ageb.est@data$localGvalues.est.num <- localGvalues.est.num
ageb.est@data$KNN <- knn
ageb.est@data$KNN <- as.character(ageb.est@data$KNN) 
ageb.est@data$localGvalues <- as.numeric(as.character(ageb.est@data$localGvalues))
ageb.est@data$localGvalues.est.num <- as.numeric(as.character(ageb.est@data$localGvalues.est.num))

#Merge distance to borders to main DF
ageb.est <- merge(x=ageb.est, y=df.AGEB.centers.tj[,c(3,28:31)], by=c("CVEGEO"), all.x=T)
ageb.est <- merge(x=ageb.est, y=df.AGEB.centers.mxl[,c(4,33:36)], by="CVEGEO", all.x=T)
ageb.est <- merge(x=ageb.est, y=df.AGEB.centers.juarez[,c(3,36:37)], by="CVEGEO", all.x=T)

#Replace NA in total number of pharmacies per ageb
ageb.est@data$total_n <- ageb.est@data$total_n %>% tidyr::replace_na(0)

#Save ageb_est
writeOGR(obj=ageb.est, dsn="AGEB_EST", layer="ageb.est", driver="ESRI Shapefile",
         overwrite_layer = T)
