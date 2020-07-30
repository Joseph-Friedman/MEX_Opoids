# Library, 
rm(list = ls())
pacman::p_load(data.table,tidyverse,stringr,stringi,DT,expss,reshape,reshape2,readxl,hablar,skimr,fuzzyjoin,ggmap,ggnewscale,rgdal,maps,sp,spdplyr,broom,colorspace,rgeos,smoothr,devtools,placement,glmmTMB,sjPlot)

# Get previously saved data, 
#Import previously saved data frames
us.border <- read.csv("us_border.csv")
municipios <- read.csv("municipios_geo.csv")
crossings <- read.csv("crossings.csv")
df.establecimientos <- read.csv("establecimientos_geo.csv")

#Import methadone and fix locations
methadone <- read.csv("cofepris_methadone.csv")
methadone <- methadone %>% filter(!X %in% c(10, 4, 5))
methadone <- methadone %>% mutate(lat = case_when(X == 16 ~ 32.664878,
                                                  TRUE ~ lat))
methadone <- methadone %>% mutate(lon = case_when(X == 16 ~ -115.484895,
                                                  TRUE ~ lon))
methadone <- methadone %>% mutate(lon = case_when(X == 17 ~ -99.171134,
                                                  TRUE ~ lon))
methadone <- methadone %>% mutate(lat = case_when(X == 16 ~ 19.407855,
                                                  TRUE ~ lat))

df.filter <- df.establecimientos

#Get US-Mexico international border
US.mex.border <- readOGR(
  "Mexico_and_US_Border/Mexico_and_US_Border.shp")
US.mex.border <- densify(US.mex.border, n = 10)
US.mex.border <- fortify(US.mex.border)

#Load base maps
load(file = "Images/p_cdmx.RData")
load(file = "Images/p_mty.RData")       
load(file = "Images/p_gdl.RData")  
load(file = "Images/p_tj.RData")
load(file = "Images/p_juarez.RData")       
load(file = "Images/p_mxl.RData")  


# Load SHP file after all the merging}
#Load ageb.est
ageb.est <- readOGR("AGEB_EST/ageb.est.shp")
names(ageb.est@data) <- c("CVE_AGEB", "CVEGEO", "CVE_ENT", "CVE_MUN", "CVE_LOC", "POB_2010", 
                          "IND1", "IND2", "IND3", "IND4", "IND5", "IND6", "IND7", "IND8",
                          "IND9", "IND10", "CVE_SUN", "NOM_SUN", "IMU2010", "GMU2010", 
                          "LUGAR_EDO", "LUGAR_NAC", "total_n", "SES", "localGvalues", 
                          "localGvalues.est.num", "KNN", "dist_num.sy", "time_mins.sy", "dist_num.otay",      
                          "time_mins.otay", "dist_num.west", "time_mins.west", "dist_num.east", "time_mins.east", 
                          "dist_num", "time_mins", "X", "establecimientos.num")
ageb.est@data$SES <- factor(ageb.est@data$SES, levels = c("Very high", "High", "Medium", "Low", "Very low"))


# Make city df, warning=FALSE}
#Subset city shape files
cdmx.shp <- ageb.est %>% filter(NOM_SUN == "Valle de México") #Filter Mexico City
mty.shp <- ageb.est %>% filter(NOM_SUN == "Monterrey")  #Filter Monterrey
gdl.shp <- ageb.est %>% filter(NOM_SUN == "Guadalajara")  #Filter Gudalajara
tj.shp <- ageb.est %>% filter(NOM_SUN == "Tijuana")  #Filter Tiuana
mxl.shp <- ageb.est %>% filter(NOM_SUN == "Mexicali")  #Filter Mexicali
juarez.shp <- ageb.est %>% filter(NOM_SUN == "Juárez")  #Filter Juarez

#Create quantiles for time to border
quantile(tj.shp@data$time_mins.sy, probs = c(0.25, 0.50, 0.75))
quantile(mxl.shp@data$time_mins.west, probs = c(0.25, 0.50, 0.75))
quantile(juarez.shp@data$time_mins, probs = c(0.25, 0.50, 0.75))
tj.shp@data <- tj.shp@data %>% mutate(distance_quartile = case_when(time_mins.sy < 15 ~ "<15 min",
                                                     time_mins.sy >= 15 & 
                                                       time_mins.sy <= 30 ~ "15-30 min",
                                                     time_mins.sy > 30 ~ ">30 min"
                                                     ))

tj.shp@data$distance_quartile <- factor(tj.shp@data$distance_quartile, 
                                        levels = c(">30 min", "15-30 min", "<15 min" ))

mxl.shp@data <- mxl.shp@data %>% mutate(distance_quartile = case_when(time_mins.west < 15 ~ "<15 min",
                                                     time_mins.west >= 15 & time_mins.west <= 30 ~ "15-30 min",
                                                     time_mins.west > 30 ~ ">30 min"
                                                     ))
mxl.shp@data$distance_quartile <- factor(mxl.shp@data$distance_quartile, 
                                        levels = c(">30 min", "15-30 min", "<15 min" ))

juarez.shp@data <- juarez.shp@data %>% mutate(distance_quartile = case_when(time_mins < 15 ~ "<15 min",
                                                     time_mins >= 15 & time_mins <= 30 ~ "15-30 min",
                                                     time_mins > 30 ~ ">30 min"
                                                     ))
juarez.shp@data$distance_quartile <- factor(juarez.shp@data$distance_quartile, 
                                        levels = c(">30 min", "15-30 min", "<15 min"))

tj.shp.travel <- maptools::unionSpatialPolygons(tj.shp, IDs = tj.shp@data$distance_quartile)
tj.travel.tidy <- tidy(tj.shp, region="distance_quartile")


cdmx.tidy <- tidy(cdmx.shp, region="CVE_AGEB") #Convert to df for ggplot
cdmx.tidy <- cdmx.tidy %>% left_join(cdmx.shp@data, by=c("id" = "CVE_AGEB"))
cdmx.tidy$total_n <- cdmx.tidy$total_n %>% replace_na(0)
df.establecimiento.cdmx <- df.filter %>% filter(Estado %in% c("CIUDAD DE MEXICO", "MEXICO")) 


mty.tidy <- tidy(mty.shp, region="CVE_AGEB") #Convert to df for ggplot
mty.tidy <- mty.tidy %>% left_join(mty.shp@data, by=c("id" = "CVE_AGEB"))
mty.tidy$total_n <- mty.tidy$total_n %>% replace_na(0)
df.establecimiento.mty <- df.filter %>% filter(Estado %in% c("NUEVO LEON")) 
df.establecimiento.mty <- df.establecimiento.mty %>% filter(!Establecimiento %in% "BOTICA - HOSPITAL GENERAL DE CERRALVO")


gdl.tidy <- tidy(gdl.shp, region="CVE_AGEB") #Convert to df for ggplot
gdl.tidy <- gdl.tidy %>% left_join(gdl.shp@data, by=c("id" = "CVE_AGEB"))
gdl.tidy$total_n <- gdl.tidy$total_n %>% replace_na(0)
df.establecimiento.gdl <- df.filter %>% filter(Estado %in% "JALISCO") 


tj.tidy <- tidy(tj.shp, region="CVE_AGEB") #Convert to df for ggplot
tj.tidy <- tj.tidy %>% left_join(tj.shp@data, by=c("id" = "CVE_AGEB"))
tj.tidy$total_n <- tj.tidy$total_n %>% replace_na(0)
df.establecimiento.tj <- df.filter %>% filter(Municipio %in% c(" TIJUANA", " PLAYAS DE ROSARITO")) 


mxl.tidy <- tidy(mxl.shp, region="CVE_AGEB") #Convert to df for ggplot
mxl.tidy <- mxl.tidy %>% left_join(mxl.shp@data, by=c("id" = "CVE_AGEB"))
mxl.tidy$total_n <- mxl.tidy$total_n %>% replace_na(0)
df.establecimiento.mxl <- df.filter %>% filter(Municipio %in% " MEXICALI") 
 

juarez.tidy <- tidy(juarez.shp, region="CVE_AGEB") #Convert to df for ggplot
juarez.tidy <- juarez.tidy %>% left_join(juarez.shp@data, by=c("id" = "CVE_AGEB"))
juarez.tidy$total_n <- juarez.tidy$total_n %>% replace_na(0)
df.establecimiento.juarez <- df.filter %>% filter(Municipio %in% " JUAREZ") 

#Flip SES variable
dfs <- c("cdmx.shp", "gdl.shp", "mty.shp", 
          "tj.shp", "mxl.shp", "juarez.shp")

for(df in dfs) {
    df.tmp <- get(df)
    df.tmp$POB_2010 <- as.numeric(df.tmp$POB_2010)
    df.tmp$IMU2010r <- df.tmp$IMU2010*-1 #flip numbers
  assign(df, df.tmp)
}


# Mexico City Map, echo=FALSE, warning=FALSE}
cdmx.map <- p.cdmx +
  geom_polygon(data = cdmx.tidy, aes(x = long, y = lat, group = group, fill = SES), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  theme_void() + 
  new_scale_color() +
  geom_point(data= df.establecimiento.cdmx, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.5) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a"), 
              size=3, shape=24, alpha=0.9) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Mexico City") + theme(legend.position = "right")
cdmx.map

ggsave("Images/map_cdmx.png", 
       plot = cdmx.map, dpi = 320, width = 8, height = 8)



# Mexico City Bar, echo=FALSE, warning=FALSE}
cdmx.bar <- ggplot(cdmx.shp@data, aes(fill=SES, y=total_n, x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
    new_scale_fill() +
  scale_fill_manual(name="Percentage of\ndispensed opioids\nby socioeconomic status",
                     values="Percentage of\ndispensed opioids\nby socioeconomic status") +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
cdmx.bar
ggsave("Images/bar_cdmx.png", dpi = 300, width = 8, height = 2)



# Mexico City Bar Population, echo=FALSE, warning=FALSE}

cdmx.bar.pob <- ggplot(cdmx.shp@data, aes(fill=SES, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
cdmx.bar.pob
ggsave("Images/bar_cdmx_pob.png", dpi = 300, width = 8, height = 2)



# Monterrey Map, warning=FALSE}
#Monterrey map
mty.map <- p.mty +
  geom_polygon(data = mty.tidy, aes(x = long, y = lat, group = group, fill = SES), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  theme_void() + 
  new_scale_color() +
  geom_point(data= df.establecimiento.mty, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.8) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=0.9) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Monterrey")
mty.map

ggsave("Images/map_monterrey.png", 
       plot = mty.map, dpi = 320, width = 8, height = 8)


# Monterrey Bar, echo=FALSE, warning=FALSE}
mty.bar <- ggplot(mty.shp@data, aes(fill=SES, y=total_n, x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
mty.bar
ggsave("Images/bar_mty.png", dpi = 300, width = 8, height = 2)



# Monterrey Bar Population, echo=FALSE, warning=FALSE}

mty.bar.pob <- ggplot(mty.shp@data, aes(fill=SES, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
mty.bar.pob
ggsave("Images/bar_mty_pob.png", dpi = 300, width = 8, height = 2)



# Guadalajara map, warning=FALSE}
gdl.map <- p.gdl +
  geom_polygon(data = gdl.tidy, aes(x = long, y = lat, group = group, fill = SES), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  theme_void() + 
  new_scale_color() +
  geom_point(data= df.establecimiento.gdl, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.8) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=0.9) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Guadalajara")
gdl.map

ggsave("Images/map_guadalajara.png", 
       plot = gdl.map, dpi = 320, width = 8, height = 8)


# Guadalajara Bar, echo=FALSE, warning=FALSE}
gdl.bar <- ggplot(gdl.shp@data, aes(fill=SES, y=total_n, x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
gdl.bar
ggsave("Images/bar_gdl.png", dpi = 300, width = 8, height = 2)



# Guadalajara Bar Population, echo=FALSE, warning=FALSE}

gdl.bar.pob <- ggplot(gdl.shp@data, aes(fill=SES, y=as.numeric(POB_2010), x=NOM_SUN)) + 
   geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip()  
gdl.bar.pob
ggsave("Images/bar_gdl_pob.png", dpi = 300, width = 8, height = 2)



# Tijuana Map, warning=FALSE}
#Tijuana map
tj.map <- p.tj +
  geom_path(data=US.mex.border, aes(x = long, y = lat, colour = "red")) +
  geom_polygon(data = tj.tidy, aes(x = long, y = lat, group = group, fill = SES), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  theme_void() + 
  geom_point(data=crossings, aes(x=lon, y=lat, colour = "navy blue"), shape=18, size=5, alpha=1) +
  scale_color_manual(label=c("Port of Entry", "U.S. - Mexico border"), 
                     values = c("navy blue", "red"), name = " ") +

  new_scale_color() +
  geom_point(data= df.establecimiento.tj, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.7) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  geom_label(data=municipios, aes(x = lon, y = lat, label=municipios2)) + 
  geom_label(data=us.border, aes(x = lon, y = lat, label=City2), 
             fill='dark green', colour="white") +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=1) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Tijuana")
tj.map

ggsave("Images/map_tijuana.png", 
       plot = tj.map, dpi = 320, width = 8, height = 8)



# Tijuana Map - Travel time, warning=FALSE}
tj.travel.map <- p.tj +
  geom_path(data=US.mex.border, aes(x = long, y = lat, colour = "red")) +
  geom_polygon(data = tj.tidy, aes(x = long, y = lat, group = group, fill = distance_quartile), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Time to main\nport of entry", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_void() + 
  geom_point(data=crossings, aes(x=lon, y=lat, colour = "navy blue"), shape=18, size=5, alpha=1) +
  scale_color_manual(label=c("Port of Entry", "U.S. - Mexico border"), 
                     values = c("navy blue", "red"), name = " ") +
  new_scale_color() +
  geom_point(data= df.establecimiento.tj, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.7) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  geom_label(data=municipios, aes(x = lon, y = lat, label=municipios2)) + 
  geom_label(data=us.border, aes(x = lon, y = lat, label=City2), 
             fill='dark green', colour="white") +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=1) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Tijuana")
tj.travel.map

ggsave("Images/map_tijuana_travel.png", 
       plot = tj.travel.map, dpi = 320, width = 8, height = 8)



# Tijuana Bar, echo=FALSE, warning=FALSE}
tj.bar <- ggplot(tj.shp@data, aes(fill=SES, y=total_n, x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
tj.bar
ggsave("Images/bar_tj.png", dpi = 300, width = 8, height = 2)



# Tijuana Bar Population, echo=FALSE, warning=FALSE}

tj.bar.pob <- ggplot(tj.shp@data, aes(fill=SES, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
tj.bar.pob
ggsave("Images/bar_tj_pob.png", dpi = 300, width = 8, height = 2)



# Tijuana Bar Crossing, echo=FALSE, warning=FALSE}
tj.bar.cross <- ggplot(tj.shp@data, aes(fill=distance_quartile, y=total_n, x=NOM_SUN)) + 
  geom_bar(position="fill", stat="identity", width = 0.1, stroke = "white") +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Time to border crossing", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = T,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
tj.bar.cross
ggsave("Images/bar_tj_cross.png", dpi = 300, width = 8, height = 2)



# Tijuana Bar Population - Crossing, echo=FALSE, warning=FALSE}

tj.bar.pob.cross <- ggplot(tj.shp@data, aes(fill=distance_quartile, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position="fill", stat="identity", width = 0.1, stroke = "white") +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Time to border crossing", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
tj.bar.pob.cross
ggsave("Images/bar_tj_pob_cross.png", dpi = 300, width = 8, height = 2)



# Mexicali map, warning=FALSE}
us.border <- us.border %>% mutate(lat = case_when(City == "Calexico, California" ~ 32.71970,
                                                  TRUE ~ lat))

#Mexicali map
mxl.map <- p.mxl +
  geom_path(data=US.mex.border, aes(x = long, y = lat, colour = "red")) +
  geom_point(data=crossings, aes(x=lon, y=lat, colour = "navy blue"), shape=18, size=5, alpha=1) +
  scale_color_manual(label=c("Port of Entry", "U.S. - Mexico border"), 
                     values = c("navy blue", "red"), name = " ") +
  geom_polygon(data = mxl.tidy, aes(x = long, y = lat, group = group, fill = SES), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  theme_void() + 
  new_scale_color() +
  geom_point(data= df.establecimiento.mxl, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.7) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  geom_label(data=municipios, aes(x = lon, y = lat, label=municipios2)) + 
  geom_label(data=us.border, aes(x = lon, y = lat, label=City2), 
             fill='dark green', colour="white") +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=1) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Mexicali")
mxl.map

ggsave("Images/map_mexicali.png", 
       plot = mxl.map, dpi = 320, width = 8, height = 8)


# Mexicali Map - Travel time, warning=FALSE}
mxl.travel.map <- p.mxl +
  geom_path(data=US.mex.border, aes(x = long, y = lat, colour = "red")) +
  geom_polygon(data = mxl.tidy, aes(x = long, y = lat, group = group, fill = distance_quartile), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Time to main\nport of entry", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_void() + 
  geom_point(data=crossings, aes(x=lon, y=lat, colour = "navy blue"), shape=18, size=5, alpha=1) +
  scale_color_manual(label=c("Port of Entry", "U.S. - Mexico border"), 
                     values = c("navy blue", "red"), name = " ") +
  new_scale_color() +
  geom_point(data= df.establecimiento.mxl, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.7) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  geom_label(data=municipios, aes(x = lon, y = lat, label=municipios2)) + 
  geom_label(data=us.border, aes(x = lon, y = lat, label=City2), 
             fill='dark green', colour="white") +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=1) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Mexicali")
mxl.travel.map

ggsave("Images/map_mexicali_travel.png", 
       plot = mxl.travel.map, dpi = 320, width = 8, height = 8)



# Mexicali Bar, echo=FALSE, warning=FALSE}
mxl.bar <- ggplot(mxl.shp@data, aes(fill=SES, y=total_n, x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
mxl.bar
ggsave("Images/bar_mxl.png", dpi = 300, width = 8, height = 2)



# Mexicali Bar Population, echo=FALSE, warning=FALSE}

mxl.bar.pob <- ggplot(mxl.shp@data, aes(fill=SES, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
mxl.bar.pob
ggsave("Images/bar_mxl_pob.png", dpi = 300, width = 8, height = 2)



# Mexicali Bar Crossing, echo=FALSE, warning=FALSE}
mxl.bar.cross <- ggplot(mxl.shp@data, aes(fill=distance_quartile, y=total_n, x=NOM_SUN)) + 
  geom_bar(position="fill", stat="identity", width = 0.1, stroke = "white") +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Time to border crossing", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = T,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 20),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
mxl.bar.cross
ggsave("Images/bar_mxl_cross.png", 
       dpi = 300, width = 6, height = 2)



# Mexicali Bar Population - Crossing, echo=FALSE, warning=FALSE}

mxl.bar.pob.cross <- ggplot(mxl.shp@data, aes(fill=distance_quartile, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position="fill", stat="identity", width = 0.1, stroke = "white") +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Time to border crossing", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
mxl.bar.pob.cross
ggsave("Images/bar_mxl_pob_cross.png", dpi = 300, width = 8, height = 2)



# Juarez Map, warning=FALSE}
juarez.map <- p.juarez +
  geom_path(data=US.mex.border, aes(x = long, y = lat, colour = "red")) +
  geom_point(data=crossings, aes(x=lon, y=lat, colour = "navy blue"), shape=18, size=5, alpha=1) +
  scale_color_manual(label=c("Port of Entry", "U.S. - Mexico border"), 
                     values = c("navy blue", "red"), name = " ") +
  geom_polygon(data = juarez.tidy, aes(x = long, y = lat, group = group, fill = SES), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  theme_void() + 
  new_scale_color() +
  geom_point(data= df.establecimiento.juarez, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.7) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  geom_label(data=municipios, aes(x = lon, y = lat, label=municipios2)) + 
  geom_label(data=us.border, aes(x = lon, y = lat, label=City2), 
             fill='dark green', colour="white") +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=1) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Ciudad Juarez")
juarez.map

ggsave("Images/map_juarez.png", 
       plot = juarez.map, dpi = 320, width = 8, height = 8)


# Juarez Map - Travel time, warning=FALSE}
juarez.travel.map <- p.juarez +
  geom_path(data=US.mex.border, aes(x = long, y = lat, colour = "red")) +
  geom_polygon(data = juarez.tidy, aes(x = long, y = lat, group = group, fill = distance_quartile), 
               colour = F, alpha=0.8) +
  scale_fill_discrete_sequential(name="Time to main\nport of entry", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_void() + 
  geom_point(data=crossings, aes(x=lon, y=lat, colour = "navy blue"), shape=18, size=5, alpha=1) +
  scale_color_manual(label=c("Port of Entry", "U.S. - Mexico border"), 
                     values = c("navy blue", "red"), name = " ") +
  new_scale_color() +
  geom_point(data= df.establecimiento.juarez, aes(x=lon, y=lat, size=total_n, color="a"), shape=21, alpha=0.7) +
  scale_color_manual(name="", labels=c("Dispensing pharmacy"), values = "black") +
  scale_size_continuous(name="Dispensed prescriptions", range = c(3, 10), guide=F) +
  geom_label(data=municipios, aes(x = lon, y = lat, label=municipios2)) + 
  geom_label(data=us.border, aes(x = lon, y = lat, label=City2), 
             fill='dark green', colour="white") +
  new_scale_color() +
  geom_point(data = methadone, aes(x=lon, y=lat, colour = "a",), 
              size=3, shape=24, alpha=1) +
  scale_color_manual(name="", labels=c("Methadone clinic"), values = "dark green") +
  labs(title = "Juarez")
juarez.travel.map

ggsave("Images/map_juarez_travel.png", 
       plot = juarez.travel.map, dpi = 320, width = 8, height = 8)



# Juarez Bar, echo=FALSE, warning=FALSE}
juarez.bar <- ggplot(juarez.shp@data, aes(fill=SES, y=total_n, x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
juarez.bar
ggsave("Images/bar_juarez.png", dpi = 300, width = 8, height = 2)



# Juarez Bar Population, echo=FALSE, warning=FALSE}

juarez.bar.pob <- ggplot(juarez.shp@data, aes(fill=SES, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position = position_fill(reverse = TRUE), 
           stat="identity", width = 0.1, stroke = "white", alpha = 0.8) +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Socioeconomic status", palette = "Heat", order = c(1:5)) +
  guides(fill = guide_legend(reverse = FALSE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
juarez.bar.pob
ggsave("Images/bar_juarez_pob.png", dpi = 300, width = 8, height = 2)



# Juarez Bar Crossing, echo=FALSE, warning=FALSE}
juarez.bar.cross <- ggplot(juarez.shp@data, aes(
  fill=distance_quartile, y=total_n, x=NOM_SUN)) + 
  geom_bar(position = "fill", stat="identity", width = 0.1, stroke = "white") +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Time to\nborder crossing", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = T,
                             title.position = "top",
                             label.position = "right",
                             keywidth = 1,
                             nrow = 3)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "right",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 20),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
juarez.bar.cross
ggsave("Images/bar_juarez_cross.png", dpi = 300, width = 8, height = 2)



# Juarez Bar Population - Crossing, echo=FALSE, warning=FALSE}

juarez.bar.pob.cross <- ggplot(juarez.shp@data, aes(fill=distance_quartile, y=as.numeric(POB_2010), x=NOM_SUN)) + 
  geom_bar(position="fill", stat="identity", width = 0.1, stroke = "white") +
  scale_x_discrete(labels = "", expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete_sequential(name="Time to border crossing", palette = "Sunset", order = c(3:1)) +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = "",
       fill = NULL,
       caption = NULL,
       title = NULL,
       subtitle = NULL) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 3, size = 16),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(30, 10, 0, 10),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white",
                                size = 2, linetype = "solid")) +
  coord_flip() 
juarez.bar.pob.cross
ggsave("Images/bar_juarez_pob_cross.png", dpi = 300, width = 8, height = 2)



# Aggregate Maps, echo=FALSE, warning=FALSE}
#Merge border opioids by SES
metro.map <- ggpubr::ggarrange(plotlist = list(cdmx.map + theme(legend.position="none"),
                                               mty.map + theme(legend.position="none"),
                                               gdl.map + theme(legend.position="none")), 
                               nrow = 1, ncol=3,
           common.legend = F)
metro.map.legend <- ggpubr::get_legend(cdmx.map)
metro.map.legend <- ggpubr::as_ggplot(metro.map.legend)

border.map <- ggpubr::ggarrange(plotlist = list(tj.map + theme(legend.position="none"),
                                                mxl.map + theme(legend.position="none"),
                                                juarez.map + theme(legend.position="none"))
                                , nrow = 1, ncol=3,
                  common.legend = F)

border.map.legend <- ggpubr::get_legend(tj.map)
border.map.legend <- ggpubr::as_ggplot(border.map.legend)

border.travel.map <- ggpubr::ggarrange(plotlist = list(tj.travel.map + theme(legend.position="none"),
                                                mxl.travel.map + theme(legend.position="none"),
                                                juarez.travel.map + theme(legend.position="none"))
                                , nrow = 1, ncol=3,
                  common.legend = F)

border.travel.map.legend <- ggpubr::get_legend(tj.travel.map)
border.travel.map.legend <- ggpubr::as_ggplot(border.travel.map.legend)

#Save maps
ggsave("Images/map_border.png", 
       plot = border.map, dpi = 320, height = 6, width = 11)
ggsave("Images/map_border_travel.png", 
       plot = border.travel.map, dpi = 320, height = 6, width = 11)
ggsave("Images/map_metro.png", 
       plot = metro.map, dpi = 320, height = 6, width = 11)


# Aggregate bars}
#Merge metro bar opioids by SES
metro.bar <- ggpubr::ggarrange(plotlist = list(cdmx.bar + theme(legend.position="none"),
                                               mty.bar + theme(legend.position="none"),
                                               gdl.bar + theme(legend.position="none")), 
                               nrow = 1, ncol=3,
                  common.legend = F)
metro.bar <- ggpubr::annotate_figure(metro.bar,
                top = ggpubr::text_grob("Percentage of dispensed opioids by socioeconomic status group", 
                                   color = "black", size = 12))
metro.bar.legend <- ggpubr::get_legend(cdmx.bar)
metro.bar.legend <- ggpubr::as_ggplot(metro.bar.legend)

#Merge SES by population bar plots - Border
metro.bar.pob <- ggpubr::ggarrange(plotlist = list(cdmx.bar.pob + theme(legend.position="none"),
                                                   mty.bar.pob + theme(legend.position="none"),
                                                   gdl.bar.pob + theme(legend.position="none")), 
                                      nrow = 1, ncol=3,
                  common.legend = FALSE)
metro.bar.pob <- ggpubr::annotate_figure(metro.bar.pob,
                top = ggpubr::text_grob("Percentage of dispensed opioids by socioeconomic status group", 
                                   color = "black", size = 12))

#Bar opioids by SES - Border
border.bar <- ggpubr::ggarrange(plotlist = list(tj.bar + theme(legend.position="none"),
                                                mxl.bar + theme(legend.position="none"),
                                                juarez.bar + theme(legend.position="none")), 
                                nrow = 1, ncol=3,
                  common.legend = FALSE)
border.bar <- ggpubr::annotate_figure(border.bar,
                top = ggpubr::text_grob("Percentage of dispensed opioids by socioeconomic status group", 
                                   color = "black", size = 12))

border.bar.legend <- ggpubr::get_legend(tj.bar)
border.bar.legend <- ggpubr::as_ggplot(border.bar.legend)

#Merge SES by population bar plots - Border
border.bar.pob <- ggpubr::ggarrange(plotlist = list(tj.bar.pob + theme(legend.position="none"),
                                                    mxl.bar.pob + theme(legend.position="none"),
                                                    juarez.bar.pob + theme(legend.position="none")),
                                      nrow = 1, ncol=3,
                  common.legend = FALSE)
border.bar.pob <- ggpubr::annotate_figure(border.bar.pob,
                top = ggpubr::text_grob("Percentage of population by socioeconomic status group", 
                                   color = "black", size = 12))

#Border crossing time by opioids
border.bar.cross <- ggpubr::ggarrange(plotlist = list(tj.bar.cross + theme(legend.position="none"),
                                                      mxl.bar.cross + theme(legend.position="none"),
                                                      juarez.bar.cross + theme(legend.position="none")), 
                                      nrow = 1, ncol=3,
                  common.legend = FALSE)
border.bar.cross <- ggpubr::annotate_figure(border.bar.cross,
                top = ggpubr::text_grob("Percentage of dispensed opioids by time to border crossing group", 
                                   color = "black", size = 12))
border.bar.cross.legend <- ggpubr::get_legend(juarez.bar.cross)
border.bar.cross.legend <- ggpubr::as_ggplot(border.bar.cross.legend)

#Border crossing by population bar plots
border.bar.cross.pop <- ggpubr::ggarrange(plotlist = list(tj.bar.pob.cross + theme(legend.position="none"),
                                                          mxl.bar.pob.cross + theme(legend.position="none"),
                                                          juarez.bar.pob.cross + theme(legend.position="none")),
                                      nrow = 1, ncol=3,
                  common.legend = FALSE)
border.bar.cross.pop <- ggpubr::annotate_figure(border.bar.cross.pop,
                top = ggpubr::text_grob("Percentage of population by time to border crossing group", 
                                   color = "black", size = 12))

ggsave("Images/bar_metro.png", 
       plot = metro.bar, dpi = 320, height = 1.5, width = 9)
ggsave("Images/bar_metro_pob.png", 
       plot = metro.bar.pob, dpi = 320, height = 1.5, width = 9)
ggsave("Images/bar_border.png", 
       plot = border.bar, dpi = 320, height = 1.5, width = 9)
ggsave("Images/bar_border_cross.png", 
       plot = border.bar.cross, dpi = 320, height = 1.5, width = 9)
ggsave("Images/bar_border_pob.png", 
       plot = border.bar.pob, dpi = 320, height = 1.5, width = 9)
ggsave("Images/bar_border_cross_pop.png", 
       plot = border.bar.cross.pop, dpi = 320, height = 1.5, width = 9)


# Aggregate all together}
#Aggregate metro maps with population and opioids dispensed
metro.map.bar.pob <- ggpubr::ggarrange(plotlist = list(metro.map, 
                                  metro.bar, 
                                  metro.bar.pob),
                  nrow = 3, ncol=1,
                  heights = c(5,1,1))
metro.map.bar.pob.legend <- ggpubr::ggarrange(plotlist = 
                                                list(metro.map.bar.pob, 
                                  metro.map.legend),
                  nrow = 1, ncol=2,
                  widths = c(5,1))

#Aggregate border maps with population and opioids dispensed
border.map.bar.pob <- ggpubr::ggarrange(plotlist = list(border.map, 
                                  border.bar, 
                                  border.bar.pob),
                  nrow = 3, ncol=1,
                  heights = c(5,1,1))
border.map.bar.pob.legend <- ggpubr::ggarrange(plotlist = list(border.map.bar.pob, 
                                  border.map.legend),
                  nrow = 1, ncol=2,
                  widths = c(5,1))
 
#Aggregate border travel maps with population and opioids dispensed
border.map.travel.bar.cross <- ggpubr::ggarrange(plotlist = list(border.travel.map, 
                                  border.bar.cross,
                                  border.bar.cross.pop),
                  nrow = 3, ncol=1,
                  heights = c(5,1,1))
border.map.travel.bar.cross.legend <- ggpubr::ggarrange(plotlist = list(border.map.travel.bar.cross, 
                                  border.travel.map.legend),
                  nrow = 1, ncol=2,
                  widths = c(5,1)) 


ggsave("Images/agg_metro_map.png", 
       plot = metro.map.bar.pob.legend, dpi = 320, height = 8, width = 11)
ggsave("Images/agg_border_map.png", 
       plot = border.map.bar.pob.legend, dpi = 320, height = 8, width = 11)
ggsave("Images/agg_border__travel_map.png", 
       plot = border.map.travel.bar.cross.legend, dpi = 320, height = 8, width = 11)

#Save TIFFS
ggsave("Images/agg_metro_map.tiff", 
       plot = metro.map.bar.pob.legend, dpi = 320, height = 8, width = 11)
ggsave("Images/agg_border_map.tiff", 
       plot = border.map.bar.pob.legend, dpi = 320, height = 8, width = 11)
ggsave("Images/agg_border__travel_map.tiff", 
       plot = border.map.travel.bar.cross.legend, dpi = 320, height = 8, width = 11)


# TJ Heatmaps}
tj.df <- tj.shp@data %>% group_by(SES, distance_quartile) %>% 
  summarise(total_n=sum(total_n), POB_2010 = sum(POB_2010)) %>% ungroup %>% mutate(percent_scripts=total_n/sum(total_n)*100, percent_population = POB_2010/sum(POB_2010)*100)

tj.df$SES <- factor(tj.df$SES, levels = c("Very high", "High", "Medium", "Low", "Very low"))
tj.df <- tj.df %>% mutate(percent_scripts_factor = case_when(percent_scripts <20 ~ "<20%",
                                                   percent_scripts >= 20 & percent_scripts <=39 ~ "20-39%",
                                                   percent_scripts >= 40 & percent_scripts <= 59 ~ "40-59%",
                                                   percent_scripts >= 60 & percent_scripts <= 79 ~ "60-79%",
                                                   percent_scripts >=80 ~ "≥80%",
                                                   ))

tj.heat <- ggplot(tj.df, aes(x = SES, y = distance_quartile, fill = percent_scripts)) + 
  geom_tile(colour = "white", size = 3) + 
  scale_fill_continuous_sequential(name="Percentage of opioids dispensed", palette = "Heat",
                                   limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_discrete(name = "",
                   breaks = c("Very high", "High", "Medium", "Low", "Very low"),
                   labels = c("Very high", "High", "Medium", "Low", "Very low")) + 
  scale_y_discrete(name = "") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() +
  labs(title = "Tijuana")
tj.heat

tj.heat.pop <- ggplot(tj.df, aes(x = SES, y = distance_quartile, fill = percent_population)) + 
  geom_tile(colour = "white", size = 3) + 
  scale_fill_continuous_sequential(name="Percentage of population", palette = "Sunset",
                                   limits = c(0,30)) +
  scale_x_discrete(name = "",
                   breaks = c("Very high", "High", "Medium", "Low", "Very low"),
                   labels = c("Very high", "High", "Medium", "Low", "Very low")) + 
  scale_y_discrete(name = "") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() +
  labs(title = "Tijuana")
tj.heat.pop


# Mexicali Heatmaps}
mxl.df <- mxl.shp@data %>% group_by(SES, distance_quartile) %>% 
  summarise(total_n=sum(total_n), POB_2010 = sum(POB_2010)) %>% ungroup %>% mutate(percent_scripts=total_n/sum(total_n)*100, percent_population = POB_2010/sum(POB_2010)*100)

mxl.df$SES <- factor(mxl.df$SES, levels = c("Very high", "High", "Medium", "Low", "Very low"))
mxl.df <- mxl.df %>% mutate(percent_scripts_factor = case_when(percent_scripts <20 ~ "<20%",
                                                   percent_scripts >= 20 & percent_scripts <=39 ~ "20-39%",
                                                   percent_scripts >= 40 & percent_scripts <= 59 ~ "40-59%",
                                                   percent_scripts >= 60 & percent_scripts <= 79 ~ "60-79%",
                                                   percent_scripts >=80 ~ "≥80%",
                                                   ))

mxl.df <- mxl.df %>% add_row(SES = "Very low", distance_quartile = "15-30 min", total_n = 0, 
                                        POB_2010 = 0, percent_scripts = 0, percent_population = 0, 
                                        percent_scripts_factor = "<20%") #fill missing box
mxl.df$percent_scripts <- as.numeric(mxl.df$percent_scripts)
mxl.df$percent_population <- as.numeric(mxl.df$percent_population)
mxl.df$SES <- factor(mxl.df$SES, levels = c("Very high", "High", "Medium", "Low", "Very low"))

mxl.heat <- ggplot(mxl.df, aes(x = SES, y = distance_quartile, fill = percent_scripts)) + 
  geom_tile(colour = "white", size = 3) + 
  scale_fill_continuous_sequential(name="Percentage of opioids dispensed", palette = "Heat",
                                   limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_discrete(name = "",
                   breaks = c("Very high", "High", "Medium", "Low", "Very low"),
                   labels = c("Very high", "High", "Medium", "Low", "Very low")) + 
  scale_y_discrete(name = "") + 
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() +
  labs(title = "Mexicali")
mxl.heat

mxl.heat.pop <- ggplot(mxl.df, aes(x = SES, y = distance_quartile, fill = percent_population)) + 
  geom_tile(colour = "white", size = 3) + 
  scale_fill_continuous_sequential(name="Percentage of population", palette = "Sunset",
                                   limits = c(0,30)) +
  scale_x_discrete(name = "",
                   breaks = c("Very high", "High", "Medium", "Low", "Very low"),
                   labels = c("Very high", "High", "Medium", "Low", "Very low")) + 
  scale_y_discrete(name = "") + 
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() +
  labs(title = "Mexicali")
mxl.heat.pop


# Juarez Heatmaps}
juarez.df <- juarez.shp@data %>% group_by(SES, distance_quartile) %>% 
  summarise(total_n=sum(total_n), POB_2010 = sum(POB_2010)) %>% ungroup %>% mutate(percent_scripts=total_n/sum(total_n)*100, percent_population = POB_2010/sum(POB_2010)*100)

juarez.df <- juarez.df %>% mutate(percent_scripts_factor = case_when(percent_scripts <20 ~ "<20%",
                                                   percent_scripts >= 20 & percent_scripts <=39 ~ "20-39%",
                                                   percent_scripts >= 40 & percent_scripts <= 59 ~ "40-59%",
                                                   percent_scripts >= 60 & percent_scripts <= 79 ~ "60-79%",
                                                   percent_scripts >=80 ~ "≥80%",
                                                   ))

juarez.df <- juarez.df %>% add_row(SES = "Very low", distance_quartile = "<15 min", total_n = 0, 
                                        POB_2010 = 0, percent_scripts = 0, percent_population = 0, 
                                        percent_scripts_factor = "<20%") #fill missing box
juarez.df$percent_scripts <- as.numeric(juarez.df$percent_scripts)
juarez.df$percent_population <- as.numeric(juarez.df$percent_population)
juarez.df$SES <- factor(juarez.df$SES, levels = c("Very high", "High", "Medium", "Low", "Very low"))

juarez.heat <- ggplot(juarez.df, aes(x = SES, y = distance_quartile, fill = percent_scripts)) + 
  geom_tile(colour = "white", size = 3) + 
  scale_fill_continuous_sequential(name="Percentage of opioids dispensed", palette = "Heat",
                                   limits = c(0,100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_discrete(name = "", 
                   breaks = c("Very high", "High", "Medium", "Low", "Very low"),
                   labels = c("Very high", "High", "Medium", "Low", "Very low")) + 
  scale_y_discrete(name = "") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() +
  labs(title = "Juarez")
juarez.heat


juarez.heat.pop <- ggplot(juarez.df, aes(x = SES, y = distance_quartile, fill = percent_population)) + 
  geom_tile(colour = "white", size = 3) + 
  scale_fill_continuous_sequential(name="Percentage of population", palette = "Sunset",
                                   limits = c(0,30)) +
  scale_x_discrete(name = "",
                   breaks = c("Very high", "High", "Medium", "Low", "Very low"),
                   labels = c("Very high", "High", "Medium", "Low", "Very low")) + 
  scale_y_discrete(name = "") + 
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() +
  labs(title = "Juarez")
juarez.heat.pop


#}
#Merge border crossing by SES by opioid heat maps
border.heat.cross <- ggpubr::ggarrange(plotlist = list(tj.heat, 
                                                       mxl.heat,
                                                       juarez.heat),
                                      nrow = 1, ncol=3,
                  common.legend = TRUE, legend = "top")
border.heat.cross <- ggpubr::annotate_figure(border.heat.cross,
                bottom = ggpubr::text_grob("Socioeconomic status", 
                                   color = "black", size = 14),
                left = ggpubr::text_grob("Time to border", rot = 90,
                                   color = "black", size = 14))
border.heat.cross
ggsave("Images/heat_border_cross.png", 
       plot = border.heat.cross, dpi = 320, height = 4, width = 12)

#Merge border crossing by SES by population heat maps
border.heat.cross.pop <- ggpubr::ggarrange(plotlist = list(tj.heat.pop, mxl.heat.pop, juarez.heat.pop), 
                                      nrow = 1, ncol=3,
                  common.legend = TRUE, legend = "top")
border.heat.cross.pop <- ggpubr::annotate_figure(border.heat.cross.pop,
                bottom = ggpubr::text_grob("Socioeconomic status", 
                                   color = "black", size = 14),
                left = ggpubr::text_grob("Time to border", rot = 90,
                                   color = "black", size = 14)) 
border.heat.cross.pop 
ggsave("Images/heat_border_cross_pop.png", 
       plot = border.heat.cross.pop, dpi = 320, height = 4, width = 12)


#}
#install.packages("glmmTMB")


#Standardize
cdmx.shp@data$IMU_stand <- scale(cdmx.shp@data$IMU2010r, center = TRUE, scale = TRUE)
gdl.shp@data$IMU_stand <- scale(gdl.shp@data$IMU2010r, center = TRUE, scale = TRUE)
mty.shp@data$IMU_stand <- scale(mty.shp@data$IMU2010r, center = TRUE, scale = TRUE)
tj.shp@data$IMU_stand <- scale(tj.shp@data$IMU2010r, center = TRUE, scale = TRUE)
mxl.shp@data$IMU_stand <- scale(mxl.shp@data$IMU2010r, center = TRUE, scale = TRUE)
juarez.shp@data$IMU_stand <- scale(juarez.shp@data$IMU2010r, center = TRUE, scale = TRUE)

df.border <- bind_rows(tj.shp@data, mxl.shp@data, juarez.shp@data)
df.border$time <- c(na.omit(df.border$time_mins.sy), 
                    na.omit(df.border$time_mins.west), 
                    na.omit(df.border$time_mins))
df.border$IMU_stand <- scale(df.border$IMU2010r, center = TRUE, scale = TRUE)

nb.m.cdmx <- glmmTMB(
  total_n ~ IMU_stand, 
  offset = log(POB_2010),
  family = nbinom1,
  data = cdmx.shp@data
)

tab_model(nb.m.cdmx)

nb.m.mty <- glmmTMB(
  total_n ~ IMU_stand,
  offset = log(POB_2010),
  family = nbinom1,
  data = mty.shp@data
)

tab_model(nb.m.mty)

nb.m.gdl <- glmmTMB(
  total_n ~ IMU_stand,
  offset = log(POB_2010),
  family = nbinom1,
  data = gdl.shp@data
)

tab_model(nb.m.gdl)

nb.m.tj <- glmmTMB(
  total_n ~ IMU_stand + time_mins.sy,
  offset = log(POB_2010),
  family = nbinom1,
  data = tj.shp@data
)

tab_model(nb.m.tj)

nb.m.mxl <- glmmTMB(
  total_n ~ IMU_stand + time_mins.west,
  offset = log(POB_2010),
  family = nbinom1,
  data = mxl.shp@data
)

tab_model(nb.m.mxl)

nb.m.juarez <- glmmTMB(
  total_n ~ IMU_stand + time_mins +
  offset(log(POB_2010)),
  family = nbinom1,
  data = juarez.shp@data
)

tab_model(nb.m.juarez)

nb.m.border <- glmmTMB(
  total_n ~ IMU_stand + time +
  offset(log(POB_2010)),
  family = nbinom1,
  data = df.border
)

tab_model(nb.m.border)


dv.labels = c("Mexico City", "Guadalajara", "Monterrey")
tab_model(nb.m.cdmx, nb.m.gdl, nb.m.mty, 
          dv.labels = dv.labels)

dv.labels = c("Tijuana", "Mexicali", "Juarez", "Combined")
tab_model(nb.m.tj, nb.m.mxl, nb.m.juarez, nb.m.border,
          dv.labels = dv.labels)

