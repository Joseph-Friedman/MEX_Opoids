rm(list = ls())
pacman::p_load(data.table,tidyverse,stringr,stringi,DT,expss,reshape,reshape2,readxl,hablar,skimr,kableExtra,leaflet,scales,viridis,rgdal,colorspace,ggnewscale,MASS,sjPlot,randomcoloR, segmented,boot,sjlabelled,Hmisc,cowplot,ggrepel)
#install.packages("devtools") #run this once
#devtools::install_github("diegovalle/mxmaps") #run this once
library("mxmaps")

#Load dataset
df <- read_excel('anexo-15118-19.xlsx', sheet = 3)
colnames(df)[4] <- "Year"
colnames(df)[5] <- "Month"
colnames(df)[6] <- "n"

#Clean up text
df$Estado <- stri_trans_general(str = df$Estado, id = "Latin-ASCII")
df$Estado <- str_to_upper(df$Estado)
df$Municipio <- stri_trans_general(str = df$Municipio, id = "Latin-ASCII")
df$Municipio <- sub('[^A-Za-z]+', '', df$Municipio)
df$Municipio <- str_to_upper(df$Municipio)

#Change Muncipio names to match INEGI
df <- df %>% mutate(Estado = ifelse(Estado %in% "QUERETARO DE ARTEAGA", "QUERETARO", Estado))
df <- df %>% mutate(Municipio = ifelse(Municipio %in% "LVARO OBREGON", "ALVARO OBREGON", Municipio))
df <- df %>% mutate(Municipio = ifelse(Municipio %in%  
"DOLORES HIDALGO", "DOLORES HIDALGO CUNA DE LA INDEPENDENCIA NACIONAL", Municipio))
df <- df %>% mutate(Municipio = ifelse(Municipio %in% "SILAO", "SILAO DE LA VICTORIA", Municipio))
df <- df %>% mutate(Municipio = ifelse(Municipio %in% "TLAQUEPAQUE", "SAN PEDRO TLAQUEPAQUE", Municipio))
df <- df %>% mutate(Municipio = ifelse(Municipio %in% "NAUCALPAN", "NAUCALPAN DE JUAREZ", Municipio))
df <- df %>% mutate(Municipio = ifelse(Municipio %in% 
"SAN PEDRO MIXTEPEC - DISTR. 22 -", "SAN PEDRO MIXTEPEC", Municipio))
df <- df %>% mutate(Municipio = ifelse(Municipio %in% "LOS MOCHIS", "AHOME", Municipio))
df <- df %>% mutate(Municipio = ifelse(Municipio %in% "ZACAPOAXTLA.", "ZACAPOAXTLA", Municipio))

df <- df %>% mutate(Region8 = ifelse(Estado %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "SONORA", "CHIHUAHUA",
"DURANGO", "SINALOA"), "NOROESTE",
ifelse(Estado %in% c("COAHUILA DE ZARAGOZA", "NUEVO LEON", "TAMAULIPAS"), "NORESTE",
ifelse(Estado %in% c("COLIMA", "JALISCO", "MICHOACAN DE OCAMPO", "NAYARIT"), "OESTE",
ifelse(Estado %in% c("HIDALGO", "PUEBLA", "TLAXCALA", "VERACRUZ DE IGNACIO DE LA LLAVE"), "ESTE",
ifelse(Estado %in% c("AGUASCALIENTES", "GUANAJUATO", "QUERETARO", 
"SAN LUIS POTOSI", "ZACATECAS"), "CENTRONORTE",
ifelse(Estado %in% c("MEXICO", "MORELOS", "CIUDAD DE MEXICO"), "CENTROSUR",
ifelse(Estado %in% c("CHIAPAS", "GUERRERO", "OAXACA"), "SUROESTE",
ifelse(Estado %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", "YUCATAN"), "SURESTE", Estado
)))))))))

#Summarize by state/municipio
df.month <- df %>% group_by(Estado, Municipio, Year, Month) %>% summarise(total_n=sum(n))
df.year <- df %>% group_by(Estado, Municipio, Year) %>% summarise(total_n=sum(n))
df.month.estado <- df %>% group_by(Estado, Year, Month) %>% summarise(total_n=sum(n))
df.year.estado <- df %>% group_by(Estado, Year) %>% summarise(total_n=sum(n))


# States wrangling
#Import population number from Inegi
df.poblacion <- read_excel('poblacion_mexico_estado.xls', sheet=3)
df.poblacion <- df.poblacion %>% filter(`Grupos quinquenales de edad`=="Total", `Estimador`=="Valor")
df.poblacion$`Entidad federativa` <- str_sub(df.poblacion$`Entidad federativa`, 4) #take away beginning numbers
colnames(df.poblacion)[1] <- "Estado"
df.poblacion$Estado <- str_to_upper(df.poblacion$Estado)
df.poblacion$Estado <- stri_trans_general(str = df.poblacion$Estado, id = "Latin-ASCII")
colnames(df.poblacion)[4] <- "Poblacion"
df.poblacion <- df.poblacion %>% dplyr::select(Estado, Poblacion)

#Calculate prescriptions per capita
df.year.estado <- merge(x=df.year.estado, y=df.poblacion, by="Estado", all.x=T) #get state population estimates
df.year.estado$Poblacion <- as.numeric(df.year.estado$Poblacion)
df.year.estado$Rate <- df.year.estado$total_n/df.year.estado$Poblacion*10000
df.year.estado$Rate <- round(df.year.estado$Rate,2)

df.month.estado <- merge(x=df.month.estado, y=df.poblacion, by="Estado") #get state population estimates
df.month.estado$Poblacion <- as.numeric(df.month.estado$Poblacion)
df.month.estado$Rate <- df.month.estado$total_n/df.month.estado$Poblacion*10000
df.month.estado$Rate <- round(df.month.estado$Rate,2)
df.month.estado <- df.month.estado %>% arrange(Estado,Year,Month)
df.month.estado <- df.month.estado %>% group_by(Estado) %>% mutate(Month.id = row_number())

#Calculate percent change
df.month.estado <- df.month.estado %>% 
group_by(Estado) %>% 
arrange(Month.id, .by_group=T) %>% 
mutate(pct_change = (Rate/lag(Rate) - 1) * 100)
df.month.estado$pct_change <- round(df.month.estado$pct_change,1)

df.year.estado <- df.year.estado %>% 
filter(Year!=2015) %>%
group_by(Estado) %>% 
arrange(Year, .by_group=T) %>% 
mutate(pct_change = (Rate/lag(Rate) - 1) * 100)
df.year.estado$pct_change <- round(df.year.estado$pct_change,1)

#Calculate average yearly percent change
df.year.estado %>%
filter(Year!=2015) %>%
group_by(Estado) %>%
mutate(avg.year.change = mean(pct_change, na.rm = TRUE))

#Wrangle to state quarters
df.month.estado <- df.month.estado %>% mutate(Quarter.id = recode(Month, 1:3 ~ 1, 
4:6 ~ 2,
7:9 ~ 3,
10:12 ~ 4))

df.month.estado$Month.id <- as.numeric(df.month.estado$Month.id)
df.month.estado$Quarter.id <- as.numeric(df.month.estado$Quarter.id)
df.month.estado$Quarter.year <- paste0(as.character(df.month.estado$Year),"-", as.character(df.month.estado$Quarter.id))
df.quarter.estado <- df.month.estado %>% group_by(Estado, Quarter.year) %>% summarise(total_n=sum(total_n))
df.quarter.estado <- merge(x=df.quarter.estado, y=df.poblacion, by="Estado", all.x=T)
df.quarter.estado$Rate <- df.quarter.estado$total_n/df.quarter.estado$Poblacion*10000
df.quarter.estado$Rate <- round(df.quarter.estado$Rate,2) 

#Calculate growth
df.quarter.estado <- df.quarter.estado %>% 
  group_by(Estado) %>% 
  arrange(Quarter.year, .by_group=T) %>% 
  mutate(Growth.percent = Rate/first(Rate) * 100)

#Calculate percent change
df.quarter.estado <- df.quarter.estado %>% 
  group_by(Estado) %>% 
  arrange(Quarter.year, .by_group=T) %>% 
  mutate(pct_change = (Rate/lag(Rate) - 1) * 100)
df.quarter.estado$pct_change <- round(df.quarter.estado$pct_change,1)

#Calculate average yearly percent change
df.quarter.estado %>%
  filter(!Quarter.year %in% c("2015-2", "2015-3", "2019-4")) %>%
  group_by(Estado) %>%
  mutate(avg.quarter.change = mean(pct_change, na.rm = TRUE))


#Municipio
df.poblacion.municipio <- read.csv("municipio_poblacion.csv")
df.poblacion.municipio$Municipio <- str_replace(df.poblacion.municipio$Municipio, '\\*', '') #remove asterisk
df.poblacion.municipio$Municipio <- str_to_upper(df.poblacion.municipio$Municipio, locale = "en")
df.poblacion.municipio$Estado <- str_to_upper(df.poblacion.municipio$Estado, locale = "en")
df.poblacion.municipio$Estado <- stri_trans_general(str = df.poblacion.municipio$Estado, id = "Latin-ASCII")
df.poblacion.municipio$Estado <- sub('[^A-Za-z]+', '', df.poblacion.municipio$Estado) #remove numbers/punctuation/space

#Calculate rates
df.year$Estado <- stri_trans_general(str = df.year$Estado, id = "Latin-ASCII")
df.year$Municipio <- stri_trans_general(str = df.year$Municipio, id = "Latin-ASCII")
df.year$Municipio <- sub('[^A-Za-z]+', '', df.year$Municipio)

df.year.municipio <- merge(x=df.year, y=df.poblacion.municipio[,c(2,3,6)], by=c("Estado", "Municipio"), all.x=T) #get municipio population estimates
df.year.municipio$Poblacion <- as.numeric(df.year.municipio$Poblacion)
df.year.municipio$Rate <- df.year.municipio$total_n/df.year.municipio$Poblacion*10000
df.year.municipio$Rate <- round(df.year.municipio$Rate,2)
df.year.municipio <- df.year.municipio %>% arrange(Estado,Municipio,Year)

df.month.municipio <- merge(x=df.month, 
                         y=df.poblacion.municipio[,c(2,3,6)], by=c("Estado", "Municipio")) #get municipio population estimates
df.month.municipio$Poblacion <- as.numeric(df.month.municipio$Poblacion)
df.month.municipio$Rate <- df.month.municipio$total_n/df.month.municipio$Poblacion*10000
df.month.municipio$Rate <- round(df.month.municipio$Rate,2)
df.month.municipio <- df.month.municipio %>% arrange(Estado,Year,Month)
df.month.municipio <- df.month.municipio %>% group_by(Estado) %>% mutate(Month.id = row_number())

#Wrangle to municipio quarters
df.month.municipio <- df.month.municipio %>% mutate(Quarter.id = recode(Month, 1:3 ~ 1, 
                                                                  4:6 ~ 2,
                                                                  7:9 ~ 3,
                                                                  10:12 ~ 4))
df.month.municipio$Month.id <- as.numeric(df.month.municipio$Month.id)
df.month.municipio$Quarter.id <- as.numeric(df.month.municipio$Quarter.id)
df.month.municipio$Quarter.year <- paste0(as.character(df.month.municipio$Year),"-", as.character(df.month.municipio$Quarter.id))
df.quarter.municipio <- df.month.municipio %>% group_by(Municipio, Estado, Quarter.year, Poblacion) %>% summarise(total_n=sum(total_n))
df.quarter.municipio$Rate <- df.quarter.municipio$total_n/df.quarter.municipio$Poblacion*10000
df.quarter.municipio$Rate <- round(df.quarter.municipio$Rate,2) 

#Calculate average yearly percent change
df.quarter.municipio.filter <- df.quarter.municipio %>%
  filter(!Quarter.year %in% c("2015-2", "2015-3", "2019-4"))
df.quarter.municipio.filter <- df.quarter.municipio %>% filter(Poblacion >= 50000)


#Prescribin Rate Tables
#Calculate total prescribing rate over time period - State
df.month.estado.total <- df.month.estado %>% group_by(Estado, Poblacion) %>% 
  filter(!Month %in% c("10") & !Year %in% c("2019")) %>% summarise(sum(total_n))
colnames(df.month.estado.total)[3] <- "total_n"
df.month.estado.total <- df.month.estado.total %>% mutate(Rate = total_n/Poblacion * 10000)

#Make title case for table
df.month.estado.total <- df.month.estado.total %>% 
  mutate(Estado1 = str_to_title(Estado)) %>% ungroup

total_n_mexico <- df.month.estado.total %>% summarise(sum(total_n))
total_n_mexico/119530753 * 10000 #Rate for entire country over study period

#State table
kable(df.month.estado.total %>% arrange(desc(Rate)) %>% dplyr::select(Estado1, Rate),
      format = "html", # default
      digits = 1,        # specify decimal places
      caption = "Table. Rates of group I medication dispensing by state in Mexico between 8/2015 and 10/2019",
      col.names = c("State", "Rate per 10,000"),
      align = c("l", "c")) %>% 
  row_spec(1:nrow(df.month.estado.total), color = "black") %>%
  kable_styling(bootstrap_options = c("striped", "hover",  "condensed", "responsive")) %>%
  footnote(number = c("Rates were calculated based on data provided by COFEPRIS for the time period between 8/2015 and 10/2019. The population denominator was based on 2015 INEGI Intercensal estimates."))


#Calculate total prescribin rate - Municipio
df.month.municipio.total <- df.month.municipio %>% group_by(Municipio, Estado, Poblacion) %>% 
  filter(!Month %in% c("10") & !Year %in% c("2019")) %>% summarise(sum(total_n))
colnames(df.month.municipio.total)[4] <- "total_n"
df.month.municipio.total <- df.month.municipio.total %>% mutate(Rate = total_n/Poblacion * 10000)

#Calculate number of excluded municipalities
df.month.municipio.total %>% filter(Poblacion >= 50000) %>% summarise(n())
df.month.municipio.total %>% summarise(n())

#Make title case for table
df.month.municipio.total <- df.month.municipio.total %>% 
  mutate(Municipio1 = str_to_title(Municipio), 
         Estado1 = str_to_title(Estado)) %>% ungroup

#Municipal table
kable(df.month.municipio.total %>% arrange(desc(Rate)) %>% 
        filter(Poblacion >= 50000) %>% dplyr::select(Municipio1, Estado1, Rate),
      format = "html", # default
      digits = 1,        # specify decimal places
      caption = "Table. Municipal rates of Group I medication dispensing in Mexico between 8/2015 and 10/2019",
      col.names = c("Municipality", "State", "Rate per 10,000"),
      align = c("l", "l", "c")) %>% 
  row_spec(1:nrow(df.month.municipio.total %>% filter(Poblacion >= 50000)), color = "black") %>%
  kable_styling(bootstrap_options = c("striped", "hover",  "condensed", "responsive")) %>%
  footnote(number = c("Rates were calculated based on data provided by COFEPRIS for the time period between 8/2015 and 10/2019. The population denominator was based on 2015 INEGI Intercensal estimates."))



#Plot municipalities
df.year.municipio.filter <- df.year.municipio %>% filter(Poblacion >= 50000)
df.year.municipio.filter <- df.year.municipio.filter %>% filter(Year!=c(2015))
df.year.municipio.filter <- df.year.municipio.filter %>% filter(Year!=c(2019))
df.year.municipio.filter <- df.year.municipio.filter %>% arrange(desc(Rate)) %>% group_by(Municipio)

g <- ggplot(df.year.municipio.filter, aes(x=reorder(Municipio, Rate), y=Rate, fill=as.factor(Year))) + 
  geom_dotplot(binaxis='y', stackdir='center', 
               stackratio=1, dotsize=0.8) +
  coord_flip() +
  scale_fill_brewer(palette="Dark2")+
  theme_minimal() +
  labs(fill = "Año", x="Municipio", y="Tasa del numero de recetas surtidas por 10,000 habitantes")

#get range to subset only top 20
g.range <- ggplot_build(g)$layout$panel_scales_x[[1]]$range$range
g.range <- as.data.frame(g.range)
g.range <- g.range %>% mutate(id = row_number())
colnames(g.range)[1] <- "Municipio" 
df.year.municipio.filter <- merge(x=df.year.municipio.filter, y=g.range, by="Municipio")
df.year.municipio.filter <- df.year.municipio.filter %>% filter(id %in% (91:110))
df.year.municipio.filter$label <- paste0(as.character(df.year.municipio.filter$Municipio),", ", as.character(df.year.municipio.filter$Estado))
df.year.municipio.filter$label <- str_to_title(df.year.municipio.filter$label)

ggplot(df.year.municipio.filter, aes(x=reorder(label, Rate), y=Rate, fill=as.factor(Year))) + 
  geom_dotplot(binaxis='y', stackdir='center', 
               stackratio=1, dotsize=0.8) +
  coord_flip() +
  scale_fill_brewer(palette="Dark2")+
  theme_minimal() +
  labs(fill = "Year", x="Municipality", y="Dispensed group I medications per 10,000 inhabitants",
       title = "Municipalities with top Group I prescribing rates\nin Mexico between 2016 and 2018")


#Plots State
#Total N plot - year
ggplot(df.year.estado, aes(x=reorder(Estado, total_n), y=total_n, fill=as.factor(Year))) + 
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1, dotsize=0.8) +
  coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  labs(fill = "Año", x="Entidad Federativa", y="Numero total de recetas")

df.year.estado$Estado1 <- str_to_title(df.year.estado$Estado)

#Rate plot - year
df.year.estado.filter <- df.year.estado %>% filter(Year != 2015)
df.year.estado.filter <- df.year.estado.filter %>% filter(Year != 2019)
ggplot(df.year.estado.filter, aes(x=reorder(Estado1, Rate), y=Rate, fill=as.factor(Year))) + 
  geom_dotplot(binaxis='y', stackdir='center', 
               stackratio=1, dotsize=0.8) +
  coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme_minimal() +
  labs(fill = "Year", x="State", y="Dispensed narcotic medications per 10,000 inhabitants",
       title = "Narcotic prescribing rates by state\nin Mexico between 2016 and 2018")
  
#Rate plot - quarters

n <- 32
palette <- distinctColorPalette(n)

df.quarter.estado.filter <- df.quarter.estado %>% filter(!Quarter.year %in% c('2019-4', '2015-2', '2015-3'))
ggplot(df.quarter.estado.filter, aes(x=Quarter.year, y=Rate, group=as.factor(Estado), colour=Estado)) +
  geom_line() + geom_point() +
  theme_minimal() +
  scale_color_manual(values = palette) +
  labs(fill = "State", y="Tasa de Recetas Surtidas por 10,000", x="Año-Trimestre") +
  theme(axis.text.x=element_text(angle=-45, hjust=0.01))



#Wrangle for Maps
#https://github.com/diegovalle/mxmaps
#https://www.diegovalle.net/projects.html
#if (!require(devtools)) {
#    install.packages("devtools")
#}
#devtools::install_github('diegovalle/mxmaps')

data("df_mxstate")
df_mxstate <- as.data.frame(df_mxstate)
df_mxmunicipio <- as.data.frame(df_mxmunicipio)
colnames(df_mxstate)[3] <- "Estado"
#df.year.estado$Estado <- str_to_title(df.year.estado$Estado)
df_mxstate$Estado <- str_to_upper(df_mxstate$Estado)
df_mxstate$Estado <- stri_trans_general(str = df_mxstate$Estado, id = "Latin-ASCII")
df.year.estado <- merge(x=df.year.estado, y=df_mxstate[,c(1,3)], by="Estado", all.x=T)
df.year.estado$value <- df.year.estado$Rate
df.year.estado <- df.year.estado %>% mutate(region = ifelse(Estado %in% "CIUDAD DE MEXICO", "09", region))


# Plot Maps
#Plot State Map
YEAR <- 2018
df.year.estado.filter <- df.year.estado %>% filter(Year==`YEAR`)
mxstate_choropleth(df.year.estado.filter, num_colors = 5, 
                   title = paste("Group I dispensing rate per 10,000 inhabitant,", YEAR),
                   legend = "Rate")

mxhexbin_choropleth(df.year.estado.filter, num_colors = 5,
                    title = paste("Group I dispensing rate per 10,000 inhabitant,", YEAR),
                    legend = "Rate")

mxhexbin_choropleth(df.year.estado.filter, num_colors = 5,
                    title = paste("Group I dispensing rate per 10,000 inhabitant,", YEAR),
                    legend = "Rate")

gf = MXHexBinChoropleth$new(df.year.estado.filter)
gf$title <- paste("Group I dispensing rate per 10,000 inhabitant,", YEAR)
gf$set_num_colors(5)
#gf$ggplot_scale <- scale_fill_brewer(palette = "RdYlGn", direction = -1, na.value = "white") #mexican flag
gf$ggplot_scale <- gf$ggplot_scale <-scale_fill_discrete_divergingx(palette = "Fall")
f <- gf$render()
f + theme_void()

gg = MXStateChoropleth$new(df.year.estado.filter)
gg$title <- paste("Group I dispensing rate per 10,000 inhabitant,", YEAR)
gg$set_num_colors(5)
gg$ggplot_scale <- scale_fill_brewer(palette = "RdYlGn", direction = -1, na.value = "white")
p <- gg$render()
p + theme_void()

#Plot Municipality Map
colnames(df_mxmunicipio)[5] <- "Estado"
colnames(df_mxmunicipio)[8] <- "Municipio"
df_mxmunicipio$Estado <- str_to_upper(df_mxmunicipio$Estado)
df_mxmunicipio$Estado <- stri_trans_general(str = df_mxmunicipio$Estado, id = "Latin-ASCII")
df_mxmunicipio$Municipio <- str_to_upper(df_mxmunicipio$Municipio)
df_mxmunicipio$Municipio <- stri_trans_general(str = df_mxmunicipio$Municipio, id = "Latin-ASCII")
df_mxmunicipio <- df_mxmunicipio %>% 
  mutate(Estado = ifelse(Estado %in% "DISTRITO FEDERAL", "CIUDAD DE MEXICO", Estado))
df.year.municipio <- merge(x=df.year.municipio, y=df_mxmunicipio[,c(1:3,5,8)], by=c("Estado","Municipio"), all.x=T)
df.year.municipio$value <- df.year.municipio$Rate

df.year.municipio.filter <- df.year.municipio %>% filter(Year==YEAR) %>% drop_na()
p <- mxmunicipio_choropleth(df.year.municipio.filter, num_colors = 5,
                       title = paste("Group I dispensing rate per 10,000 inhabitant,", YEAR))
  
gg = MXMunicipioChoropleth$new(df.year.municipio.filter)
gg$title <- paste("Group I dispensing rate per 10,000 inhabitant,", YEAR)
gg$set_num_colors(9)
gg$ggplot_scale <- scale_fill_brewer(palette = "RdYlGn", direction = -1, na.value = "white")
p <- gg$render()
p + theme_void()



#Maps by total over study period - State
#State map
df.month.estado.total$value <- df.month.estado.total$Rate
df.month.estado.total <- merge(x = df.month.estado.total, y = df_mxstate[,c(1,3)], 
                               by = "Estado", all.x = T)
df.month.estado.total <- df.month.estado.total %>% 
  mutate(region = case_when(Estado == "CIUDAD DE MEXICO" ~ "09",
                            TRUE ~ region)) 

df.month.estado.total <- df.month.estado.total %>% 
  mutate(value = case_when(Rate < 5 ~ "Less than 5",
                           Rate >= 5 & Rate <10 ~ "5 - 9.9",
                           Rate >= 10 & Rate <20 ~ "10 - 19.9",
                           Rate >= 20 & Rate <30 ~ "20 - 29.9",
                           Rate >= 30 ~ "More than 30"))
df.month.estado.total$value <- factor(df.month.estado.total$value,
                                      levels = c("Less than 5", "5 - 9.9", "10 - 19.9", 
                                                 "20 - 29.9", "More than 30"))
                         
gg = MXStateChoropleth$new(df.month.estado.total)
#gg$title <- paste("Group I opioid dispensing rate per 10,000 inhabitants")
gg$set_num_colors(5)
gg$ggplot_scale <-scale_fill_discrete_sequential(palette = "Heat", name = "Rate per 10,000")
p <- gg$render()
p + theme_void()

ggsave("Images/state_rate_national.png",
       dpi = 320, height = 8, width = 10)


#Maps by total over study period - Municipal
#Municipal map
df.month.municipio.total <- merge(x=df.month.municipio.total, y=df_mxmunicipio[,c(1:3,5,8)],
                                  by=c("Estado","Municipio"), all.x=T)
df.month.municipio.total$value <- df.month.municipio.total$Rate
df.month.municipio.total$region <- paste0(df.month.municipio.total$state_code,
                                          df.month.municipio.total$municipio_code)
#quantile(df.month.municipio.total$Rate, c(.20, .40, .60, .80)) 
df.month.municipio.total <- df.month.municipio.total %>% 
  mutate(value = case_when(Rate < 5 ~ "Less than 5",
                           Rate >= 5 & Rate <10 ~ "5 - 9.9",
                           Rate >= 10 & Rate <20 ~ "10 - 19.9",
                           Rate >= 20 & Rate <30 ~ "20 - 29.9",
                           Rate >= 30 ~ "More than 30"))
df.month.municipio.total$value <- factor(df.month.municipio.total$value,
                                      levels = c("Less than 5", "5 - 9.9", "10 - 19.9", 
                                                 "20 - 29.9", "More than 30"))

#df.month.municipio.total <- df.month.municipio.total %>% group_by(region) %>% filter(row_number() == 1)

#Create regions to seperate map
df.month.municipio.total <- 
  df.month.municipio.total %>% mutate(Region8 = ifelse(Estado %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
                                                                   "SONORA", "CHIHUAHUA",
                                                                   "DURANGO", "SINALOA"), "NOROESTE",
                               ifelse(Estado %in% c("COAHUILA DE ZARAGOZA", "NUEVO LEON", 
                                                    "TAMAULIPAS"), "NORESTE",
                               ifelse(Estado %in% c("COLIMA", "JALISCO", "MICHOACAN DE OCAMPO", 
                                                    "NAYARIT"), "OESTE",
                               ifelse(Estado %in% c("HIDALGO", "PUEBLA", "TLAXCALA"
                                                    ), "ESTE",
                               ifelse(Estado %in% c("AGUASCALIENTES", "GUANAJUATO", "QUERETARO", 
                                                    "SAN LUIS POTOSI", "ZACATECAS"), "CENTRONORTE",
                               ifelse(Estado %in% c("MEXICO", "MORELOS", "CIUDAD DE MEXICO"), "CENTROSUR",
                               ifelse(Estado %in% c("CHIAPAS", "GUERRERO", "OAXACA"), "SUROESTE",
                               ifelse(Estado %in% c("CAMPECHE", "QUINTANA ROO", "TABASCO", 
                                                    "YUCATAN","VERACRUZ DE IGNACIO DE LA LLAVE"),
                                      "SURESTE", Estado
                                      )))))))))

norte.map <- mxmunicipio_choropleth(df.month.municipio.total, num_colors = 9,
                       zoom = subset(df.month.municipio.total, Region8 %in% c("NOROESTE", "NORESTE"))$region,
                       title = "",
                       show_states = T,
                       legend = "Rate") + 
  scale_fill_discrete_sequential(palette = "Heat", name = "Rate per 10,000")

centro.map <- mxmunicipio_choropleth(df.month.municipio.total, num_colors = 9,
                       zoom = subset(df.month.municipio.total, Region8 %in% c("CENTRONORTE",
                                                                              "ESTE",
                                                                              "OESTE",
                                                                              "CENTROSUR"))$region,
                       title = "",
                       show_states = T,
                       legend = "Rate") + 
  scale_fill_discrete_sequential(palette = "Heat", name = "Rate per 10,000")

sur.map <- mxmunicipio_choropleth(df.month.municipio.total, num_colors = 9,
                       zoom = subset(df.month.municipio.total, Region8 %in% c("SUROESTE",
                                                                              "SURESTE"))$region,
                       title = "",
                       show_states = T,
                       legend = "Rate") + 
  scale_fill_discrete_sequential(palette = "Heat", name = "Rate per 10,000")

norte.map
centro.map
sur.map

ggsave(plot = norte.map, 
       "Images/state_rate_norte.png",
       dpi = 320, height = 8, width = 10)
ggsave(plot = centro.map, 
       "Images/state_rate_centro.png",
       dpi = 320, height = 8, width = 10)
ggsave(plot = sur.map, 
       "Images/state_rate_sur.png",
       dpi = 320, height = 8, width = 10)


municipal.all.map <- ggpubr::ggarrange(norte.map, centro.map, sur.map, 
                                       labels = c("A", "B", "C"),
                                       ncol = 1, 
                  common.legend = T, legend = "right")
ggsave(plot = municipal.all.map, 
       "Images/state_rate_all.png",
       dpi = 320, height = 8, width = 8)


# Joinpoint analysis
#https://rpubs.com/MarkusLoew/12164
df.quarter.estado <- transform(df.quarter.estado, Estado.id=as.numeric(factor(Estado)))
df.quarter.estado <- transform(df.quarter.estado, Quarter.id=as.numeric(factor(Quarter.year)))
df.quarter.national <- df.quarter.estado %>% group_by(Quarter.year) %>% 
  dplyr::summarize(Recetas=sum(total_n)) %>% ungroup()
df.quarter.national <- df.quarter.national %>% filter(!Quarter.year %in% c("2015-2","2015-3","2019-4"))
df.quarter.national <- df.quarter.national %>% mutate(Quarter.id=row_number())
df.quarter.national$Poblacion <- 119530753
df.quarter.national <- df.quarter.national %>% mutate(Rate = round(Recetas/Poblacion * 10000,2))

#write.csv(df.quarter.national, 'Joinpoint/joinpoint_national.csv' )
#write.csv(df.quarter.estado, 'Joinpoint/joinpoint_state.csv' )

#Plot crude
p <- ggplot(df.quarter.national, aes(x = Quarter.year, y = Recetas)) + geom_line(group = 1, color="blue") +
  theme_minimal() +
  labs(y="Number of group I prescriptions dispensed", x="Year-Quarter") +
  theme(axis.text.x=element_text(angle=-45, hjust=0.01))
p

#Linear regression
my.lm <- lm(Recetas ~ Quarter.id, data = df.quarter.national)
summary(my.lm)

#Automatic breakpoints estimate
o<-segmented.lm(my.lm,seg.Z=~Quarter.id,psi=list(x=NA,z=.3), 
    control=seg.control(fix.npsi=FALSE, n.boot=0, tol=1e-7, it.max = 50, K=5, display=TRUE))

#Breakpoint analysis
my.seg <- segmented(my.lm, seg.Z = ~ Quarter.id, npsi= 1)
summary(my.seg)
my.seg$psi #Get breakpoints
slope(my.seg)
aapc(my.seg, parm = 'Quarter.id', wrong.se = "F")
davies.test(my.seg, k=2)
confint(my.seg, method = "score")
slope(my.seg, APC = TRUE)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(Quarter = df.quarter.national$Quarter.id, Recetas = my.fitted)

# plot the fitted model
ggplot(my.model, aes(x = Quarter, y = Recetas)) + geom_line()
p + geom_line(data = my.model, aes(x = Quarter, y = Recetas), colour = "tomato")

# add vertical lines to indicate the break locations
# second row of the psi-matrix
my.lines <- my.seg$psi[, 2]

df.quarter.national <- df.quarter.national %>% mutate(Period = case_when(Quarter.id <= my.lines[1] ~ 1,
                               #Quarter.id > my.lines[1] & Quarter.id < my.lines[2] ~ 2,
                               Quarter.id > my.lines[1] ~ 3))


df.quarter.national <- df.quarter.national %>% 
  group_by(Period) %>% 
  arrange(Quarter.year, .by_group=T) %>% 
  mutate(pct_change = (Recetas/lag(Recetas) - 1) * 100)
df.quarter.national$pct_change <- round(df.quarter.national$pct_change,1)

df.quarter.national.aqc <- df.quarter.national %>% group_by(Period) %>%
  summarise(avg.quarter.change = mean(pct_change, na.rm = TRUE))
df.quarter.national.aqc$avg.quarter.change <- round(df.quarter.national.aqc$avg.quarter.change,1)
df.quarter.national.aqc$x <- c("2016-1", "2018-1")
df.quarter.national.aqc$y <- c(500, 500)

# Joinpoint for rate
#Plot crude
p <- ggplot(df.quarter.national, aes(x = Quarter.year, y = Rate)) + geom_line(group = 1, color="blue") +
  theme_minimal() +
  labs(y="Number of group I prescriptions dispensed", x="Year-Quarter") +
  theme(axis.text.x=element_text(angle=-45, hjust=0.01))
p

#Linear regression
my.lm <- lm(Rate ~ Quarter.id, data = df.quarter.national)
summary(my.lm)

#Automatic breakpoints estimate
o<-segmented.lm(my.lm,seg.Z=~Quarter.id,psi=list(x=NA,z=.3), 
    control=seg.control(fix.npsi=FALSE, n.boot=0, tol=1e-7, it.max = 50, K=5, display=TRUE))

#Breakpoint analysis
my.seg <- segmented(my.lm, seg.Z = ~ Quarter.id, npsi= 1)
summary(my.seg)
my.seg$psi #Get breakpoints

davies.test(my.seg, k=2)
confint(my.seg)
slope(my.seg)
slope(my.seg, APC=T)
aapc(my.seg, parm = 'Quarter.id', exp.it = T, wrong.se = "F")

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(Quarter = df.quarter.national$Quarter.id, Rate = my.fitted)

# plot the fitted model
ggplot(my.model, aes(x = Quarter, y = Rate)) + geom_line()
p + geom_line(data = my.model, aes(x = Quarter, y = Rate), colour = "tomato")

# add vertical lines to indicate the break locations
# second row of the psi-matrix
my.lines <- my.seg$psi[, 2]

df.quarter.national <- df.quarter.national %>% mutate(Period2 = case_when(Quarter.id <= my.lines[1] ~ 1,
                               #Quarter.id > my.lines[1] & Quarter.id < my.lines[2] ~ 2,
                               Quarter.id > my.lines[1] ~ 2))


#Calculate percent change
df.quarter.national <- df.quarter.national %>% 
  group_by(Period2) %>% 
  arrange(Quarter.year, .by_group=T) %>% 
  mutate(pct_change = (Rate/lag(Rate) - 1) * 100)
df.quarter.national$pct_change <- round(df.quarter.national$pct_change,1)

df.quarter.national.aqc <- df.quarter.national %>% group_by(Period) %>%
  summarise(avg.quarter.change = mean(pct_change, na.rm = TRUE))
df.quarter.national.aqc$avg.quarter.change <- round(df.quarter.national.aqc$avg.quarter.change,1)
df.quarter.national.aqc$x <- c("2016-2", "2018-2")
df.quarter.national.aqc$y2 <- c(4, 4)

df.quarter.national.aqc$avg.quarter.change <- c("103.5%\n(95% CI 27.8-224.2)", "3.2%\n(95% CI 1.6-4.8)")

#plot
set.seed(1)
p.jp.rate <- ggplot(df.quarter.national, aes(x = Quarter.year, y = Rate, color="b")) + 
  geom_line(group = 1) +
  theme_minimal() +
  labs(y="Rate of opioid prescriptions dispensed\nper 10,000", x="Year-Quarter") +
  theme(axis.text.x=element_text(angle=-45, hjust=0.01)) + 
  geom_vline(xintercept = my.lines, linetype = "dashed") + 
  geom_line(data = my.model, aes(x = Quarter, y = Rate, colour = "r")) +
  scale_color_manual(name = "", values = c("b" = "blue", "r" = "red"), labels = c("Observed", "Fitted")) +
  geom_label(data = df.quarter.national.aqc, 
                   mapping = aes(x=x, y=y2, label = paste0("QPC ", avg.quarter.change, "%", sep=" "),
                    fill = factor(Period)), color = 'white',
                    size = 3.5) +
  scale_fill_manual(guide=FALSE, name = "Period", values = c("black","black","black")) +
  labs(caption = "Opioid prescribing rate is defined as the total number of group I opioid prescriptions dispensed in the quarter per 10,000 inhabitants.") +
   theme(legend.position = "bottom") + 
  ylim(0,5)
p.jp.rate
ggsave("Images/joinpoint_national.png",
       plot = p.jp.rate, dpi = 320)


#Import SER Joinpoint program output
#Clean up estado clave (INEGI mistakes)
estado.clave <- read.csv('tc_esep_entidad.csv')

#Work on National
jp.n <- read_excel("COFEPRIS_national_output.xlsx",
           sheet = 4)
jp.n <- jp.n[,c(5,6,7,10)]
colnames(jp.n)[1] <- "AQPC"
colnames(jp.n)[2] <- "Lower"
colnames(jp.n)[3] <- "Upper"
colnames(jp.n)[4] <- "p value"
jp.n$`95.CI` <- paste0("(", jp.n$Lower, " - ", jp.n$Upper, ")")
jp.n$Estado1 <- "National"
jp.n$Rate <- round(39312/119530753 * 10000, 1)

#Work on State
jp.s.1 <- read_excel("COFEPRIS_state_output_1.xlsx", sheet = 4)
jp.s.2 <- read_excel("COFEPRIS_state_output_2.xlsx", sheet = 4)
jp.s <- rbind(jp.s.1, jp.s.2)
jp.s <- jp.s[,c(1,6,7,8,11)]
colnames(jp.s)[1] <- "ID_ENTIDAD"
colnames(jp.s)[2] <- "AQPC"
colnames(jp.s)[3] <- "Lower"
colnames(jp.s)[4] <- "Upper"
colnames(jp.s)[5] <- "p value"
jp.s$`95.CI` <- paste0("(", jp.s$Lower, " - ", jp.s$Upper, ")")
jp.s <- merge(x=jp.s, y=estado.clave, by="ID_ENTIDAD", all.x=T)
jp.s$Estado1 <- jp.s$Estado
jp.s$Estado <- toupper(jp.s$Estado)
jp.rate <- df.quarter.estado.filter %>% group_by(Estado) %>% filter(row_number()==n()) %>% dplyr::select(Estado, Rate)
jp.s <- merge(x=jp.s, y=jp.rate, by="Estado")

#Merge national to state
jp.table <- bind_rows(jp.n[1,], jp.s)
jp.table <- jp.table %>% dplyr::select(Estado1, Rate, AQPC, `95.CI`)

#Make table
kable(jp.table,
      format = "html", # default
      digits = 1,        # specify decimal places
      caption = "Table. National and state trends in Group I medication dispensing in Mexico, 2015-2019",
      col.names = c("", "Rate per 10,000", "AQPC", "95% CI"),
      align = c("c")) %>% 
  row_spec(1:31, color = "black") %>%
  kable_styling(bootstrap_options = c("striped", "hover",  "condensed", "responsive")) %>%
  footnote(general = "Abbreviations: AQPC = average quarterly percent change; CI = confidence interval.",
number = c("The Group I dispensing rate reported in the table is that of the last quarter with complete data (2019-3).",
           "Rates were calculated based on data provided by COFEPRIS for the time period between 8/2015 and 10/2019. The population denominator was based on 2015 INEGI Intercensal estimates.",
           "AQPC was calculated using NCI Joinpoint Regression Program 4.7.0.0"))



#Import Global Burden of diseases data
GBD <- read.csv("cod_cofepris.csv")

#Clean state names for later merging
colnames(GBD)[2] <- "Estado"
GBD$Estado <- stri_trans_general(str = GBD$Estado, id = "Latin-ASCII")
GBD <- GBD %>% mutate(Estado = ifelse(Estado %in% "Mexico City", "Ciudad de Mexico", Estado))
GBD <- GBD %>% mutate(Estado = ifelse(Estado %in% "Coahuila", "Coahuila de Zaragoza", Estado))


#Filter prevalence/death into rate/number dfs
GBD.death.rate <- GBD %>% filter(measure_name == "Deaths" & metric_name == "Rate")
GBD.prevalence.rate <- GBD %>% filter(measure_name == "Prevalence" & metric_name == "Rate")
GBD.death.number <- GBD %>% filter(measure_name == "Deaths" & metric_name == "Number")
GBD.prevalence.number <- GBD %>% filter(measure_name == "Prevalence" & metric_name == "Number")

#Summarise by state
GBD.prevalence.number.s <- GBD.prevalence.number %>% group_by(Estado) %>% 
  summarise(prevalence20.number = sum(val, na.rm = T))
GBD.death.number.s <- GBD.death.number %>% group_by(Estado) %>% summarise(death20.number = sum(val))
                 
#Import weights and calculate palliative care need                                                 
GBD.weights <- read.csv("lancet_methods_table.csv")
colnames(GBD.weights)[4] <- "cause_name" 
names(GBD.weights)
GBD.weights$Weight_palliative_care_need <-
  round(as.numeric(as.character(GBD.weights$Weight_palliative_care_need)),3)
GBD.weights$Prop_need_opioid <-
  round(as.numeric(as.character(GBD.weights$Prop_need_opioid)),3)
GBD.weights$Days_opioid <-
  round(as.numeric(as.character(GBD.weights$Days_opioid)),3)
GBD.weights$Morphine <-
  round(as.numeric(as.character(GBD.weights$Morphine)),3)

GBD.weights.prevalence <- GBD.weights %>% filter(Death.Prevalence == "Prevalence")
GBD.weights.death <- GBD.weights %>% filter(Death.Prevalence == "Death")
GBD.death.number.w <- GBD.death.number %>% left_join(GBD.weights.death, by = "cause_name")
GBD.death.number.w <- GBD.death.number.w %>% 
  mutate(Opioid.need = val * Weight_palliative_care_need * Prop_need_opioid * 
                              Days_opioid * Morphine)
GBD.prevalence.number.w <- GBD.prevalence.number %>% left_join(GBD.weights.prevalence, by = "cause_name")
GBD.prevalence.number.w <- GBD.prevalence.number.w %>% 
  mutate(Opioid.need = val * Weight_palliative_care_need * Prop_need_opioid * 
                              Days_opioid * Morphine)

GBD.opioid.score <- rbind(GBD.prevalence.number.w, GBD.death.number.w)
GBD.opioid.sum <- GBD.opioid.score %>% spread(cause_name, Opioid.need)
GBD.opioid.sum <- GBD.opioid.sum %>% dplyr::select(c(2,22:48)) %>% 
  group_by(Estado) %>% summarise_all(sum, na.rm = T)
GBD.opioid.sum <- GBD.opioid.sum %>% dplyr::select(!c(`Acute glomerulonephritis`, `African trypanosomiasis`,
                                               Asthma, `Idiopathic epilepsy`, `Neonatal preterm birth`,
                                               Schistosomiasis))
df.poblacion <- df.poblacion %>% filter(!Estado == "ADOS UNIDOS MEXICANOS") %>% 
  mutate(Estado = str_to_title(Estado))
GBD.opioid.sum <- GBD.opioid.sum %>% mutate(Estado = str_to_title(Estado))

GBD.opioid.sum.stand <- GBD.opioid.sum
GBD.opioid.sum.stand[2:22] <- lapply(GBD.opioid.sum.stand[2:22], function(x) {
  y<-scale(x, center=TRUE, scale=TRUE)
  }
)

#Create percapita
GBD.opioid.sum <- merge(x = GBD.opioid.sum, y = df.poblacion, by = "Estado")
GBD.opioid.sum.capita <- GBD.opioid.sum %>% dplyr::mutate_if(is.numeric, ~ ./Poblacion)
GBD.opioid.sum.capita.stand <- GBD.opioid.sum.capita %>% mutate_if(is.numeric, scale)

#Create overall score
GBD.opioid.score <- GBD.opioid.score %>% group_by(Estado) %>% 
  summarise(Opioid.need = sum(Opioid.need, na.rm = TRUE))


#Heatmap by overall need
names(GBD.opioid.sum.stand) <- c("Estado", "Dementias", "Cardiovascular", 
                                        "Kidney",
                                 "Respiratory", "Liver",
                                 "Congenital", "Encephalitis", "HIV/AIDS", "Injuries",
                                 "Measles", "Multiple sclerosis", "Musculoskeletal", 
                                 "Neoplasms", "Digestive", "Neurological",
                                 "Parkinson's", "Malnutrition", "Rabies",
                                 "Syphilis", "Tetanus", "Tuberculosis")

GBD.opioid.sum.stand_long <- GBD.opioid.sum.stand %>% gather(Disease, Value, 2:22, factor_key=TRUE)

GBD.opioid.heat <- ggplot(GBD.opioid.sum.stand_long, aes(x = Disease, y = Estado, fill = Value)) + 
  geom_tile(colour = "white", size = 1) + 
  scale_fill_continuous_sequential(name="Standardized opioid need", palette = "Heat") +
  scale_x_discrete(name = "Disease") + 
  scale_y_discrete(name = "State") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() +
  labs(title = "Standardized total opioid need")
GBD.opioid.heat

# Heatmap by standardized need per capita
names(GBD.opioid.sum.capita.stand) <- c("Estado", "Dementias", "Cardiovascular", 
                                        "Kidney",
                                 "Respiratory", "Liver",
                                 "Congenital", "Encephalitis", "HIV/AIDS", "Injuries",
                                 "Measles", "Multiple sclerosis", "Musculoskeletal", 
                                 "Neoplasms", "Digestive", "Neurological",
                                 "Parkinson's", "Malnutrition", "Rabies",
                                 "Syphilis", "Tetanus", "Tuberculosis")

GBD.opioid.sum.capita.stand_long <- GBD.opioid.sum.capita.stand %>% gather(Disease, Value, 2:22, factor_key=TRUE)
GBD.opioid.sum.capita.stand_long$Estado <- factor(GBD.opioid.sum.capita.stand_long$Estado)
GBD.opioid.sum.capita.stand_long$Disease <- factor(as.character(GBD.opioid.sum.capita.stand_long$Disease))


GBD.opioid.heat.capita <- ggplot(GBD.opioid.sum.capita.stand_long, 
                                 aes(x = Disease, y = Estado, 
                                     fill = Value)) + 
  geom_tile(colour = "white", size = 1) + 
  scale_fill_continuous_sequential(name="Standardized opioid need", palette = "Heat") +
  scale_x_discrete(name = "Disease group") + 
  scale_y_discrete(name = "State", limits = rev(levels(GBD.opioid.sum.capita.stand_long$Estado))) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(title = "Standardized opioid need per capita by state in Mexico") +
  coord_equal() 
GBD.opioid.heat.capita

ggsave("Images/opiod_need_capita_state.png", 
       dpi = 320, height = 8, width = 10)



#Import demographic data for State regression
#Marginalization - SES
Marginalization.2015 <- readOGR("IMEF_2015/IME2015.shp")
Marginalization.2015 <- Marginalization.2015@data
colnames(Marginalization.2015)[2] <- "Estado"
Marginalization.2015$Estado <- stri_trans_general(str = Marginalization.2015$Estado, id = "Latin-ASCII")
Marginalization.2015$IME <- stri_trans_general(str = Marginalization.2015$IME, id = "Latin-ASCII")
Marginalization.2015 <- Marginalization.2015 %>% 
  mutate(Estado = ifelse(Estado %in% "Distrito Federal", "Ciudad de Mexico", Estado))
Marginalization.2015 <- Marginalization.2015 %>% 
  mutate(Estado = ifelse(Estado %in% "Coahuila", "Coahuila de Zaragoza", Estado))
Marginalization.2015 <- Marginalization.2015 %>% 
  mutate(Estado = ifelse(Estado %in% "Coahuila de Zaragoza", "Coahuila de Zaragoza", Estado))
Marginalization.2015 <- Marginalization.2015 %>% 
  mutate(Estado = ifelse(Estado %in% "Veracruz", "Veracruz De Ignacio De La Llave", Estado))
Marginalization.2015 <- Marginalization.2015 %>% 
  mutate(Estado = ifelse(Estado %in% "Aguas  Calientes", "Aguascalientes", Estado))
Marginalization.2015 <- Marginalization.2015 %>% 
  mutate(Estado = ifelse(Estado %in% "Michoac", "Michoacan de Ocampo", Estado))
Marginalization.2015$IME <- as.numeric(Marginalization.2015$IME)
Marginalization.2015 <- Marginalization.2015 %>% dplyr::select(Estado, IME, GME)

#Align population labels for merge
df.poblacion <- df.poblacion %>% 
  mutate(Estado = ifelse(Estado %in%  "Veracruz De Ignacio De La Llave", 
                         "Veracruz de Ignacio de la Llave", Estado))
df.poblacion <- df.poblacion %>% 
  mutate(Estado = ifelse(Estado %in%  "Coahuila De Zaragoza", "Coahuila de Zaragoza",
                         Estado))
df.poblacion <- df.poblacion %>% 
  mutate(Estado = ifelse(Estado %in%  "Ciudad De Mexico", "Ciudad de Mexico",
                         Estado))
df.poblacion <- df.poblacion %>% 
  mutate(Estado = ifelse(Estado %in%  "Michoacan De Ocampo", "Michoacan de Ocampo",
                         Estado))

#Merge to one dataset
df.r <- merge(x=Marginalization.2015, y=df.poblacion, by="Estado", all.x=T)
df.r <- merge(x=df.r, y=GBD.opioid.score, by="Estado", all.x=T)
df.r$Estado1 <- df.r$Estado
df.r$Estado <- str_to_upper(df.r$Estado)
#df.r$Estado <- sub('[^A-Za-z]+', '', df.r$Estado)
df.r$Estado <- stri_trans_general(str = df.r$Estado, id = "Latin-ASCII")

#Calculate rates
df.r <- df.r %>% mutate(opioid.need_capita = Opioid.need/Poblacion)
df.year.total <- df.year.estado %>% group_by(Estado) %>% summarise(total_n = sum(total_n))
df.year.estado.r <- merge(x=df.year.total, y=df.r, by="Estado", all.x=T)

#Pull region/gender
df_mxstate <- df_mxstate %>% mutate(Estado = ifelse(Estado %in% "DISTRITO FEDERAL", "CIUDAD DE MEXICO", Estado))
df_mxstate <- df_mxstate %>% mutate(Estado = ifelse(Estado %in% "Ciudad de Mexico", "CIUDAD DE MEXICO", Estado))
df.year.estado.r <- merge(x=df.year.estado.r, y=df_mxstate, by="Estado", all.x=T)

#Methadone clinics
methadone <- read.csv("cofepris_methadone.csv")
methadone$Estado <- stri_trans_general(str = methadone$Estado, id = "Latin-ASCII")
methadone <- methadone %>% group_by(Estado) %>% summarise(methadone.clinics = n())
methadone <- methadone %>% mutate(Estado = ifelse(Estado %in% "Ciudad de Mexico", "CIUDAD DE MEXICO", Estado))
methadone <- methadone %>% mutate(Estado = ifelse(Estado %in% "BAJA\nCALIFORNIA", "BAJA CALIFORNIA", Estado))
methadone <- methadone %>% drop_na
methadone <- methadone %>% mutate(methadone_dico = case_when(methadone.clinics >= 0 ~ 1,
                                                        methadone.clinics <0 ~ 0))
df.year.estado.r <- merge(x=df.year.estado.r, y=methadone, by="Estado", all.x=T)
df.year.estado.r$methadone.clinics <- df.year.estado.r$methadone.clinics %>% replace_na(0)
df.year.estado.r <- df.year.estado.r %>% mutate(methadone_dico = case_when(methadone.clinics > 0 ~ 1,
                                                        methadone.clinics == 0 ~ 0))
df.year.estado.r <- df.year.estado.r %>% mutate(opioid_rate = total_n/Poblacion * 10000)

#Tertiary hospitals and oncologic centers
#Import DGIS file
#Available at http://sinaiscap.salud.gob.mx:8080/DGIS/ or http://gobi.salud.gob.mx/
df.dgis <- read_excel("/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/Reviewer_response/ESTABLECIMIENTO_SALUD_202008.xlsx")

df.dgis <- df.dgis %>% dplyr::select(2:13, 60:64, 67:69, 70, 74:77, 42)
df.dgis <- df.dgis %>% dplyr::rename(Estado = `NOMBRE DE LA ENTIDAD`)

#Import Secretaria de Salud recurso file
#Available at https://datos.gob.mx/busca/dataset/recursos-en-salud-nivel-central
df.hosp <- read.csv("/Users/davigood/Box Sync/UCLA/2017_Opioids Mexico/COFEPRIS/Reviewer_response/Recursos_Salud_2017_edited.csv", check.names = F)

df.hosp <- df.hosp %>% dplyr::select(1:13, consultorio.oncologos = `N˙mero de Consultorios de OncologÌa`,
                                     camas.oncologos = `N˙mero de camas de OncologÌa`,
                                     camas.osea = `N˙mero de camas en la unidad de transplante de mÈdula Ûsea`,
                                     med.oncologos = `N˙mero de MÈdicos OncÛlogos`)
df.hosp <- df.hosp %>% left_join(df.dgis, by = "CLUES") #Merge with DGIS file to get additional info on each

#Replace missing with zeros
df.hosp$camas.oncologos <- df.hosp$camas.oncologos %>% replace_na(0)
df.hosp$consultorio.oncologos <- df.hosp$consultorio.oncologos %>% replace_na(0)
df.hosp$camas.osea <- df.hosp$camas.osea %>% replace_na(0)
df.hosp$med.oncologos <- df.hosp$med.oncologos %>% replace_na(0) 

#Filter only those with cancer doctors/beds or be it a tertiary hospital
df.hosp <- df.hosp %>% filter(med.oncologos > 0 | camas.osea > 0 | 
                                    consultorio.oncologos > 0 | camas.oncologos > 0 |
                                    `NIVEL ATENCION` == "TERCER NIVEL")

df.hosp.count <- df.hosp %>% group_by(`Nombre Estado`) %>% summarise(Tercer.nivel = n())
df.hosp.count <- df.hosp.count %>% dplyr::rename(Estado = `Nombre Estado`)

#Merge to main data frame
df.year.estado.r <- df.year.estado.r %>% left_join(df.hosp.count, by = "Estado") 

#Make table of Socioeconomic status by State
df.year.estado.r %>% summarise(Count = n(), Mean = mean(IME), 
                               SD = sd(IME), min = min(IME), max = max(IME))
estado.ses.table <- df.year.estado.r %>% group_by(GME) %>% summarise(Count = n(), Mean = mean(IME), 
                                                                     SD = sd(IME), min = min(IME), max = max(IME))
estado.ses.table[,c(3:6)] <- round(estado.ses.table[,c(3:6)], 2)

#Standardize marginalization index/opioid need scale
df.year.estado.r$IME_stand <- scale(df.year.estado.r$IME, center = TRUE, scale = TRUE)
df.year.estado.r$opioid.need_stand <- scale(df.year.estado.r$opioid.need_capita, center = TRUE, scale = TRUE)

#Regression models
#Regression - negative binomial
label(df.year.estado.r$opioid.need_capita) <- "Opioid need per capita"
label(df.year.estado.r$opioid.need_stand) <- "Standardized opioid need"
label(df.year.estado.r$IME) <- "Socioeconomic status"
label(df.year.estado.r$methadone.clinics) <- "Methadone clinics (per number of licensed clinics)"
label(df.year.estado.r$methadone_dico) <- "Methadone clinics (yes/no)"
label(df.year.estado.r$Tercer.nivel) <- "Tertiary hospitals/oncologic centers"
df.year.estado.r$GME <- recode_factor(df.year.estado.r$GME,  
              `Muy alto` = "Very low", Alto = "Low", Medio = "Medium", 
              Bajo = "High", `Muy bajo` = "Very high")
df.year.estado.r$IME <- df.year.estado.r$IME*-1

dv.labels <- c("Model 1", "Model 2", "Model 3", "Model 4")

m.nb1 <- glm.nb(total_n ~ opioid.need_capita + 
       offset(log(Poblacion)),
       data=df.year.estado.r)
m.nb2 <- glm.nb(total_n ~ opioid.need_capita + 
                  IME + 
       offset(log(Poblacion)),
       data=df.year.estado.r)
m.nb3 <- glm.nb(total_n ~ opioid.need_capita + IME + 
                  methadone.clinics + 
       offset(log(Poblacion)),
       data=df.year.estado.r)
m.nb4 <- glm.nb(total_n ~ opioid.need_capita + IME + 
                  methadone.clinics + Tercer.nivel +
        offset(log(Poblacion)),
        data=df.year.estado.r)

tab_model(m.nb1, m.nb2, m.nb3, m.nb4, show.intercept = F, dv.labels = dv.labels,
          title = "Negative binomial models of opioid dispensing rates 
          for federal entities in Mexico between August 2015 and October 2019",
          string.est = "IRR", digits.p = 2, digits = 3)

m.nb1.cat <- glm.nb(total_n ~ opioid.need_capita + 
       offset(log(Poblacion)),
       data=df.year.estado.r)
m.nb2.cat <- glm.nb(total_n ~ opioid.need_capita + 
                  GME + 
       offset(log(Poblacion)),
       data=df.year.estado.r)
m.nb3.cat <- glm.nb(total_n ~ opioid.need_capita + GME + 
                  methadone.clinics + 
       offset(log(Poblacion)),
       data=df.year.estado.r)
m.nb4.cat <- glm.nb(total_n ~ opioid.need_capita + GME + 
                      methadone.clinics + Tercer.nivel +
                      offset(log(Poblacion)),
                    data=df.year.estado.r)


tab_model(m.nb1.cat, m.nb2.cat, m.nb3.cat, m.nb4.cat, show.intercept = F, dv.labels = dv.labels,
          title = "Negative binomial models of opioid dispensing rates 
          for federal entities in Mexico between August 2015 and October 2019",
          string.est = "IRR", digits.p = 2, digits = 3)

glm.diag.plots(m.nb1)
glm.diag.plots(m.nb2)
glm.diag.plots(m.nb3)
glm.diag.plots(m.nb4)

#Residuals and Predicted Rate
residuals.nb <- residuals(m.nb1, type = c("response"))

df.year.estado.r$residuals <- residuals.nb
df.year.estado.r <- df.year.estado.r %>% mutate(residuals.qual = case_when(residuals <= 0 ~ "Negative",
                                                             residuals > 0 ~ "Positive")) 

#Compare fitted/predicted to observed rates
fitted.nb <- fitted(m.nb1)
df.year.estado.r$fitted.count <- fitted.nb
df.year.estado.r <- df.year.estado.r %>% mutate(fitted_rate = fitted.count/Poblacion * 10000)
df.year.estado.r <- df.year.estado.r %>% mutate(observed.expected = ((opioid_rate-fitted_rate)/fitted_rate)*100)

df.national <- df.year.estado.r %>% summarise(observed.count = sum(total_n), 
                                              fitted.count = sum(fitted.count), 
                                              Poblacion = sum(Poblacion),
                                              Opioid.need = sum(Opioid.need))

format(df.national$Opioid.need, scientific = FALSE)

df.national <- df.national %>% mutate(fitted_rate = fitted.count/Poblacion * 10000)
df.national <- df.national %>% mutate(observed_rate = observed.count/Poblacion * 10000)
df.national <- df.national %>% mutate(observed.expected = ((observed_rate-fitted_rate)/fitted_rate)*100)

#Observed/Expected scatterplot - Figure 1 Panel A
p.scatter <- ggplot(data = df.year.estado.r, aes(y = opioid_rate, x = fitted_rate, 
                                    size = Poblacion, 
                                    label = state_abbr)) +
  #geom_hline(yintercept = median(df.year.estado.r$fitted_rate), colour="black") + 
  #geom_vline(xintercept = median(df.year.estado.r$opioid_rate), colour="black") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "longdash", alpha = 0.8) +
  #geom_smooth(method = glm.nb, se = F, color = "red", linetype = "longdash", alpha = 0.5) +
  geom_point(shape = 21, color = "black", aes(fill = GME)) +
  scale_y_continuous(name = "Observed rate dispensed per 10,000", trans = "log",
                     breaks = c(0, 1, 10, 30, 100, 300),
                     limits = c(5, 300)) +
  scale_x_continuous(name = "Predicted rate based on estimated opioid need per 10,000", trans = "log",
                     breaks = c(0, 1, 10, 30, 100, 300),
                     limits = c(5, 300)) +
  scale_fill_discrete_sequential(palette = "Heat", order = c(5:1), 
                                  name = "Socioeconomic status") +
  scale_size(name = "Population", range = c(3, 7), guide = F) +
  geom_label_repel(show.legend = FALSE, fill = "white", aes(color = GME), force = 10) + 
  scale_color_discrete_sequential(palette = "Heat", order = c(5:1), 
                                  name = "", guide = F) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  #labs(caption = "Axis are on a log scale.\n
  #Horizontal and vertial lines are at the median values for each axis.\n
  #Diagonal dashed red line is the line of equality based on the negative binomial model.\n
  #Circle size is proportional to the states population.") +
  guides(fill = guide_legend(override.aes = list(size=6)))
p.scatter

ggsave("Images/opiod_need_x_y.png", 
       dpi = 320, height = 8, width = 10)


#Residuals, echo=FALSE, warning=FALSE
df.year.estado.r$residuals_stand <- scale(df.year.estado.r$residuals/df.year.estado.r$Poblacion, 
                                          center = T, scale = T)

pos <- position_jitter(width = 0.1, seed = 1)

p.residual <- ggplot(df.year.estado.r, aes(fill = GME, y = residuals_stand, x = GME, label = state_abbr)) +
  geom_hline(yintercept=0, colour="red", linetype = "longdash", alpha = 0.5) + 
  geom_jitter(size = 3, shape = 21, color = "black",
              position = pos) +
  scale_fill_discrete_sequential(name="", palette = "Heat", order = c(5:1), guide = F) +
  geom_label_repel(show.legend = FALSE, fill = "white", aes(color = GME), force = 5,
                   position = pos) + 
  scale_color_discrete_sequential(name="", palette = "Heat", order = c(5:1), guide = F) +
  
  labs(x = "Socioeconomic status", y = "Standardized residuals") +
  theme_minimal() +
  theme(text = element_text(size = 14))
p.residual

ggsave("Images/bar_residuals.png", dpi = 300, width = 5, height = 4)


#Figure 1 Panel B
p.bar.oe <- ggplot(df.year.estado.r, aes(fill = GME, y = observed.expected, x = GME, label = state_abbr)) +
  geom_hline(yintercept=0, colour="red", linetype = "longdash", alpha = 0.5) + 
  #geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  geom_boxplot(outlier.shape=21,
                outlier.size=3) +
  scale_fill_discrete_sequential(name="", palette = "Heat", order = c(5:1), guide = F) +
  labs(x = "Socioeconomic status", 
       y = "Percentage of observed/expected\nopioid dispensing") +
  theme_minimal() +
  theme(text = element_text(size = 14))
p.bar.oe

by(df.year.estado.r$observed.expected, df.year.estado.r$GME, median)
by(df.year.estado.r$observed.expected, df.year.estado.r$GME, mean)


#Figure 1}
#Combine scatter, residual and legend 

plots <- plot_grid(p.scatter + theme(legend.position="none"), p.bar.oe, align = 'v', axis = 'l',
                   labels = c("A", "B"))
plots

ggsave("Images/combined_scatter_residual.png",
       dpi = 300, 
       width = 10, height = 6)



#Observed/Expected Plot - Supplemental Figure 6}
#Plot O/E
df.year.estado.r$value <- df.year.estado.r$observed.expected
df.year.estado.r  <- df.year.estado.r  %>% mutate(value_type = case_when(value >= 0 ~ "above",
                                                        value <0 ~ "below"))

#Diverging dot plot
ggplot(df.year.estado.r, aes(x=reorder(Estado1, value), y=value, label=round(value,1))) + 
  new_scale_color() +
  geom_point(stat='identity', aes(col=as.factor(value_type)), size=8)  +
  scale_color_discrete_diverging(palette = "Red-Green", guide = F) + 
  #  scale_color_manual(name="", 
  #                   labels = c("Above expected", "Below expected"), 
  #                   values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_hline(yintercept = 0, color = "red", alpha = 0.8, linetype = "longdash") +
  geom_text(color="white", size=2) +
  labs(#title = "Percentage of observed over expected opioid dispensing rate",
       x = "State",
       y = "Percentage of observed group I opioid dispensing rate\nabove or below expected dispensing rate") + 
  ylim(-100, 250) +
  coord_flip() + theme(text = element_text(size=16)) +
  theme_minimal()

ggsave("Images/diverging_bar.png", dpi = 300, width = 6, height = 8)

