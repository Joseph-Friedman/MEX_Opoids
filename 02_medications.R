#Load packages
#install.packages("waffle", repos = "https://cinc.rud.is")

rm(list = ls())
pacman::p_load(data.table,tidyverse,stringr,stringi,DT,expss,reshape,reshape2,readxl,hablar,skimr,lubridate,ggridges,waffle,magrittr,colorspace)

#r Import data
df <- read_excel('anexo-15118-19.xlsx', sheet = 2)
colnames(df)[3] <- "Date"
colnames(df)[4] <- "Med"
df %>% distinct(Med)

#Match brand names to generic names
df <- df %>% mutate(Med2 = ifelse(agrepl("dormicum", Med, ignore.case=T, max.distance=0.1), "Midazolam",
                           ifelse(agrepl("panazeclox", Med, ignore.case=T, max.distance=0.1), "Clonazepam",
                           ifelse(agrepl("OPÉRATIVÁN", Med, ignore.case=T, max.distance=0.1), "Ativan",
                                  ifelse(agrepl("OPERATIAVAN", Med, ignore.case=T, max.distance=0.1), "Ativan",
                           ifelse(agrepl("tafil", Med, ignore.case=T, max.distance=0.0), "Alprazolam",
                                  Med))))))

df <- df %>% mutate(Med2 = ifelse(agrepl("fentanilo", Med, ignore.case=T, max.distance=0.1), "Fentanyl", 
                           ifelse(agrepl("fenta", Med, ignore.case=T, max.distance=0.1), "Fentanyl",
                           ifelse(agrepl("torafen", Med, ignore.case=T, max.distance=0.1), "Fentanyl",
                           ifelse(agrepl("filtaten", Med, ignore.case=T, max.distance=0.1), "Fentanyl",
                           ifelse(agrepl("filtanten", Med, ignore.case=T, max.distance=0.1), "Fentanyl",
                           ifelse(agrepl("durogesic", Med, ignore.case=T, max.distance=0.1), "Fentanyl",
                           ifelse(agrepl("durogesig", Med, ignore.case=T, max.distance=0.1), "Fentanyl",        
                           ifelse(agrepl("Fenodid", Med, ignore.case=T, max.distance=0.1), "Fentanyl", 
                           ifelse(agrepl("Utranil", Med, ignore.case=T, max.distance=0.1), "Fentanyl",
                           ifelse(agrepl("DURAGESIG", Med, ignore.case=T, max.distance=0.1), "Fentanyl",       
                           ifelse(agrepl("zuftil", Med, ignore.case=T, max.distance=0.1), "Sufentanyl",
                           ifelse(agrepl("ultiva", Med, ignore.case=T, max.distance=0.1), "Remifentanyl",
                           ifelse(agrepl("morfina", Med, ignore.case=T, max.distance=0.1), "Morphine",
                           ifelse(agrepl("analfin", Med, ignore.case=T, max.distance=0.1), "Morphine",
                           ifelse(agrepl("anlafin", Med, ignore.case=T, max.distance=0.1), "Morphine",
                           ifelse(agrepl("anlfin", Med, ignore.case=T, max.distance=0.1), "Morphine",
                           ifelse(agrepl("ANAFLIN", Med, ignore.case=T, max.distance=0.1), "Morphine",         
                           ifelse(agrepl("graten", Med, ignore.case=T, max.distance=0.1), "Morphine",
                           ifelse(agrepl("garten", Med, ignore.case=T, max.distance=0.1), "Morphine",
                           ifelse(agrepl("biogenfine", Med, ignore.case=T, max.distance=0.1), "Morphine",
                           ifelse(agrepl("metadona", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("me tadona", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("metadota", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("metadon", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("metano", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("metadora", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("met adona", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("amidone", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("AMDIONE", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("metadina", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("rubidexol", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("MTEADONA", Med, ignore.case=T, max.distance=0.1), "Methadone",
                           ifelse(agrepl("oxicodona", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("plexicodim", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("endocodil", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("endocidil", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("ENDODOCIL", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("oxicontyn", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("oxicotin", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("oxycotin", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("oxyc", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("oxicotin", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("OXYNCON", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("oyicontin", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("OXICINTIN", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("TARGINIQ", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                           ifelse(agrepl("TARGINIC", Med, ignore.case=T, max.distance=0.1), "Oxycodone",
                                Med2))))))))))))))))))))))))))))))))))))))))))))))))

df <- df %>% mutate(Med2 = ifelse(agrepl("temgesic", Med, ignore.case=T, max.distance=0.1), "Buprenorphine",
                           ifelse(agrepl("transtec", Med, ignore.case=T, max.distance=0.1), "Buprenorphine",
                           ifelse(agrepl("soloro", Med, ignore.case=T, max.distance=0.1), "Buprenorphine",
                           ifelse(agrepl("bupr", Med, ignore.case=T, max.distance=0.1), "Buprenorphine",
                           ifelse(agrepl("tergenic", Med, ignore.case=T, max.distance=0.1), "Buprenorphine",
                           ifelse(agrepl("Tylex", Med, ignore.case=T, max.distance=0.1), "Codeine",
                           ifelse(agrepl("tapentadol", Med, ignore.case=T, max.distance=0.1), "Tapentadol",
                           ifelse(agrepl("palexia", Med, ignore.case=T, max.distance=0.1), "Tapentadol",
                           ifelse(agrepl("plexia", Med, ignore.case=T, max.distance=0.1), "Tapentadol",
                           ifelse(agrepl("paleexia", Med, ignore.case=T, max.distance=0.1), "Tapentadol",
                           ifelse(agrepl("pal", Med, ignore.case=T, max.distance=0.1), "Tapentadol",
                           ifelse(agrepl("himop", Med, ignore.case=T, max.distance=0.1), "Hydromorphone",
                           ifelse(agrepl("hidromorfona", Med, ignore.case=T, max.distance=0.1), "Hydromorphone",
                                  Med2))))))))))))))


df <- df %>% transform(Med3=replace(Med2, !(Med2 %in% c("Fentanyl","Tapentadol",
                                                  "Codeine", "Oxycodone", "Hydromorphone",
                                                  "Methadone", "Morphine", "Sufentanyl", "Remifentanyl",
                                                  "Ativan", "Clonazepam", "Midazolam", "Buprenorphine",
                                                  "Alprazolam")), NA))

df <- df %>% mutate(Med4=case_when(Med3 == "Fentanyl" ~ "Fentanyl", 
                                   Med3 == "Tapentadol" ~ "Tapentadol",
                                   Med3 == "Oxycodone" ~ "Oxycodone",
                                   Med3 == "Hydromorphone" ~ "Hydromorphone",
                                   Med3 == "Methadone" ~ "Methadone",
                                   Med3 == "Morphine" ~ "Morphine",
                                   Med3 == "Sufentanyl" ~ "Fentanyl",
                                   Med3 == "Remifentanyl" ~ "Fentanyl"))
                              

df <- df %>% mutate(day=date(Date))
df <- df %>% mutate(month=month(day))
df <- df %>% mutate(year=year(day))
df$year <- as.factor(df$year)
df$month <- as.factor(df$month)

write.csv(df, '200228_COFEPRIS_medications.csv')


#r Data Wrangle}
##Data wrangle for density plots
#install.packages("waffle", repos = "https://cinc.rud.is")
df <- read.csv('200228_COFEPRIS_medications.csv')

df %>% group_by(Med2) %>% tally()
df %>% group_by(Med3) %>% tally()
df.tally <- df %>% group_by(Med4) %>% tally()
df.tally <- df.tally %>% mutate(n.1000 = n/1000)
df.tally <- df.tally %>% mutate(perc = n/sum(n)*100)

df$Med4 <- fct_explicit_na(df$Med4)
df.tally.month <- df %>% group_by(month, year, Med4) %>% tally() %>% arrange(year, month)
df.tally.month <- df.tally.month %>% mutate(Quarter.id = recode(month, 1:3 ~ 1, 
                                                                4:6 ~ 2,
                                                                7:9 ~ 3,
                                                                10:12 ~ 4))
df.tally.month$Quarter.year <- paste0(as.character(df.tally.month$year),"-", as.character(df.tally.month$Quarter.id))
df.tally.month$poblacion <- 119530753
df.tally.month <- df.tally.month %>% mutate(Rate = n/poblacion * 1000000)

#Calculate growth
df.tally.month <- df.tally.month %>% 
  group_by(Med4) %>% 
  arrange(Quarter.year, .by_group=T) %>% 
  mutate(Growth.percent = Rate/first(Rate) * 100)

#Calculate percent change
df.tally.month <- df.tally.month %>%
  group_by(Med4) %>% 
  arrange(Quarter.year, .by_group=T) %>% 
  mutate(pct_change = (Rate/lag(Rate) - 1) * 100)
df.tally.month$pct_change <- round(df.tally.month$pct_change,1)

df.tally.month$Medication <- df.tally.month$Med4 
df.tally.month$Medication_num <- df.tally.month %>% group_by(Medication) %>% group_indices
df.tally.month <- df.tally.month %>% arrange(Quarter.year)
df.tally.month <- df.tally.month %>% filter(!Quarter.year %in% c("2015-2", "2019-4"))
df.tally.month$Quarter_num <- df.tally.month %>% group_by(Quarter.year) %>% group_indices

df.quarter <- df.tally.month %>% group_by(Medication, Quarter.year) %>% summarise(n = sum(n), poblacion = min(poblacion))
df.quarter$Medication_num <- df.quarter %>% group_by(Medication) %>% group_indices
df.quarter$Quarter_num <- df.quarter %>% group_by(Quarter.year) %>% group_indices
df.quarter <- df.quarter %>% mutate(Rate = n/poblacion * 1000000)
df.quarter <- df.quarter %>% filter(!Medication == "(Missing)")
df.quarter <- df.quarter %>% arrange(Medication_num, Quarter_num)

write.csv(df.quarter, 
          '200228_COFEPRIS_medications_quarter.csv')

df.filter <- df %>% filter(!is.na(Med4))
df.filter <- df.filter %>% filter(!Med4 %in% "(Missing)")
df.filter <- df.filter %>% filter(Med4!="Codeine")
df.filter <- df.filter %>% mutate(Med4 = factor(Med4, levels = rev(c("Tapentadol", "Oxycodone", "Morphine",
                                                                     "Methadone", "Hydromorphone", "Fentanyl"))))

aqpc.med <- read_excel("COFEPRIS_medication_output.xlsx",
           sheet = 4)
aqpc.med$`"Medication"` <- c("Fentanyl", "Hydromorphone", "Methadone", "Morphine", "Oxycodone", "Tapentadol")
colnames(aqpc.med)[1] <- "Med4"
colnames(aqpc.med)[6] <- "AQPC"
colnames(aqpc.med)[7] <- "Lower"
colnames(aqpc.med)[8] <- "Upper"
#aqpc.med <- aqpc.med %>% select(Medication, AQPC, Lower, Upper)
df.filter <- merge(x = df.filter, y = aqpc.med, by = "Med4", all.x = T)
paste0(aqpc.med$Med4, " AQPC ", aqpc.med$AQPC, " (95% CI ", aqpc.med$Lower, " - ", aqpc.med$Upper, ")")


#r Waffle Plot}
ggplot(df.tally, (aes(fill=Med4, values=n.1000))) + 
  geom_waffle(color="white", size=1, n_rows=10) + 
  labs(#title = "Figure 1. Number of prescriptions in COFEPRIS database",
       caption = "Each square is equal to 1,000 prescriptions./n
       Hydromorphone was not displayed as the sum of prescriptions was less than 1,000.",
       fill = "Medication") +
  coord_equal() +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_minimal() +
  theme_enhance_waffle() +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("Images/Figure 2 - Waffle.png", dpi = 300)


#r Density Plot
#Density plot
ggplot(df.filter %>% filter(!Med4 == "Hydromorphone"), aes(x=day, group=Med4, fill=Med4)) +
    geom_density(adjust=1.0, alpha=.4) +
    facet_wrap(~Med4, ncol = 2, strip.position = c("bottom")) +
  labs(x="Year", y="Density", title = "Figure 2. Density of dispensed narcotic prescriptions in Mexico") +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Images/Figure 2 - Densities.png", dpi = 300)


