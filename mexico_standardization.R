#### Mexico tables 
library(DescTools)
library(raster)
library(tidyverse)


setwd("C:/Users/alexb/Documents/github/Mexico-State-Finance-Data")
data <- read.csv("efipem_state_poblacion_v1.csv", encoding = "utf-8")%>%
  select( ID_ENTIDAD, estado, ANIO, Ingresos_propios.pc, Inversion.publica.pc,
         Total.de.egresos.pc, Total.de.ingresos.pc, Ingresos_menos_finanzas.pc)%>%
  filter(!year == 2019)
  
data <-data%>%
  group_by(ANIO)%>%
  mutate(across(Ingresos_propios.pc:Ingresos_menos_finanzas.pc, Winsorize, .names = "wins_{col}" ))%>%
  ungroup()

summary(data$Ingresos_propios.pc)
summary(data$wins_Ingresos_propios.pc)



cv <- data%>%
  group_by(ANIO)%>%
  summarise(#ingresos_tributarios = cv(wins_Ingresos_propios.pc),
            ingresos_tributarios = (sd(wins_Ingresos_propios.pc)/mean(wins_Ingresos_propios.pc)),
            gastos = (sd(wins_Total.de.egresos.pc)/mean(wins_Total.de.egresos.pc)),)

cv%>%
  ggplot(aes(x=ANIO))+
  geom_line(aes(y=ingresos_tributarios))+
  geom_line(aes(y=gastos))+
  theme_classic() +
  labs(title = "Gobiernos regionales",
       subtitle = "Mexico",
       x = "", 
       y = "Promedio")
  
