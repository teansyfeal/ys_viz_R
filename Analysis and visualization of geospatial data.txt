library(dplyr)
library(leaflet)
library(tmap)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

lf.plot <- leaflet() %>%
  addTiles() %>%
  setView(lng = 26.9871, lat = 49.4230, zoom = 5.5) %>%
  addMarkers(lng=27.0129, lat=49.4514,
             label = "ТЦ Агора.Фестиваль хіп хоп культури",
             labelOptions = labelOptions(noHide = T))%>%
  addMarkers(lng=26.9696, lat=49.4088,
             label = "Сквер танкістів",
             labelOptions = labelOptions(noHide = T))%>%
  addMarkers(lng=26.9871331, lat=49.422983,
             label = "ПАН ДИВАН, МЕБЛІ-ЦЕНТР",
             labelOptions = labelOptions(noHide = T))%>%
  addMarkers(lng=27.0475044, lat=49.4372448,
             label = "Вул.Ганжі - Pp Kovtun",
             labelOptions = labelOptions(noHide = T))%>%
  addMarkers(lng=26.955294, lat=49.3858388,
             label = "Детроїт. Ресторан",
             labelOptions = labelOptions(noHide = T))
lf.plot %>% print()

##########################################################################
world_map <- ne_countries(scale = 50, returnclass = 'sf')

european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czech Rep.","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                    "Portugal","Romania","Slovakia","Slovenia","Spain",
                    "Sweden","United Kingdom")

european_union_map <- 
  World %>% 
  filter(name %in% european_union)

path <- "geoMap.csv"
  df <- read.csv(path, header = TRUE) 
  df <- filter(df, df$qnt != "")
  df <- df %>% 
    rename("name" = "Country" )
  
data = df %>% right_join(european_union_map, by = "name")

data <- data %>%
  mutate(trend1 = case_when(is.na(qnt)~0, TRUE ~ as.integer(qnt)*1))%>%
  mutate(trend = rank(trend1))


data <- st_sf(data)

tmap_mode('view')

tm.plot = tm_shape(data) +
  tm_fill("trend")+
  tm_text("name", size = 1)+
  tm_borders()

tm.plot %>% print()
##########################################################################



library(rvest)

link = "https://en.wikipedia.org/wiki/List_of_countries_by_military_expenditures"
page = read_html(link)

name = page %>% html_nodes("caption~ tbody td:nth-child(1)") %>% html_text()
sovereignt = str_trim(name)
percent_of_gdp = page %>% html_nodes("td:nth-child(6)") %>% html_text()

data_from_wiki = data.frame(sovereignt, percent_of_gdp, stringsAsFactors = FALSE)

data_from_wiki <- data_from_wiki %>%
  mutate(gdpn = as.integer(percent_of_gdp))

data2 = data_from_wiki %>% right_join(european_union_map, by = "sovereignt")

data2 <- st_sf(data2)

library(ggplot2)
library(rayshader)

gdp.map = ggplot(data2) + theme_bw()+
  geom_sf(aes(fill = gdpn), color = "black")+
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "3D map of European Union",
       fill = "% of GDP")


plot_gg(gdp.map, multicore = T, zoom = 0.8, scale = 25, windowsize = c(900, 1050))

