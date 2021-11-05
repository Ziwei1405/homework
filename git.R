library(sf)
library(here)
library(tidyverse)
library(countrycode)
library(janitor)


HDI <- read.csv("Data/Gender Inequality Index (GII).csv", 
                skip = 5, header = TRUE, na.strings = "..", nrow = 189)

WCG <- st_read(here::here("data", "World_Countries__Generalized_.shp"))



HDIcols <- HDI %>%
  clean_names()%>%
  select(country,x2019,x2010)%>%
  mutate(difference=x2019-x2010)%>%
  slice(1:189,)%>%
  mutate(iso_code=countrycode(country, origin='country.name',destination = 'iso2c'))
  

join_HDI <- WCG%>%
  clean_names()%>%
  left_join(.,
            HDIcols,
            by=c('aff_iso'='iso_code'))
#29th Oct


#plot
library(tmap)
library(tmaptools)

#tmap mode set to plotting
#static map
tmap_mode("plot")
tmap_options(bg.color = "white", legend.text.color = "black")

tm_shape(join_HDI) +
  tm_polygons(col = "difference", n = 5,
              style = "jenks")+
  tm_scale_bar(position = c(0.02,0.095), 
               text.size = .75,
               lwd = .5,
               width = .2)+
  tm_layout(legend.text.size=.65, 
            legend.title.size = 1.1,
            legend.outside = TRUE,
            frame=FALSE)+
  tm_compass(type = 'arrow', position=c(0.015,0.2), size = 1.15)

#interactive map
tmap_mode("view")

tm_shape(join_HDI) + 
  tm_polygons("difference", n = 5)

#compare the difference of GII
join_HDI2 <- join_HDI%>%
  mutate(summary = case_when(difference < 0 ~"improved",
                             difference > 0 ~"worse",
                             TRUE~"the same"))
  









