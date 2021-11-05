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


# plot
library(tmap)
library(tmaptools)

# tmap mode set to plotting
# static map
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

# interactive map
tmap_mode("view")

tm_shape(join_HDI) + 
  tm_polygons("difference", n = 5)

# classify the difference of GII
join_HDI2 <- join_HDI%>%
  mutate(summary = case_when(difference < 0 ~"improved",
                             difference > 0 ~"worse",
                             TRUE~"the same"))

# crs transformation, mollweide preserves size not direction like WGS84
join_HDI = st_transform(join_HDI, crs = "+proj=moll")

#change column names
names(join_HDI)[9] <- '(a) 2019 and (b) 2010'
names(join_HDI)[11] <- '(c) GII difference between \n2010 and 2019'

# plot each map

tmap_mode("plot")

# set breaks
breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)
diffbreaks=c(-0.4,-0.3,-0.2,-0.1,0,0.1)

# map 2019
tmap_2019 <- tm_shape(join_HDI)+
  tm_polygons("(a) 2019 and (b) 2010",  
              palette= "PuBu",
              breaks = breaks)+
  tm_legend(show = FALSE)+
  tm_layout(frame = FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

# map 2010
tmap_2010 <- tm_shape(join_HDI)+
  tm_polygons("x2010",  
              palette= "PuBu",
              breaks = breaks)+
  tm_legend(show = FALSE)+
  tm_layout(frame = FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1.5)

# map GII difference
tmap_difference <- tm_shape(join_HDI)+
  tm_polygons("(c) GII difference between \n2010 and 2019",  
              palette =  "Blues",
              midpoint = NA)+
  tm_legend(show = FALSE)+
  tm_layout(frame = FALSE)+
  tm_credits("(c)", position=c(0,0.85), size=1.5)

# legend
legend <- tm_shape(join_HDI)+
  tm_polygons("(a) 2019 and (b) 2010", 
              palette= "PuBu",
              breaks = breaks)+
  tm_legend(show=TRUE)+
  tm_layout(legend.only = TRUE, legend.position=c(0.3,0.25),asp=0.1)+ 
  tm_credits("Mapped data:\nUN Gender Inequality Index\nWorld outline:\nArcGIS Hub 
               ", position=c(0.5,0.15), just="left")+
  # GII difference legend
  tm_shape(join_HDI)+
  tm_polygons("(c) GII difference between \n2010 and 2019",  
              palette = "Blues",
              midpoint = NA)+
  tm_compass(north=0, position=c(0.6,0.6))+
  # asp means Aspect ratio
  tm_layout(legend.only = TRUE, legend.position=c(0.1,0.1),asp=0.1)

t = tmap_arrange(tmap_2019, tmap_2010, tmap_difference, legend, ncol=2)

t



