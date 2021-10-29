install.packages('sf')
install.packages('here')
install.packages('tidyverse')
install.packages('janitor')
install.packages('raster')
install.packages('countrycode')

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
  mutate(iso_code=countrycode(HDIcols$country,origin ='country.name',destination = 'iso2c'))
  

join_HDI <- WCG%>%
  clean_names()%>%
  left_join(.,
            HDIcols,
            by=c('aff_iso'='iso_code'))
#29th Oct
