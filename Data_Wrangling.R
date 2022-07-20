library(tidyverse)
library(knitr)
library(stringr)
library(readxl)
library(curl) # downloads data from the web
library(sf)


# defining file address
url <- "http://aasa.ut.ee/Rspatial/data/FarmedAnimalsByLocation_31102018.xlsx"

# name the downloaded file 
destfile <- "FarmedAnimalsByLocation_31102018.xlsx"

# download data according to the parameters
curl_download(url, destfile)

# import data to R
agr_animal <- read_excel(destfile)
glimpse(agr_animal)


# select relevant variable
agr_animal <- agr_animal %>%
  select(`action place`, 
         `estonian holstein cattle`, 
         `estonian red cattle`,  
         `estonian native cattle`, 
         `beef cattle`, 
         sheeps, 
         goats, 
         pigs, 
         `X koordinaat`, 
         `Y koordinaat`, 
         municipality)
glimpse(agr_animal)

# renaming fields where necessary
agr_animal <- agr_animal %>% 
  rename(action_place = `action place`,
                                  estonian_holstein_cattle = `estonian holstein cattle`,
                                  estonian_red_cattle = `estonian red cattle`,
                                  estonian_native_cattle = `estonian native cattle`, 
                                  beef_cattle = `beef cattle`, 
                                  X_coord = `X koordinaat`,
                                  Y_coord = `Y koordinaat`)
glimpse(agr_animal)

# convert coordinates from char to numeric
agr_animal <- agr_animal %>%
  mutate(X_coord = as.numeric(X_coord), 
         Y_coord = as.numeric(Y_coord)) 
glimpse(agr_animal)


ggplot()+
  geom_point(data = agr_animal, aes(x=X_coord, y=Y_coord))

# rename Estonian coordinates
agr_animal <- agr_animal %>%
  rename(x = Y_coord, y = X_coord)

ggplot()+
  geom_point(data = agr_animal, aes(x = x, y = y))

# getting the municipality data
download.file("https://geoportaal.maaamet.ee/docs/haldus_asustus/omavalitsus_shp.zip", destfile="omavalitsus_shp.zip")
unzip('omavalitsus_shp.zip')

list.files(pattern='shp')

municip <- st_read("omavalitsus_20220701.shp", quiet=T)
glimpse(municip)

# check crs
st_crs(municip)

# plot municipality together with animal data
ggplot()+
  geom_sf(data = municip, colour = "grey40", fill = "grey80", size=0.5)+ 
  geom_point(data = agr_animal, aes(x=x, y=y, size=pigs), colour = "red")

# convert wide format to long
agr_animal_1 <- gather(agr_animal, "key", "value", 2:8)
agr_animal_1 <- agr_animal_1 %>%
  filter(value>0)


# long
agr_animal_1 %>%
  head()%>%
  kable()

# plotting pigs in Estonia
ggplot()+
  geom_sf(data = municip, colour = 'grey40', fill = 'grey80', size = 0.1)+
  geom_point(data = agr_animal_1 %>% 
               dplyr::filter(key == "pigs"), 
             aes(x = x, y = y, size = value, colour = key))+
  labs(fill = "N", 
       title = "Pigs distribution in Estonia",
       subtitle = "Agricultural Registers and Information Board",
       caption = "Author: Chris Mutugi")




glimpse(agr_animal_1)

# conversion of plain table to sf'
agr_animal_1_sf <- st_as_sf(agr_animal_1, coords = c("x", "y"), crs = 3301)
glimpse(agr_animal_1_sf)

ggplot()+
  geom_sf(data = municip)+
  geom_sf(data = agr_animal_1_sf, aes(col = key, alpha = value))


# spatial join for animals sf with the municipalities
agr_animal_1_sf_municip <- st_join(st_transform(agr_animal_1_sf, 3301), st_transform(municip, 3301), join = st_intersects)

agr_animal_1_sf_municip <- agr_animal_1_sf_municip %>% 
  st_drop_geometry()


# group output by 'OKOOD' using summarise()
agr_animal_1_sf_municip_aggr <- agr_animal_1_sf_municip %>% 
  group_by(OKOOD, key) %>% 
  summarise(sum = sum(value)) %>% 
  ungroup()

agr_animal_1_sf_municip_aggr <- agr_animal_1_sf_municip_aggr %>% 
  spread(key, sum)

glimpse(agr_animal_1_sf_municip_aggr)



# joining municipality counts to municipalities geolayer
agr_animal_1_sf_municip_aggr <- left_join(municip, agr_animal_1_sf_municip_aggr, by="OKOOD")

glimpse(municip)

# pigs plot
ggplot()+
  geom_sf(data = agr_animal_1_sf_municip_aggr, aes(fill=pigs), size=0.25, colour = "grey70")+
  scale_fill_gradientn(colours = c("darkgreen", "grey80", "orange", "red", "brown"), na.value = "magenta")+
  labs(fill = "N", 
       title = "Pigs in Estonian municipalities",
       subtitle = "Agricultural Registers and Information Board",
       caption = "Author: Chris Mutugi")

# calculating municipalities' areas
agr_animal_1_sf_municip_aggr$area <- st_area(agr_animal_1_sf_municip_aggr)

# converting area from m2 to km2
agr_animal_1_sf_municip_aggr <- agr_animal_1_sf_municip_aggr %>% 
  mutate(area = as.numeric(area) / 1000000)


# plot pigs density per km2
ggplot()+
  geom_sf(data = agr_animal_1_sf_municip_aggr, aes(fill = pigs / area), size=0.25, colour = "grey70")+
  scale_fill_gradientn(colours = c("darkgreen", "grey80", "orange", "red", "brown"), na.value = "magenta")+
  labs(fill = "N per km2", 
       title = "Pigs density in Estonian municipalities",
       subtitle = "Agricultural Registers and Information Board",
       caption = "Author: Chris Mutugi")


# THE NUMBER OF PIGS IS HIGHEST IN VILJANDI MUNICIPALITY.
# THE MUNICIPALITY ALSO HAS THE HIGHEST PIGS PER KM2
