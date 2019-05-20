Border Frontiers
================
Martin Kosík
May 1, 2019

``` r
knitr::opts_chunk$set(echo = TRUE, fig.show = 'hide')
library(data.table)
library(tidyverse)
library(here)
library(sf)
Sys.setlocale("LC_CTYPE", "russian") 
library(mapview)
# library(mapedit) (if you want to draw polygons yourself)
```

Load the geocoded dataset from Zhukov and Talibova (2018) and our Memorial data.

``` r
selected_vars <- c("person_id", "nation", "arest_date", "process_date","live_place", 
                   "birth_place", "birth_year",  "surname", "name", "patronimic", "memory_book")

memorial_lists <- fread(here::here("memo_list/memorial_lists.tsv"), encoding="UTF-8", sep="\t", 
                        select = selected_vars, quote="", na.strings = "None")

load(here::here("data/eventsClean_v1.RData"))

events <- events %>% 
  dplyr::select(YEAR_STATUTE, YEAR, DATE, ORIGIN_LONG, ORIGIN_LAT, NAME, BIRTH_DATE, SOURCE)

events <- events %>% 
  mutate(names_split = str_split(NAME, pattern = " "),
         first_name = as.factor(map_chr(names_split, nth, 2)),
         surname = as.factor(map_chr(names_split, 1)),
         patronymic = as.factor(map_chr(names_split, nth, 3)),
         number_of_names = as.factor(map_int(names_split, length))) %>% 
  dplyr::select(-c(names_split, NAME))

saveRDS(events, file = here::here("data/eventsClean_v2.RData"))

events <- readRDS(here::here("data/eventsClean_v2.RData"))

events <- events %>% 
  mutate(BIRTH_DATE = as.numeric(BIRTH_DATE))
```

    ## Warning in evalq(as.numeric(BIRTH_DATE), <environment>): NAs introduced by
    ## coercion

We drop possible duplicates.

``` r
events_distinct <- events %>% 
  distinct(surname, first_name, BIRTH_DATE, patronymic, SOURCE, ORIGIN_LONG, ORIGIN_LAT)
```

We merge Zhukov and Talibova (2018) dataset to our data based names, birth year, source.

``` r
merged <- memorial_lists %>% 
  left_join(events_distinct, by = c("surname", "name" = "first_name", "birth_year" = "BIRTH_DATE",
                           "patronimic" = "patronymic", "memory_book" = "SOURCE"))
```

    ## Warning: Column `surname` joining character vector and factor, coercing
    ## into character vector

    ## Warning: Column `name`/`first_name` joining character vector and factor,
    ## coercing into character vector

    ## Warning: Column `patronimic`/`patronymic` joining character vector and
    ## factor, coercing into character vector

We drop observations with no or multiple matches.

``` r
merged <- merged %>% 
  dplyr::select(person_id, ORIGIN_LONG, ORIGIN_LAT) %>% 
  filter(!is.na(ORIGIN_LONG), !is.na(ORIGIN_LAT))

rm(list = c("events", "events_distinct", "memorial_lists"))
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  1863128  99.6   18385923  982.0  44887511 2397.3
    ## Vcells 35149442 268.2  269573013 2056.7 316177570 2412.3

``` r
multiple_matches <- merged %>% 
  count(person_id, sort = T) %>% 
  filter(n > 1)

merged <- merged %>% 
  anti_join(multiple_matches, by = "person_id")

rm(multiple_matches)
```

We load the shape of USSR borders and convert it to Multiline object (instead of multiple polygon object).

``` r
ussr_map <- st_read(here::here("data/ussr_shapefiles/1926SovietUnion.shp"))
```

    ## Reading layer `1926SovietUnion' from data source `C:\Users\Martin\Desktop\Thesis2\Geopolitics of repressions\Geopolitics-of-Repressions2\Geopolitics-of-Repressions\data\ussr_shapefiles\1926SovietUnion.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 67 features and 187 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 26.02808 ymin: 35.14065 xmax: 191.0054 ymax: 81.85871
    ## epsg (SRID):    NA
    ## proj4string:    NA

``` r
st_crs(ussr_map) <- "+proj=longlat +datum=WGS84 +no_defs"

ussr_borders <-  ussr_map %>% 
    summarise(geometry = st_union(.)) %>% 
  st_boundary()

ussr_map <- ussr_map %>% 
  dplyr::select(Id, NameENG, NameRUS)
```

In the following code chunk, I drew polygons along the Soviet border with other countries which I then interest with the Multiline object of Soviet boundaries. If I would not do this, the coastlines would be treated as borders too. Thus, do not run this code chunk (unless you want draw the polygons yourself).

``` r
editMap(mapview(ussr_map))

drawed_polygons <- editMap(mapview(ussr_borders))


saveRDS(drawed_polygons, file = here::here("data/drawed_polygons.RData"))
```

``` r
drawed_polygons <- readRDS(here::here("data/drawed_polygons.RData"))

# you can check the drawed polygons with by runnung mapview(drawed_polygons$drawn)

border_lines <- st_intersection(ussr_borders, drawed_polygons$drawn)
```

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries

``` r
buffer_250km <- border_lines %>% 
  st_transform(crs = 7801) %>% 
  st_buffer(dist = 250000, nQuadSegs = 1) %>% 
  st_intersection(ussr_map %>% st_transform(crs = 7801)) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
```

    ## Warning: attribute variables are assumed to be spatially constant
    ## throughout all geometries

``` r
ggplot(ussr_map) +
    geom_sf(fill = "gray95", color = "gray50", size = 0.5) +
    geom_sf(data = st_transform(buffer_250km, crs = "+proj=longlat +datum=WGS84 +no_defs"), fill = "gray50")+
    theme_void() +
    coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs", ndiscr = F) + 
    labs(fill = "Border province (in Europe)") + 
    theme(legend.position="bottom")
```

``` r
ggsave(here::here("plots/final/border_buffer_map.pdf"))
```

    ## Saving 7 x 5 in image

``` r
arrests_sf <- st_as_sf(merged, coords = c("ORIGIN_LONG", "ORIGIN_LAT"), crs = 4326) 

joined <-  arrests_sf %>%
  st_join(buffer_250km, suffix = c("", "_buffer")) %>% 
   st_join(ussr_map, suffix = c("", "_whole")) 
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
joined_df <- joined %>% 
  st_set_geometry(NULL) %>% 
  mutate(location = case_when(!is.na(NameENG) ~ "within 250 km border buffer", 
                              !is.na(NameENG_whole) ~ "outside 250 km border buffer",
                              TRUE ~ "outside USSR"))


joined_df <- joined_df %>% 
  dplyr::select(person_id, location)

fwrite(joined_df, here::here("data/border_arrests.csv"))
```
