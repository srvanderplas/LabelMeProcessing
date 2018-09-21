# library(imager)
library(xml2)
library(XML)
library(sf)
library(purrr)
library(magrittr)
library(tidyverse)

library(furrr)
plan(multicore, workers = 40)

source("Code/Images.R")
source("Code/ParseAnnotations.R")

# files <- list.files("Annotations/Shoes", "*.xml", full.names = T) #%>% sample(size = 150)
# images <- str_replace(files, "Annotations", "Images") %>% str_replace("xml", "jpg")
# file.copy(files, file.path("Test", "Annotations", basename(files)))
# file.copy(images, file.path("Test", "Images", basename(images)))

files <- list.files("Annotations/Shoes", "*.xml", full.names = T)
images <- str_replace(files, "Annotations", "Images") %>% str_replace("xml", "jpg")

df <- data_frame(
  id = 1:length(files),
  file = files,
  image = images
) %>%
  # filter(row_number() < 500) %>%
  mutate(
    xml = map(file, annotationObj_extract),
    objs = map_int(xml, ~length(unlist(.)))
  ) %>%
  filter(objs > 0) %>%
  mutate(annot = future_map(xml, annotationObj_parse_rename))

# Reset future processes
# plan(sequential)
# gc()
# plan(multiprocess, workers = 40)

# dfunnest <- df %>% select(-xml) %>% unnest()

df <- df %>%
  mutate(
    fullannot = future_map(annot, ~try(polygon_addfulllabels(.)))
    # intersection = future_map(annot, polygon_intersect),
    # intersect_segs = future_map_int(intersection, nrow)
  )

which(sapply(df$fullannot, function(x) "try-error" %in% class(x)))

df_work <- dplyr::select(df, -xml) # make it easier to see what's going on

dfunion <- dplyr::select(df_work, id, image, fullannot) %>%
  # filter(row_number() < 200) %>%
  unnest() %>%
  filter(!str_detect(name, "exclude")) %>%
  mutate(geost = future_map(poly_sf, geo_stats)) %>%
  unnest(geost) %>%
  filter(area > 64^2, area / diagdist > 64 * .5) %>%
  ungroup() %>%
  group_by(image, name) %>%
  select(id, image, name, attributes, objID = id1, poly_sf, area, diagdist, angle, angle_adj, mbr) %>%
  mutate(toobig = ifelse(area > 680^2, "toslice/", ""),
         mbr = sf::st_polygon(mbr)) %>% 
  group_by(image, name) %>%
  mutate(filename = sprintf("OneHotContext/%s%s-%d-%s", toobig, name, row_number(), basename(image))) %>%
  ungroup()

# dfintersect <- dplyr::select(df_work, id, image, intersection) %>%
#   unnest() %>%
#   filter(name.1 != "exclude" & name.2 != "exclude" & name.1 != "other" & name.2 != "other") %>%
#   mutate(geost = map(geometry, geo_stats)) %>%
#   unnest(geost) %>%
#   filter(area > 128^2, area/diagdist > 128*.5)

# Create chunks of data frame
dfsplit <- split(dfunion, floor(dfunion$id / 10))
tmpsplit <- map(dfsplit, ~try(fix_save_imgs(.)), .progress = T)

save(tmpsplit, dfunion, df, file = "cropped_photos.Rdata")
