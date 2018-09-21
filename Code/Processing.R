# library(imager)
library(xml2)
library(XML)
library(sf)
library(purrr)
library(magrittr)
library(tidyverse)

library(furrr)
plan(multicore, workers = 40)

################################################################################
### Set up directories if not already in the environment (e.g. if this is    ###
### not called from another script)                                          ###
################################################################################
if (!exists("code_dir")) {
  code_dir <- "Code"
}

if (!exists("image_dir")) {
  image_dir <- "Images/Shoes"
}

if (!exists("annotation_dir")) {
  annotation_dir <- "Annotations/Shoes"
}

if (!exists("process_dir")) {
  process_dir <- file.path(getwd(), "OneHotContext")
}
################################################################################

source(file.path(code_dir, "Images.R"))
source(file.path(code_dir, "ParseAnnotations.R"))

files <- list.files(annotation_dir, "*.xml", full.names = T)
images <- file.path(image_dir, str_replace(basename(files), "xml", "jpg"))

df <- data_frame(
  id = 1:length(files),
  file = files,
  image = images
) %>%
  mutate(
    xml = map(file, annotationObj_extract),
    objs = map_int(xml, ~length(unlist(.)))
  ) %>%
  filter(objs > 0) %>%
  mutate(annot = future_map(xml, annotationObj_parse_rename))

# Reset future processes
plan(sequential)
gc()
plan(multicore, workers = 40)


df <- df %>%
  mutate(
    fullannot = future_map(annot, ~try(polygon_addfulllabels(.)))
  )

sapply(df$fullannot, function(x) "try-error" %in% class(x)) %>%
  which() %>% 
  paste(collapse = ", ") %>%
  paste("Errors in annotations", .) %>%
  message()

df_work <- dplyr::select(df, -xml) # make it easier to see what's going on

if (!dir.exists(file.path(process_dir, "toslice"))) {
  dir.create(file.path(process_dir, "toslice"))
}

dfunion <- dplyr::select(df_work, id, image, fullannot) %>%
  unnest() %>%
  filter(!str_detect(name, "exclude")) %>%
  mutate(geost = future_map(poly_sf, geo_stats)) %>%
  unnest(geost) %>%
  filter(area > 64^2, area / diagdist > 64 * .5) %>%
  ungroup() %>%
  group_by(image, name) %>%
  select(id, image, name, attributes, objID = id1, poly_sf, area, diagdist, 
         angle, angle_adj, mbr) %>%
  mutate(toobig = ifelse(area > 680^2, "toslice/", ""),
         mbr = sf::st_polygon(mbr)) %>% 
  group_by(image, name) %>%
  mutate(filename = sprintf("%s/%s%s-%d-%s", process_dir, toobig, name, 
                            row_number(), basename(image))) %>%
  ungroup()


# Create chunks of data frame
dfsplit <- split(dfunion, floor(dfunion$id / 10))
tmpsplit <- map(dfsplit, ~try(fix_save_imgs(.)), .progress = T)

save(tmpsplit, dfunion, df, file = "cropped_photos.Rdata")
