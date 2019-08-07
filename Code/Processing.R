# library(imager)
library(xml2)
library(XML)
library(sf)
library(purrr)
library(magrittr)
library(imager)
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

if (!exists("annot_dir")) {
  annot_dir <- "Annotations/Shoes"
}

if (!exists("process_dir")) {
  format(lubridate::now(), "%Y%m%d-%H%M%S")
}

if (!exists("contrast_correct")) {
  contrast_correct <- F
}
################################################################################

source(file.path(code_dir, "Images.R"))
source(file.path(code_dir, "ParseAnnotations.R"))

files <- list.files(annot_dir, "*.xml", full.names = T)
images <- file.path(image_dir, str_replace(basename(files), "xml", "jpg"))


# Set up data frame of files
df <- tibble(
  id = 1:length(files),
  file = files,
  image = images
) %>%
  mutate(
    xml = map(file, ~try(annotationObj_extract(.))),
    objs = map_int(xml, ~length(unlist(.)))
  ) %>%
  filter(objs > 0) %>%
  mutate(annot = future_map(xml, annotationObj_parse_rename))

# Reset future processes
plan(sequential)
gc()
plan(multicore, workers = 40)


# Add labels to polygons
df <- df %>%
  mutate(
    fullannot = future_map(annot, ~try(polygon_addfulllabels(.)))
  )

msg <- sapply(df$fullannot, function(x) "try-error" %in% class(x)) %>%
  which()
if (length(msg) > 0) {
  msg %>%
    paste(collapse = ", ") %>%
    paste("Errors in annotations", .) %>%
    message()
}
rm(msg)

df_work <- dplyr::select(df, -xml) %>% # make it easier to see what's going on
  mutate(annot_ok = sapply(fullannot, is.data.frame)) %>%
  filter(annot_ok)

if (!dir.exists(file.path(process_dir, "toslice"))) {
  dir.create(file.path(process_dir, "toslice"))
}
if (!dir.exists(file.path(process_dir, "images"))) {
  dir.create(file.path(process_dir, "images"))
}

dfunion <- dplyr::select(df_work, id, image, fullannot) %>%
  unnest() %>%
  filter(!str_detect(name, "exclude")) %>%
  mutate(geost = future_map(poly_sf, geo_stats)) %>%
  unnest(geost) %>%
  filter(area > 96^2) %>%
  filter(area < 512*512) %>%
  ungroup() %>%
  group_by(image, name) %>%
  select(id, image, name, attributes, objID = id1, poly_sf, area, diagdist,
         angle, angle_adj, mbr) %>%
  mutate(mbr = sf::st_polygon(mbr)) %>%
  ungroup() %>%
  mutate(name = str_remove(name, "^_") %>% str_remove("_$")) %>%
  group_by(image, name) %>%
  mutate(filename = sprintf("%s/images/%s-%d-%s", process_dir, name,
                            row_number(), basename(image))) %>%
  ungroup()

# Don't save things that are already there
existing_files <- list.files(process_dir, full.names = T, recursive = T)
existing_files <- existing_files[str_detect(existing_files, "images/")]

dfunion_unsaved <- dfunion %>%
  filter(!filename %in% existing_files)

# Create chunks of data frame
dfsplit <- split(dfunion_unsaved, floor(dfunion_unsaved$id / 10))
tmpsplit <- map(dfsplit, ~try(fix_save_imgs(.)), .progress = T)

save(tmpsplit, dfunion, df,
     file = file.path(process_dir, "cropped_photos.Rdata"))


# Reset future processes
plan(sequential)
gc()
