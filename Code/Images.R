library(sf)
library(raster)
library(imager)
library(tidyverse)

# Make empty image

# blankimg <- as.cimg(matrix(255, nrow = 8, ncol = 8))
# blankimg_df <- as.data.frame(blankimg)
# blankimg_points <- st_multipoint(x = as.matrix(blankimg_df)) %>%
#   st_sfc() %>%
#   st_cast("POINT")
# 
# polycoords <- matrix(c(3, 1, 1, 3, 3, 3, 3, 1), ncol = 2, byrow = T)
# polymask <- st_polygon(list(polycoords))
# 
# maskimg <- !st_intersects(blankimg_points, polymask, sparse = F) 
# 
# blankimg_df$value <- maskimg*255
# 
# 
# ggplot() + 
#   geom_raster(aes(x = x, y = y), fill = "white", data = blankimg_df) + 
#   geom_tile(aes(x = x, y = y, fill = as.character(value)), color = "black", data = blankimg_df) + 
#   geom_sf(aes(fill = "Polygon"), data = st_polygon(list(polycoords)))

img <- load.image(str_replace(df$file[1], "Annotations", "Images") %>% str_replace("xml", "jpg"))

poly <- df$annotation[[1]]$poly_sf[[1]]

mask_polygon <- function(img, poly, invert = F) {
  
  mask <- as.data.frame(img)
  # flipdf <- mirror(img, "y") %>% as.data.frame()
  
  polycoords <- as.matrix(poly) %>%
    as.data.frame() %>% 
    set_names(c("x", "y"))
  
  if (invert) {
    mask$value <- point.in.polygon(mask$x, mask$y, polycoords$x, polycoords$y)
  } else {
    mask$value <- !point.in.polygon(mask$x, mask$y, polycoords$x, polycoords$y)
  }
  
  mask <- as.cimg(mask, dims = dim(img)) %>% threshold()
  tmp <- img
  tmp[mask == 0] <- 1
  plot(tmp)
}
