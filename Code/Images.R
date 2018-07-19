library(sf)
# library(imager)
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

# img <- imager::load.image(
#   stringr::str_replace(df$file[1], "Annotations", "Images") %>% 
#     stringr::str_replace("xml", "jpg"))
# 
# poly <- df$annotation[[1]]$poly_sf[[1]]

mask_polygon <- function(img, poly, invert = F, ret = "image", plotres = T) {
  stopifnot(ret %in% c("image", "mask"))
  
  mask <- as.data.frame(imager::as.cimg(img))
  # flipdf <- mirror(img, "y") %>% as.data.frame()
  
  polymat <- as.matrix(poly)
  if (max(dim(polymat)) == 1) {
    polymat <- as(polymat[[1]], "matrix")
  }
  
  polycoords <- polymat %>%
    as.data.frame() %>% 
    magrittr::set_names(c("x", "y"))
  
  if (invert) {
    mask$value <- sp::point.in.polygon(mask$x, mask$y, polycoords$x, polycoords$y)
  } else {
    mask$value <- !sp::point.in.polygon(mask$x, mask$y, polycoords$x, polycoords$y)
  }
  
  mask <- imager::as.cimg(mask, dims = dim(img)) %>% imager::threshold(.1)
  tmp <- img
  tmp[mask == 0] <- 1
  
  if (ret == "image") {
    if (plotres) {
      plot(tmp)
    }
    return(tmp)
  } 
  
  if (ret == "mask") {    
    if (plotres) {
      plot(mask)
    }
    return(mask)
  }
}

fix_img <- function(im, poly, invert, ret, angle) {
  if (is.character(im)) im <- imager::load.image(im)
  
  try({
    x <- mask_polygon(img = im, poly = poly, ret = ret, invert = invert, plotres = F) %>%
      imager::imrotate(angle = angle, boundary = 1) %>%
      imager::autocrop()
    
    # plot(x)
    x
  })
}

fix_img_all <- function(impath, imagedf, ret = "image", invert = T, angle = 0) {
  im <- imager::load.image(impath)
  
  fix_img_sub <- partial(fix_img, im = im)
  select(imagedf, poly, angle) %>%
    mutate(ret = ret, invert = invert) %>%
    pmap(fix_img_sub)
}

img_split_resize <- function(im, xsize = 256, ysize = 256, interpolation_type = 1, boundary_conditions = 0) {
  splitimgs <- imager::imsplit(im, axis = "x", nb = -xsize) %>%
    imager::as.imlist() %>%
    map(imager::imsplit, axis = "y", nb = -ysize) %>%
    unlist(recursive = F) %>%
    imager::as.imlist()
  
  resizeimgs <- splitimgs %>%
    map(imager::resize, size_x = xsize, size_y = ysize, 
        interpolation_type = interpolation_type, 
        boundary_conditions = boundary_conditions) %>%
    imager::as.imlist()
  
  enough_to_resize_idx <- sapply(splitimgs, function(x) prod(dim(x)[1:2] >= c(xsize/10, ysize/10)) == 1)
  color_variation_idx <- sapply(resizeimgs, function(x) length(unique(x)) >= 50)
  
  resizeimgs[enough_to_resize_idx & color_variation_idx]
}

