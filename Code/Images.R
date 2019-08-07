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

color_px <- function(im, del = 0) {
  abs(im - c(.5, .5, .5)) < (c(.5, .5, .5) - del)
}

fix_rotate <- function(impoly, angle, ...) {
  r1 <- impoly %>%
    imager::autocrop() %>%
    imager::imrotate(angle = -angle, ...)

  r1bbox <- r1 %>%
    color_px() %>%
    imager::clean(px = ., x = 10, boundary = T)

  r1 <- r1 %>%
    imager::crop.bbox(r1bbox)

  r2 <- impoly %>%
    imager::autocrop() %>%
    imager::imrotate(angle = angle, ...)

  r2bbox <- r2 %>%
    color_px() %>%
    imager::clean(px = ., x = 10, boundary = T)

  r2 <- r2 %>%
    imager::crop.bbox(r2bbox)

  if (prod(dim(r1)[1:2]) < prod(dim(r2)[1:2])) {
    return(r1)
  } else {
    return(r2)
  }
}

median_cc <- function(image, px) {
  imager::colorise(image, px, median(image[!px], na.rm = T))
}

fix_border <- function(imx) {
  bw_im <- (!color_px(imx, del = .01)) # %>%
  # imager::clean(x = 2, boundary = T)
  if (mean(bw_im) > 0) {
    imret <- imx # %>%
    # imager::imsplit(axis = 'c') %>%
    # imager::map_il(median_cc, px = bw_im) %>%
    # imager::imappend(axis = 'c')
  } else {
    imret <- imx
  }
  imret
}

fix_img <- function(im, poly, invert, ret, angle) {
  if (is.character(im)) im <- imager::load.image(im)

  try({
    x <- mask_polygon(img = im, poly = poly, ret = ret, invert = invert, plotres = F) %>%
      fix_rotate(angle = angle, boundary = 0) %>%
      imager::crop.borders(nPix = 1) %>%
      fix_border() %>%
      imager::autocrop()

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
    map(imager::resize,
      size_x = xsize, size_y = ysize,
      interpolation_type = interpolation_type,
      boundary_conditions = boundary_conditions
    ) %>%
    imager::as.imlist()

  enough_to_resize_idx <- sapply(splitimgs, function(x) prod(dim(x)[1:2] >= c(xsize / 10, ysize / 10)) == 1)
  color_variation_idx <- sapply(resizeimgs, function(x) length(unique(x)) >= 50)
  nonwhite_idx <- sapply(resizeimgs, function(x) {
    sum(imager::grayscale(x) != 1) > (.75 * 256 * 256)
  })

  resizeimgs[enough_to_resize_idx & color_variation_idx & nonwhite_idx]
}

img_contrast_fix <- function(im, contrast_level = 64) {
  cor_factor <- 259 * (contrast_level + 255) / (255 * (259 - contrast_level))
  adj <- ifelse(max(im) <= 1, .5, 128)

  res <- im %>%
    imsplit("c") %>%
    purrr::map(~(cor_factor * (. - adj) + adj)) %>%
    imappend("c")

  res[res < 0] <- 0
  res[res > 1] <- 1

  res
}

im_hist_normalize <- function(im) {
  res <- im %>%
    imsplit("c") %>%
    purrr::map(~(. - min(.)) / (max(.) - min(.))) %>%
    imappend("c") %T>%
    plot()
}

im_gamma_cor <- function(im, gamma = 2.2) {
  gcor <- 1 / gamma
  im %>%
    imsplit("c") %>%
    purrr::map(~.^gcor) %>%
    imappend("c")
}

equalize <- function(x) {
  # Stolen from EBImage and modified
  if (max(x) > 1) { # Assume LAB
    range <- c(0, 100)
  } else {
    range <- c(0, 1)
  }

  levels <- 1024
  r <- range(x)
  if (r[1L] == r[2L]) {
    warning("Image histogram is single-valued and cannot be equalized.")
    x
  } else {
    if (!is.numeric(range) || length(range) != 2L) {
      stop("'range' must be a numeric vector of length 2.")
    }

    levels <- as.integer(levels)
    if (is.na(levels) || levels < 1L) {
      stop("Levels must be at least equal 1.")
    }
    breaks <- seq(range[1L], range[2L], length.out = levels + 1L)
    d <- dim(x)
    n <- d[1L] * d[2L]

    # equalize each frame separately
    .equalize <- function(y) {
      h <- hist.default(y, breaks = breaks, plot = FALSE)
      cdf <- cumsum(h$counts)
      cdf_min <- min(cdf[cdf > 0])

      equalized <- ((cdf - cdf_min) / (prod(dim(y)) - cdf_min) * (range[2L] - range[1L])) + range[1L]
      bins <- round((y - range[1L]) / (range[2L] - range[1L]) * (levels - 1L)) + 1L

      res <- equalized[bins]
    }

    ld <- length(d[d != 1])
    res <- if (ld > 2L) {
      apply(x, 3L:ld, .equalize)
    } else {
      .equalize(x)
    }

    imager::as.cimg(res, dim = d)
  }
}

im_equalize <- function(im, plot_res = F) {
  tmp <- im %>%
    imager::RGBtoHSL() %>%
    imager::imsplit("c")

  total_var <- purrr::map_dbl(imager::imsplit(im, "c"), var) %>% sum()
  if (var(tmp[[3]]) > 0.0001 & total_var > 0.001) {
    tmp[[3]] <- equalize(tmp[[3]])
  }
  res <- tmp %>%
    imager::imappend("c") %>%
    imager::HSLtoRGB()

  if (plot_res) {
    plot(imlist(im, res), layout = "row")
  }
  res
}

fix_save_imgs <- function(mydf, correct = c("histogram")) { # mydf is a chunk of dfunion
  img_fixed <- tibble(
    im = mydf$image,
    poly = mydf$mbr,
    invert = T,
    ret = "image",
    angle = mydf$angle
  ) %>%
    future_pmap(fix_img, .progress = T)

  err <- map_lgl(img_fixed, ~"try-error" %in% class(.))
  if (sum(err) > 0) {
    errors <- mydf[err, ]
    warning(sprintf("Could not generate %d images", sum(err)))
  }

  if ("contrast" %in% correct) {
    img_fixed[!err] <- img_fixed[!err] %>%
      future_map(img_contrast_fix, .progress = T)
  }

  if ("histogram" %in% correct) {
    img_fixed[!err] <- img_fixed[!err] %>%
      future_map(im_equalize, .progress = T)
  }

  rightsize <- future_map(
    img_fixed[!err],
    ~imager::resize(im = ., size_x = 256, size_y = 256, interpolation_type = 1)
  )

  future_map2(rightsize, mydf$filename[!err], ~imager::save.image(.x, .y, quality = 1))

  mydf$error <- err
  mydf
}
