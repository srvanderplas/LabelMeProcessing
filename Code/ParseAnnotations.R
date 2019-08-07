library(xml2)
library(XML)
library(sf)
library(sp)
library(magrittr)
library(tidyverse)
library(purrr)

fix_names <- function(x) {
  if (is.na(x)) return("")
  y <- str_split(x, pattern = "_|(, )", simplify = T) %>%
    str_trim()
  z <- str_replace_all(y, c(
    "(.*)s$" = "\\1",
    "quardilateral|quarilateral|qudarilateral|qiadro.*|_atera_quad|quadrilateral|qaud|qud|rectangle" = "quad",
    "^qua$" = "quad",
    "othre|othere|iother|oter" = "other",
    "cirlce|cricle|cirle" = "circle",
    "^quad(.*)" = "quad",
    "^square" = "quad",
    "pengaton|pentagon" = "polygon",
    "octagon" = "polygon",
    "lines|(curved line)" = "line",
    "hexagon|heagon" = "polygon",
    "ribb?on" = "ribbon",
    "^(tet|ext|texdt|etxt)" = "text",
    "ttriangle|trianlge|triangel" = "triangle",
    "triangl$" = "triangle",
    "star|stars|start" = "star",
    "cheron" = "chevron",
    "bowite|bootie|blowtie" = "bowtie",
    "^_exclude" = "exclude",
    "exclud$" = "exclude",
    "circlemtraignle" = "circle,triangle",
    "eclude|exclulde|exxclude|exclude|exlcude|ecxlude|remove|excluded" = "exclude"
  ))
  z %>%
    unique() %>%
    sort() %>%
    paste(collapse = "_")
}

fix_attrs <- function(x) {
  if (is.na(x)) return("")
  y <- str_split(x, ",", simplify = T) %>% str_trim()
  z <- str_replace_all(y, c(
    "curved|rouded|rounding|rrounded" = "rounded",
    "texdture|texture" = "texture",
    "texture_ " = "texture_",
    "crep$|creep|crpee" = "crepe",
    "lines" = "line",
    "dotss|dots|dotted" = "dot",
    "segemented" = "segmented",
    "smoothh|smooth|smoot$|smoth|smotoh" = "smooth",
    "texture_" = "texture|",
    "^smooth$" = "texture|smooth"
  ))
  z %>%
    unique() %>%
    sort() %>%
    paste(collapse = ",")
}

merge_name_attr <- function(x, y) {
  # Needs to be before names are merged
  xl <- str_split(x, "_|,", simplify = T) %>% str_trim()
  yl <- str_split(y, ",", simplify = T) %>% str_trim()

  ylattr <- NULL
  if ("rounded" %in% yl) {
    ylattr <- c(ylattr, "R")
  }
  if ("elongated" %in% yl) {
    ylattr <- c(ylattr, "E")
  }

  textures <- yl[grepl("texture", yl)]
  # Remove circle from useful textures
  useful_textures <- textures[grepl("line", textures)] %>%
    str_replace("texture\\|", "")
  if (length(ylattr) > 0) {
    ylattr <- sprintf("%s(%s)", xl, paste(ylattr, collapse = ""))
  } else {
    ylattr <- xl
  }

  c(ylattr, useful_textures) %>% paste(collapse = "_")
}

bbox_to_polygon <- function(bbox) {
  if (is.list(bbox)) bbox <- unlist(bbox)

  stopifnot(
    "xmin" %in% names(bbox),
    "xmax" %in% names(bbox),
    "ymin" %in% names(bbox),
    "ymax" %in% names(bbox)
  )
  xc <- bbox[c("xmin", "xmin", "xmax", "xmax")] %>% as.numeric()
  yc <- bbox[c("ymin", "ymax", "ymax", "ymin")] %>% as.numeric()
  matrix(c(xc, yc), ncol = 2, byrow = F) %>%
    rbind(., .[1, ]) %>%
    list() %>%
    st_polygon() %>%
    st_sfc()
}

annotationObj_extract <- function(x) {
  y <- x %>% read_xml() %>% xmlParse() %>% magrittr::extract("//annotation/object")
  y
}

annotationObj_parseOne <- function(x) {
  if (is.list(x)) x <- unlist(x)
  if (length(x) == 0) return(NULL)
  # class(x) <- c("XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode")

  # Get regular info
  allnames <- xpathSApply(x, ".//.", xmlName)
  names <- allnames[which(allnames == "text") - 1]
  idx <- which(!names %in% c(
    "x", "y", "xmin", "ymin",
    "xmax", "ymax", "mask", "scribble_name"
  ))
  values <- xpathSApply(x, ".//text()", xmlValue)
  names(values) <- names

  df <- as_tibble(t(values[idx]))

  if ("polygon" %in% allnames) {
    # Get points
    xx <- xpathSApply(x, "polygon/pt/x", xmlValue) %>% as.numeric()
    yy <- xpathSApply(x, "polygon/pt/y", xmlValue) %>% as.numeric()

    if (length(xx) >= 3) {
      df <- df %>%
        mutate(
          poly_sf = matrix(c(xx, yy), ncol = 2, byrow = F) %>%
            rbind(., .[1, ]) %>%
            list() %>%
            st_polygon() %>%
            st_sfc(),
          mask = ""
        )
    } else {
      df <- df %>%
        mutate(poly_sf = NA)
    }
  }

  if ("segm" %in% allnames) {
    df <- df %>%
      bind_cols(
        tibble(mask = xpathSApply(x, "segm/mask", xmlValue)),
        xpathSApply(x, "segm/box/*", xmlValue) %>%
          as.numeric() %>%
          t() %>%
          as_tibble() %>%
          set_names(xpathSApply(x, "segm/box/*", xmlName)) %>%
          nest(.key = "bbox"),
        xpathSApply(x, "segm/scribbles/*[self::xmin or self::xmax or self::ymin or self::ymax]", xmlValue) %>%
          as.numeric() %>%
          t() %>%
          as_tibble() %>%
          set_names(xpathSApply(x, "segm/scribbles/*[self::xmin or self::xmax or self::ymin or self::ymax]", xmlName)) %>%
          mutate(scribble_mask = xpathSApply(x, "segm/scribbles/scribble_name", xmlValue)) %>%
          nest(.key = "scribble")
      ) %>%
      mutate(
        poly_sf = bbox_to_polygon(bbox)
      )
  }

  df
}

annotationObj_parseAll <- function(x) {
  if (length(x) == 0) {
    return(tibble())
  }
  map_df(unlist(x), annotationObj_parseOne) %>%
    mutate(poly_sf = st_as_sfc(poly_sf))
}

annotationObj_parse_rename <- function(x) {
  y <- annotationObj_parseAll(x)
  if (nrow(y) == 0) {
    return(tibble())
  }
  if (!"attributes" %in% names(y)) {
    y$attributes <- NA
  }

  y %>%
    mutate(
      origname = name,
      name2 = map_chr(name, fix_names),
      attributes2 = map_chr(attributes, fix_attrs)
    ) %>%
    mutate(name = map2_chr(name2, attributes2, merge_name_attr)) %>%
    mutate(name = str_replace(name, "other_(.*)", "\\1")) %>%
    filter(deleted == 0)
}

polygon_intersect <- function(annot_df) {
  stopifnot("name" %in% names(annot_df))
  stopifnot("poly_sf" %in% names(annot_df))

  annot_df <- dplyr::filter(annot_df, sapply(annot_df$poly_sf, length) > 0)

  if (nrow(annot_df) == 0) {
    return(NULL)
  }


  poly <- annot_df$poly_sf
  name <- annot_df$name

  # Convert to geometry column
  if (!"sfc" %in% class(poly)) {
    poly2 <- try(sf::st_as_sfc(poly))
    if (!"try-error" %in% class(poly2)) {
      poly <- poly2
    }
  }

  tmp <- dplyr::tibble(name, poly)

  # Create Spatial Polygons
  tmpsp <- methods::as(tmp$poly, "Spatial")
  tmpsp <- rgeos::gBuffer(tmpsp, byid = T, width = 0)
  class(tmp) <- "data.frame"
  tmp$ID <- row.names(tmpsp)
  row.names(tmp) <- row.names(tmpsp)
  tmpspdf <- sp::SpatialPolygonsDataFrame(Sr = tmpsp, data = dplyr::select(tmp, name, ID))


  # Get intersections that aren't with self() or something of the same label
  tmpintersections <- raster::intersect(tmpspdf, tmpspdf) %>%
    subset(name.1 != name.2)

  if (length(tmpintersections) > 0) {
    tmpintersections@data$name <- with(
      tmpintersections@data,
      paste0(name.1, ", ", name.2)
    )
    tmpintersections@data$name <- sapply(
      tmpintersections@data$name,
      function(x)
        stringr::str_split(x, ", ", simplify = T) %>%
          sort() %>%
          paste(collapse = ", ")
    )
    tmpintersections@data$ID1 <- as.numeric(stringr::str_replace(tmpintersections@data$ID.1, "ID", ""))
    tmpintersections@data$ID2 <- as.numeric(stringr::str_replace(tmpintersections@data$ID.2, "ID", ""))

    tmpintersections <- subset(tmpintersections, ID1 >= ID2)

    tmpintersections@data <- dplyr::select(tmpintersections@data, -ID1, -ID2)
  }


  methods::as(tmpintersections, "sf")
}

MBR <- function(geo) {
  points <- unique(as.matrix(geo))
  # Analyze the convex hull edges
  a <- alphahull::ashape(points, alpha = 1000) # One way to get a convex hull...
  e <- a$edges[, 5:6] - a$edges[, 3:4] # Edge directions
  norms <- apply(e, 1, function(x) sqrt(x %*% x)) # Edge lengths
  v <- diag(1 / norms) %*% e # Unit edge directions
  w <- cbind(-v[, 2], v[, 1]) # Normal directions to the edges

  # Find the MBR
  vertices <- (points) [a$alpha.extremes, 1:2] # Convex hull vertices
  minmax <- function(x) c(min(x), max(x)) # Computes min and max
  x <- apply(vertices %*% t(v), 2, minmax) # Extremes along edges
  y <- apply(vertices %*% t(w), 2, minmax) # Extremes normal to edges
  areas <- (y[1, ] - y[2, ]) * (x[1, ] - x[2, ]) # Areas
  k <- which.min(areas) # Index of the best edge (smallest area)

  # Form a rectangle from the extremes of the best edge
  cbind(x[c(1, 2, 2, 1, 1), k], y[c(1, 1, 2, 2, 1), k]) %*% rbind(v[k, ], w[k, ])
}

angle_calc <- function(mbr) {
  idx <- order(mbr[1:4, 2])[1:2]
  x <- mbr[idx, ]
  (180 / pi * atan2(x[2, 2] - x[1, 2], x[2, 1] - x[1, 1]))
}

angle_adjust <- function(angle) {
  angle %>%
    (function(y) ifelse(abs(y) > 90, sign(y) * (abs(y) - 180), y)) %>%
    (function(y) ifelse(abs(y) > 45, -sign(y) * (90 - abs(y)), y))
}

geo_stats <- function(geo) {
  mbrmat <- geo %>% MBR()
  tibble(
    # area = raster::area(as(geo, "Spatial")),
    area = st_area(geo),
    mbr = st_polygon(list(mbrmat)),
    bbox_area = st_area(mbr),
    diagdist = dist(mbrmat[c(1, 3), ]),
    angle = angle_calc(mbr = mbrmat),
    angle_adj = angle_adjust(angle)
  )
}

polygon_addfulllabels <- function(annot_df) {
  stopifnot("name" %in% names(annot_df))
  stopifnot("poly_sf" %in% names(annot_df))

  annot_df <- dplyr::filter(annot_df, sapply(annot_df$poly_sf, length) > 0)

  if (nrow(annot_df) == 0) {
    return(NULL)
  }

  poly <- annot_df$poly_sf
  name <- annot_df$name

  # Convert to geometry column
  if (!"sfc" %in% class(poly)) {
    poly2 <- try(sf::st_as_sfc(poly))
    if (!"try-error" %in% class(poly2)) {
      poly <- poly2
    }
  }

  tmp <- tibble::tibble(name, poly) %>%
    filter(sf::st_area(poly) > 0)

  # Create Spatial Polygons
  tmpsp <- methods::as(tmp$poly, "Spatial")
  tmpsp <- rgeos::gBuffer(tmpsp, byid = T, width = 0)
  class(tmp) <- "data.frame"
  tmp$ID <- row.names(tmpsp)
  tmp$area <- raster::area(tmpsp)
  row.names(tmp) <- row.names(tmpsp)
  tmpspdf <- sp::SpatialPolygonsDataFrame(Sr = tmpsp, data = dplyr::select(tmp, name, ID, area))


  # Get intersections that aren't with self() or something of the same label
  tmpintersections <- raster::intersect(tmpspdf, tmpspdf) %>%
    subset(name.1 != name.2)
  tmpintersectdata <- tmpintersections %>%
    attr("data")

  if (length(tmpintersections) > 0) {
    tmpintersectdata <- tmpintersectdata %>%
      as_() %>%
      mutate_at(vars(starts_with("ID")), ~str_replace(., "ID", "")) %>%
      mutate(
        area_int = raster::area(tmpintersections),
        pct1 = area_int / area.1, pct2 = area_int / area.2
      ) %>%
      # Keep only tags which overlap by more than 40%
      filter(pct1 > .4 | pct2 > .4)
  }

  sort_names <- function(x) {
    sample(x, length(x), replace = F)
  }

  if (nrow(tmpintersectdata) > 0) {
    tmpintersectdata <- tmpintersectdata %>%
      dplyr::group_by(name.1, ID.1) %>%
      dplyr::summarize(fullname = sort_names(fix_names(unique(c(name.1, name.2)))) %>%
        paste(collapse = "_")) %>%
      dplyr::rename(name = name.1, id = ID.1)
  } else {
    tmpintersectdata <- tibble(name = tmpintersectdata$name.1, id = tmpintersectdata$ID.1, fullname = tmpintersectdata$name.1)
  }

  left_join(annot_df, tmpintersectdata, by = c("name", "id")) %>%
    mutate(fullname = ifelse(is.na(fullname), name, fullname))
}



# files <- list.files("Annotations/Shoes", "*.xml", full.names = T) %>% sample(size = 50)
#
# df <- tibble(file = files) %>%
#   mutate(xml = map(file, annotationObj_extract),
#          annotation = map(xml, annotationObj_parseAll))
