library(magrittr)
library(tidyverse)
library(htmltidy)
library(purrr)
library(xml2)
library(XML)
library(sf)
library(sp)



getIntersectionPolys <- function(annot_df) {
  stopifnot("name" %in% names(annot_df))
  stopifnot("poly_sf" %in% names(annot_df))

  annot_df <- filter(annot_df, sapply(annot_df$poly_sf, length) > 0)

  if (nrow(annot_df) == 0) {
    return(NULL)
  }


  poly <- annot_df$poly_sf
  name <- annot_df$name

  # Convert to geometry column
  if (!"sfc" %in% class(poly)) {
    poly2 <- try(st_as_sfc(poly))
    if (!"try-error" %in% class(poly2)) {
      poly <- poly2
    }
  }

  tmp <- data_frame(name, poly)

  # Create Spatial Polygons
  tmpsp <- as(tmp$poly, "Spatial")
  tmpsp <- rgeos::gBuffer(tmpsp, byid = T, width = 0)
  class(tmp) <- "data.frame"
  tmp$ID <- row.names(tmpsp)
  row.names(tmp) <- row.names(tmpsp)
  tmpspdf <- SpatialPolygonsDataFrame(Sr = tmpsp, data = select(tmp, name, ID))


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
        str_split(x, ", ", simplify = T) %>%
          sort() %>%
          paste(collapse = ", ")
    )
    tmpintersections@data$ID1 <- as.numeric(str_replace(tmpintersections@data$ID.1, "ID", ""))
    tmpintersections@data$ID2 <- as.numeric(str_replace(tmpintersections@data$ID.2, "ID", ""))

    tmpintersections <- subset(tmpintersections, ID1 >= ID2)

    tmpintersections@data <- select(tmpintersections@data, -ID1, -ID2)
  }


  as(tmpintersections, "sf")
}

# Get df info
#
# df$intersections <- map(df$annotation, getIntersectionPolys)
