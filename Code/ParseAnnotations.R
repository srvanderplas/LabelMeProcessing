library(magrittr)
library(tidyverse)
library(htmltidy)
library(purrr)
library(xml2)
library(XML)
library(sf)

annotationObj_parseOne <- function(x) { 
  if (is.list(x)) x <- unlist(x)
  
  # class(x) <- c("XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode")
  
  # Get regular info
  names <- xpathSApply(x, './/.', xmlName) 
  names <- names[which(names == "text") - 1]
  idx <- which(!names %in% c("x", "y"))
  values <- xpathSApply(x, ".//text()", xmlValue)
  names(values) <- names
  
  # Get points
  xx <- xpathSApply(x, "polygon/pt/x", xmlValue) %>% as.numeric
  yy <- xpathSApply(x, "polygon/pt/y", xmlValue) %>% as.numeric
  
  as_data_frame(t(values[idx])) %>%
    mutate(poly_sf = matrix(c(xx, yy), ncol = 2, byrow = F) %>%
             rbind(., .[1,]) %>%
             list() %>%
             st_polygon() %>%
             st_sfc())
}

annotationObj_parseAll <- function(x) {
  map_df(unlist(x), annotationObj_parseOne)
}

annotationObj_extract <- function(x) {
  y <- x %>% read_xml %>% xmlParse %>% magrittr::extract("//annotation/object")
  data_frame(obj = y)
}

files <- list.files("Annotations/Shoes", "*.xml", full.names = T) %>% sample(size = 15)

df <- data_frame(file = files) %>%
  mutate(xml = map(file, annotationObj_extract),
         annotation = map(xml, annotationObj_parseAll)) 

