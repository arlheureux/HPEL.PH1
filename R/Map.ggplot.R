## _________________ Infos générales
## Nom : TS.agreg.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 18 décembre 2023
##
## Copyright (c) Arnaud Lheureux, 2023
## R version : 4.3
## _________________

## _________________ Notes
#
#' Fonction qui génère un fond de carte avec ggplot
#' @param xlim Coordonnées gps en x
#' @param ylim Coordonnées gps en y
#' @param proj.to Numerique : epsg du système vers lequel projeter
#' @param already.proj Logical : les données sont-elles deja au systeme vers lequel projeter
#' @return Un fond de carte
#' @import sf rnaturalearth rnaturalearthdata rnaturalearthhires
#' @importFrom ggplot2 geom_sf element_rect
#' @export
#
## ---------------------------

Map.ggplot <- function(xlim = c(-15,15), ylim = c(30,60), proj.to=2154, already.proj = FALSE, ...){
  
  
  if(!isTRUE(already.proj)) {
    bbox <- st_sfc(
      st_point(c(floor(min(xlim)), floor(min(ylim)))),
      st_point(c(ceiling(max(xlim)), ceiling(max(ylim)))),
      crs = 4326
    )

    bbox <- st_coordinates(st_transform(bbox, crs = paste0("epsg:",proj.to)))

  }else {
    bbox <- cbind(xlim, ylim)
  }

  
  countries <- ne_countries(scale = "large", returnclass = "sf", continent = c("Europe","Africa"))
  
  
  if(!is.null(proj.to)){
    countries <- st_transform(countries, crs = proj.to)
  }
  
  
  ggplot() + 
    geom_sf(data = countries, fill="grey20", ...) +
    xlim(bbox[,1]) + ylim(bbox[,2]) + 
    theme(panel.background = element_rect(fill="aliceblue")) 
  
}
