#' @export

#esta função serve para ver o mapa 3d da area da cordenada escolhida

mapa <- function(){

  library(roxygen2)
  library(raster)
  library(rayshader)
  library(geoviz)
  library(tibble)
  library(dplyr)
  library(ggplot2)
  library(devtools)
  library(rayshader)
  library(magick)


  #Escolher latitude e longitude da area que deseja fazer o mapa
  lat = -8.05428
  lon = -34.8813
  #Designar o raio desejado em km²
  square_km = 5
  #Aumentar o max_tiles vai resultar em uma imagem com maior definição
  max_tiles = 60

  dem <- mapzen_dem(lat, lon, square_km, max_tiles = max_tiles)

  #Mude o image_type para "terrain", "toner" ou "watercolor"
  overlay_image <-
    slippy_overlay(dem,
                   image_source = "stamen",
                   image_type = "terrain",
                   png_opacity = 0.3,
                   max_tiles = max_tiles)

  #Renderizar com o rayshader
  heightmap = matrix(
    raster::extract(dem, raster::extent(dem), method = 'bilinear'),
    nrow = ncol(dem),
    ncol = nrow(dem)
  )
  #As texturas diponiveis são: "imhof1", "imhof2", "imhof3", "imhof4", "desert", "bw" e "unicorn"
  sunshade <- heightmap %>%
    sphere_shade(sunangle = 270, texture = "imhof4") %>%
    add_overlay(overlay_image)

  rayshader::plot_3d(
    sunshade,
    heightmap,
    zscale = raster_zscale(dem) / 3,
    solid = TRUE,
    shadow = TRUE,
    soliddepth = -raster_zscale(dem),
    water=TRUE,
    waterdepth = 0.5,
    wateralpha = 0.5,
    watercolor = "lighblue",
    waterlinecolor = "white",
    waterlinealpha = 0.5
  )

  #Adicionando escalas
  render_scalebar(limits=c(0, 5),label_unit = "km",position = "W", y=50,scale_length = c(0.10,1))

  render_highquality(samples=200, scale_text_size = 24,clear=TRUE)
  rayshader::render_depth(
    filename = "Mapa3d.png")

}
