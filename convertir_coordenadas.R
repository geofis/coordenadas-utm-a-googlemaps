library(sf)

generar_coords_url_google <- function(cadena) {
  
  extraer_coords_utm <- function(cadena) {
    r1 <- gsub('E|N', '', cadena)
    r2 <- as.numeric(strsplit(r1, ',')[[1]])
    r3 <- data.frame(E=r2[1], N=r2[2])
    salida <- r3
    return(salida)
  }
  
  coords_utm <- extraer_coords_utm(cadena = cadena)
  
  generar_coords_dd <- function(coords_utm) {
    r1 <- st_as_sf(coords_utm, coords = c('E', 'N'), crs = "EPSG:32619")
    r2 <- st_transform(r1, crs = "EPSG:4326")
    r3 <- c(r2$geometry[[1]][[1]], r2$geometry[[1]][[2]])
    salida <- r3
    return(salida)
  }
  
  coords_dd <- generar_coords_dd(coords_utm = coords_utm)
  
  generar_url <- function(coords_dd) {
    r1 <- paste0('https://maps.google.com/maps/place/', coords_dd[2], ',', coords_dd[1], '/@', coords_dd[2], ',', coords_dd[1], ',15z/data=!3m1!1e3')
    r2 <- paste0('[', cadena, ']', '(', r1, ')')
    salida <- r2
    return(salida)
  }
  
  url <- generar_url(coords_dd = coords_dd)
  
  return(url)
  
}