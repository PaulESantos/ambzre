#' Exportar shapefiles
#'
#' @param sf objeto sf
#' @param folder path dela carpeta donde se guardara el archivo
#' @param filename nombre con el cual se guardara el archivo
#'
#' @return Save shapefile
#' @export
shape_write <- function(sf, folder, filename) {

    sf::st_zm(sf, drop = TRUE, what = 'ZM') |>
    sf::st_write( dsn = folder, # carpeta
              layer = filename,
              # nombre del archivo
              driver = "ESRI Shapefile",
              append = FALSE)
}
