#' Diccionario de datos ecosistemas y espacios naturales
#'
#' @param filename path to shp file
#'
#' @return sf
#' @export
diccionario_een <- function(filename) {
  # read shapefile
  shp <- sf::st_read(filename) |>
    janitor::clean_names()
  cv_names <- names(shp)
  colnames(shp) <- gsub("_", "", cv_names)
  # seleccionar columnas
  print(head(shp))

  message(crayon::green(toupper(gsub(
    ".shp",
    "",
    str_extract_after(
      string = filename,
      pattern = "/",
      which = "last"
    )
  ))))


  # ingresar código de las zona de reglamentación
  zre <- readline(prompt = "Ingresar el nombre de la ZRE:")
  if (!grepl("[a-z][0-9]", zre)) {
    zre <- NA_real_
  }

  nombre_eco <- readline( prompt = "Seleccionar nombre del ECOSISTEMA:")
  if(!grepl("^[a-z]+$", nombre_eco)){
    nombre_eco <- NA_real_
  }
  tipo_eco <- readline(prompt = "Seleccionar el TIPO ECOSISTEMA:")
  if(!grepl("^[a-z]+$", tipo_eco)){
    tipo_eco <- NA_real_
  }
  est_conser <- readline( prompt = "Seleccionar la variable ESTADO DE CONSERVACION:")
  if(!grepl("^[a-z]+$", est_conser)){
    est_conser <- NA_real_
  }

  vars <- c(nombre_eco, tipo_eco, est_conser)
  names(vars) <- c("nombre_eco", "tipo_eco", "est_conser")
  vars1 <- vars[!is.na(vars)]
  names_vars1 <- c(names(vars1), "geometry")
  out <- shp |>
    dplyr::select(
      dplyr::all_of(vars1),
      geometry
    )
  colnames(out) <- names_vars1

  out <- shp |>
    dplyr::select(
      dplyr::all_of(vars1),
      geometry
    )
  colnames(out) <- names_vars1
  polygon_area <- sf::st_area(out)
  vars_2 <- names(out)
  # return(out)
  output <- out |>
    dplyr::mutate_if(is.character, ~ toupper(.) |>
      stringr::str_squish() |>
      stringr::str_trim()) |>
    dplyr::mutate(
      zre = paste0("ZRE", zre) |>  toupper(),
      documento = "DIAGNÓSTICO",
      doc_reglam = "PE",
      fuente = "TRABAJO DE CAMPO",
      area_m2 = polygon_area,
      area_ha = area_m2 / 10000L
    ) |>
    dplyr::mutate(codigo_eco = dplyr::case_when(
      stringr::str_detect(tipo_eco, "ESTRE") ~ "ET",
      stringr::str_detect(tipo_eco, "TICO") ~ "EA",
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(distrito = dplyr::case_when(
      stringr::str_detect(zre, "CU") ~ "CUSCO",
      stringr::str_detect(zre, "SA") ~ "SANTIAGO",
      stringr::str_detect(zre, "SS") ~ "SAN SEBASTIAN",
      stringr::str_detect(zre, "SJ") ~ "SAN JERONIMO",
    )) |>
    dplyr::select(
      dplyr::all_of(vars_2), codigo_eco, zre, distrito, documento,
      doc_reglam, fuente, area_m2, area_ha
    ) |>
    dplyr::mutate_if(is.character, ~ toupper(.) |>
      stringr::str_squish() |>
      stringr::str_trim()) |>
    dplyr::relocate(geometry, .after = dplyr::last_col()) |>
    dplyr::relocate(codigo_eco, .before = est_conser)

  full_names <- c(
    "nombre_eco", "tipo_eco", "codigo_eco", "est_conser", "zre", "distrito", "documento",
    "doc_reglam", "fuente", "area_m2", "area_ha", "geometry"
  )
  if (length(names(output)) == length(full_names)) {
    # print(head(output))
     return(output)
    message(crayon::green("Informacion completa!!."))
  } else {
    message(crayon::green("Añadir informacion"))
   return(output)
  }
}
