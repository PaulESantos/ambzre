#' Diccionario propuesta de zonificacion
#' @description Diccionario de los shapefiles de las propuestas de zonificacion:
#' - **ZPCE** ZONAS DE PROTECCION Y CONSERVACION ECOLOGICA
#' - **EPCE** ESPACIOS DE PROTECCION Y CONSERVACION ECOLOGICA
#' - **EIERE** ESPACIOS DE INTERVENCION ESPECIESL Y RECUPERACION DE ECOSISTEMAS
#' - **EPRH** ESPACIOS DE PROTECCION DEL RECURSO HIDRICO
#'
#' @param filename File path
#'
#' @return sf
#' @export

diccionario_propzoni <- function(filename) {
  # read shapefile
  shp <- sf::st_read(filename) |>
    janitor::clean_names()
  cv_names <- names(shp)
  colnames(shp) <- gsub("_", "", cv_names)
  # seleccionar columnas
  print(head(shp))

  message(crayon::green(filename))

  zre <- readline(prompt = "Ingresar nombre de la ZRE:")
  codigo <- readline(prompt = "Ingresar codigo.\n 1 - ZPCE / 2 - EPCE\n 3 - EIERE / 4 - EPRH :")
  if (codigo == 1) {
    codigo <- "ZPCE"
  }else if (codigo == 2) {
    codigo <- "EPCE"
  }else if (codigo == 3) {
    codigo <- "EIERE"
  } else if (codigo == 4) {
    codigo <- "EPRH"
  }

  out <- shp |>
    dplyr::select(geometry)

  polygon_area <- sf::st_area(out)

  output <- out |>
    dplyr::mutate(
      zre = paste0("ZRE", zre) |> toupper(),
      codigo = codigo,
      documento = "PROPUESTA",
      doc_reglam = "PE",
      fuente = "EQUIPO TÉCNICO AMB-PM41ZRE",
      area_m2 = polygon_area,
      area_ha = area_m2 / 10000L
    ) |>
    dplyr::mutate(propuesta = dplyr::case_when(
      codigo == "ZPCE" ~ "ZONA DE PROTECCIÓN Y CONSERVACIÓN ECOLÓGICA",
      codigo == "EPCE" ~ "ESPACIOS DE PROTECCIÓN Y CONSERVACIÓN ECOLÓGICA",
      codigo == "EIERE" ~ "ESPACIOS DE INTERVENCIÓN ESPECIAL Y RESTAURACIÓN DE ECOSISTEMAS",
      codigo == "EPRH" ~ "ESPACIOS DE PROTECCIÓN DE RECURSO HÍDRICO",
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(distrito = dplyr::case_when(
      stringr::str_detect(zre, "CU") ~ "CUSCO",
      stringr::str_detect(zre, "SA") ~ "SANTIAGO",
      stringr::str_detect(zre, "SS") ~ "SAN SEBASTIAN",
      stringr::str_detect(zre, "SJ") ~ "SAN JERONIMO"
    )) |>
    dplyr::select(
      propuesta, codigo, zre, distrito, documento,
      doc_reglam, fuente, area_m2, area_ha
    ) |>
    dplyr::mutate_if(is.character, ~ toupper(.) |>
      stringr::str_squish() |>
      stringr::str_trim()) |>
    dplyr::relocate(geometry, .after = dplyr::last_col())
  print(head(output))
  full_names <- c(
    "propuesta", "codigo", "zre", "distrito", "documento",
    "doc_reglam", "fuente", "area_m2", "area_ha", "geometry"
  )

  if (length(names(output)) == length(full_names)) {
    # print(head(output))
    return(output)
    message(crayon::green("Informacion completa!!"))
  } else {
    message(crayon::green("Añadir informacion!!!"))
    return(output)
  }
}
