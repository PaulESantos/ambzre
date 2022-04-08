#' Diccionario de datos Propuesta de establecimiento de la ZPCE
#'
#' @param filename file path
#'
#' @return sf
#' @export
diccionario_delim <- function(filename) {
  # read shapefile
  shp <- sf::st_read(filename)|>
    janitor::clean_names()
  cv_names <- names(shp)
  cv_names
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


  zre <- readline(prompt = "Ingresar nombre de la ZRE:")
  especies <- readline(prompt = "Selecciona columna ESPECIES/TIPO:")
  if (!grepl("^[a-z]+$", especies)) {
    especies <- NA_real_
  }

  vars <- c(especies)

  names(vars) <- c("especies")

  vars1 <- vars[!is.na(vars)]

  names_vars1 <- c(names(vars1), "geometry")

  out <- shp|>
    dplyr::select(
    dplyr::all_of(vars1),
      geometry
    )

  colnames(out) <- names_vars1

  output <- out|>
    dplyr::mutate(
      zre = paste0("ZRE", zre)|>  toupper(),
      codigo = "EDZPCE",
      documento = "PROPUESTA",
      doc_reglam = "PE",
      fuente = "EQUIPO TÉCNICO AMB-PM41ZRE"
    )|>
    dplyr::mutate(propuesta = "ESTABLECIMIENTO Y DELIMITACIÓN DE ZPCE")|>
    dplyr::mutate(distrito = dplyr::case_when(
      stringr::str_detect(zre, "CU") ~ "CUSCO",
      stringr::str_detect(zre, "SA") ~ "SANTIAGO",
      stringr::str_detect(zre, "SS") ~ "SAN SEBASTIAN",
      stringr::str_detect(zre, "SJ") ~ "SAN JERONIMO"
    ))|>
    dplyr::mutate_if(is.character, ~ toupper(.)|>
      stringr::str_squish()|>
      stringr::str_trim())

  if (is.na(especies) == TRUE) {
    output1 <- output|>
      dplyr::select(
        propuesta, codigo, zre, distrito, documento,
        doc_reglam, fuente
      )|>
      dplyr::relocate(geometry, .after = dplyr::last_col())
  } else if (is.na(especies) == FALSE) {
    output1 <- output |>
      dplyr::select(
        propuesta, codigo, zre, distrito, documento,
        doc_reglam, fuente, especies
      )|>
      dplyr::relocate(geometry, .after = dplyr::last_col())
  }
  return(output1)

}
