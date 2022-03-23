#' Validación de las variables tipo de cobertura y cobertura vegetal
#'
#' @param sf  Objeto resultante de la función 'diccionari_cv'
#'
#' @return Message
#' @export
#'
#' @examples
#' # diccionario_cv(filename) |>
#' # validacion_cv()
validacion_cv <- function(sf) {
  "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
  #' validacion de tipo de cobertura
  niveles_cv <- c("ARBÓREA", "MATORRAL", "HERBAZAL", "PASTIZAL",
                  "ESCASA COBERTURA", "ZONA URBANA")
  #
  cobert_veg <- sf |>
    pull(cobert_veg) |>
    unique()
  if("tipo_cober" %in% names(sf)){
    tipo_cober <- c("COBERTURA ANTRÓPICA", "COBERTURA NATURAL")
    tipo <- sf |>
      pull(tipo_cober) |>
      unique()
    cober <- sf |>
      pull(cobert_veg) |>
      unique()
    xx <- cober %w/o% niveles_cv
    x <- tipo %w/o% tipo_cober
    if(length(x) > 0 ){
      message(paste0(crayon::white("Remplazar "),
                     crayon::green(x),
                     crayon::white(" utilizando \n"),
                     crayon::blue(paste0(tipo_cober,
                                         collapse = " - "))))

      message(paste0(crayon::white("Remplazar "),
                     crayon::green(paste0(xx,
                                          collapse = " - ")),
                     crayon::white(" utilizando \n"),
                     crayon::blue(paste0(niveles_cv,
                                         collapse = " - "))))
    }
  }
  else if(!"tipo_cober" %in% names(sf)){

    cober <- sf |>
      pull(cobert_veg) |>
      unique()
    xx <- cober %w/o% niveles_cv

    if(length(xx) > 0 ){

      message(paste0(crayon::white("Remplazar "),
                     crayon::green(paste0(xx,
                                          collapse = " - ")),
                     crayon::white(" utilizando \n"),
                     crayon::blue(paste0(niveles_cv,
                                         collapse = " - "))))
    }
  }

}
