
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ambzre

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ambzre)](https://CRAN.R-project.org/package=ambzre)
<!-- badges: end -->

El objetivo de **ambzre** es permitir la revisión y estandarización de
las tablas de atributos de los shapefiles producidos por el componente
ambiental del proyecto ZRE41.

## Instalacion

Para instalar **ambzre** desde el repositoriod e GitHub:

``` r
remotes::install_github("PaulESantos/ambzre")
```

## Ejemplo

-   Lista de archivos shp:

``` r
library(ambzre)
library(here)
library(tidyverse)
```

Para modificar los diccionarios de datos de los diferentes shapefiles:

``` r
df <- diccionario_cv(system.file("extdata/cobertura_vegetal_sa01.shp",
                     package = "ambzre"))
```

``` r
Reading layer `cobertura_vegetal_sa01' from data source `D:\ambzre\inst\extdata\cobertura_vegetal_sa01.shp' using driver `ESRI Shapefile'
Simple feature collection with 230 features and 7 fields
Geometry type: MULTIPOLYGON
Dimension:     XY
Bounding box:  xmin: 175506.5 ymin: 8502019 xmax: 176404.5 ymax: 8502925
Projected CRS: WGS 84 / UTM zone 19S
Simple feature collection with 6 features and 7 fields
Geometry type: MULTIPOLYGON
Dimension:     XY
Bounding box:  xmin: 175560.4 ymin: 8502077 xmax: 175694.6 ymax: 8502163
Projected CRS: WGS 84 / UTM zone 19S
  objectid gridcode origfid    aream2 coberveg shapeleng shapearea                       geometry
1        1        1       0  17.26362  Arborea  17.53265  17.26362 MULTIPOLYGON (((175567.7 85...
2        2        1       0  19.54285  Arborea  28.04657  19.54285 MULTIPOLYGON (((175595.8 85...
3        3        1       0 317.44111  Arborea 193.35411 317.44111 MULTIPOLYGON (((175574.5 85...
4        4        1       0  26.30083  Arborea  19.41983  26.30083 MULTIPOLYGON (((175640.4 85...
5        5        1       0  24.83902  Arborea  27.95120  24.83902 MULTIPOLYGON (((175662 8502...
6        6        1       0  88.82461  Arborea  56.55997  88.82461 MULTIPOLYGON (((175691.1 85...
```

Esta función solicita ingresar información referente al código de la
**ZRE**

``` r
Ingresar codigo de la ZRE:
```

Ingresar el nombre de la columna que contiene la información del tipo de
cobertura (**Natural o antropica**)

``` r
Seleccionar la variable TIPO COBERTURA:
```

Ingresar el nombre de la columna que contiene la información de la
categoría de **Cobertura vegetal**:

``` r
Seleccionar la variable COBERTURA VEGETAL:
```

Una vez ingresada esta información y ejecutada la función el shapefile
puede ser exportado con la función ´shape_write()´

``` r
df |> 
shp_write(folder = "path de la cartepa donde se guardara los datos",
           filename = "nombrecon el cual se guardara el archivo shp"
           )
```
