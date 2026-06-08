# Paquetes requeridos ---------------------------------------------------------
# Este script carga las dependencias necesarias para renderizar el informe.
# Si faltan paquetes en una ejecución local, intenta instalarlos automáticamente.

required_packages <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "lubridate",
  "plotly",
  "DT",
  "glue",
  "scales",
  "htmltools",
  "forcats"
)

install_if_missing <- function(packages) {
  missing_packages <- packages[!packages %in% rownames(installed.packages())]

  if (length(missing_packages) > 0) {
    if (identical(Sys.getenv("CI"), "true")) {
      stop(
        "Faltan paquetes en CI: ",
        paste(missing_packages, collapse = ", "),
        call. = FALSE
      )
    }

    install.packages(missing_packages, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))
