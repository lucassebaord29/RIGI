# Lectura de datos ------------------------------------------------------------

excel_path <- "data/RIGI_tracker_data_final_con_proyectos_integrados.xlsx"
sheet_proyectos <- "Proyectos"

load_proyectos <- function(path = excel_path, sheet = sheet_proyectos) {
  if (!file.exists(path)) {
    stop(
      "No se encontró el archivo Excel en: ", path,
      "\nVerificá que el archivo esté dentro de la carpeta data/.",
      call. = FALSE
    )
  }

  readxl::read_excel(
    path = path,
    sheet = sheet,
    guess_max = 10000
  )
}

get_file_update_time <- function(path = excel_path) {
  if (!file.exists(path)) return(as.POSIXct(NA))
  file.info(path)$mtime
}
