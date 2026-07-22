# Limpieza y estandarización --------------------------------------------------

na_labels <- c(
  "", "NA", "N/A", "S/D", "s/d", "sd", "Sin dato", "sin dato",
  "No informado", "no informado", "NO INFORMADO", "No informa", "-", "--",
  "nan", "NaN", "NULL", "null"
)

empty_to_na <- function(x) {
  if (!is.character(x)) return(x)
  x <- stringr::str_squish(x)
  x[x %in% na_labels] <- NA_character_
  x
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_to_lower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x
}

normalize_si_no <- function(x) {
  x_norm <- normalize_text(x)
  dplyr::case_when(
    is.na(x_norm) | x_norm %in% c("", "na", "n/a", "s/d", "sd", "no informado") ~ NA_character_,
    stringr::str_detect(x_norm, "^(si|s)($|\\b)") ~ "Sí",
    stringr::str_detect(x_norm, "^(no|n)($|\\b)") ~ "No",
    TRUE ~ as.character(x)
  )
}

coalesce_text_cols <- function(data, candidates) {
  n <- nrow(data)
  out <- rep(NA_character_, n)
  for (col in candidates) {
    if (col %in% names(data)) {
      out <- dplyr::coalesce(out, empty_to_na(as.character(data[[col]])))
    }
  }
  out
}

coalesce_raw_cols <- function(data, candidates) {
  n <- nrow(data)
  out <- rep(NA_character_, n)
  for (col in candidates) {
    if (col %in% names(data)) {
      out <- dplyr::coalesce(out, as.character(data[[col]]))
    }
  }
  out
}

# Conversión robusta para montos, activos computables y empleos.
# Preserva columnas numéricas si ya vienen como numeric/double desde Excel.
parse_numeric_rigi <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x_chr <- as.character(x)
  x_chr <- stringr::str_trim(x_chr)
  x_chr[x_chr %in% na_labels] <- NA_character_

  # Remover símbolos y texto, preservando dígitos, coma, punto y signo negativo.
  x_chr <- stringr::str_replace_all(x_chr, "[^0-9,.-]", "")

  # Si viene con formato argentino 1.234,56.
  x_chr <- ifelse(
    stringr::str_detect(x_chr, "\\.\\d{3}") & stringr::str_detect(x_chr, ","),
    stringr::str_replace_all(x_chr, "\\.", ""),
    x_chr
  )

  # Si hay más de un punto y no hay coma, interpretamos puntos como separadores de miles.
  x_chr <- ifelse(
    !stringr::str_detect(x_chr, ",") & stringr::str_count(x_chr, "\\.") > 1,
    stringr::str_replace_all(x_chr, "\\.", ""),
    x_chr
  )

  x_chr <- stringr::str_replace_all(x_chr, ",", ".")
  suppressWarnings(as.numeric(x_chr))
}

convert_excel_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  x_chr <- as.character(x)
  x_chr <- stringr::str_squish(x_chr)
  x_chr[x_chr %in% na_labels] <- NA_character_

  out <- rep(as.Date(NA), length(x_chr))

  numeric_guess <- suppressWarnings(as.numeric(x_chr))
  numeric_idx <- !is.na(numeric_guess)

  # Excel usa 1899-12-30 como origen práctico para fechas seriales.
  out[numeric_idx] <- as.Date(numeric_guess[numeric_idx], origin = "1899-12-30")

  text_idx <- !numeric_idx & !is.na(x_chr)
  if (any(text_idx)) {
    parsed_text <- suppressWarnings(lubridate::parse_date_time(
      x_chr[text_idx],
      orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY", "d/m/Y", "Y-m-d")
    ))
    out[text_idx] <- as.Date(parsed_text)
  }

  out
}

clean_proyectos <- function(raw_data) {
  data <- raw_data |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(where(is.character), empty_to_na))

  proyecto <- coalesce_text_cols(data, c("proyecto", "vpu", "nombre_proyecto_matcheado"))
  descripcion <- coalesce_text_cols(data, c("descripcion_del_proyecto", "descripcion", "description"))
  empresa <- coalesce_text_cols(data, c("empresa", "empresas"))
  titular <- coalesce_text_cols(data, c("titular_proyecto", "vpu_o_sociedad", "sociedad", "titular"))
  cuit <- coalesce_text_cols(data, c("cuit", "cuit_titular"))
  sector <- coalesce_text_cols(data, c("sector"))
  subsector <- coalesce_text_cols(data, c("subsector"))
  actividad_subsector <- coalesce_text_cols(data, c("actividad_subsector_resolucion_mecon", "actividad_subsector"))
  provincia <- coalesce_text_cols(data, c("provincia"))
  localidad_region <- coalesce_text_cols(data, c("localidad_region", "localidad", "region"))
  estado <- coalesce_text_cols(data, c("estado_administrativo", "estado"))
  norma <- coalesce_text_cols(data, c("norma_aprobacion", "norma"))
  link_norma <- coalesce_text_cols(data, c("link_norma", "url_norma", "enlace_norma"))
  fuentes <- coalesce_text_cols(data, c("fuentes", "fuente"))
  preexistencia <- coalesce_text_cols(data, c("clasificacion_preexistencia_boletin_oficial", "clasificacion_preexistencia"))
  justificacion_preexistencia <- coalesce_text_cols(data, c("justificacion_preexistencia_boletin_oficial", "justificacion_preexistencia"))
  proyecto_exportacion <- coalesce_text_cols(data, c(
    "proyectos_de_exportacion_estrategica_de_largo_plazo_peelp",
    "proyecto_de_exportacion_estrategica_de_largo_plazo_peelp",
    "proyectos_de_exportacion_estrategica_largo_plazo_peelp",
    "proyecto_de_exportacion_estrategica_largo_plazo_peelp",
    "proyecto_de_exportacion_estrategia_a_largo_plazo",
    "proyecto_de_exportacion_estrategia_a_largo_plazo_"
  ))

  monto_raw <- coalesce_raw_cols(data, c("monto_mill_usd", "monto_usd_mill", "monto"))
  activos_raw <- coalesce_raw_cols(data, c("activos_computables_mill_usd", "activos_computables_usd_mill", "activos_computables"))
  empleos_raw <- coalesce_raw_cols(data, c(
    "empleos_directos_e_indirectos",
    "empleos_directos_indirectos",
    "empleos_directos_e_indirectos_",
    "empleos",
    "empleo",
    "empleos_directos_indirectos_total"
  ))

  fecha_presentacion_raw <- coalesce_raw_cols(data, c("fecha_presentacion", "fecha_de_presentacion"))
  fecha_adhesion_raw <- coalesce_raw_cols(data, c("fecha_adhesion_rigi", "fecha_adhesion", "fecha_de_adhesion_rigi"))
  fecha_publicacion_bo_raw <- coalesce_raw_cols(data, c("fecha_publicacion_bo", "fecha_publicacion_boletin_oficial", "fecha_publicacion"))
  fecha_aprobacion_raw <- coalesce_raw_cols(data, c("fecha_aprobacion", "fecha_de_aprobacion"))

  fecha_presentacion <- convert_excel_date(fecha_presentacion_raw)
  fecha_adhesion_rigi <- convert_excel_date(fecha_adhesion_raw)
  fecha_publicacion_bo <- convert_excel_date(fecha_publicacion_bo_raw)
  fecha_aprobacion_original <- convert_excel_date(fecha_aprobacion_raw)

  # Para aprobados, la fecha operacional de aprobación se prioriza como publicación en BO.
  fecha_aprobacion <- dplyr::coalesce(fecha_aprobacion_original, fecha_publicacion_bo, fecha_adhesion_rigi)

  estado_norm <- normalize_text(estado)
  aprobado <- stringr::str_detect(estado_norm, "aprob") & !stringr::str_detect(estado_norm, "no aprob|rechaz|desest")
  pendiente <- !aprobado & stringr::str_detect(estado_norm, "evalu|pend|anal|present|tram|anunci")

  base <- tibble::tibble(
    row_id = seq_len(nrow(data)),
    id_proyecto = coalesce_text_cols(data, c("id_proyecto", "id")),
    proyecto = proyecto,
    descripcion_del_proyecto = descripcion,
    proyecto_de_exportacion_estrategia_largo_plazo = normalize_si_no(proyecto_exportacion),
    proyecto_exportacion_estrategia_largo_plazo_si = normalize_si_no(proyecto_exportacion) == "Sí",
    empresa = empresa,
    titular_proyecto = titular,
    vpu_o_sociedad = titular,
    cuit = cuit,
    sector = sector,
    subsector = subsector,
    actividad_subsector_resolucion_mecon = actividad_subsector,
    provincia_original = provincia,
    provincia = provincia,
    localidad_region = localidad_region,
    monto_usd_mill = parse_numeric_rigi(monto_raw),
    activos_computables_usd_mill = parse_numeric_rigi(activos_raw),
    empleos_directos_indirectos = parse_numeric_rigi(empleos_raw),
    estado = estado,
    fecha_presentacion = fecha_presentacion,
    fecha_adhesion_rigi = fecha_adhesion_rigi,
    fecha_publicacion_bo = fecha_publicacion_bo,
    fecha_aprobacion = fecha_aprobacion,
    norma_aprobacion = norma,
    clasificacion_preexistencia_boletin_oficial = preexistencia,
    justificacion_preexistencia_boletin_oficial = justificacion_preexistencia,
    link_norma = link_norma,
    fuentes = fuentes,
    estado_simplificado = dplyr::case_when(
      aprobado ~ "Aprobado",
      pendiente ~ "Pendiente de aprobación",
      stringr::str_detect(estado_norm, "no aprob|rechaz|desest") ~ "Rechazado",
      is.na(estado) ~ "No informado",
      TRUE ~ "Otros"
    ),
    aprobado = aprobado,
    pendiente_aprobacion = pendiente,
    sector_simplificado = dplyr::coalesce(sector, "No informado"),
    subsector_simplificado = dplyr::coalesce(subsector, "No informado"),
    provincia_simplificada = dplyr::coalesce(provincia, "No informado"),
    anio_presentacion = lubridate::year(fecha_presentacion),
    anio_aprobacion = lubridate::year(fecha_aprobacion),
    mes_presentacion = lubridate::floor_date(fecha_presentacion, "month"),
    mes_aprobacion = lubridate::floor_date(fecha_aprobacion, "month")
  )

  base |>
    dplyr::mutate(
      n_provincias = stringr::str_count(dplyr::coalesce(provincia_original, "No informado"), ";") + 1L,
      n_provincias = dplyr::if_else(is.na(n_provincias) | n_provincias < 1L, 1L, n_provincias),
      proyecto_multiprovincial = n_provincias > 1L
    )
}

expand_provincias <- function(data) {
  data |>
    dplyr::mutate(
      provincia_expandida = dplyr::coalesce(provincia_original, "No informado"),
      provincia_expandida = dplyr::if_else(provincia_expandida == "", "No informado", provincia_expandida)
    ) |>
    tidyr::separate_rows(provincia_expandida, sep = ";") |>
    dplyr::mutate(
      provincia_expandida = stringr::str_squish(provincia_expandida),
      provincia_expandida = dplyr::if_else(is.na(provincia_expandida) | provincia_expandida == "", "No informado", provincia_expandida)
    ) |>
    dplyr::group_by(row_id) |>
    dplyr::mutate(n_provincias_expandida = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      monto_usd_mill_asignado_prop = monto_usd_mill / n_provincias_expandida,
      activos_computables_usd_mill_asignado_prop = activos_computables_usd_mill / n_provincias_expandida,
      empleos_directos_indirectos_asignado_prop = empleos_directos_indirectos / n_provincias_expandida,
      provincia_simplificada = provincia_expandida
    )
}

make_download_table <- function(data) {
  data |>
    dplyr::transmute(
      id_proyecto = id_proyecto,
      Proyecto = proyecto,
      `Descripción del proyecto` = descripcion_del_proyecto,
      `Proyectos de exportación estratégica de largo plazo (PEELP)` = proyecto_de_exportacion_estrategia_largo_plazo,
      empresa = empresa,
      titular_proyecto = titular_proyecto,
      CUIT = cuit,
      sector = sector,
      subsector = subsector,
      provincia = provincia_original,
      localidad_region = localidad_region,
      `Monto (mill. USD)` = as.numeric(monto_usd_mill),
      `Activos Computables (mill. USD)` = as.numeric(activos_computables_usd_mill),
      `Empleos (directos e indirectos)` = as.numeric(empleos_directos_indirectos),
      `Estado administrativo` = estado,
      fecha_presentacion = fecha_presentacion,
      fecha_adhesion_rigi = fecha_adhesion_rigi,
      fecha_publicacion_bo = fecha_publicacion_bo,
      fecha_aprobacion = fecha_aprobacion,
      norma_aprobacion = norma_aprobacion,
      link_norma = link_norma,
      Fuentes = fuentes,
      `Clasificación preexistencia BO` = clasificacion_preexistencia_boletin_oficial,
      `Justificación preexistencia BO` = justificacion_preexistencia_boletin_oficial
    )
}

create_download_files <- function(data, output_dir = "downloads") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  aprobados_download <- data |>
    dplyr::filter(aprobado) |>
    make_download_table()

  pendientes_download <- data |>
    dplyr::filter(pendiente_aprobacion) |>
    make_download_table()

  readr::write_csv(aprobados_download, file.path(output_dir, "base_interactiva_aprobados.csv"), na = "")
  readr::write_csv(pendientes_download, file.path(output_dir, "base_interactiva_pendientes.csv"), na = "")

  writexl::write_xlsx(aprobados_download, file.path(output_dir, "base_interactiva_aprobados.xlsx"))
  writexl::write_xlsx(pendientes_download, file.path(output_dir, "base_interactiva_pendientes.xlsx"))

  invisible(list(aprobados = aprobados_download, pendientes = pendientes_download))
}
