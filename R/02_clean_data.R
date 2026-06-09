# Limpieza y estandarización --------------------------------------------------

na_labels <- c(
  "", "NA", "N/A", "S/D", "s/d", "sd", "Sin dato", "sin dato",
  "No informado", "no informado", "NO INFORMADO", "No informa", "-", "--"
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

parse_numeric_mixed <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x_chr <- as.character(x)
  x_chr <- stringr::str_squish(x_chr)
  x_chr[x_chr %in% na_labels] <- NA_character_
  x_chr <- stringr::str_replace_all(x_chr, "USD|US\\$|u\\$s|U\\$S|millones|mill\\.|M", "")
  x_chr <- stringr::str_replace_all(x_chr, "\\s", "")
  x_chr <- stringr::str_replace_all(x_chr, "[^0-9,\\.\\-]", "")

  out <- rep(NA_real_, length(x_chr))

  for (i in seq_along(x_chr)) {
    value <- x_chr[i]
    if (is.na(value) || value == "") next

    has_comma <- stringr::str_detect(value, ",")
    has_dot <- stringr::str_detect(value, "\\.")

    if (has_comma && has_dot) {
      last_comma <- max(gregexpr(",", value, fixed = TRUE)[[1]])
      last_dot <- max(gregexpr(".", value, fixed = TRUE)[[1]])

      if (last_comma > last_dot) {
        value <- stringr::str_replace_all(value, "\\.", "")
        value <- stringr::str_replace(value, ",", ".")
      } else {
        value <- stringr::str_replace_all(value, ",", "")
      }
    } else if (has_comma && !has_dot) {
      value <- stringr::str_replace(value, ",", ".")
    } else if (!has_comma && has_dot) {
      n_dots <- stringr::str_count(value, "\\.")
      if (n_dots > 1) value <- stringr::str_replace_all(value, "\\.", "")
    }

    out[i] <- suppressWarnings(as.numeric(value))
  }

  out
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

  # La versión actualizada del Excel usa VPU como nombre del proyecto.
  # Esta limpieza conserva compatibilidad con versiones anteriores que tenían columna Proyecto.
  proyecto <- coalesce_text_cols(data, c("proyecto", "vpu", "nombre_proyecto_matcheado"))
  descripcion <- coalesce_text_cols(data, c("descripcion_del_proyecto", "descripcion", "description"))
  empresa <- coalesce_text_cols(data, c("empresa", "empresas"))
  titular <- coalesce_text_cols(data, c("titular_proyecto", "vpu_o_sociedad", "sociedad", "titular"))
  cuit <- coalesce_text_cols(data, c("cuit", "cuit_titular"))
  sector <- coalesce_text_cols(data, c("sector"))
  subsector <- coalesce_text_cols(data, c("subsector"))
  actividad_subsector <- coalesce_text_cols(data, c("actividad_subsector_resolucion_mecon", "actividad_subsector_resolucion_mec_on", "actividad_subsector"))
  provincia <- coalesce_text_cols(data, c("provincia"))
  localidad_region <- coalesce_text_cols(data, c("localidad_region", "localidad", "region"))
  estado <- coalesce_text_cols(data, c("estado_administrativo", "estado"))
  norma <- coalesce_text_cols(data, c("norma_aprobacion", "norma"))
  link_norma <- coalesce_text_cols(data, c("link_norma", "url_norma", "enlace_norma"))
  fuentes <- coalesce_text_cols(data, c("fuentes", "fuente"))
  preexistencia <- coalesce_text_cols(data, c("clasificacion_preexistencia_boletin_oficial", "clasificacion_preexistencia"))
  justificacion_preexistencia <- coalesce_text_cols(data, c("justificacion_preexistencia_boletin_oficial", "justificacion_preexistencia"))
  proyecto_exportacion <- coalesce_text_cols(data, c("proyecto_de_exportacion_estrategia_a_largo_plazo", "proyecto_de_exportacion_estrategia_a_largo_plazo_"))

  monto_raw <- coalesce_raw_cols(data, c("monto_mill_usd", "monto_usd_mill", "monto"))
  activos_raw <- coalesce_raw_cols(data, c("activos_computables_mill_usd", "activos_computables_usd_mill", "activos_computables"))

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

  tibble::tibble(
    id_proyecto = coalesce_text_cols(data, c("id_proyecto", "id")),
    proyecto = proyecto,
    descripcion_del_proyecto = descripcion,
    proyecto_de_exportacion_estrategia_largo_plazo = proyecto_exportacion,
    empresa = empresa,
    titular_proyecto = titular,
    vpu_o_sociedad = titular,
    cuit = cuit,
    sector = sector,
    subsector = subsector,
    actividad_subsector_resolucion_mecon = actividad_subsector,
    provincia = provincia,
    localidad_region = localidad_region,
    monto_usd_mill = parse_numeric_mixed(monto_raw),
    activos_computables_usd_mill = parse_numeric_mixed(activos_raw),
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
      stringr::str_detect(estado_norm, "no aprob|rechaz|desest") ~ "Rechazado",
      stringr::str_detect(estado_norm, "aprob") ~ "Aprobado",
      stringr::str_detect(estado_norm, "evalu|pend|anal|present|tram|anunci") ~ "Pendiente de aprobación",
      is.na(estado) ~ "No informado",
      TRUE ~ "Otros"
    ),
    sector_simplificado = dplyr::coalesce(sector, "No informado"),
    subsector_simplificado = dplyr::coalesce(subsector, "No informado"),
    provincia_simplificada = dplyr::coalesce(provincia, "No informado"),
    monto_usd_bill = monto_usd_mill / 1000,
    anio_presentacion = lubridate::year(fecha_presentacion),
    anio_aprobacion = lubridate::year(fecha_aprobacion),
    mes_presentacion = lubridate::floor_date(fecha_presentacion, unit = "month"),
    mes_aprobacion = lubridate::floor_date(fecha_aprobacion, unit = "month"),
    aprobado = estado_simplificado == "Aprobado",
    pendiente_aprobacion = estado_simplificado == "Pendiente de aprobación",
    fuente_analitica = dplyr::case_when(
      aprobado ~ "Boletín Oficial + empresas inferidas por Globaris",
      pendiente_aprobacion ~ "Dashboard de Globaris",
      TRUE ~ dplyr::coalesce(fuentes, "No informado")
    )
  )
}
