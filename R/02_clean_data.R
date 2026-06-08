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

rename_if_exists <- function(data, old, new) {
  if (old %in% names(data) && !new %in% names(data)) {
    data <- dplyr::rename(data, !!new := !!rlang::sym(old))
  }
  data
}

add_missing_columns <- function(data, cols) {
  for (col in cols) {
    if (!col %in% names(data)) data[[col]] <- NA
  }
  data
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
        # Formato tipo 1.234,56
        value <- stringr::str_replace_all(value, "\\.", "")
        value <- stringr::str_replace(value, ",", ".")
      } else {
        # Formato tipo 1,234.56
        value <- stringr::str_replace_all(value, ",", "")
      }
    } else if (has_comma && !has_dot) {
      # Formato tipo 1234,56
      value <- stringr::str_replace(value, ",", ".")
    } else if (!has_comma && has_dot) {
      # Si hay más de un punto, se interpreta como separador de miles.
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

  required_core <- c(
    "proyecto", "empresa", "sector", "provincia",
    "monto_mill_usd", "estado_administrativo"
  )

  missing_core <- setdiff(required_core, names(data))
  if (length(missing_core) > 0) {
    stop(
      "Faltan columnas clave en la solapa Proyectos: ",
      paste(missing_core, collapse = ", "),
      call. = FALSE
    )
  }

  data <- data |>
    rename_if_exists("monto_mill_usd", "monto_usd_mill") |>
    rename_if_exists("activos_computables_mill_usd", "activos_computables_usd_mill") |>
    rename_if_exists("estado_administrativo", "estado")

  data <- add_missing_columns(
    data,
    c(
      "id_proyecto",
      "proyecto",
      "nombre_proyecto_matcheado",
      "empresa",
      "vpu_o_sociedad",
      "sector",
      "subsector",
      "provincia",
      "localidad_region",
      "monto_usd_mill",
      "activos_computables_usd_mill",
      "estado",
      "fecha_presentacion",
      "fecha_aprobacion",
      "norma_aprobacion"
    )
  )

  data |>
    dplyr::mutate(
      monto_usd_mill = parse_numeric_mixed(monto_usd_mill),
      activos_computables_usd_mill = parse_numeric_mixed(activos_computables_usd_mill),
      fecha_presentacion = convert_excel_date(fecha_presentacion),
      fecha_aprobacion = convert_excel_date(fecha_aprobacion),
      estado = empty_to_na(as.character(estado)),
      sector = empty_to_na(as.character(sector)),
      subsector = empty_to_na(as.character(subsector)),
      provincia = empty_to_na(as.character(provincia)),
      empresa = empty_to_na(as.character(empresa)),
      proyecto = empty_to_na(as.character(proyecto)),
      estado_lower = stringr::str_to_lower(estado),
      estado_simplificado = dplyr::case_when(
        stringr::str_detect(estado_lower, "aprob") ~ "Aprobado",
        stringr::str_detect(estado_lower, "rechaz|desest|no aprob") ~ "Rechazado",
        stringr::str_detect(estado_lower, "evalu|pend|anal|present|tram|trám") ~ "En evaluación",
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
      aprobado = estado_simplificado == "Aprobado"
    ) |>
    dplyr::select(-estado_lower)
}
