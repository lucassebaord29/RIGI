# Indicadores, tablas auxiliares y resumen automático -------------------------

fmt_number <- function(x, accuracy = 1) {
  if (length(x) == 0) return(character(0))
  out <- scales::number(x, accuracy = accuracy, big.mark = ".", decimal.mark = ",")
  out[is.na(x)] <- "s/d"
  out
}

fmt_currency_mill <- function(x, accuracy = 1) {
  if (length(x) == 0) return(character(0))
  out <- paste0("USD ", fmt_number(x, accuracy = accuracy), " M")
  out[is.na(x)] <- "s/d"
  out
}

fmt_currency_bill <- function(x, accuracy = 0.01) {
  if (length(x) == 0) return(character(0))
  out <- paste0("USD ", fmt_number(x, accuracy = accuracy), " B")
  out[is.na(x)] <- "s/d"
  out
}

fmt_pct <- function(x, accuracy = 0.1) {
  if (length(x) == 0) return(character(0))
  out <- scales::percent(x, accuracy = accuracy, decimal.mark = ",")
  out[is.na(x) | is.nan(x)] <- "s/d"
  out
}

fmt_date <- function(x) {
  if (length(x) == 0) return(character(0))
  x_date <- as.Date(x)
  out <- format(x_date, "%d/%m/%Y")
  out[is.na(x_date)] <- "s/d"
  out
}

fmt_datetime <- function(x) {
  if (length(x) == 0) return(character(0))
  x_posix <- as.POSIXct(x)
  out <- format(x_posix, "%d/%m/%Y %H:%M")
  out[is.na(x_posix)] <- "s/d"
  out
}

safe_sum <- function(x) sum(x, na.rm = TRUE)
safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_median <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

top_category <- function(data, label_col, value_col) {
  if (nrow(data) == 0) return("No informado")
  data <- data |>
    dplyr::filter(!is.na(.data[[label_col]])) |>
    dplyr::arrange(dplyr::desc(.data[[value_col]]))
  if (nrow(data) == 0) return("No informado")
  value <- data[[label_col]][1]
  if (is.na(value) || value == "") "No informado" else value
}

summarise_category <- function(data, group_col) {
  if (nrow(data) == 0) {
    out <- tibble::tibble(
      n_proyectos = integer(),
      monto_usd_mill = numeric(),
      monto_usd_bill = numeric()
    )
    out[[group_col]] <- character()
    return(out |> dplyr::select(dplyr::all_of(group_col), dplyr::everything()))
  }

  data |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      n_proyectos = dplyr::n(),
      monto_usd_mill = safe_sum(monto_usd_mill),
      monto_usd_bill = monto_usd_mill / 1000,
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(monto_usd_mill))
}

make_top_projects <- function(data, n = 10) {
  data |>
    dplyr::filter(!is.na(monto_usd_mill)) |>
    dplyr::arrange(dplyr::desc(monto_usd_mill)) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(
      proyecto,
      empresa,
      sector_simplificado,
      subsector_simplificado,
      provincia_simplificada,
      estado_simplificado,
      monto_usd_mill,
      monto_usd_bill,
      fecha_presentacion,
      fecha_aprobacion
    )
}

make_indicator_tables <- function(proyectos) {
  proyectos_aprobados <- proyectos |>
    dplyr::filter(estado_simplificado == "Aprobado")

  estado_tbl <- summarise_category(proyectos, "estado_simplificado")
  sector_tbl <- summarise_category(proyectos, "sector_simplificado")
  subsector_tbl <- summarise_category(proyectos, "subsector_simplificado")
  provincia_tbl <- summarise_category(proyectos, "provincia_simplificada")

  sector_tbl_aprobados <- summarise_category(proyectos_aprobados, "sector_simplificado")
  subsector_tbl_aprobados <- summarise_category(proyectos_aprobados, "subsector_simplificado")
  provincia_tbl_aprobados <- summarise_category(proyectos_aprobados, "provincia_simplificada")

  top_projects <- make_top_projects(proyectos, n = 10)
  top_projects_aprobados <- make_top_projects(proyectos_aprobados, n = 10)

  sector_estado_tbl <- proyectos |>
    dplyr::group_by(sector_simplificado, estado_simplificado) |>
    dplyr::summarise(
      n_proyectos = dplyr::n(),
      monto_usd_mill = safe_sum(monto_usd_mill),
      .groups = "drop"
    )

  timeline_tbl <- proyectos |>
    dplyr::select(
      proyecto,
      empresa,
      sector_simplificado,
      provincia_simplificada,
      estado_simplificado,
      monto_usd_mill,
      fecha_presentacion,
      fecha_aprobacion
    ) |>
    tidyr::pivot_longer(
      cols = c(fecha_presentacion, fecha_aprobacion),
      names_to = "tipo_fecha",
      values_to = "fecha"
    ) |>
    dplyr::mutate(
      evento = dplyr::case_when(
        tipo_fecha == "fecha_presentacion" ~ "Presentación",
        tipo_fecha == "fecha_aprobacion" ~ "Aprobación",
        TRUE ~ tipo_fecha
      )
    ) |>
    dplyr::filter(!is.na(fecha)) |>
    dplyr::arrange(fecha)

  list(
    estado_tbl = estado_tbl,
    sector_tbl = sector_tbl,
    subsector_tbl = subsector_tbl,
    provincia_tbl = provincia_tbl,
    sector_tbl_aprobados = sector_tbl_aprobados,
    subsector_tbl_aprobados = subsector_tbl_aprobados,
    provincia_tbl_aprobados = provincia_tbl_aprobados,
    top_projects = top_projects,
    top_projects_aprobados = top_projects_aprobados,
    sector_estado_tbl = sector_estado_tbl,
    timeline_tbl = timeline_tbl,
    n_sin_fecha_presentacion = sum(is.na(proyectos$fecha_presentacion)),
    n_sin_fecha_aprobacion = sum(is.na(proyectos$fecha_aprobacion))
  )
}

make_indicators <- function(proyectos, path = excel_path) {
  tablas <- make_indicator_tables(proyectos)
  proyectos_aprobados <- proyectos |>
    dplyr::filter(estado_simplificado == "Aprobado")

  n_proyectos <- nrow(proyectos)
  n_aprobados <- nrow(proyectos_aprobados)

  monto_total <- safe_sum(proyectos$monto_usd_mill)
  monto_total_bill <- monto_total / 1000

  monto_aprobado <- safe_sum(proyectos_aprobados$monto_usd_mill)
  monto_aprobado_bill <- monto_aprobado / 1000

  monto_evaluacion <- safe_sum(proyectos$monto_usd_mill[proyectos$estado_simplificado == "En evaluación"])
  monto_rechazado <- safe_sum(proyectos$monto_usd_mill[proyectos$estado_simplificado == "Rechazado"])

  n_evaluacion <- sum(proyectos$estado_simplificado == "En evaluación", na.rm = TRUE)
  n_rechazados <- sum(proyectos$estado_simplificado == "Rechazado", na.rm = TRUE)

  monto_promedio_total <- safe_mean(proyectos$monto_usd_mill)
  monto_mediano_total <- safe_median(proyectos$monto_usd_mill)
  monto_promedio_aprobado <- safe_mean(proyectos_aprobados$monto_usd_mill)
  monto_mediano_aprobado <- safe_median(proyectos_aprobados$monto_usd_mill)

  prop_aprobados_cantidad <- ifelse(n_proyectos > 0, n_aprobados / n_proyectos, NA_real_)
  prop_aprobados_monto <- ifelse(monto_total > 0, monto_aprobado / monto_total, NA_real_)

  list(
    n_proyectos = n_proyectos,
    n_proyectos_fmt = fmt_number(n_proyectos, accuracy = 1),
    monto_total = monto_total,
    monto_total_fmt = fmt_currency_mill(monto_total, accuracy = 1),
    monto_total_bill = monto_total_bill,
    monto_total_bill_fmt = fmt_currency_bill(monto_total_bill, accuracy = 0.01),

    n_aprobados = n_aprobados,
    n_aprobados_fmt = fmt_number(n_aprobados, accuracy = 1),
    monto_aprobado = monto_aprobado,
    monto_aprobado_fmt = fmt_currency_mill(monto_aprobado, accuracy = 1),
    monto_aprobado_bill = monto_aprobado_bill,
    monto_aprobado_bill_fmt = fmt_currency_bill(monto_aprobado_bill, accuracy = 0.01),

    n_evaluacion = n_evaluacion,
    n_evaluacion_fmt = fmt_number(n_evaluacion, accuracy = 1),
    monto_evaluacion = monto_evaluacion,
    monto_evaluacion_fmt = fmt_currency_mill(monto_evaluacion, accuracy = 1),
    n_rechazados = n_rechazados,
    n_rechazados_fmt = fmt_number(n_rechazados, accuracy = 1),
    monto_rechazado = monto_rechazado,
    monto_rechazado_fmt = fmt_currency_mill(monto_rechazado, accuracy = 1),

    monto_promedio = monto_promedio_total,
    monto_promedio_fmt = fmt_currency_mill(monto_promedio_total, accuracy = 1),
    monto_mediano = monto_mediano_total,
    monto_mediano_fmt = fmt_currency_mill(monto_mediano_total, accuracy = 1),
    monto_promedio_aprobado = monto_promedio_aprobado,
    monto_promedio_aprobado_fmt = fmt_currency_mill(monto_promedio_aprobado, accuracy = 1),
    monto_mediano_aprobado = monto_mediano_aprobado,
    monto_mediano_aprobado_fmt = fmt_currency_mill(monto_mediano_aprobado, accuracy = 1),

    prop_aprobados = prop_aprobados_cantidad,
    prop_aprobados_fmt = fmt_pct(prop_aprobados_cantidad),
    prop_monto_aprobado = prop_aprobados_monto,
    prop_monto_aprobado_fmt = fmt_pct(prop_aprobados_monto),

    sector_top_monto = top_category(tablas$sector_tbl, "sector_simplificado", "monto_usd_mill"),
    sector_top_cantidad = top_category(tablas$sector_tbl, "sector_simplificado", "n_proyectos"),
    provincia_top_monto = top_category(tablas$provincia_tbl, "provincia_simplificada", "monto_usd_mill"),
    provincia_top_cantidad = top_category(tablas$provincia_tbl, "provincia_simplificada", "n_proyectos"),
    estado_predominante = top_category(tablas$estado_tbl, "estado_simplificado", "n_proyectos"),

    sector_top_monto_aprobado = top_category(tablas$sector_tbl_aprobados, "sector_simplificado", "monto_usd_mill"),
    sector_top_cantidad_aprobado = top_category(tablas$sector_tbl_aprobados, "sector_simplificado", "n_proyectos"),
    provincia_top_monto_aprobado = top_category(tablas$provincia_tbl_aprobados, "provincia_simplificada", "monto_usd_mill"),
    provincia_top_cantidad_aprobado = top_category(tablas$provincia_tbl_aprobados, "provincia_simplificada", "n_proyectos"),

    fecha_actualizacion = Sys.Date(),
    fecha_actualizacion_fmt = fmt_date(Sys.Date()),
    fecha_modificacion_archivo = get_file_update_time(path),
    fecha_modificacion_archivo_fmt = fmt_datetime(get_file_update_time(path)),
    n_sin_fecha_presentacion = tablas$n_sin_fecha_presentacion,
    n_sin_fecha_aprobacion = tablas$n_sin_fecha_aprobacion
  )
}

format_top_projects_text <- function(top_projects, n = 3) {
  out <- top_projects |>
    dplyr::slice_head(n = n) |>
    dplyr::mutate(
      txt = paste0(
        proyecto,
        " (",
        fmt_currency_mill(monto_usd_mill, accuracy = 1),
        ")"
      )
    ) |>
    dplyr::pull(txt) |>
    paste(collapse = "; ")

  if (length(out) == 0 || is.na(out) || out == "") {
    out <- "no hay proyectos con monto informado"
  }

  out
}

make_summary_text <- function(indicadores, tablas) {
  top_projects_total_text <- format_top_projects_text(tablas$top_projects, n = 3)
  top_projects_aprobados_text <- format_top_projects_text(tablas$top_projects_aprobados, n = 3)

  glue::glue(
    "En el universo total, la base contiene {indicadores$n_proyectos_fmt} proyectos vinculados al RIGI, ",
    "por un monto total informado de {indicadores$monto_total_fmt} ",
    "({indicadores$monto_total_bill_fmt}). ",
    "Dentro de ese total, {indicadores$n_aprobados_fmt} proyectos se encuentran aprobados, ",
    "equivalentes al {indicadores$prop_aprobados_fmt} de los registros y al ",
    "{indicadores$prop_monto_aprobado_fmt} del monto informado. ",
    "El monto aprobado acumulado asciende a {indicadores$monto_aprobado_fmt} ",
    "({indicadores$monto_aprobado_bill_fmt}). ",
    "Además, {indicadores$n_evaluacion_fmt} proyectos figuran en evaluación ",
    "y {indicadores$n_rechazados_fmt} aparecen clasificados como rechazados. ",
    "\n\n",
    "Para el total de proyectos, el sector con mayor monto acumulado es ",
    "{indicadores$sector_top_monto}, mientras que el sector con mayor cantidad de proyectos es ",
    "{indicadores$sector_top_cantidad}. ",
    "La provincia o región con mayor monto registrado en el total es ",
    "{indicadores$provincia_top_monto}. ",
    "Los principales proyectos del universo total por monto son: {top_projects_total_text}. ",
    "\n\n",
    "Al considerar solo los proyectos aprobados, el sector con mayor monto aprobado acumulado es ",
    "{indicadores$sector_top_monto_aprobado}, mientras que el sector con mayor cantidad de aprobaciones es ",
    "{indicadores$sector_top_cantidad_aprobado}. ",
    "La provincia o región con mayor monto aprobado es {indicadores$provincia_top_monto_aprobado}. ",
    "Los principales proyectos aprobados por monto son: {top_projects_aprobados_text}. ",
    "El informe fue generado el {indicadores$fecha_actualizacion_fmt}; ",
    "el archivo fuente registra como última modificación ",
    "{indicadores$fecha_modificacion_archivo_fmt}."
  )
}

kpi_card <- function(label, value, sublabel = NULL) {
  htmltools::div(
    class = "kpi-card",
    htmltools::div(class = "kpi-label", label),
    htmltools::div(class = "kpi-value", value),
    htmltools::div(class = "kpi-sublabel", sublabel %||% "")
  )
}

make_kpi_cards_total <- function(indicadores) {
  htmltools::div(
    class = "kpi-grid",
    kpi_card("Total de proyectos", indicadores$n_proyectos_fmt, "Todos los estados administrativos"),
    kpi_card("Monto total", indicadores$monto_total_bill_fmt, "Monto informado acumulado"),
    kpi_card("Monto promedio", indicadores$monto_promedio_fmt, "Promedio por proyecto"),
    kpi_card("Monto mediano", indicadores$monto_mediano_fmt, "Mediana por proyecto"),
    kpi_card("Sector líder", indicadores$sector_top_monto, "Mayor monto total"),
    kpi_card("Provincia líder", indicadores$provincia_top_monto, "Mayor monto total")
  )
}

make_kpi_cards_aprobados <- function(indicadores) {
  htmltools::div(
    class = "kpi-grid kpi-grid-approved",
    kpi_card("Proyectos aprobados", indicadores$n_aprobados_fmt, paste0(indicadores$prop_aprobados_fmt, " del total")),
    kpi_card("Monto aprobado", indicadores$monto_aprobado_bill_fmt, paste0(indicadores$prop_monto_aprobado_fmt, " del monto total")),
    kpi_card("Promedio aprobado", indicadores$monto_promedio_aprobado_fmt, "Promedio entre aprobados"),
    kpi_card("Mediana aprobada", indicadores$monto_mediano_aprobado_fmt, "Mediana entre aprobados"),
    kpi_card("Sector líder aprobado", indicadores$sector_top_monto_aprobado, "Mayor monto aprobado"),
    kpi_card("Provincia líder aprobada", indicadores$provincia_top_monto_aprobado, "Mayor monto aprobado")
  )
}

make_kpi_cards_estado <- function(indicadores) {
  htmltools::div(
    class = "kpi-grid kpi-grid-status",
    kpi_card("Aprobados", indicadores$n_aprobados_fmt, indicadores$monto_aprobado_fmt),
    kpi_card("En evaluación", indicadores$n_evaluacion_fmt, indicadores$monto_evaluacion_fmt),
    kpi_card("Rechazados", indicadores$n_rechazados_fmt, indicadores$monto_rechazado_fmt),
    kpi_card("% aprobados", indicadores$prop_aprobados_fmt, "Participación en cantidad"),
    kpi_card("% monto aprobado", indicadores$prop_monto_aprobado_fmt, "Participación en monto"),
    kpi_card("Estado predominante", indicadores$estado_predominante, "Según cantidad de proyectos")
  )
}

# Compatibilidad con la versión anterior del dashboard.
make_kpi_cards <- function(indicadores) {
  make_kpi_cards_estado(indicadores)
}
