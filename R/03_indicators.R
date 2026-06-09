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
      titular_proyecto,
      sector_simplificado,
      subsector_simplificado,
      provincia_simplificada,
      estado_simplificado,
      monto_usd_mill,
      monto_usd_bill,
      fecha_presentacion,
      fecha_adhesion_rigi,
      fecha_publicacion_bo,
      fecha_aprobacion,
      norma_aprobacion,
      link_norma,
      fuentes,
      fuente_analitica
    )
}

make_timeline <- function(data) {
  presentaciones <- data |>
    dplyr::filter(!is.na(fecha_presentacion)) |>
    dplyr::transmute(
      proyecto, empresa, sector_simplificado, provincia_simplificada,
      estado_simplificado, monto_usd_mill,
      fecha = fecha_presentacion,
      evento = "Presentación"
    )

  adhesiones <- data |>
    dplyr::filter(!is.na(fecha_adhesion_rigi)) |>
    dplyr::transmute(
      proyecto, empresa, sector_simplificado, provincia_simplificada,
      estado_simplificado, monto_usd_mill,
      fecha = fecha_adhesion_rigi,
      evento = "Adhesión RIGI"
    )

  publicaciones <- data |>
    dplyr::filter(!is.na(fecha_publicacion_bo)) |>
    dplyr::transmute(
      proyecto, empresa, sector_simplificado, provincia_simplificada,
      estado_simplificado, monto_usd_mill,
      fecha = fecha_publicacion_bo,
      evento = "Publicación BO"
    )

  dplyr::bind_rows(presentaciones, adhesiones, publicaciones) |>
    dplyr::arrange(fecha)
}

make_indicator_tables <- function(proyectos) {
  proyectos_aprobados <- proyectos |>
    dplyr::filter(aprobado)

  proyectos_pendientes <- proyectos |>
    dplyr::filter(pendiente_aprobacion)

  estado_tbl <- summarise_category(proyectos, "estado_simplificado")
  sector_tbl <- summarise_category(proyectos, "sector_simplificado")
  subsector_tbl <- summarise_category(proyectos, "subsector_simplificado")
  provincia_tbl <- summarise_category(proyectos, "provincia_simplificada")

  sector_tbl_aprobados <- summarise_category(proyectos_aprobados, "sector_simplificado")
  subsector_tbl_aprobados <- summarise_category(proyectos_aprobados, "subsector_simplificado")
  provincia_tbl_aprobados <- summarise_category(proyectos_aprobados, "provincia_simplificada")

  sector_tbl_pendientes <- summarise_category(proyectos_pendientes, "sector_simplificado")
  subsector_tbl_pendientes <- summarise_category(proyectos_pendientes, "subsector_simplificado")
  provincia_tbl_pendientes <- summarise_category(proyectos_pendientes, "provincia_simplificada")

  top_projects <- make_top_projects(proyectos, n = 10)
  top_projects_aprobados <- make_top_projects(proyectos_aprobados, n = 10)
  top_projects_pendientes <- make_top_projects(proyectos_pendientes, n = 10)

  sector_estado_tbl <- proyectos |>
    dplyr::group_by(sector_simplificado, estado_simplificado) |>
    dplyr::summarise(
      n_proyectos = dplyr::n(),
      monto_usd_mill = safe_sum(monto_usd_mill),
      .groups = "drop"
    )

  list(
    proyectos_aprobados = proyectos_aprobados,
    proyectos_pendientes = proyectos_pendientes,
    estado_tbl = estado_tbl,
    sector_tbl = sector_tbl,
    subsector_tbl = subsector_tbl,
    provincia_tbl = provincia_tbl,
    sector_tbl_aprobados = sector_tbl_aprobados,
    subsector_tbl_aprobados = subsector_tbl_aprobados,
    provincia_tbl_aprobados = provincia_tbl_aprobados,
    sector_tbl_pendientes = sector_tbl_pendientes,
    subsector_tbl_pendientes = subsector_tbl_pendientes,
    provincia_tbl_pendientes = provincia_tbl_pendientes,
    top_projects = top_projects,
    top_projects_aprobados = top_projects_aprobados,
    top_projects_pendientes = top_projects_pendientes,
    sector_estado_tbl = sector_estado_tbl,
    timeline_tbl = make_timeline(proyectos),
    timeline_aprobados_tbl = make_timeline(proyectos_aprobados),
    timeline_pendientes_tbl = make_timeline(proyectos_pendientes),
    n_sin_fecha_presentacion = sum(is.na(proyectos$fecha_presentacion)),
    n_sin_fecha_aprobacion = sum(is.na(proyectos$fecha_aprobacion)),
    n_sin_fecha_presentacion_aprobados = sum(is.na(proyectos_aprobados$fecha_presentacion)),
    n_sin_fecha_publicacion_bo_aprobados = sum(is.na(proyectos_aprobados$fecha_publicacion_bo)),
    n_sin_fecha_presentacion_pendientes = sum(is.na(proyectos_pendientes$fecha_presentacion))
  )
}

make_indicators <- function(proyectos, path = excel_path) {
  tablas <- make_indicator_tables(proyectos)
  aprobados <- tablas$proyectos_aprobados
  pendientes <- tablas$proyectos_pendientes
  rechazados <- proyectos |> dplyr::filter(estado_simplificado == "Rechazado")

  n_total <- nrow(proyectos)
  n_aprobados <- nrow(aprobados)
  n_pendientes <- nrow(pendientes)
  n_rechazados <- nrow(rechazados)

  monto_total <- safe_sum(proyectos$monto_usd_mill)
  monto_aprobado <- safe_sum(aprobados$monto_usd_mill)
  monto_pendiente <- safe_sum(pendientes$monto_usd_mill)
  monto_rechazado <- safe_sum(rechazados$monto_usd_mill)

  list(
    n_proyectos = n_total,
    n_proyectos_fmt = fmt_number(n_total, accuracy = 1),
    monto_total = monto_total,
    monto_total_fmt = fmt_currency_mill(monto_total, accuracy = 1),
    monto_total_bill = monto_total / 1000,
    monto_total_bill_fmt = fmt_currency_bill(monto_total / 1000, accuracy = 0.01),

    n_aprobados = n_aprobados,
    n_aprobados_fmt = fmt_number(n_aprobados, accuracy = 1),
    monto_aprobado = monto_aprobado,
    monto_aprobado_fmt = fmt_currency_mill(monto_aprobado, accuracy = 1),
    monto_aprobado_bill = monto_aprobado / 1000,
    monto_aprobado_bill_fmt = fmt_currency_bill(monto_aprobado / 1000, accuracy = 0.01),
    monto_promedio_aprobado = safe_mean(aprobados$monto_usd_mill),
    monto_promedio_aprobado_fmt = fmt_currency_mill(safe_mean(aprobados$monto_usd_mill), accuracy = 1),
    monto_mediano_aprobado = safe_median(aprobados$monto_usd_mill),
    monto_mediano_aprobado_fmt = fmt_currency_mill(safe_median(aprobados$monto_usd_mill), accuracy = 1),
    prop_aprobados = ifelse(n_total > 0, n_aprobados / n_total, NA_real_),
    prop_aprobados_fmt = fmt_pct(ifelse(n_total > 0, n_aprobados / n_total, NA_real_)),
    prop_monto_aprobado = ifelse(monto_total > 0, monto_aprobado / monto_total, NA_real_),
    prop_monto_aprobado_fmt = fmt_pct(ifelse(monto_total > 0, monto_aprobado / monto_total, NA_real_)),

    n_pendientes = n_pendientes,
    n_pendientes_fmt = fmt_number(n_pendientes, accuracy = 1),
    monto_pendiente = monto_pendiente,
    monto_pendiente_fmt = fmt_currency_mill(monto_pendiente, accuracy = 1),
    monto_pendiente_bill = monto_pendiente / 1000,
    monto_pendiente_bill_fmt = fmt_currency_bill(monto_pendiente / 1000, accuracy = 0.01),
    monto_promedio_pendiente = safe_mean(pendientes$monto_usd_mill),
    monto_promedio_pendiente_fmt = fmt_currency_mill(safe_mean(pendientes$monto_usd_mill), accuracy = 1),
    monto_mediano_pendiente = safe_median(pendientes$monto_usd_mill),
    monto_mediano_pendiente_fmt = fmt_currency_mill(safe_median(pendientes$monto_usd_mill), accuracy = 1),
    prop_pendientes = ifelse(n_total > 0, n_pendientes / n_total, NA_real_),
    prop_pendientes_fmt = fmt_pct(ifelse(n_total > 0, n_pendientes / n_total, NA_real_)),
    prop_monto_pendiente = ifelse(monto_total > 0, monto_pendiente / monto_total, NA_real_),
    prop_monto_pendiente_fmt = fmt_pct(ifelse(monto_total > 0, monto_pendiente / monto_total, NA_real_)),

    n_rechazados = n_rechazados,
    n_rechazados_fmt = fmt_number(n_rechazados, accuracy = 1),
    monto_rechazado = monto_rechazado,
    monto_rechazado_fmt = fmt_currency_mill(monto_rechazado, accuracy = 1),

    monto_promedio = safe_mean(proyectos$monto_usd_mill),
    monto_promedio_fmt = fmt_currency_mill(safe_mean(proyectos$monto_usd_mill), accuracy = 1),
    monto_mediano = safe_median(proyectos$monto_usd_mill),
    monto_mediano_fmt = fmt_currency_mill(safe_median(proyectos$monto_usd_mill), accuracy = 1),

    sector_top_monto = top_category(tablas$sector_tbl, "sector_simplificado", "monto_usd_mill"),
    provincia_top_monto = top_category(tablas$provincia_tbl, "provincia_simplificada", "monto_usd_mill"),
    estado_predominante = top_category(tablas$estado_tbl, "estado_simplificado", "n_proyectos"),

    sector_top_monto_aprobado = top_category(tablas$sector_tbl_aprobados, "sector_simplificado", "monto_usd_mill"),
    sector_top_cantidad_aprobado = top_category(tablas$sector_tbl_aprobados, "sector_simplificado", "n_proyectos"),
    provincia_top_monto_aprobado = top_category(tablas$provincia_tbl_aprobados, "provincia_simplificada", "monto_usd_mill"),
    provincia_top_cantidad_aprobado = top_category(tablas$provincia_tbl_aprobados, "provincia_simplificada", "n_proyectos"),

    sector_top_monto_pendiente = top_category(tablas$sector_tbl_pendientes, "sector_simplificado", "monto_usd_mill"),
    sector_top_cantidad_pendiente = top_category(tablas$sector_tbl_pendientes, "sector_simplificado", "n_proyectos"),
    provincia_top_monto_pendiente = top_category(tablas$provincia_tbl_pendientes, "provincia_simplificada", "monto_usd_mill"),
    provincia_top_cantidad_pendiente = top_category(tablas$provincia_tbl_pendientes, "provincia_simplificada", "n_proyectos"),

    fecha_actualizacion = Sys.Date(),
    fecha_actualizacion_fmt = fmt_date(Sys.Date()),
    fecha_modificacion_archivo = get_file_update_time(path),
    fecha_modificacion_archivo_fmt = fmt_datetime(get_file_update_time(path)),
    n_sin_fecha_presentacion = tablas$n_sin_fecha_presentacion,
    n_sin_fecha_aprobacion = tablas$n_sin_fecha_aprobacion,
    n_sin_fecha_presentacion_aprobados = tablas$n_sin_fecha_presentacion_aprobados,
    n_sin_fecha_publicacion_bo_aprobados = tablas$n_sin_fecha_publicacion_bo_aprobados,
    n_sin_fecha_presentacion_pendientes = tablas$n_sin_fecha_presentacion_pendientes
  )
}

format_top_projects_text <- function(top_projects, n = 3) {
  out <- top_projects |>
    dplyr::slice_head(n = n) |>
    dplyr::mutate(
      txt = paste0(proyecto, " (", fmt_currency_mill(monto_usd_mill, accuracy = 1), ")")
    ) |>
    dplyr::pull(txt) |>
    paste(collapse = "; ")

  if (length(out) == 0 || is.na(out) || out == "") out <- "no hay proyectos con monto informado"
  out
}

make_summary_text <- function(indicadores, tablas) {
  top_aprobados <- format_top_projects_text(tablas$top_projects_aprobados, n = 3)
  top_pendientes <- format_top_projects_text(tablas$top_projects_pendientes, n = 3)

  glue::glue(
    "El foco principal del informe está puesto en los proyectos aprobados. ",
    "La base registra {indicadores$n_aprobados_fmt} proyectos aprobados, por un monto aprobado acumulado de ",
    "{indicadores$monto_aprobado_fmt} ({indicadores$monto_aprobado_bill_fmt}). ",
    "Estos proyectos representan el {indicadores$prop_aprobados_fmt} de los registros y el ",
    "{indicadores$prop_monto_aprobado_fmt} del monto total informado. ",
    "Entre los aprobados, el sector con mayor monto acumulado es {indicadores$sector_top_monto_aprobado}; ",
    "la provincia o región con mayor monto aprobado es {indicadores$provincia_top_monto_aprobado}. ",
    "Los principales proyectos aprobados por monto son: {top_aprobados}. ",
    "\n\n",
    "En segundo lugar, el informe muestra los proyectos pendientes de aprobación o en evaluación. ",
    "Este subconjunto contiene {indicadores$n_pendientes_fmt} proyectos, por un monto informado de ",
    "{indicadores$monto_pendiente_fmt} ({indicadores$monto_pendiente_bill_fmt}). ",
    "Los pendientes representan el {indicadores$prop_pendientes_fmt} de los registros y el ",
    "{indicadores$prop_monto_pendiente_fmt} del monto total informado. ",
    "El sector con mayor monto pendiente es {indicadores$sector_top_monto_pendiente}; ",
    "la provincia o región con mayor monto pendiente es {indicadores$provincia_top_monto_pendiente}. ",
    "Los principales proyectos pendientes por monto son: {top_pendientes}. ",
    "\n\n",
    "Como contexto, el universo total incluye {indicadores$n_proyectos_fmt} proyectos y suma ",
    "{indicadores$monto_total_fmt} ({indicadores$monto_total_bill_fmt}). ",
    "El informe fue generado el {indicadores$fecha_actualizacion_fmt}; el archivo fuente registra como última modificación ",
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

make_source_note <- function() {
  htmltools::div(
    class = "source-note-box",
    htmltools::strong("Aclaración metodológica: "),
    htmltools::span(
      "Para los proyectos aprobados, se utilizó la información del Boletín Oficial y las empresas inferidas por Globaris. ",
      "Para los proyectos en evaluación, se utilizó la información del dashboard de Globaris."
    )
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

make_kpi_cards_pendientes <- function(indicadores) {
  htmltools::div(
    class = "kpi-grid kpi-grid-pending",
    kpi_card("Pendientes de aprobación", indicadores$n_pendientes_fmt, paste0(indicadores$prop_pendientes_fmt, " del total")),
    kpi_card("Monto pendiente", indicadores$monto_pendiente_bill_fmt, paste0(indicadores$prop_monto_pendiente_fmt, " del monto total")),
    kpi_card("Promedio pendiente", indicadores$monto_promedio_pendiente_fmt, "Promedio entre pendientes"),
    kpi_card("Mediana pendiente", indicadores$monto_mediano_pendiente_fmt, "Mediana entre pendientes"),
    kpi_card("Sector líder pendiente", indicadores$sector_top_monto_pendiente, "Mayor monto pendiente"),
    kpi_card("Provincia líder pendiente", indicadores$provincia_top_monto_pendiente, "Mayor monto pendiente")
  )
}

make_kpi_cards_total <- function(indicadores) {
  htmltools::div(
    class = "kpi-grid",
    kpi_card("Total de proyectos", indicadores$n_proyectos_fmt, "Todos los estados administrativos"),
    kpi_card("Monto total", indicadores$monto_total_bill_fmt, "Monto informado acumulado"),
    kpi_card("Aprobados", indicadores$n_aprobados_fmt, indicadores$monto_aprobado_fmt),
    kpi_card("Pendientes", indicadores$n_pendientes_fmt, indicadores$monto_pendiente_fmt),
    kpi_card("Rechazados", indicadores$n_rechazados_fmt, indicadores$monto_rechazado_fmt),
    kpi_card("Estado predominante", indicadores$estado_predominante, "Según cantidad de proyectos")
  )
}

make_kpi_cards_estado <- function(indicadores) {
  htmltools::div(
    class = "kpi-grid kpi-grid-status",
    kpi_card("Aprobados", indicadores$n_aprobados_fmt, indicadores$monto_aprobado_fmt),
    kpi_card("Pendientes", indicadores$n_pendientes_fmt, indicadores$monto_pendiente_fmt),
    kpi_card("Rechazados", indicadores$n_rechazados_fmt, indicadores$monto_rechazado_fmt),
    kpi_card("% aprobados", indicadores$prop_aprobados_fmt, "Participación en cantidad"),
    kpi_card("% pendientes", indicadores$prop_pendientes_fmt, "Participación en cantidad"),
    kpi_card("% monto aprobado", indicadores$prop_monto_aprobado_fmt, "Participación en monto")
  )
}

# Compatibilidad con la versión anterior del dashboard.
make_kpi_cards <- function(indicadores) {
  make_kpi_cards_total(indicadores)
}
