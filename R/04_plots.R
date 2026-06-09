# Visualizaciones -------------------------------------------------------------

rigi_colors <- c(
  "Aprobado" = "#0B4F6C",
  "Pendiente de aprobación" = "#F9A03F",
  "Rechazado" = "#A23E48",
  "Otros" = "#6C757D",
  "No informado" = "#ADB5BD"
)

scope_colors <- c(
  "Aprobados" = "#0B4F6C",
  "Pendientes" = "#F9A03F",
  "Total" = "#145DA0"
)

bar_color <- "#145DA0"
bar_color_alt <- "#2E8BC0"
bar_color_approved <- "#0B4F6C"
bar_color_pending <- "#F9A03F"

rigi_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#1F2937"),
      plot.subtitle = ggplot2::element_text(color = "#4B5563"),
      axis.title = ggplot2::element_text(color = "#374151"),
      axis.text = ggplot2::element_text(color = "#374151"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 15, 10, 15)
    )
}

empty_plot <- function(message = "No hay datos disponibles") {
  plotly::plot_ly() |>
    plotly::layout(
      title = list(text = message),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

wrap_label <- function(x, width = 36) {
  stringr::str_wrap(as.character(x), width = width)
}

plot_estado <- function(estado_tbl) {
  if (nrow(estado_tbl) == 0) return(empty_plot())

  data <- estado_tbl |>
    dplyr::mutate(
      estado_simplificado = forcats::fct_reorder(estado_simplificado, n_proyectos),
      hover = glue::glue(
        "Estado: {estado_simplificado}<br>",
        "Proyectos: {fmt_number(n_proyectos, accuracy = 1)}<br>",
        "Monto: {fmt_currency_mill(monto_usd_mill, accuracy = 1)}"
      )
    )

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = estado_simplificado, y = n_proyectos, fill = estado_simplificado, text = hover)
  ) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = n_proyectos), hjust = -0.2, size = 3.6) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = rigi_colors, na.value = "#ADB5BD") +
    ggplot2::labs(
      title = "Proyectos por estado administrativo",
      subtitle = "Distingue aprobados, pendientes de aprobación y otros estados",
      x = NULL,
      y = "Cantidad de proyectos"
    ) +
    rigi_theme()

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(showlegend = FALSE, margin = list(l = 120, r = 30, t = 70, b = 45))
}

plot_bar_monto <- function(data, label_col, title, subtitle = NULL, top_n = NULL, fill_color = bar_color) {
  if (nrow(data) == 0 || all(is.na(data$monto_usd_mill))) return(empty_plot())

  plot_data <- data |>
    dplyr::filter(!is.na(monto_usd_mill)) |>
    dplyr::arrange(dplyr::desc(monto_usd_mill))

  if (!is.null(top_n)) plot_data <- dplyr::slice_head(plot_data, n = top_n)

  plot_data <- plot_data |>
    dplyr::mutate(
      label_value = .data[[label_col]],
      label_plot = wrap_label(label_value, width = 34),
      label_plot = forcats::fct_reorder(label_plot, monto_usd_mill),
      hover = glue::glue(
        "{label_value}<br>",
        "Proyectos: {fmt_number(n_proyectos, accuracy = 1)}<br>",
        "Monto: {fmt_currency_mill(monto_usd_mill, accuracy = 1)}"
      )
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = label_plot, y = monto_usd_mill, text = hover)) +
    ggplot2::geom_col(fill = fill_color, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = function(x) fmt_number(x, accuracy = 1)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Millones de USD") +
    rigi_theme()

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(margin = list(l = 130, r = 30, t = 70, b = 45))
}

plot_compare_aprobado_pendiente <- function(aprobado_tbl, pendiente_tbl, label_col, title, subtitle = NULL, top_n = 12) {
  if (nrow(aprobado_tbl) == 0 && nrow(pendiente_tbl) == 0) return(empty_plot())

  aprobado_data <- aprobado_tbl |>
    dplyr::transmute(categoria = .data[[label_col]], universo = "Aprobados", n_proyectos, monto_usd_mill)

  pendiente_data <- pendiente_tbl |>
    dplyr::transmute(categoria = .data[[label_col]], universo = "Pendientes", n_proyectos, monto_usd_mill)

  categorias_top <- dplyr::bind_rows(aprobado_data, pendiente_data) |>
    dplyr::group_by(categoria) |>
    dplyr::summarise(monto_usd_mill = sum(monto_usd_mill, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(monto_usd_mill)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(categoria)

  plot_data <- dplyr::bind_rows(aprobado_data, pendiente_data) |>
    dplyr::filter(categoria %in% categorias_top) |>
    tidyr::complete(categoria, universo = c("Aprobados", "Pendientes"), fill = list(n_proyectos = 0, monto_usd_mill = 0)) |>
    dplyr::mutate(
      universo = factor(universo, levels = c("Aprobados", "Pendientes")),
      categoria_plot = wrap_label(categoria, width = 34),
      categoria_plot = forcats::fct_reorder(categoria_plot, monto_usd_mill, .fun = max),
      hover = glue::glue(
        "Universo: {universo}<br>",
        "Categoría: {categoria}<br>",
        "Proyectos: {fmt_number(n_proyectos, accuracy = 1)}<br>",
        "Monto: {fmt_currency_mill(monto_usd_mill, accuracy = 1)}"
      )
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = categoria_plot, y = monto_usd_mill, fill = universo, text = hover)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.68) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = scope_colors) +
    ggplot2::scale_y_continuous(labels = function(x) fmt_number(x, accuracy = 1)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Millones de USD") +
    rigi_theme()

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", x = 0, y = -0.15), margin = list(l = 140, r = 30, t = 75, b = 90))
}

# Compatibilidad con la función anterior: compara total vs aprobados.
plot_compare_total_aprobado <- function(total_tbl, aprobado_tbl, label_col, title, subtitle = NULL, top_n = 12) {
  if (nrow(total_tbl) == 0) return(empty_plot())

  total_data <- total_tbl |>
    dplyr::transmute(categoria = .data[[label_col]], universo = "Total", n_proyectos, monto_usd_mill)
  aprobado_data <- aprobado_tbl |>
    dplyr::transmute(categoria = .data[[label_col]], universo = "Aprobados", n_proyectos, monto_usd_mill)

  categorias_top <- total_data |>
    dplyr::arrange(dplyr::desc(monto_usd_mill)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(categoria)

  plot_data <- dplyr::bind_rows(total_data, aprobado_data) |>
    dplyr::filter(categoria %in% categorias_top) |>
    tidyr::complete(categoria, universo = c("Total", "Aprobados"), fill = list(n_proyectos = 0, monto_usd_mill = 0)) |>
    dplyr::mutate(
      universo = factor(universo, levels = c("Total", "Aprobados")),
      categoria_plot = wrap_label(categoria, width = 34),
      categoria_plot = forcats::fct_reorder(categoria_plot, monto_usd_mill, .fun = max),
      hover = glue::glue(
        "Universo: {universo}<br>",
        "Categoría: {categoria}<br>",
        "Proyectos: {fmt_number(n_proyectos, accuracy = 1)}<br>",
        "Monto: {fmt_currency_mill(monto_usd_mill, accuracy = 1)}"
      )
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = categoria_plot, y = monto_usd_mill, fill = universo, text = hover)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.68) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = scope_colors) +
    ggplot2::scale_y_continuous(labels = function(x) fmt_number(x, accuracy = 1)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Millones de USD") +
    rigi_theme()

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", x = 0, y = -0.15), margin = list(l = 140, r = 30, t = 75, b = 90))
}

plot_top_proyectos <- function(top_projects, title = "Top 10 proyectos por monto", subtitle = NULL, fill_color = bar_color_alt) {
  if (nrow(top_projects) == 0) return(empty_plot())

  data <- top_projects |>
    dplyr::mutate(
      proyecto_plot = wrap_label(proyecto, width = 42),
      proyecto_plot = forcats::fct_reorder(proyecto_plot, monto_usd_mill),
      hover = glue::glue(
        "Proyecto: {proyecto}<br>",
        "Empresa: {empresa}<br>",
        "Titular: {titular_proyecto}<br>",
        "Sector: {sector_simplificado}<br>",
        "Estado: {estado_simplificado}<br>",
        "Provincia/región: {provincia_simplificada}<br>",
        "Monto: {fmt_currency_mill(monto_usd_mill, accuracy = 1)}"
      )
    )

  p <- ggplot2::ggplot(data, ggplot2::aes(x = proyecto_plot, y = monto_usd_mill, text = hover)) +
    ggplot2::geom_col(fill = fill_color, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = function(x) fmt_number(x, accuracy = 1)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Millones de USD") +
    rigi_theme()

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(margin = list(l = 170, r = 30, t = 70, b = 45))
}

plot_sector_estado <- function(sector_estado_tbl) {
  if (nrow(sector_estado_tbl) == 0) return(empty_plot())

  data <- sector_estado_tbl |>
    dplyr::mutate(
      sector_plot = wrap_label(sector_simplificado, width = 30),
      hover = glue::glue(
        "Sector: {sector_simplificado}<br>",
        "Estado: {estado_simplificado}<br>",
        "Proyectos: {fmt_number(n_proyectos, accuracy = 1)}<br>",
        "Monto: {fmt_currency_mill(monto_usd_mill, accuracy = 1)}"
      )
    )

  plotly::plot_ly(
    data = data,
    x = ~estado_simplificado,
    y = ~sector_plot,
    z = ~monto_usd_mill,
    type = "heatmap",
    text = ~hover,
    hoverinfo = "text",
    colors = "Blues"
  ) |>
    plotly::layout(
      title = list(text = "Matriz sector-estado por monto total"),
      xaxis = list(title = "Estado administrativo"),
      yaxis = list(title = "Sector"),
      margin = list(l = 130, r = 30, t = 65, b = 70)
    )
}

plot_timeline <- function(timeline_tbl, title = "Línea de tiempo de presentaciones y aprobaciones") {
  if (nrow(timeline_tbl) == 0) return(empty_plot("No hay fechas informadas para construir la línea de tiempo"))

  data <- timeline_tbl |>
    dplyr::mutate(
      proyecto_plot = wrap_label(proyecto, width = 44),
      proyecto_plot = forcats::fct_reorder(proyecto_plot, fecha),
      size_plot = dplyr::case_when(is.na(monto_usd_mill) ~ 8, monto_usd_mill <= 0 ~ 8, TRUE ~ sqrt(monto_usd_mill)),
      hover = glue::glue(
        "Proyecto: {proyecto}<br>",
        "Evento: {evento}<br>",
        "Fecha: {fmt_date(fecha)}<br>",
        "Sector: {sector_simplificado}<br>",
        "Estado: {estado_simplificado}<br>",
        "Monto: {fmt_currency_mill(monto_usd_mill, accuracy = 1)}"
      )
    )

  plotly::plot_ly(
    data = data,
    x = ~fecha,
    y = ~proyecto_plot,
    type = "scatter",
    mode = "markers",
    color = ~evento,
    size = ~size_plot,
    sizes = c(8, 28),
    marker = list(opacity = 0.75, line = list(width = 1, color = "#FFFFFF")),
    text = ~hover,
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = list(text = title),
      xaxis = list(title = "Fecha"),
      yaxis = list(title = "Proyecto", automargin = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.18),
      margin = list(l = 220, r = 30, t = 65, b = 90)
    )
}

datatable_proyectos <- function(proyectos) {
  table_data <- proyectos |>
    dplyr::transmute(
      Proyecto = proyecto,
      Descripción = descripcion_del_proyecto,
      Empresa = empresa,
      `Titular / VPU` = titular_proyecto,
      CUIT = cuit,
      Sector = sector_simplificado,
      Subsector = subsector_simplificado,
      `Actividad subsector` = actividad_subsector_resolucion_mecon,
      Provincia = provincia_simplificada,
      `Localidad / región` = localidad_region,
      `Monto (mill. USD)` = monto_usd_mill,
      `Activos computables (mill. USD)` = activos_computables_usd_mill,
      `Estado administrativo` = estado_simplificado,
      `Fecha de presentación` = fmt_date(fecha_presentacion),
      `Fecha adhesión RIGI` = fmt_date(fecha_adhesion_rigi),
      `Fecha publicación BO` = fmt_date(fecha_publicacion_bo),
      `Norma de aprobación` = norma_aprobacion,
      `Preexistencia BO` = clasificacion_preexistencia_boletin_oficial,
      `Justificación preexistencia` = justificacion_preexistencia_boletin_oficial,
      `Link norma` = link_norma,
      Fuentes = fuentes,
      `Fuente analítica` = fuente_analitica
    )

  DT::datatable(
    table_data,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons", "Responsive"),
    class = "stripe hover compact nowrap",
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      pageLength = 10,
      autoWidth = TRUE,
      scrollX = TRUE,
      responsive = TRUE,
      language = list(url = "https://cdn.datatables.net/plug-ins/1.13.8/i18n/es-ES.json")
    )
  ) |>
    DT::formatRound(columns = c("Monto (mill. USD)", "Activos computables (mill. USD)"), digits = 1, mark = ".", dec.mark = ",")
}
