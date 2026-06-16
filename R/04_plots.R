# Visualizaciones y componentes HTML -----------------------------------------

bar_color_approved <- "#0B4F6C"
bar_color_pending <- "#F59E0B"
bar_color_compare_approved <- "#0B4F6C"
bar_color_compare_pending <- "#F59E0B"
bar_color_employment <- "#145DA0"
bar_color_neutral <- "#334155"

make_source_note <- function() {
  htmltools::div(
    class = "source-note-box",
    htmltools::strong("Aclaración metodológica: "),
    "Para los proyectos aprobados, se utilizó la información del Boletín Oficial y las empresas inferidas por ",
    htmltools::a(
      "Globaris",
      href = "https://app.powerbi.com/view?r=eyJrIjoiNTFjY2E4NTYtOTVlNy00YmFiLWIwYmMtNWZkMjE4OTNhYmRiIiwidCI6IjNlMDUxM2Q2LTY4ZmEtNDE2ZS04ZGUxLTZjNWNkYzMxOWZmYSIsImMiOjR9&pageName=d1ee75596a51a9bde708",
      target = "_blank"
    ),
    ". Para los proyectos en evaluación, se utilizó la información del dashboard de ",
    htmltools::a(
      "Globaris",
      href = "https://app.powerbi.com/view?r=eyJrIjoiNTFjY2E4NTYtOTVlNy00YmFiLWIwYmMtNWZkMjE4OTNhYmRiIiwidCI6IjNlMDUxM2Q2LTY4ZmEtNDE2ZS04ZGUxLTZjNWNkYzMxOWZmYSIsImMiOjR9&pageName=d1ee75596a51a9bde708",
      target = "_blank"
    ),
    ". Los datos de empleos directos e indirectos se obtuvieron de la página web del ",
    htmltools::a(
      "Ministerio de Economía",
      href = "https://www.argentina.gob.ar/economia/rigi",
      target = "_blank"
    ),
    "."
  )
}

make_province_note <- function() {
  htmltools::div(
    class = "note-box",
    "Cuando un proyecto tiene más de una provincia separada por ';', el monto, los activos computables y el empleo se asignan proporcionalmente entre las provincias informadas para evitar doble conteo."
  )
}

make_download_links <- function(type = c("aprobados", "pendientes")) {
  type <- match.arg(type)
  label <- if (type == "aprobados") "aprobados" else "pendientes"
  htmltools::div(
    class = "download-box",
    htmltools::strong(paste0("Descargas de la base de ", label, ": ")),
    htmltools::a(
      "Excel",
      href = paste0("downloads/base_interactiva_", type, ".xlsx"),
      target = "_blank",
      class = "download-button"
    ),
    htmltools::a(
      "CSV",
      href = paste0("downloads/base_interactiva_", type, ".csv"),
      target = "_blank",
      class = "download-button"
    )
  )
}

make_kpi_card <- function(label, value, sublabel = NULL) {
  htmltools::div(
    class = "kpi-card",
    htmltools::div(class = "kpi-label", label),
    htmltools::div(class = "kpi-value", value),
    if (!is.null(sublabel)) htmltools::div(class = "kpi-sublabel", sublabel)
  )
}

make_kpi_cards_aprobados <- function(ind) {
  htmltools::div(
    class = "kpi-grid kpi-grid-approved",
    make_kpi_card("Proyectos aprobados", fmt_integer(ind$n_aprobados), "Cantidad de proyectos"),
    make_kpi_card("Monto aprobado", fmt_currency_mill(ind$monto_aprobado, accuracy = 1), "Millones de USD"),
    make_kpi_card("Activos computables aprobados", fmt_currency_mill(ind$activos_aprobados, accuracy = 1), "Millones de USD"),
    make_kpi_card("Empleo informado", fmt_integer(ind$empleos_aprobados), "Directos e indirectos"),
    make_kpi_card("Monto promedio aprobado", fmt_currency_mill(ind$monto_promedio_aprobado, accuracy = 1), "Por proyecto aprobado"),
    make_kpi_card("Monto mediano aprobado", fmt_currency_mill(ind$monto_mediano_aprobado, accuracy = 1), "Por proyecto aprobado")
  )
}

make_kpi_cards_empleo_aprobado <- function(ind) {
  htmltools::div(
    class = "kpi-grid kpi-grid-employment",
    make_kpi_card("Empleo total aprobado", fmt_integer(ind$empleos_aprobados), "Directos e indirectos"),
    make_kpi_card("Empleo promedio aprobado", fmt_integer(ind$empleos_promedio_aprobados), "Por proyecto aprobado"),
    make_kpi_card("Empleo mediano aprobado", fmt_integer(ind$empleos_mediana_aprobados), "Por proyecto aprobado"),
    make_kpi_card("Proyecto con mayor empleo", ind$empleo_top_proyecto, "Entre aprobados"),
    make_kpi_card("Sector con mayor empleo", ind$empleo_top_sector, "Entre aprobados"),
    make_kpi_card("Provincia con mayor empleo", ind$empleo_top_provincia, "Asignación proporcional si es multiprovincial")
  )
}

make_kpi_cards_pendientes <- function(ind) {
  htmltools::div(
    class = "kpi-grid kpi-grid-pending",
    make_kpi_card("Proyectos pendientes/en evaluación", fmt_integer(ind$n_pendientes), "Cantidad de proyectos"),
    make_kpi_card("Monto pendiente/en evaluación", fmt_currency_mill(ind$monto_pendiente, accuracy = 1), "Millones de USD"),
    make_kpi_card("Activos computables pendientes", fmt_currency_mill(ind$activos_pendientes, accuracy = 1), "Millones de USD"),
    make_kpi_card("Monto promedio pendiente", fmt_currency_mill(ind$monto_promedio_pendiente, accuracy = 1), "Por proyecto pendiente"),
    make_kpi_card("Monto mediano pendiente", fmt_currency_mill(ind$monto_mediano_pendiente, accuracy = 1), "Por proyecto pendiente")
  )
}

make_kpi_cards_total <- function(ind) {
  htmltools::div(
    class = "kpi-grid kpi-grid-status",
    make_kpi_card("Total de proyectos", fmt_integer(ind$n_total), "Universo de la base"),
    make_kpi_card("Monto total informado", fmt_currency_mill(ind$monto_total, accuracy = 1), "Millones de USD"),
    make_kpi_card("Aprobados / total", fmt_pct(ind$participacion_proyectos_aprobados), "Participación en cantidad"),
    make_kpi_card("Monto aprobado / total", fmt_pct(ind$participacion_monto_aprobado), "Participación en monto")
  )
}

empty_plot_message <- function(message = "No hay datos disponibles para este gráfico.") {
  htmltools::div(class = "empty-plot-box", message)
}

plot_bar_monto <- function(data, label_col, title, subtitle = NULL, fill_color = bar_color_neutral) {
  if (nrow(data) == 0 || all(is.na(data$monto_usd_mill))) return(empty_plot_message())

  count_col <- dplyr::case_when(
    "n_proyectos" %in% names(data) ~ "n_proyectos",
    "n_incidencias_provinciales" %in% names(data) ~ "n_incidencias_provinciales",
    TRUE ~ NA_character_
  )

  data_plot <- data |>
    dplyr::filter(!is.na(monto_usd_mill)) |>
    dplyr::arrange(monto_usd_mill) |>
    dplyr::mutate(
      label = forcats::fct_inorder(.data[[label_col]]),
      count_info = if (!is.na(count_col)) as.numeric(.data[[count_col]]) else NA_real_
    )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = monto_usd_mill,
    y = label,
    text = paste0(
      .data[[label_col]],
      "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1),
      "<br>Proyectos/incidencias: ", fmt_integer(count_info)
    )
  )) +
    ggplot2::geom_col(fill = fill_color, width = 0.72) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Millones de USD", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 10.5),
      panel.grid.major.y = ggplot2::element_blank()
    )

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(margin = list(l = 130, r = 30, b = 60, t = 80))
}

plot_bar_empleo <- function(data, label_col, title, subtitle = NULL, fill_color = bar_color_employment) {
  if (nrow(data) == 0 || all(is.na(data$empleos_directos_indirectos))) return(empty_plot_message("No hay datos de empleo disponibles para este gráfico."))

  count_col <- dplyr::case_when(
    "n_proyectos" %in% names(data) ~ "n_proyectos",
    "n_incidencias_provinciales" %in% names(data) ~ "n_incidencias_provinciales",
    TRUE ~ NA_character_
  )

  data_plot <- data |>
    dplyr::filter(!is.na(empleos_directos_indirectos)) |>
    dplyr::arrange(empleos_directos_indirectos) |>
    dplyr::mutate(
      label = forcats::fct_inorder(.data[[label_col]]),
      count_info = if (!is.na(count_col)) as.numeric(.data[[count_col]]) else NA_real_
    )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = empleos_directos_indirectos,
    y = label,
    text = paste0(
      .data[[label_col]],
      "<br>Empleo: ", fmt_integer(empleos_directos_indirectos),
      "<br>Proyectos/incidencias: ", fmt_integer(count_info)
    )
  )) +
    ggplot2::geom_col(fill = fill_color, width = 0.72) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Empleos directos e indirectos", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 10.5),
      panel.grid.major.y = ggplot2::element_blank()
    )

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(margin = list(l = 160, r = 30, b = 60, t = 80))
}

plot_top_proyectos_monto <- function(data, title, fill_color = bar_color_neutral) {
  if (nrow(data) == 0 || all(is.na(data$monto_usd_mill))) return(empty_plot_message())

  data_plot <- data |>
    dplyr::filter(!is.na(monto_usd_mill)) |>
    dplyr::arrange(monto_usd_mill) |>
    dplyr::mutate(label = forcats::fct_inorder(proyecto))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = monto_usd_mill,
    y = label,
    text = paste0(
      proyecto,
      "<br>Empresa: ", dplyr::coalesce(empresa, "s/d"),
      "<br>Sector: ", dplyr::coalesce(sector, "s/d"),
      "<br>Provincia: ", dplyr::coalesce(provincia_original, "s/d"),
      "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1)
    )
  )) +
    ggplot2::geom_col(fill = fill_color, width = 0.72) +
    ggplot2::labs(title = title, x = "Millones de USD", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      panel.grid.major.y = ggplot2::element_blank()
    )

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(margin = list(l = 210, r = 30, b = 60, t = 80))
}

plot_top_proyectos_empleo <- function(data, title, fill_color = bar_color_employment) {
  if (nrow(data) == 0 || all(is.na(data$empleos_directos_indirectos))) return(empty_plot_message("No hay datos de empleo disponibles para este gráfico."))

  data_plot <- data |>
    dplyr::filter(!is.na(empleos_directos_indirectos)) |>
    dplyr::arrange(empleos_directos_indirectos) |>
    dplyr::mutate(label = forcats::fct_inorder(proyecto))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = empleos_directos_indirectos,
    y = label,
    text = paste0(
      proyecto,
      "<br>Empresa: ", dplyr::coalesce(empresa, "s/d"),
      "<br>Sector: ", dplyr::coalesce(sector, "s/d"),
      "<br>Provincia: ", dplyr::coalesce(provincia_original, "s/d"),
      "<br>Empleo: ", fmt_integer(empleos_directos_indirectos)
    )
  )) +
    ggplot2::geom_col(fill = fill_color, width = 0.72) +
    ggplot2::labs(title = title, x = "Empleos directos e indirectos", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      panel.grid.major.y = ggplot2::element_blank()
    )

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(margin = list(l = 210, r = 30, b = 60, t = 80))
}

plot_estado <- function(data) {
  if (nrow(data) == 0) return(empty_plot_message())

  data_plot <- data |>
    dplyr::mutate(label = forcats::fct_reorder(estado_simplificado, n_proyectos))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = n_proyectos,
    y = label,
    text = paste0(
      estado_simplificado,
      "<br>Proyectos: ", fmt_integer(n_proyectos),
      "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1)
    )
  )) +
    ggplot2::geom_col(fill = "#145DA0", width = 0.72) +
    ggplot2::labs(title = "Proyectos por estado administrativo", x = "Cantidad de proyectos", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15))

  plotly::ggplotly(p, tooltip = "text")
}

plot_compare_aprobado_pendiente <- function(aprobado_tbl, pendiente_tbl, label_col, title) {
  data_plot <- dplyr::bind_rows(
    aprobado_tbl |> dplyr::transmute(label = .data[[label_col]], estado = "Aprobado", monto_usd_mill),
    pendiente_tbl |> dplyr::transmute(label = .data[[label_col]], estado = "Pendiente / en evaluación", monto_usd_mill)
  ) |>
    dplyr::filter(!is.na(monto_usd_mill)) |>
    dplyr::group_by(label) |>
    dplyr::mutate(total_label = sum(monto_usd_mill, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(total_label) |>
    dplyr::mutate(label = forcats::fct_inorder(label))

  if (nrow(data_plot) == 0) return(empty_plot_message())

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = monto_usd_mill,
    y = label,
    fill = estado,
    text = paste0(label, "<br>", estado, "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1))
  )) +
    ggplot2::geom_col(position = "dodge", width = 0.72) +
    ggplot2::scale_fill_manual(values = c("Aprobado" = bar_color_compare_approved, "Pendiente / en evaluación" = bar_color_compare_pending)) +
    ggplot2::labs(title = title, x = "Millones de USD", y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15))

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(margin = list(l = 160, r = 30, b = 60, t = 80))
}

plot_compare_counts_montos <- function(ind) {
  data_plot <- tibble::tibble(
    grupo = c("Aprobados", "Pendientes / en evaluación"),
    proyectos = c(ind$n_aprobados, ind$n_pendientes),
    monto_usd_mill = c(ind$monto_aprobado, ind$monto_pendiente)
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = grupo,
    y = monto_usd_mill,
    fill = grupo,
    text = paste0(grupo, "<br>Proyectos: ", fmt_integer(proyectos), "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1))
  )) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::scale_fill_manual(values = c("Aprobados" = bar_color_compare_approved, "Pendientes / en evaluación" = bar_color_compare_pending)) +
    ggplot2::labs(title = "Monto informado: aprobados vs. pendientes", x = NULL, y = "Millones de USD") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold", size = 15))

  plotly::ggplotly(p, tooltip = "text")
}

plot_timeline <- function(data, date_col = "fecha_aprobacion", title = "Línea de tiempo") {
  if (nrow(data) == 0 || all(is.na(data[[date_col]]))) return(empty_plot_message())

  data_plot <- data |>
    dplyr::filter(!is.na(.data[[date_col]])) |>
    dplyr::arrange(.data[[date_col]]) |>
    dplyr::mutate(y_pos = dplyr::row_number())

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = .data[[date_col]],
    y = y_pos,
    size = monto_usd_mill,
    text = paste0(
      proyecto,
      "<br>Fecha: ", fmt_date(.data[[date_col]]),
      "<br>Estado: ", estado_simplificado,
      "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1)
    )
  )) +
    ggplot2::geom_point(color = "#0B4F6C", alpha = 0.85) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 15)
    )

  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(showlegend = FALSE)
}

make_datatable <- function(data, caption = NULL) {
  data_display <- data |>
    dplyr::select(
      proyecto,
      empresa,
      titular_proyecto,
      sector,
      subsector,
      provincia_original,
      localidad_region,
      monto_usd_mill,
      activos_computables_usd_mill,
      empleos_directos_indirectos,
      estado,
      fecha_presentacion,
      fecha_aprobacion,
      norma_aprobacion,
      link_norma,
      fuentes
    ) |>
    dplyr::rename(
      Proyecto = proyecto,
      Empresa = empresa,
      `Titular / VPU` = titular_proyecto,
      Sector = sector,
      Subsector = subsector,
      Provincia = provincia_original,
      `Localidad / región` = localidad_region,
      `Monto (mill. USD)` = monto_usd_mill,
      `Activos Computables (mill. USD)` = activos_computables_usd_mill,
      `Empleos (directos e indirectos)` = empleos_directos_indirectos,
      `Estado administrativo` = estado,
      `Fecha de presentación` = fecha_presentacion,
      `Fecha de aprobación` = fecha_aprobacion,
      `Norma de aprobación` = norma_aprobacion,
      `Link norma` = link_norma,
      Fuentes = fuentes
    )

  DT::datatable(
    data_display,
    caption = caption,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons"),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      language = list(url = "https://cdn.datatables.net/plug-ins/1.13.7/i18n/es-ES.json")
    )
  ) |>
    DT::formatRound(columns = c("Monto (mill. USD)", "Activos Computables (mill. USD)"), digits = 1, mark = ".", dec.mark = ",") |>
    DT::formatRound(columns = c("Empleos (directos e indirectos)"), digits = 0, mark = ".", dec.mark = ",")
}
