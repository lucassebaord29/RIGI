# Visualizaciones y componentes HTML -----------------------------------------

# Paleta visual del dashboard: colores sobrios, contrastados y aptos para lectura en web.
bar_color_approved <- "#2563EB"        # azul
bar_color_pending <- "#F97316"         # naranja
bar_color_compare_approved <- "#2563EB"
bar_color_compare_pending <- "#F97316"
bar_color_employment <- "#059669"      # verde
bar_color_neutral <- "#475569"         # slate
bar_color_timeline <- "#7C3AED"        # violeta
bar_color_timeline_border <- "#4C1D95"

# Helpers de estética ---------------------------------------------------------

wrap_axis_label <- function(x, width = 28, max_lines = 2) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- "s/d"

  wrapped <- stringr::str_wrap(x, width = width)

  vapply(wrapped, function(z) {
    parts <- unlist(strsplit(z, "\n", fixed = TRUE), use.names = FALSE)
    if (length(parts) <= max_lines) {
      return(paste(parts, collapse = "\n"))
    }

    first_line <- parts[1]
    rest <- paste(parts[-1], collapse = " ")
    second_line <- stringr::str_trunc(rest, width = width, side = "right")
    paste(first_line, second_line, sep = "\n")
  }, character(1))
}

wrap_title <- function(x, width = 78) {
  stringr::str_wrap(as.character(x), width = width)
}

format_month_year_es <- function(x) {
  meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  x <- as.Date(x)
  out <- paste0(meses[as.integer(format(x, "%m"))], "
", format(x, "%Y"))
  out[is.na(x)] <- ""
  out
}

label_box_style <- list(
  fill = "#FFFFFF",
  color = "#0F172A",
  label.size = 0,
  alpha = 0.96
)

smart_left_margin <- function(labels, min_margin = 135, max_margin = 260) {
  labels <- wrap_axis_label(labels)
  max_chars <- max(nchar(gsub("\n", "", labels)), na.rm = TRUE)
  margin <- min_margin + max(0, max_chars - 18) * 3.2
  max(min_margin, min(max_margin, margin))
}

smart_height <- function(n, min_height = 380, per_row = 34, max_height = 760) {
  n <- max(1, n)
  min(max_height, max(min_height, 170 + n * per_row))
}

theme_rigi_chart <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#1F2937"),
      plot.title = ggplot2::element_text(face = "bold", size = 15.5, color = "#0F172A", margin = ggplot2::margin(b = 7)),
      plot.subtitle = ggplot2::element_text(size = 10.5, color = "#64748B", margin = ggplot2::margin(b = 10), lineheight = 1.05),
      axis.title.x = ggplot2::element_text(size = 10.5, color = "#475569", margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "#475569", size = 10),
      axis.text.y = ggplot2::element_text(color = "#334155", size = 10.5, lineheight = 0.92),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#E2E8F0", linewidth = 0.35),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = "#334155", size = 10.5),
      plot.margin = ggplot2::margin(14, 16, 10, 12)
    )
}

style_plotly <- function(p, margin_left = 150, margin_right = 35, margin_bottom = 65,
                         margin_top = 88, height = NULL, showlegend = NULL) {
  out <- plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      margin = list(l = margin_left, r = margin_right, b = margin_bottom, t = margin_top),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif", color = "#1F2937"),
      hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#CBD5E1", font = list(color = "#0F172A"))
    )

  if (!is.null(height)) out <- out |> plotly::layout(height = height)
  if (!is.null(showlegend)) out <- out |> plotly::layout(showlegend = showlegend)

  out |> plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

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
      label_original = .data[[label_col]],
      label = forcats::fct_inorder(wrap_axis_label(.data[[label_col]], width = 30)),
      count_info = if (!is.na(count_col)) as.numeric(.data[[count_col]]) else NA_real_
    )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = monto_usd_mill,
    y = label,
    text = paste0(
      label_original,
      "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1),
      "<br>Proyectos/incidencias: ", fmt_integer(count_info)
    )
  )) +
    ggplot2::geom_col(fill = fill_color, width = 0.68, alpha = 0.97) +
    ggplot2::geom_label(
      ggplot2::aes(label = fmt_number(monto_usd_mill, accuracy = 1)),
      hjust = -0.06,
      size = 3.15,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.96
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.24))) +
    ggplot2::labs(title = wrap_title(title), subtitle = if (!is.null(subtitle)) wrap_title(subtitle, 95) else NULL, x = "Millones de USD", y = NULL) +
    theme_rigi_chart()

  style_plotly(
    p,
    margin_left = smart_left_margin(data_plot$label_original),
    height = smart_height(nrow(data_plot))
  )
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
      label_original = .data[[label_col]],
      label = forcats::fct_inorder(wrap_axis_label(.data[[label_col]], width = 30)),
      count_info = if (!is.na(count_col)) as.numeric(.data[[count_col]]) else NA_real_
    )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = empleos_directos_indirectos,
    y = label,
    text = paste0(
      label_original,
      "<br>Empleo: ", fmt_integer(empleos_directos_indirectos),
      "<br>Proyectos/incidencias: ", fmt_integer(count_info)
    )
  )) +
    ggplot2::geom_col(fill = fill_color, width = 0.68, alpha = 0.97) +
    ggplot2::geom_label(
      ggplot2::aes(label = fmt_integer(empleos_directos_indirectos)),
      hjust = -0.06,
      size = 3.15,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.96
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.24))) +
    ggplot2::labs(title = wrap_title(title), subtitle = if (!is.null(subtitle)) wrap_title(subtitle, 95) else NULL, x = "Empleos directos e indirectos", y = NULL) +
    theme_rigi_chart()

  style_plotly(
    p,
    margin_left = smart_left_margin(data_plot$label_original),
    height = smart_height(nrow(data_plot))
  )
}

plot_top_proyectos_monto <- function(data, title, fill_color = bar_color_neutral) {
  if (nrow(data) == 0 || all(is.na(data$monto_usd_mill))) return(empty_plot_message())

  data_plot <- data |>
    dplyr::filter(!is.na(monto_usd_mill)) |>
    dplyr::arrange(monto_usd_mill) |>
    dplyr::mutate(label = forcats::fct_inorder(wrap_axis_label(proyecto, width = 34)))

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
    ggplot2::geom_col(fill = fill_color, width = 0.68, alpha = 0.97) +
    ggplot2::geom_label(
      ggplot2::aes(label = fmt_number(monto_usd_mill, accuracy = 1)),
      hjust = -0.06,
      size = 3.1,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.96
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.24))) +
    ggplot2::labs(title = wrap_title(title), x = "Millones de USD", y = NULL) +
    theme_rigi_chart()

  style_plotly(
    p,
    margin_left = smart_left_margin(data_plot$proyecto, min_margin = 175, max_margin = 310),
    height = smart_height(nrow(data_plot), min_height = 430, per_row = 39)
  )
}

plot_top_proyectos_empleo <- function(data, title, fill_color = bar_color_employment) {
  if (nrow(data) == 0 || all(is.na(data$empleos_directos_indirectos))) return(empty_plot_message("No hay datos de empleo disponibles para este gráfico."))

  data_plot <- data |>
    dplyr::filter(!is.na(empleos_directos_indirectos)) |>
    dplyr::arrange(empleos_directos_indirectos) |>
    dplyr::mutate(label = forcats::fct_inorder(wrap_axis_label(proyecto, width = 34)))

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
    ggplot2::geom_col(fill = fill_color, width = 0.68, alpha = 0.97) +
    ggplot2::geom_label(
      ggplot2::aes(label = fmt_integer(empleos_directos_indirectos)),
      hjust = -0.06,
      size = 3.1,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.96
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.24))) +
    ggplot2::labs(title = wrap_title(title), x = "Empleos directos e indirectos", y = NULL) +
    theme_rigi_chart()

  style_plotly(
    p,
    margin_left = smart_left_margin(data_plot$proyecto, min_margin = 175, max_margin = 310),
    height = smart_height(nrow(data_plot), min_height = 430, per_row = 39)
  )
}

plot_estado <- function(data) {
  if (nrow(data) == 0) return(empty_plot_message())

  data_plot <- data |>
    dplyr::mutate(
      label_original = estado_simplificado,
      label = forcats::fct_reorder(wrap_axis_label(estado_simplificado, width = 28), n_proyectos)
    )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = n_proyectos,
    y = label,
    text = paste0(
      label_original,
      "<br>Proyectos: ", fmt_integer(n_proyectos),
      "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1)
    )
  )) +
    ggplot2::geom_col(fill = "#0891B2", width = 0.68, alpha = 0.97) +
    ggplot2::geom_label(
      ggplot2::aes(label = fmt_integer(n_proyectos)),
      hjust = -0.07,
      size = 3.2,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.96
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.24))) +
    ggplot2::labs(title = "Proyectos por estado administrativo", x = "Cantidad de proyectos", y = NULL) +
    theme_rigi_chart()

  style_plotly(p, margin_left = smart_left_margin(data_plot$label_original), height = smart_height(nrow(data_plot), min_height = 320))
}

plot_compare_aprobado_pendiente <- function(aprobado_tbl, pendiente_tbl, label_col, title) {
  data_plot <- dplyr::bind_rows(
    aprobado_tbl |> dplyr::transmute(label_original = .data[[label_col]], estado = "Aprobado", monto_usd_mill),
    pendiente_tbl |> dplyr::transmute(label_original = .data[[label_col]], estado = "Pendiente / en evaluación", monto_usd_mill)
  ) |>
    dplyr::filter(!is.na(monto_usd_mill)) |>
    dplyr::group_by(label_original) |>
    dplyr::mutate(total_label = sum(monto_usd_mill, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(total_label) |>
    dplyr::mutate(label = forcats::fct_inorder(wrap_axis_label(label_original, width = 30)))

  if (nrow(data_plot) == 0) return(empty_plot_message())

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = monto_usd_mill,
    y = label,
    fill = estado,
    text = paste0(label_original, "<br>", estado, "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1))
  )) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62, alpha = 0.97) +
    ggplot2::geom_label(
      ggplot2::aes(label = fmt_number(monto_usd_mill, accuracy = 1)),
      position = ggplot2::position_dodge(width = 0.72),
      hjust = -0.07,
      size = 2.85,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.96,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = c("Aprobado" = bar_color_compare_approved, "Pendiente / en evaluación" = bar_color_compare_pending)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.30))) +
    ggplot2::labs(title = wrap_title(title), x = "Millones de USD", y = NULL, fill = NULL) +
    theme_rigi_chart()

  style_plotly(
    p,
    margin_left = smart_left_margin(data_plot$label_original),
    height = smart_height(dplyr::n_distinct(data_plot$label_original), min_height = 430, per_row = 38)
  )
}

plot_compare_counts_montos <- function(ind) {
  data_plot <- tibble::tibble(
    grupo_original = c("Aprobados", "Pendientes / en evaluación"),
    proyectos = c(ind$n_aprobados, ind$n_pendientes),
    monto_usd_mill = c(ind$monto_aprobado, ind$monto_pendiente)
  ) |>
    dplyr::mutate(grupo = wrap_axis_label(grupo_original, width = 18))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = grupo,
    y = monto_usd_mill,
    fill = grupo_original,
    text = paste0(grupo_original, "<br>Proyectos: ", fmt_integer(proyectos), "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1))
  )) +
    ggplot2::geom_col(width = 0.58, alpha = 0.97) +
    ggplot2::geom_label(
      ggplot2::aes(label = fmt_number(monto_usd_mill, accuracy = 1)),
      vjust = -0.25,
      size = 3.3,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.96
    ) +
    ggplot2::scale_fill_manual(values = c("Aprobados" = bar_color_compare_approved, "Pendientes / en evaluación" = bar_color_compare_pending)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::labs(title = "Monto informado: aprobados vs. pendientes", x = NULL, y = "Millones de USD") +
    theme_rigi_chart() +
    ggplot2::theme(legend.position = "none")

  style_plotly(p, margin_left = 60, height = 410, showlegend = FALSE)
}

plot_timeline <- function(data, date_col = "fecha_aprobacion", title = "Línea de tiempo") {
  if (nrow(data) == 0 || all(is.na(data[[date_col]]))) return(empty_plot_message())

  data_plot <- data |>
    dplyr::filter(!is.na(.data[[date_col]])) |>
    dplyr::arrange(.data[[date_col]]) |>
    dplyr::mutate(
      y_pos = dplyr::row_number(),
      monto_label = fmt_number(monto_usd_mill, accuracy = 1)
    )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(
    x = .data[[date_col]],
    y = y_pos,
    size = monto_usd_mill,
    text = paste0(
      proyecto,
      "<br>Fecha: ", fmt_date(.data[[date_col]]),
      "<br>Estado: ", estado_simplificado,
      "<br>Monto: ", fmt_currency_mill(monto_usd_mill, accuracy = 1),
      "<br>Tamaño del punto: monto del proyecto"
    )
  )) +
    ggplot2::geom_point(color = bar_color_timeline, fill = bar_color_timeline, alpha = 0.78) +
    ggplot2::geom_label(
      ggplot2::aes(label = monto_label),
      nudge_y = 0.24,
      size = 2.75,
      fill = "#FFFFFF",
      color = "#0F172A",
      label.size = 0,
      alpha = 0.94,
      show.legend = FALSE
    ) +
    ggplot2::scale_size_continuous(
      range = c(5, 18),
      name = "Monto del proyecto\n(millones de USD)",
      labels = function(x) fmt_number(x, accuracy = 1)
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 month",
      labels = format_month_year_es,
      expand = ggplot2::expansion(mult = c(0.04, 0.08))
    ) +
    ggplot2::labs(
      title = wrap_title(title),
      subtitle = "Lectura mensual: cada punto representa un proyecto; el tamaño del punto indica el monto informado del proyecto.",
      x = "Mes",
      y = NULL
    ) +
    theme_rigi_chart() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9.5, lineheight = 0.95),
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 9.5),
      legend.title = ggplot2::element_text(size = 9.5, face = "bold", color = "#334155")
    )

  style_plotly(p, margin_left = 55, margin_right = 118, margin_bottom = 86, height = 460, showlegend = TRUE)
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
    class = "stripe hover order-column compact",
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
