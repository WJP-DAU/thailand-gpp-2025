donut_plot <- function(data = data2plot) {
  # Colores fijos por categoría
  legend_color <- c(
    "Positive" = "#003B88",
    "Neutral"  = "#B5B5B5",
    "Negative" = "#fa4d57"
  )
  
  # Vector nombrado: nombre = categoría, valor = texto de leyenda
  # (toma 1 texto por categoría desde la base)
  legend_labels <- data |>
    dplyr::distinct(category, category_label) |>
    dplyr::filter(category %in% names(legend_color)) |>
    dplyr::mutate(
      category = factor(
        category,
        levels = c("Positive", "Negative", "Neutral")   # orden deseado
      )
    ) |>
    dplyr::arrange(category) |>
    tibble::deframe()   # => c("Positive"="...", "Negative"="...", "Neutral"="...")
  
  p <- ggplot(
    data,
    aes(
      fill = category,
      ymax = ymax,
      ymin = ymin,
      xmax = 2,
      xmin = 1
    )
  ) +
    geom_rect() +
    coord_polar(theta = "y", start = -pi/2) +  # media dona
    geom_text(
      aes(
        label = label,
        y     = labpos,
        x     = 1.5
      ),
      color    = "white",
      size     = 4.5,
      family   = "inter",
      fontface = "bold"
    ) +
    scale_x_continuous(limits = c(0, 2)) +
    scale_y_continuous(limits = c(0, 200)) +
    scale_fill_manual(
      values = legend_color,
      breaks = names(legend_labels),   # asegura orden de la leyenda
      labels = legend_labels           # textos tomados de la BD
    ) +
    labs(y = "", x = "") +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.ticks         = element_blank(),
      axis.title.x       = element_blank(),
      axis.text.x        = element_blank(),
      axis.text.y        = element_blank(),
      
      # ---- Leyenda ----
      legend.background      = element_rect(fill = NA, colour = NA),
      legend.position        = c(-2, 0.75),
      legend.justification   = c(0, 0.5),
      legend.title           = element_blank(),
      legend.text            = element_text(size = 14, family = "inter"),
      legend.key.width       = unit(0.4, "cm"),
      legend.key.height      = unit(0.4, "cm"),
      legend.key.justification = "top",
      legend.key             = element_rect(fill = NA, colour = NA),
      legend.box.spacing     = unit(1, "cm"),
      legend.key.spacing.y   = unit(0.4, "cm"),
      legend.key.spacing.x   = unit(0.3, "cm"),
      
      plot.margin            = margin(10, 10, 10, 450)
    ) +
    guides(
      fill = guide_legend(
        byrow         = TRUE,
        label.position = "right",
        label.hjust   = 0,
        label.vjust   = 0.5,
        direction     = "vertical"
      )
    )
  
  return(p)
}

