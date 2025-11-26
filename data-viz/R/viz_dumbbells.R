gen_large_dumbbells <- function(
    data, 
    p0 = "2018", 
    p1 = "2025",
    c0 = "#b200aa",
    c1 = "#575796",
    ylabs_var = "label",
    pivot_var = "year", 
    value_var = "value2plot",
    panel_var = "panel",
    id_vars = c("country", "variable")
){
  
  data4dumbbells <- data %>% 
    rename(
      ylabs = all_of(ylabs_var),
      pivot = all_of(pivot_var),
      value = all_of(value_var),
      panel = all_of(panel_var)
    ) %>% 
    pivot_wider(
      id_cols = c(id_vars, ylabs, panel), 
      names_from = pivot, 
      values_from = value
    ) %>% 
    mutate(
      panel_label = glue::glue("<span style='color:#575796;'><b><i>{panel}</b></i></span>")
    ) %>% 
    rename(
      p0 = all_of(p0),
      p1 = all_of(p1)
    ) %>% 
    mutate(
      pdiff = p1-p0,
      label_position_p0 = if_else(pdiff > 0, p0-5, p0+5),
      label_position_p1 = if_else(pdiff > 0, p1+5, p1-5)
    )
  
  ggplot(
    data4dumbbells,
    aes(
      y = ylabs
    )
  ) +
    geom_segment(
      aes(
        x = p0,
        xend = p1
      ),
      color = "#e5e8e8",
      linewidth = 4,
    ) +
    geom_point(
      aes(
        x = p1
      ),
      size  = 3.5,
      color = c1
    ) +
    geom_point(
      aes(
        x = p0
      ),
      size  = 3.5,
      color = c0
    ) +
    geom_text(
      aes(
        x = label_position_p0,
        label = paste0(
          format(
            round(p0,0),
            nsmall=0
          ),"%"
        )
      ),
      family   = "inter",
      fontface = "plain",
      color    = c0,
      size     = 5,
      na.rm    = TRUE
    ) +
    geom_text(
      aes(
        x = label_position_p1,
        label = paste0(
          format(
            round(p1,0),
            nsmall=0
          ),"%"
        )
      ),
      family   = "inter",
      fontface = "plain",
      color    = c1,
      size     = 5,
      na.rm    = TRUE
    ) +
    facet_grid(
      rows   = vars(panel),
      scales = "free",
      space  = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(-5,105),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      position = "top"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a",
        size   = 13
      ),
      axis.text.y = ggtext::element_markdown(
        size   = 14,
        hjust  = 0,
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x.top = element_line(
        linetype = "solid",
        linewidth = 0.5,
        color = "#1a1a1a"
      ),
      panel.grid.minor.x = element_blank(),
      panel.spacing      = unit(12, "mm"),
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      strip.text.y.left  = element_text(
        angle  = 0,
        size   = 16,
        color  = "#575796",
        hjust  = 0, 
        vjust  = 1, 
        family = "inter",
        face   = "bold.italic",
        margin = margin(-15,-20,0,0)
      ),
      strip.switch.pad.grid = unit(-25, "mm"),
      strip.placement = "outside",
      strip.clip      = "off"
    )
}

gen_single_dumbbells <- function(
    data, 
    p0 = "2018", 
    p1 = "2025",
    ylabs_var = "variable",
    pivot_var = "year", 
    value_var = "value2plot",
    id_vars = "country"
){
  
  data4dumbbells <- data %>% 
    rename(
      ylabs = all_of(ylabs_var),
      pivot = all_of(pivot_var),
      value = all_of(value_var)
    ) %>% 
    pivot_wider(
      id_cols = c(id_vars, ylabs), 
      names_from = pivot, 
      values_from = value
    ) %>% 
    rename(
      p0 = all_of(p0),
      p1 = all_of(p1)
    ) %>% 
    mutate(
      pdiff = p1-p0,
      label_position_p0 = if_else(pdiff > 0, p0-5, p0+5),
      label_position_p1 = if_else(pdiff > 0, p1+5, p1-5)
    )
  
  ggplot(
    data4dumbbells,
    aes(
      y = ylabs
    )
  ) +
    geom_segment(
      aes(
        x = p0,
        xend = p1
      ),
      color = "#e5e8e8",
      linewidth = 4,
    ) +
    geom_point(
      aes(
        x = p1
      ),
      size  = 3.5,
      color = "#575796"
    ) +
    geom_point(
      aes(
        x = p0
      ),
      size  = 3.5,
      color = "#b200aa"
    ) +
    geom_text(
      aes(
        x = label_position_p0,
        label = paste0(
          format(
            round(p0,0),
            nsmall=0
          ),"%"
        )
      ),
      family   = "inter",
      fontface = "plain",
      color    = "#b200aa",
      size     = 3,
      na.rm    = TRUE
    ) +
    geom_text(
      aes(
        x = label_position_p1,
        label = paste0(
          format(
            round(p1,0),
            nsmall=0
          ),"%"
        )
      ),
      family   = "inter",
      fontface = "plain",
      color    = "#575796",
      size     = 3,
      na.rm    = TRUE
    ) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(-8,105),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      position = "top"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a",
        size   = 9
      ),
      axis.text.y = ggtext::element_markdown(
        size   = 9,
        hjust  = 0,
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      axis.line.x.top = element_line(
        linetype = "solid",
        linewidth = 0.5,
        color = "#1a1a1a"
      ),
      panel.grid.minor.x = element_blank()
    )
  
}
