gen_logit <- function(){
  
  data4logit <- thai_gpp %>% 
    filter(
      year==2025
    ) %>% 
    mutate(
      safe=case_when(
        q9<=2~1,
        q9<=4~0
      ),
      young=case_when(
        age<=31 ~ 1,
        TRUE ~ 0
      ),
      rural=case_when(
        location=="Rural" ~ 1,
        location=="Urban" ~ 0,
      ),
      fin=case_when(
        fin<=2 ~ 1,
        fin<=5 ~ 0,
      ),
      higher_edu=case_when(
        edu %in% c(5,6) ~ 1,
        edu %in% c(1,2,3,4,7) ~ 0
      ),
      female=case_when(
        gender=="Female"~1,
        gender=="Male"~0
      )
    )
  
  model_logit <- glm(
    safe~female+young+rural+fin+higher_edu+crime4,
    family  = binomial(link = "logit"),
    data    = data4logit
  )
  
  mgeffects <- marginaleffects:: avg_comparisons(model_logit)
  
  ggplot(
    data = mgeffects %>% 
      mutate(
        label = case_when(
          term == "higher_edu" ~ "Higher Education",
          term == "fin" ~ "Financially Constrained",
          term == "female" ~ "Female",
          term == "rural"  ~ "Rural",
          term == "young"  ~ "Young Adult",
          term == "crime4" ~ "Previous Crime Victim",
        )
      ),
    aes(
      x = estimate,
      y = label
    )
  ) +
    geom_vline(
      xintercept=0,
      linetype="solid",
      color="red"
    ) +
    geom_segment(
      aes(
        x = conf.low,
        xend = conf.high
      ),
      color = "#BEBEDA",
      linewidth = 3,
    ) +
    geom_point(
      aes(
        x = estimate
      ),
      size  = 3.5,
      color = "#575796"
    ) +
    scale_x_continuous(
      position = "top",
      limits = c(-0.3, +0.3),
      breaks = seq(-3, +0.3, 0.1)
    ) +
    labs(x = "Less likely                                 More likely\n") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a",
        size   = 11
      ),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a",
        size   = 11
      ),
      axis.text.y = ggtext::element_markdown(
        size   = 11,
        hjust  = 0,
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        linetype = "dashed",
        linewidth = 0.5,
        color = "grey75"
      ),
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