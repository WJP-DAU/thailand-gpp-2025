## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Data Visualization
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     October 21, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (interactive()){
  source("R/config.R")
  source("R/outline.R")
}
suppressMessages(
  suppressWarnings(
    library(tidyverse, quietly = TRUE)
  )
)
library(ggrepel)
library(ggtext)

thai_data_bank <- readr::read_csv(
  "../data/thailand_data_bank.csv",
  show_col_types = FALSE
)

WJPr::wjp_fonts()

if(!interactive()){
  verbose_message("--- Preparing individual tabs for visualizations...")
}
data_tabs <- purrr::imap(
  outline,
  function(figure, figure_id){
    
    years <- as.numeric(figure[["years"]])
    variables <- figure[["var_id"]]
    demographic <- figure[["sample"]]
    reporting_values <- as.numeric(figure[["reportValues"]])
    
    data2plot <- thai_data_bank %>% 
      dplyr::filter(
        (variable %in% variables) &
          (year %in% years) &
          (target %in% reporting_values) &
          (sample %in% demographic)
      ) %>% 
      dplyr::group_by(country, year, variable) %>% 
      dplyr::summarise(
        value2plot = sum(ratio)*100,
        .groups = "keep"
      )
    
    # Specific changes to data tabs
    if(figure_id %in% c("Figure_4_A")){
      data2plot <- data2plot %>% 
        dplyr::mutate(
          variable = dplyr::case_when(
            variable == "q17_1"  ~ "Ancestry",
            variable == "q17_2"  ~ "Gender",
            variable == "q17_3"  ~ "Race",
            variable == "q17_4"  ~ "Age",
            variable == "q17_5"  ~ "Religion",
            variable == "q17_6"  ~ "Physical Appareance",
            variable == "q17_7"  ~ "Physical Appareance",
            variable == "q17_8"  ~ "Physical Appareance",
            variable == "q17_9"  ~ "Physical or Mental Disability",
            variable == "q17_10" ~ "Sexual Orientation",
            variable == "q17_11" ~ "Education",
            variable == "q17_12" ~ "Nationality",
            variable == "q17_13" ~ "Skin Color",
            variable == "q17_14" ~ "Tribe"
          )
        )
    }
    
    if(figure_id %in% c("Figure_9_A")){
      data2plot <- data2plot %>% 
        dplyr::filter(
          variable != "q4b"
        ) %>% 
        dplyr::mutate(
          variable = dplyr::case_when(
            variable == "q4a"  ~ "Government Permit",
            variable == "q4b"  ~ "Public Benefits",
            variable == "q4c"  ~ "Birth Certificate",
            variable == "q4d"  ~ "Public School Placement",
            variable == "q4e"  ~ "Public Health Service"
          )
        )
    }
    
    if(figure_id %in% c("Figure_12_A")){
      data2plot <- data2plot %>%
        dplyr::group_by(variable) %>% 
        dplyr::mutate(
          label = case_when(
            variable == 'q49a'    ~ paste("Is **effective** in bringing<br>people who commit<br>crimes to justice"),
            variable == 'q49b_G2' ~ paste("Ensures **equal treatment<br>of victims** by allowing all<br>",
                                          "victims to seek justice<br>regardless of who they are"),
            variable == 'q49e_G2' ~ paste("Safeguards the<br>**presumption of<br>innocence** by treating<br>those",
                                          "accused of<br>crimes as innocent<br>until proven guilty"),
            variable == 'q49c_G2' ~ paste("Ensures **equal treatment of<br>the accused** by giving all a<br>",
                                          "fair trial regardless of who<br>they are"),
            variable == 'q49e_G1' ~ paste("Gives **appropriate<br>punishments** that fit<br>the crime"),
            variable == 'q49d_G1' ~ paste("Ensures **uniform quality** by<br>providing equal service<br>",
                                          "regardless of where<br>they live",
                                          "</span>"),
            variable == 'q49c_G1' ~ paste("Ensures everyone<br>has **access** to the<br>justice system"),
            variable == 'q49b_G1' ~ paste("Ensures **timeliness**<br>by dealing with<br>cases promptly",
                                          "and<br>efficiently")
          ),
          value_2025   = value2plot,
          value_2025   = if_else(year == 2025, value_2025, NA_real_),
          value_2025   = first(value_2025, na_rm = T),
          value_2025   = paste0(format(round(value_2025, 0), nsmall=0), "%"),
          
          value_2018 = value2plot,
          value_2018 = if_else(year == 2018, value_2018, NA_real_),
          value_2018 = first(value_2018, na_rm = T),
          value_2018 = paste0(format(round(value_2018, 0), nsmall=0), "%"),
          
          axis_label_md = paste0(
            "<span style='color:#49178e;font-size:4.217518mm'>",value_2025,"</span>",
            "<span style='color:#524F4C;font-size:4.217518mm'> | </span>",
            "<span style='color:#dd58b1;font-size:4.217518mm'>",value_2018,"</span><br>",
            "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",label,"</span>"
          )
          
          
        )
    }
    
    readr::write_csv(
      data2plot,
      glue::glue("../data/tabs/{figure_id}.csv")
    )
    
    return(data2plot)
  }
)


if(!interactive()){
  verbose_message("--- Drawing visualizations...")
}
data_viz <- purrr::imap(
  outline,
  function(figure, figure_id){
    
    df <- data_tabs[[figure_id]]
    type <- figure[["type"]]
    legend_colors <- figure[["legend_color"]]
    
    chart <- NULL
    
    if (type == "Lines") {
      
      data4lines <- df %>% 
        dplyr::mutate(
          label = scales::percent(value2plot/100)
        )
      
      nlines <- length(legend_colors)
      if (nlines > 1){
        names(legend_colors) <- figure[["legend_eqs"]]
        ng <- data4lines$variable
      }
      if (nlines == 1){
        ng <- 1
      }
      
      chart <- WJPr::wjp_lines(
        data4lines,
        target   = "value2plot",
        grouping = "year",
        ngroups  = ng, 
        colors   = "variable",
        cvec     = legend_colors,
        labels   = "label",
        repel    = TRUE 
      ) +
        ggplot2::scale_x_continuous(
          limits = c(2015,2025),
          breaks = seq(2015,2025,2),
          labels = seq(2015,2025,2)
        )
    }
    
    if (type == "Slopes") {
      
      data4slopes <- df %>% 
        dplyr::mutate(
          label = scales::percent(value2plot/100)
        )
      
      nlines <- length(legend_colors)
      if (nlines > 1){
        names(legend_colors) <- figure[["legend_eqs"]]
        ng <- data4slopes$variable
      }
      if (nlines == 1){
        ng <- 1
      }
      
      chart <- WJPr::wjp_slope(
        data4slopes,                    
        target    = "value2plot",             
        grouping  = "year",
        ngroups   = ng,                 
        labels    = "label",
        colors    = "variable",
        cvec      = legend_colors,
        repel     = TRUE
      )
      w <- 8
      h <- 4
    }
    
    if (type == "Bars") {
      
      data4bars <- df %>% 
        dplyr::mutate(
          year  = as.character(year),
          label = scales::percent(value2plot/100),
          label_position = value2plot+5,
          year_str = as.character(year)
        )
      
      if ( length(figure[["var_id"]]) == 1 ){
        gvar <- "year"
        cvar <- "year_str"
        names(legend_colors) <- figure[["legend_eqs"]]
        w <- 8
        h <- 2
        
      } else {
        gvar <- "variable"
        cvar <- "country"
        w <- 8
        h <- 8
      }
      
      chart <- WJPr::wjp_bars(
        data4bars,              
        target    = "value2plot",        
        grouping  = gvar,
        labels    = "label",
        lab_pos   = "label_position",
        colors    = cvar,
        cvec      = legend_colors,
        direction = "horizontal"
      )
    }
    
    if (type == "Dumbbells") {
      
      names(legend_colors) <- figure[["legend_eqs"]]
      data4dumbbells <- df %>% 
        dplyr::mutate(
          label = scales::percent(value2plot/100),
          year_str = as.character(year),
          label_position = dplyr::case_when(
            (
              variable %in% c("Government Permit")
            ) & (year == 2018) ~ value2plot-5,
            (
              variable %in% c(
                "Birth Certificate", "Public School Placement", "Public Health Service"
              )
            ) & (year == 2025) ~ value2plot-5,
            TRUE ~ value2plot+5
          )
        )
      
      chart <- WJPr::wjp_dumbbells(
        data4dumbbells,              
        target    = "value2plot",        
        grouping  = "variable",
        labels    = "label",
        labpos    = "label_position",
        color     = "year_str",
        cvec      = legend_colors,
        cgroups   = c("2018", "2025"),
      ) +
        scale_y_continuous(
          limits = c(-10,30),
          breaks = c(0, 10, 20, 30),
          labels = c("0%", "10%", "20%", "30%")
        )
      
      w <- 8
      h <- 4
    }
    
    if (type == "Radar") {
      names(legend_colors) <- figure[["legend_eqs"]]
      chart <- WJPr::wjp_radar(
        df,             
        axis_var    = "variable",         
        target      = "value2plot",       
        labels      = "axis_label_md",        
        colors      = "year",
        maincat     = "2025",
        cvec        = legend_colors
      )
      
      w <- 8
      h <- 8
    }
    
    ggsave(
      filename = glue::glue("outputs/{figure_id}.svg"),
      plot = chart,
      width = w,
      height = h
    )
    
    return(chart)
  }
)
