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
source("R/viz_dumbbells.R")
source("R/viz_waffle.R")
source("R/viz_logit.R")
source("R/viz_rose.R")
source("R/viz_dots_roli.R")
source("R/viz_roli.R")

suppressMessages(
  suppressWarnings(
    library(tidyverse, quietly = TRUE)
  )
)
library(ggrepel)
library(ggtext)
library(readxl)
library(showtext)
thai_data_bank <- readr::read_csv(
  "../data/thailand_data_bank.csv",
  show_col_types = FALSE
)
thai_gpp <- readr::read_csv(
  "../data/thailand_gpp_data.csv", 
  show_col_types = FALSE
)

southeast_asia <- c(
  "Thailand", "Cambodia", "Indonesia", "Malaysia", "Myanmar",
  "Philippines", "Singapore", "Vietnam"
)
roli_data <- readxl::read_excel("../data/ROLI_data.xlsx") %>%  
  filter(country %in% southeast_asia)

WJPr::wjp_fonts()

#Loading fonts
path2fonts <- file.path(
  paths[[Sys.info()[['user']]]][['path2DA']],
  "6. Country Reports",
  "0. Fonts/"
)

font_add(family     = "inter",
         regular      = paste0(path2fonts, "InterTight-Medium.ttf"),
         italic     = paste0(path2fonts, "InterTight-MediumItalic.ttf"),
         bold       = paste0(path2fonts, "InterTight-SemiBold.ttf"),
         bolditalic = paste0(path2fonts, "InterTight-SemiBoldItalic.ttf"))
font_add(family  = "inter Light",
         regular = paste0(path2fonts, "InterTight-Italic.ttf"))
font_add(family  = "inter Black",
         regular = paste0(path2fonts, "InterTight-ExtraBold.ttf"))
font_add(family  = "inter Black Italic",
         regular = paste0(path2fonts, "InterTight-ExtraBoldItalic.ttf"))
showtext_auto()

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
      dplyr::group_by(country, year, variable, sample) %>% 
      dplyr::summarise(
        value2plot = sum(ratio)*100,
        .groups = "keep"
      )
    
    # Specific changes to data tabs
    if(figure_id %in% c("Figure_1", "Figure_1_X", "Figure_1_Y")){
      data2plot <- data2plot %>% 
        dplyr::mutate(
          label = dplyr::case_when(
            variable == "q46c_G2" ~ "People can express opinions against the government",
            variable == "q46f_G2" ~ "Civil society organizations can express opinions against the government",
            variable == "q46g_G2" ~ "Political parties can express opinions against the government",
            variable == "q46c_G1" ~ "The media can express opinions against the government without fear of retaliation",
            variable == "q46e_G2" ~ "The media can expose cases of corruption",
            variable == "q46d_G2" ~ "People can attend community meetings",
            variable == "q46f_G1" ~ "People can join any political organization",
            variable == "q46a_G2" ~ "People can organize around an issue or petition",
            variable == "q46a_G2" ~ "Local government officials are elected through a clean process",
            variable == "q46e_G1" ~ "People can vote freely without feeling harassed or pressured",
            variable == "q46h_G2" ~ "Religious minorities can observe their holy days"
          ),
          panel = dplyr::case_when(
            variable %in% c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2") ~ "Expression",
            variable %in% c("q46d_G2", "q46f_G1", "q46a_G2") ~ "Participation",
            variable %in% c("q46a_G2", "q46e_G1") ~ "Elections",
            variable %in% c("q46h_G2") ~ "Religion"
          )
        )
    }
    
    if(figure_id %in% c("Figure_2_2")){
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
        ) %>% 
        arrange(desc(value2plot)) %>%
        ungroup() %>% 
        mutate(
          npos = row_number()
        )
    }
    
    if(figure_id %in% c("Figure_3")){
      data2plot <- data2plot %>% 
        dplyr::group_by(variable) %>% 
        dplyr::mutate(
          label = dplyr::case_when(
            variable == "q48g_G2"  ~ "Judges decide cases<br>in an independent<br>manner",
            variable == "q48f_G2"  ~ "Prosecutors prosecute<br>crimes in an<br>independent manner",
            variable == "q48e_G1"  ~ "Courts guarantee<br>everyone a<br>fair trial",
            variable == "q48g_G1"  ~ "Courts are biased<br>towards money<br>or influence",
            variable == "q48f_G1"  ~ "Courts are more<br>concerned with<br>process than with<br>justice"
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
            "<span style='color:#575796;font-size:4.217518mm'>",value_2025,"</span>",
            "<span style='color:#524F4C;font-size:4.217518mm'> | </span>",
            "<span style='color:#b200aa;font-size:4.217518mm'>",value_2018,"</span><br>",
            "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",label,"</span>"
          )
        )
    }
    
    if(figure_id %in% c("Figure_5_2")){
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
    
    if(figure_id %in% c("Figure_7_1")){
      data2plot <- data2plot %>% 
        dplyr::mutate(
          variable = dplyr::case_when(
            variable == "crime1"  ~ "Property crimes",
            variable == "crime2"  ~ "Corruption, financial,\nand commercial crimes",
            variable == "crime3"  ~ "Crimes against life and\nintegrity of individuals",
            variable == "crime4"  ~ "Thailand National Rate",
          )
        )
    }
    
    if(figure_id %in% c("Figure_9")){
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
            "<span style='color:#575796;font-size:4.217518mm'>",value_2025,"</span>",
            "<span style='color:#524F4C;font-size:4.217518mm'> | </span>",
            "<span style='color:#b200aa;font-size:4.217518mm'>",value_2018,"</span><br>",
            "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",label,"</span>"
          )
        )
    }
    
    if(figure_id %in% c("Figure_9_Y")){
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
          value_female = value2plot,
          value_female = if_else(sample == "Female", value_female, NA_real_),
          value_female = first(value_female, na_rm = T),
          value_female = paste0(format(round(value_female, 0), nsmall=0), "%"),
          
          value_male = value2plot,
          value_male = if_else(sample == "Male", value_male, NA_real_),
          value_male = first(value_male, na_rm = T),
          value_male = paste0(format(round(value_male, 0), nsmall=0), "%"),
          
          axis_label_md = paste0(
            "<span style='color:#35605A;font-size:4.217518mm'>",value_female,"</span>",
            "<span style='color:#524F4C;font-size:4.217518mm'> | </span>",
            "<span style='color:#00120B;font-size:4.217518mm'>",value_male,"</span><br>",
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
    
    if (type == "Large Dumbbells") {
      if (figure_id %in% c("Figure_1")){
        chart <- gen_large_dumbbells(data = df)
      }
      if (figure_id %in% c("Figure_1_X")){
        chart <- gen_large_dumbbells(
          data = df, 
          p0 = "Urban", 
          p1 = "Rural",
          c0 = "#35605A",
          c1 = "#00120B",
          pivot_var = "sample" 
        )
      }
      if (figure_id %in% c("Figure_1_Y")){
        chart <- gen_large_dumbbells(
          data = df, 
          p0 = "Female", 
          p1 = "Male",
          c0 = "#35605A",
          c1 = "#00120B",
          pivot_var = "sample" 
        )
      }
      
      w <- 14
      h <- 6
    }
    
    if (type == "Single Dumbbells") {
      
      chart <- gen_single_dumbbells(data = df)
      w <- 8
      h <- 2
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
      w <- 4
      h <- 2
    }
    
    if (type == "Bars") {
      
      data4bars <- df %>% 
        dplyr::mutate(
          year  = as.character(year),
          label = scales::percent(value2plot/100, accuracy=1),
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
      
      if (figure_id %in% c("Figure_2_2")){
        data4bars <- data4bars %>% 
          mutate(
            label_position = (value2plot*0.05)+3
          )
        chart <- WJPr::wjp_bars(
          data4bars,              
          target    = "value2plot",        
          grouping  = gvar,
          labels    = "label",
          lab_pos   = "label_position",
          colors    = cvar,
          cvec      = legend_colors,
          direction = "horizontal",
          order     = "npos"
        ) 
      } else {
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
      
      chart <- chart +
        theme(
          panel.grid.major.x = element_blank(),
          axis.text.x  = element_text(
            family = "inter",
            face   = "plain",
            color  = "#1a1a1a",
            size   = 13
          ),
          axis.line.x.top = element_line(
            linetype = "solid",
            linewidth = 0.5,
            color = "#1a1a1a"
          )
        )
      
    }
    
    if (type == "Radar") {
      
      if (figure_id=="Figure_9_Y"){
        cc = "sample"
        dd = "Female"
      } else {
        cc = "year"
        dd = "2025"
      }
      
      names(legend_colors) <- figure[["legend_eqs"]]
      chart <- WJPr::wjp_radar(
        df,             
        axis_var    = "variable",         
        target      = "value2plot",       
        labels      = "axis_label_md",        
        colors      = cc,
        maincat     = dd,
        cvec        = legend_colors
      )
      
      w <- 8
      h <- 8
    }
    
    if (type == "Lollipops") {
      
      chart <- WJPr::wjp_lollipops(
        data = df,
        target = "value2plot",
        grouping = "variable",
        line_color    = "#c4c4c4",
        line_size     = 3,
        point_size    = 7,
        point_color   = "#575796"
      ) +
        theme(
          panel.grid.major.x = element_blank(),
          axis.text.x  = element_text(
            family = "inter",
            face   = "plain",
            color  = "#1a1a1a",
            size   = 13
          ),
          axis.line.x.top = element_line(
            linetype = "solid",
            linewidth = 0.5,
            color = "#1a1a1a"
          )
        )
      w <- 8
      h <- 2
    }
    
    if (type == "Waffle") {
      chart <- gen_waffle(df$value2plot)
      w <- 8
      h <- 3
    }
    
    if (type == "Logit") {
      chart <- gen_logit()
      w <- 7
      h <- 3
    }
    
    ggsave(
      filename = 
        file.path(
          paths[[Sys.info()[['user']]]][['path2DA']],
          "6. Country Reports",
          "thailand-gpp-2025",
          "data-viz",
          "outputs",
          paste0(figure_id, ".svg")
          ),
      plot = chart,
      width = w,
      height = h
    )
    
    
    return(chart)
  }
)

# ROLI

gen_roli_rose(roli_data)
gen_roli_dots.fn(roli_data)

