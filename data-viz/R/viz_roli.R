library(WJPr)

gen_roli_rose <- function(roli_data){
  
  # Subsetting data
  data2plot <- roli_data %>%
    filter(year == 2025 & country == "Thailand") %>%
    select(f1, f2, f3, f4, f5, f6, f7, f8) %>%
    pivot_longer(
      everything(),
      names_to  = "category",
      values_to = "avg"
    ) %>%
    mutate(
      valab = format(round(avg, 2),
                     nsmall = 2),
      label = case_when(
        category == "f1" ~ "Constraints on\nGovernment Powers",
        category == "f2" ~ "Absence of\nCorruption",
        category == "f3" ~ "Open\nGovernment",
        category == "f4" ~ "Fundamental\nRights",
        category == "f5" ~ "Order and\nSecurity",
        category == "f6" ~ "Regulatory\nEnforcement",
        category == "f7" ~ "Civil\nJustice",
        category == "f8" ~ "Criminal\nJustice"
      ),
      order_value = case_when(
        category == "f1" ~ 1,
        category == "f2" ~ 2,
        category == "f3" ~ 3,
        category == "f4" ~ 4,
        category == "f5" ~ 5,
        category == "f6" ~ 6,
        category == "f7" ~ 7,
        category == "f8" ~ 8
      ),
      across(label,
             function(raw_label){
               html <- paste0("<span style='color:#2E2E95;font-size:2.911678mm;font-weight:bold'>",  
                              valab, "</span>",
                              "<br>",
                              "<span style='color:#524F4C;font-size:2.911678mm'>",
                              str_replace_all(raw_label, "\\n", "<br>"),
                              "</span>")
               return(html)
             }),
      avg = avg-0.1 # I substract 0.1 just for aesthetic purposes
    )
  
  colors4plot <- rep("#2E2E95", 8)
  
  # Applying plotting function
  chart <- rose_chart.viz(
    data          = data2plot,
    target_var    = "avg",
    grouping_var  = "category",
    alabels_var   = "label",
    plabels_var   = "valab",
    order_var     = "order_value",
    colors        = colors4plot) 
  
  ggsave(
    filename = 
      file.path(
        paths[[Sys.info()[['user']]]][['path2DA']],
        "6. Country Reports",
        "thailand-gpp-2025",
        "data-viz",
        "outputs",
        paste0("roli_rose", ".svg")
      ),
    plot   = chart,
    width  = 4,
    height = 4
  )

}

gen_roli_dots.fn <- function(roli_data){
  
  # Sub-setting data
  data2plot <- roli_data %>%
    filter(year == 2025) %>%
    select(country, starts_with("sf")) %>%
    mutate(
      country = if_else(
        country == "Thailand",
        "Southeast Asia",
        "Thailand"
      )
    ) %>%
    group_by(country) %>%
    summarise(across(everything(),
                     \(x) mean(x, na.rm = T))) %>%
    pivot_longer(!country,
                 values_to = "value2plot",
                 names_to  = "category") %>%
    mutate(
      factor    = str_extract(category, "(?<=sf)\\d{1}"),
      factor    = as.integer(factor),
      order_var = str_extract(category, "(?<=sf\\d{1})\\d{1}"),
      order_var = as.integer(order_var),
      category = case_when(
        category == "sf11" ~ "1.1 Limits by the legislature ",
        category == "sf12" ~ "1.2 Limits by the judiciary   ",
        category == "sf13" ~ "1.3 Independent auditing      ",
        category == "sf14" ~ "1.4 Sanctions against the \ngoverment",
        category == "sf15" ~ "1.5 Non-governmental checks   ",
        category == "sf16" ~ "1.6 Lawful transition of power",
        
        category == "sf21" ~ "2.1 In the executive branch   ",
        category == "sf22" ~ "2.2 In the judiciary          ",
        category == "sf23" ~ "2.3 In the police/military    ",
        category == "sf24" ~ "2.4 In the legislature        ",
        
        category == "sf31" ~ "3.1 Publicized data           ",
        category == "sf32" ~ "3.2 Right to information      ",
        category == "sf33" ~ "3.3 Civi Participation        ",
        category == "sf34" ~ "3.4 Complaint mechanisms      ",
        
        category == "sf41" ~ "4.1 No discrimination         ",
        category == "sf42" ~ "4.2 Right to life and security",
        category == "sf43" ~ "4.3 Due process of law        ",
        category == "sf44" ~ "4.4 Freedom of expression     ",
        category == "sf45" ~ "4.5 Freedom of religion       ",
        category == "sf46" ~ "4.6 Right to privacy          ",
        category == "sf47" ~ "4.7 Freedom of association    ",
        category == "sf48" ~ "4.8 Labor rights",
        
        category == "sf51" ~ "5.1 Absence of crime          ",
        category == "sf52" ~ "5.2 Limit of civil conflict   ",
        category == "sf53" ~ "5.3 Absence of violent redress",
        
        category == "sf61" ~ "6.1 Effective enforcement     ",
        category == "sf62" ~ "6.2 No improper influence     ",
        category == "sf63" ~ "6.3 No unreasonable delay     ",
        category == "sf64" ~ "6.4 Respect for due process   ",
        category == "sf65" ~ "6.5 No expropriation          ",
        
        category == "sf71" ~ "7.1 Accessibility             ",
        category == "sf72" ~ "7.2 No discrimination         ",
        category == "sf73" ~ "7.3 No corruption             ",
        category == "sf74" ~ "7.4 No improper gov't influence",
        category == "sf75" ~ "7.5 No unreasonable delay     ",
        category == "sf76" ~ "7.6 Effective enforcement     ",
        category == "sf77" ~ "7.7 Effective ADRs",
        
        category == "sf81" ~ "8.1 Effective investigations  ",
        category == "sf82" ~ "8.2 Effective adjudication    ",
        category == "sf83" ~ "8.3 Effective correct. system ",
        category == "sf84" ~ "8.4 No discrimination         ",
        category == "sf85" ~ "8.5 No corruption             ",
        category == "sf86" ~ "8.6 No improper gov't influence",
        category == "sf87" ~ "8.7 Due process of law        "
      ),
    )
  
  colors4plot = c("Southeast Asia"   = "#fa4d57",
                  "Thailand" = "#003b8a")
  
  shapes4plot = c("Southeast Asia"   = 18,
                  "Thailand" = 16)
  
  # Plotting individual panels
  imap(c("A" = 1, "B" = 2, "C" = 3, "D" = 4,
         "E" = 5, "F" = 6, "G" = 7, "H" = 8),
       function(cfactor, panelName) {
         
         data2plot <- data2plot %>%
           filter(factor %in% cfactor)
         
         chart <- ROLI_dots(   data         = data2plot,
                               target_var   = "value2plot",
                               sd_var = NULL,
                               n_obs  = NULL,
                               grouping_var = "country",
                               labels_var   = "category",
                               colors       = colors4plot,
                               order_var    = "order_var",
                               diffShp      = T,
                               shapes       = shapes4plot,
                               draw_ci      = F,
                               y_upper      = 1,
                               dsize        = 2.5)
         
         # Defining height
         if (max(data2plot$order_var) == 3 ) {
           h = 1.25
         }
         if (max(data2plot$order_var) == 4 ) {
           h = 1.5
         }
         if (max(data2plot$order_var) == 5 ) {
           h = 1.75
         }
         if (max(data2plot$order_var) == 6 ) {
           h = 2
         }
         if (max(data2plot$order_var) == 7 ) {
           h = 2.25
         }
         if (max(data2plot$order_var) == 8 ) {
           h = 2.5
         }
         
         # Saving panels
         ggsave(
           filename = 
             file.path(
               paths[[Sys.info()[['user']]]][['path2DA']],
               "6. Country Reports",
               "thailand-gpp-2025",
               "data-viz",
               "outputs",
               paste0("roli_dots_",panelName,".svg")
               ),
           plot = chart,
           width = 5,
           height = h
         )
         
       })
}
