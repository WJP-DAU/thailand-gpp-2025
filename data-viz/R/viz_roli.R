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
  
  # Defining colors
  colors4plot        <- rep("#2E2E95", 8)
  
  # Applying plotting function
  chart <- LAC_roseChart(data          = data2plot,
                         target_var    = "avg",
                         grouping_var  = "category",
                         alabels_var   = "label",
                         plabels_var   = "valab",
                         order_var     = "order_value",
                         colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = NULL,
            w      = 88.91933,
            h      = 75.56386)
  
}