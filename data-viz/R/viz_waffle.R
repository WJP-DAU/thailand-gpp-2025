gen_waffle <- function(value){
  
  value_yes = round(value,0)
  value_no = 100-value_yes
  
  data4waffle <- tribble(
    ~category, ~value,
    glue::glue("Yes - {value_yes}%"), value_yes,
    glue::glue("No - {value_no}%"), value_no
  )
    
  
  waffle::waffle(
    data4waffle, 
    rows = 5, 
    colors = c("#575796", "grey90"), 
    legend_pos = "left"
  ) +
    theme(
      legend.text = element_text(size=14)
    )
}
