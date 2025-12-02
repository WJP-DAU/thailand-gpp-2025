## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Data Loading and Data Bank
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     October 20, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (interactive()){
  source("R/config.R")
  source("R/outline.R")
}

path2merge <- paste0(
  paths[[Sys.info()["user"]]][["path2GPP"]],
  "/GPP 2025/Merged Files/Historical Files/Merged.dta"
)

target_variables <- c(
  # Fundamental Freedoms
  "q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2", "q46d_G2", 
  "q46f_G1", "q46a_G2", "q46d_G1", "q46e_G1", "q46h_G2", "q46i_G1", 
  "q46a_G1", "q46b_G1", "q46b_G2", "q46j_G1",
  
  # Discrimination
  "discrimination1",
  "q16a", "q16b", "q16c", "q16d", "q16e",
  "q17_1", "q17_2", "q17_3", "q17_4", "q17_7", "q17_11", "q17_12", 
  "q17_13", "q17_14", 
  
  # Perceptions of the Judiciary
  "q48g_G2", "q48f_G2", "q48e_G1", "q48g_G1", "q48h_G1", "q48f_G1",
  
  # Perceptions of Corruption
  "q2a", "q2d", "q2c", "q2b", "q2e", "q2g", "q2f",
  "q2a_inverted", "q2d_inverted", "q2c_inverted", 
  "q2b_inverted", "q2e_inverted", "q2g_inverted", 
  "q2f_inverted",
  
  # Perceptions of Accountability
  "q43_G2",
  
  # Bribery Victimization
  "q4a", "q4b", "q4c", "q4d", "q4e", 
  
  # Trust
  "q1a", "q1d", "q1b", "q1c", "q1e", "q1g", "q1f", 
  
  # Crime & Security
  "q9", "crime1", "crime2", "crime3", "crime4", "q8d",
  
  # Perceptions of the Criminal Justice System 
  "q49a", "q49b_G2", "q49e_G2", "q49c_G2", "q49e_G1", "EXP_q23d_G1", 
  "q49c_G1", "q49b_G1", "q49d_G1", "q44_G2",
  
  # Attitudes toward Authoritarianism
  "att_aut_p1_p", "att_aut_p1_n", "att_aut_p1_na",
  "att_aut_p2_p", "att_aut_p2_n", "att_aut_p2_na",
  "att_aut_p3_p", "att_aut_p3_n", "att_aut_p3_na",
  "att_aut_p4_p", "att_aut_p4_n", "att_aut_p4_na",
  
  # Police dashboard
  
  "q48c_G2", "q48e_G2", "q48d_G1", # Integrity
  "q48b_G2", "q48a_G2", "q48b_G1", # Crime control
  "q18a", "q18b", "q18c", "q18d", "q18e", # Discrimination
  "q48a_G1", "q48c_G1", "q48d_G2" # Due Process
  
)

if(!interactive()){
  verbose_message("--- Loading master GPP data...")
}
master_data <- haven::read_dta(path2merge)

if(!interactive()){
  verbose_message("--- Preparing Thailand data subset...")
}
thai_subset <- master_data %>% 
  dplyr::filter(
    country == "Thailand" & year > 2013
  ) %>%
  dplyr::mutate(
    gender = dplyr::case_when(
      gend==1 ~ "Male",
      gend==2 ~ "Female"
    ),
    location = dplyr::case_when(
      Urban==1 ~ "Urban",
      Urban==2 ~ "Rural"
    ),
    discrimination1 = dplyr::case_when(
      q16a <= 3 ~ 1,
      q16b <= 3 ~ 1,
      q16c <= 3 ~ 1,
      q16d <= 3 ~ 1,
      q16e <= 3 ~ 1,
      q16a <= 6 ~ 0,
      q16b <= 6 ~ 0,
      q16c <= 6 ~ 0,
      q16d <= 6 ~ 0,
      q16e <= 6 ~ 0,
      q16a == 99 & q16b == 99 & q16c == 99 &
        q16d == 99 & q16e == 99 ~ 99
    ),
    crime1 = dplyr::case_when(
      EXP_q8a_1 == 1 ~ 1, 
      EXP_q8a_2 == 1 ~ 1, 
      EXP_q8a_3 == 1 ~ 1, 
      EXP_q8a_4 == 1 ~ 1, 
      EXP_q8a_5 == 1 ~ 1, 
      EXP_q8a_6 == 1 ~ 1, 
      EXP_q8a_7 == 1 ~ 1, 
      EXP_q8a_8 == 1 ~ 1, 
      EXP_q8a_1 == 99 & EXP_q8a_2 == 99 & EXP_q8a_3 == 99 & EXP_q8a_4 == 99 & 
        EXP_q8a_5 == 99 & EXP_q8a_6 == 99 & EXP_q8a_7 == 99 & EXP_q8a_8 == 99 ~ 99,
      TRUE ~ 0
    ),
    crime2 = dplyr::case_when(
      EXP_q8a_9  == 1 ~ 1, 
      EXP_q8a_10 == 1 ~ 1, 
      EXP_q8a_11 == 1 ~ 1, 
      EXP_q8a_9 == 99 & EXP_q8a_10 == 99 & EXP_q8a_11 == 99 ~ 99,
      TRUE ~ 0
    ),
    crime3 = dplyr::case_when(
      EXP_q8a_12 == 1 ~ 1, 
      EXP_q8b_1  == 1 ~ 1, 
      EXP_q8b_2  == 1 ~ 1, 
      EXP_q8b_3  == 1 ~ 1, 
      CAR_q47b_5 == 1 ~ 1, 
      EXP_q8a_12 == 99 & EXP_q8b_1 == 99 & EXP_q8b_2 == 99 & EXP_q8b_3 == 99 & 
        CAR_q47b_5 == 99 ~ 99,
      TRUE ~ 0
    ),
    crime4 = dplyr::case_when(
      crime1 == 1 ~ 1,
      crime2 == 1 ~ 1,
      crime3 == 1 ~ 1,
      crime1==99 & crime2==99 & crime3==99 ~ 99,
      TRUE ~ 0
    ),
    att_aut_p1_p = dplyr::case_when(
      q50 == 1 ~ 0,
      q50 == 2 ~ 0,
      q50 == 3 ~ 1,
      q50 == 4 ~ 1,
      q50 == 5 ~ 0,
      q50 == 99 ~ 0
    ),
    att_aut_p1_n = dplyr::case_when(
      q50 == 1 ~ 1,
      q50 == 2 ~ 1,
      q50 == 3 ~ 0,
      q50 == 4 ~ 0,
      q50 == 5 ~ 0,
      q50 == 99 ~ 0
    ),
    att_aut_p1_na = dplyr::case_when(
      q50 == 1  ~ 0,
      q50 == 2  ~ 0,
      q50 == 3  ~ 0,
      q50 == 4  ~ 0,
      q50 == 5  ~ 1,
      q50 == 99 ~ 1
    ),
    att_aut_p2_p = dplyr::case_when(
      q51 == 1 ~ 0,
      q51 == 2 ~ 0,
      q51 == 3 ~ 1,
      q51 == 4 ~ 1,
      q51 == 5 ~ 0,
      q51 == 99 ~ 0
    ),
    att_aut_p2_n = dplyr::case_when(
      q51 == 1 ~ 1,
      q51 == 2 ~ 1,
      q51 == 3 ~ 0,
      q51 == 4 ~ 0,
      q51 == 5 ~ 0,
      q51 == 99 ~ 0
    ),
    att_aut_p2_na = dplyr::case_when(
      q51 == 1  ~ 0,
      q51 == 2  ~ 0,
      q51 == 3  ~ 0,
      q51 == 4  ~ 0,
      q51 == 5  ~ 1,
      q51 == 99 ~ 1
    ),
    att_aut_p3_p = dplyr::case_when(
      q52 == 1 ~ 0,
      q52 == 2 ~ 0,
      q52 == 3 ~ 1,
      q52 == 4 ~ 1,
      q52 == 5 ~ 0,
      q52 == 99 ~ 0
    ),
    att_aut_p3_n = dplyr::case_when(
      q52 == 1 ~ 1,
      q52 == 2 ~ 1,
      q52 == 3 ~ 0,
      q52 == 4 ~ 0,
      q52 == 5 ~ 0,
      q52 == 99 ~ 0
    ),
    att_aut_p3_na = dplyr::case_when(
      q52 == 1  ~ 0,
      q52 == 2  ~ 0,
      q52 == 3  ~ 0,
      q52 == 4  ~ 0,
      q52 == 5  ~ 1,
      q52 == 99 ~ 1
    ),
    att_aut_p4_p = dplyr::case_when(
      q44_G2 == 1 ~ 0,
      q44_G2 == 2 ~ 0,
      q44_G2 == 3 ~ 1,
      q44_G2 == 99 ~ 0
    ),
    att_aut_p4_n = dplyr::case_when(
      q44_G2 == 1 ~ 1,
      q44_G2 == 2 ~ 1,
      q44_G2 == 3 ~ 0,
      q44_G2 == 99 ~ 0
    ),
    att_aut_p4_na = dplyr::case_when(
      q44_G2 == 1  ~ 0,
      q44_G2 == 2  ~ 0,
      q44_G2 == 3  ~ 0,
      q44_G2 == 99 ~ 1
    ),
    q2a_inverted = dplyr::case_when(
      q2a == 1 ~ 4,
      q2a == 2 ~ 3,
      q2a == 3 ~ 2,
      q2a == 4 ~ 1,
      q2a == 5 ~ 5,
      TRUE ~ q2a
    ),
    q2b_inverted = dplyr::case_when(
      q2b == 1 ~ 4,
      q2b == 2 ~ 3,
      q2b == 3 ~ 2,
      q2b == 4 ~ 1,
      q2b == 5 ~ 5,
      TRUE ~ q2b
    ),
    q2c_inverted = dplyr::case_when(
      q2c == 1 ~ 4,
      q2c == 2 ~ 3,
      q2c == 3 ~ 2,
      q2c == 4 ~ 1,
      q2c == 5 ~ 5,
      TRUE ~ q2c
    ),
    q2d_inverted = dplyr::case_when(
      q2d == 1 ~ 4,
      q2d == 2 ~ 3,
      q2d == 3 ~ 2,
      q2d == 4 ~ 1,
      q2d == 5 ~ 5,
      TRUE ~ q2d
    ),
    q2e_inverted = dplyr::case_when(
      q2e == 1 ~ 4,
      q2e == 2 ~ 3,
      q2e == 3 ~ 2,
      q2e == 4 ~ 1,
      q2e == 5 ~ 5,
      TRUE ~ q2e
    ),
    q2f_inverted = dplyr::case_when(
      q2f == 1 ~ 4,
      q2f == 2 ~ 3,
      q2f == 3 ~ 2,
      q2f == 4 ~ 1,
      q2f == 5 ~ 5,
      TRUE ~ q2f
    ),
    q2g_inverted = dplyr::case_when(
      q2g == 1 ~ 4,
      q2g == 2 ~ 3,
      q2g == 3 ~ 2,
      q2g == 4 ~ 1,
      q2g == 5 ~ 5,
      TRUE ~ q2g
    )
  ) %>% 
  dplyr::select(
    country, year, gender, location, age, fin, edu, 
    tidyselect::all_of(target_variables)
  ) 

if(!interactive()){
  verbose_message("--- Saving Thailand data subset...")
}
readr::write_csv(
  thai_subset,
  "../data/thailand_gpp_data.csv"
)

if(!interactive()){
  verbose_message("--- Preparing Thailand data bank...")
}
thai_data_bank <- purrr::imap_dfr(
  outline,
  function(figure, figure_id){
    
    if ("None" %in% figure[["var_id"]]){
      return(NULL)
    }
    
    print(glue::glue("Figure: {figure_id}"))
    vars <- figure[["var_id"]]
    variable_data <- purrr::map_dfr(
      vars,
      function(x){
        
        print(glue::glue("Variable: {x}"))
        
        df_national <- thai_subset %>% 
          dplyr::select(
            country, year, 
            target = tidyselect::all_of(x)
          ) %>% 
          dplyr::filter(target != 99) %>% 
          dplyr::mutate(
            sample = "National"
          ) %>% 
          dplyr::group_by(country, year, sample, target) %>% 
          dplyr::summarise(
            count = dplyr::n(),
            .groups = "keep"
          ) %>% 
          dplyr::filter(
            !is.na(target)
          ) %>% 
          dplyr::group_by(country, year) %>% 
          dplyr::mutate(
            total = sum(count),
            ratio = count/total
          )
        
        df_subsample_gender <- thai_subset %>% 
          dplyr::select(
            country, year, sample = gender, 
            target = tidyselect::all_of(x)
          ) %>% 
          dplyr::filter(target != 99) %>% 
          dplyr::group_by(country, year, sample, target) %>% 
          dplyr::summarise(
            count = dplyr::n(),
            .groups = "keep"
          ) %>% 
          dplyr::filter(
            !is.na(target)
          ) %>% 
          dplyr::group_by(country, year, sample) %>% 
          dplyr::mutate(
            total = sum(count),
            ratio = count/total
          )
        
        df_subsample_location <- thai_subset %>% 
          dplyr::select(
            country, year, sample = location, 
            target = tidyselect::all_of(x)
          ) %>% 
          dplyr::filter(target != 99) %>% 
          dplyr::group_by(country, year, sample, target) %>% 
          dplyr::summarise(
            count = dplyr::n(),
            .groups = "keep"
          ) %>% 
          dplyr::filter(
            !is.na(target)
          ) %>% 
          dplyr::group_by(country, year, sample) %>% 
          dplyr::mutate(
            total = sum(count),
            ratio = count/total
          )
        
        df <- dplyr::bind_rows(
          df_national,
          df_subsample_gender,
          df_subsample_location,
        ) %>% 
          dplyr::mutate(
            variable = x,
            ratio = dplyr::if_else(
              total <= 30, NA_real_, ratio # Minimum of 30 answers to showcase a value
            )
          )
        
      }
    )
  }
) %>% 
  dplyr::distinct()

readr::write_csv(
  thai_data_bank,
  "../data/thailand_data_bank.csv"
)

