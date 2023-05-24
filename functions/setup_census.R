
list_files_census <-
  list.files(
    path = 'data/processed',
    full.names = TRUE,
    recursive = TRUE,
    pattern = 'statcan_census'
  )

census_data_analysis <-
  purrr::map_df(
    list_files_census,
    function(x) {
      
      df <- 
        arrow::read_parquet(x)
      
      if(unique(df$file_year) == "2001") {
        
        df <- 
          df %>% 
          mutate(
            agegrp = case_when(
              agep >= 0 & agep <= 4 ~ 1,
              agep >= 5 & agep <= 6 ~ 2,
              agep >= 7 & agep <= 9 ~ 3,
              agep >= 10 & agep <= 11 ~ 4,
              agep >= 12 & agep <= 14 ~ 5,
              agep >= 15 & agep <= 17 ~ 6,
              agep >= 18 & agep <= 19 ~ 7,
              agep >= 20 & agep <= 24 ~ 8,
              agep >= 25 & agep <= 29 ~ 9,
              agep >= 30 & agep <= 34 ~ 10,
              agep >= 35 & agep <= 39 ~ 11,
              agep >= 40 & agep <= 44 ~ 12,
              agep >= 45 & agep <= 49 ~ 13,
              agep >= 50 & agep <= 54 ~ 14,
              agep >= 55 & agep <= 59 ~ 15,
              agep >= 60 & agep <= 64 ~ 16,
              agep >= 65 & agep <= 69 ~ 17,
              agep >= 70 & agep <= 74 ~ 18,
              agep >= 75 & agep <= 79 ~ 19,
              agep >= 80 & agep <= 84 ~ 20,
              agep >= 85 ~ 21,
              TRUE ~ 88
            )
          ) %>% 
          select(
            file_year,
            weight = weightp,
            pr = provp,
            pr5 = prov5p,
            pr1 = prov1p,
            sex = sexp,
            totinc = totincp, #Total Income
            agegrp,
          )
        
      } else {
        
        df <- 
          df %>% 
          select(
            file_year,
            weight,
            pr,
            pr5,
            pr1,
            totinc, #Total Income
            sex,
            agegrp
          )
      }
      
      return(df)
      
    }
  ) %>% 
  mutate(
    across(
      c(pr, pr1, pr5),
      ~ case_when(
        .x == 10 ~ "Newfoundland and Labrador",
        .x == 11 ~ "Prince Edward Island",
        .x == 12 ~ "Nova Scotia",
        .x == 13 ~ "New Brunswick",
        .x == 24 ~ "Quebec",
        .x == 35 ~ "Ontario",
        .x == 46 ~ "Manitoba",
        .x == 47 ~ "Saskatchewan",
        .x == 48 ~ "Alberta",
        .x == 59 ~ "British Columbia",
        .x %in% c(60, 70) ~ "Northern Canada",
        TRUE ~ "Not applicable"
      )
    ),
    agegrp = case_when(
      agegrp == 1 ~ "0 to 4 years",
      agegrp == 2 ~ "5 to 9 years",
      agegrp == 3 ~ "5 to 9 years",
      agegrp == 4 ~ "10 to 14 years",
      agegrp == 5 ~ "10 to 14 years",
      agegrp == 6 ~ "15 to 19 years",
      agegrp == 7 ~ "15 to 19 years",
      agegrp == 8 ~ "20 to 24 years",
      agegrp == 9 ~ "25 to 29 years",
      agegrp == 10 ~ "30 to 34 years",
      agegrp == 11 ~ "35 to 39 years",
      agegrp == 12 ~ "40 to 44 years",
      agegrp == 13 ~ "45 to 49 years",
      agegrp == 14 ~ "50 to 54 years",
      agegrp == 15 ~ "55 to 59 years",
      agegrp == 16 ~ "60 to 64 years",
      agegrp == 17 ~ "65 to 69 years",
      agegrp == 18 ~ "70 to 74 years",
      agegrp == 19 ~ "75 to 79 years",
      agegrp == 20 ~ "80 to 84 years",
      agegrp == 21 ~ "85 years or older",
      TRUE ~ "Not applicable"
    ),
    sex = ifelse(sex == 1, "Female", "Male")
  ) %>% 
  filter(
    if_all(
      starts_with("pr"),
      ~ . != "Not applicable"
    ),
    !agegrp %in% c("Not applicable", "0 to 4 years", "5 to 9 years", "10 to 14 years")
  )
