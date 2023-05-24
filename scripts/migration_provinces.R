library(sf)
library(dplyr)
library(ggplot2)

cad_map <-
  readr::read_rds('data/cad_provinces_map.rds')

provinces_centroids <-
  cad_map %>% 
  sf::st_centroid()

census_2016 <-
  arrow::read_parquet('data/processed/statcan_census_ind_2016.parquet')

xx <- 
  census_2016 %>% 
  select(
    agegrp,
    sex,
    pr,
    pr1,
    pr5,
    weight
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
    )
  ) %>% 
  filter(
    if_all(
      starts_with("pr"),
      ~ . != "Not applicable"
    )
  ) %>% 
  group_by(pr, pr5) %>% 
  summarise(
    pop = sum(weight)
  )

yy <- 
  xx %>% 
  filter(
    pr != pr5
  ) %>% 
  left_join(
    xx %>% 
      filter(
        pr != pr5
      ) %>% 
      mutate(
        pop = pop
      ),
    by = c("pr" = "pr5",
           "pr5" = "pr"),
    suffix = c('', '_opposite')
  ) %>% 
  mutate(
    net = pop_opposite - pop
  ) %>% 
  filter(
    net > 0
  )

zz <-
  yy %>% 
  left_join(
    provinces_centroids %>% 
      mutate(
        lon = sf::st_coordinates(.)[,1],
        lat = sf::st_coordinates(.)[,2]
      ) %>% 
      sf::st_drop_geometry() %>% 
      as_tibble(),
    by = c("pr5" = "PRENAME")
  ) %>% 
  left_join(
    provinces_centroids %>% 
      mutate(
        lon = sf::st_coordinates(.)[,1],
        lat = sf::st_coordinates(.)[,2]
      ) %>% 
      sf::st_drop_geometry() %>% 
      as_tibble(),
    by = c("pr" = "PRENAME"),
    suffix = c("_origin", "_destination")
  ) %>% 
  select(
    origin = pr5,
    destination = pr,
    pop = net,
    ends_with("origin"),
    ends_with("destination")
  )
