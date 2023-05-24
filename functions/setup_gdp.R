js_tooltip_rank <-
  paste0(
    "function(){return ('Province: <strong>' + this.point.geo + ' - ' + this.x +",
    "'</strong> <br> Rank: ' + this.y + ",
    "' <br> GDP <em>per capita</em>: $' + this.point.gdp_per_capita)}"
  )

cad_gdp_by_province <-
  arrow::read_parquet('data/processed/cad_gdp_provinces.parquet') %>% 
  select(
    ref_date,
    geo,
    estimates,
    prices,
    value
  ) %>% 
  filter(
    !geo %in% c("Outside Canada", "Northwest Territories including Nunavut", "Canada"),
    prices == "2012 constant prices",
    estimates == "Final consumption expenditure"
  ) %>% 
  mutate(
    geo = ifelse(
      geo %in% c("Yukon", "Nunavut", "Northwest Territories"), "Northern Canada", geo
    )
  ) %>% 
  group_by(ref_date, geo) %>% 
  summarise(
    gdp = sum(value, na.rm = T)
  ) %>% 
  ungroup()

cad_population_provinces <-
  arrow::read_parquet('data/processed/cad_population_provinces.parquet') %>% 
  filter(
    !geo %in% c("Northwest Territories including Nunavut", "Canada"),
    sex == "Both sexes",
    age_group == "All ages"
  ) %>% 
  mutate(
    geo = ifelse(
      geo %in% c("Yukon", "Nunavut", "Northwest Territories"), "Northern Canada", geo
    )
  ) %>% 
  group_by(ref_date, geo) %>% 
  summarise(
    pop = sum(value, na.rm = TRUE)
  ) %>% 
  ungroup()
