### My Sample Code for the Estimation of the effects
### of different point of views on European Integration


# Merging & Keeping Columns -----------------------------------------------

library(tidyverse)
library(haven)
library(labelled)
library(tidymodels)

setwd("~/Documents/İTÜ/Semesters/7th semester/EU/eu_paper/ess/all_in_one")

files_paths <- list.files()

files_paths <- files_paths[endsWith(files_paths, "dta")]

get_data <- function(path){ 
  
  raw <- read_dta(path, encoding = "latin1")
  
  label_tbl <- 
    var_label(raw) %>% 
    as_tibble() %>% 
    pivot_longer(
      cols = everything(),
      names_to = "colname",
      values_to = "explanations"
    )
  
  #View(label_tbl)
  
  cols_eu <- 
    c("trstep", "euftf", "atcherp", "vteurmmb")
  
  cols_trst <- 
    c("trstprl", "trstlgl", "trstplt", "trstprt", "trstep", "trstun", "vote")
  
  cols_politic <- 
    c("polintr", "psppsgva", "actrolga", "psppipla", "cptppola", "vote", "lrscale")
  
  cols_sat <- 
    c("stflife", "stfeco", "stfgov", "stfdem", "stfedu", "stfhlth")
  
  cols_im <- 
    c("imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt", "imwbcnt")
  
  cols_ch <- 
    c("marsts", "marstgb", "edulvlb", "grspnum", "netinum")
  
  cols_misc <- 
    c("cntry", "region", "inwdds", "inwmms",  "inwyys", "dweight", 
      "stratum", "anweigth", "pspwght", "pweight")
  
  
  v_to_tbl <- 
    function(n_v){
      n_v %>% 
        as_tibble() 
    }
  
  cols_interested <- 
    tibble(
      vectors = 
        list(
          cols_eu, cols_trst,
          cols_politic, cols_sat,
          cols_im, cols_ch, cols_misc
        ),
      tbls = map(
        vectors, v_to_tbl
      ),
      types = c(
        "cols_eu", "cols_trst",
        "cols_politic", "cols_sat",
        "cols_im", "cols_ch", "cols_misc")
    )  %>% 
    select(-vectors) %>% 
    unnest(cols = c(tbls, types)) %>%
    left_join(
      label_tbl, by = c("value" = "colname")
    ) 
  
  raw %>% 
    select(
      any_of(cols_interested$value)
    )
  
}

all_files <- 
  files_paths %>% 
  as_tibble() %>% 
  mutate(
    tbls = map(value, get_data)
  )

label_tbl <- 
  var_label(all_files$tbls[[8]]) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "colname",
    values_to = "explanations"
  )


not_avialable_cols <- 
  all_files %>% 
  mutate(
    col_names = 
      map(tbls, colnames)
  ) %>% 
  select(value, col_names) %>% 
  unnest(col_names) %>% 
  left_join(
    label_tbl, by = c("col_names" = "colname")) %>% 
  add_count(col_names) %>% 
  filter(n != 8) %>% 
  mutate(
    edition = str_sub(value, 4, 4) %>% as.numeric()
  ) 

data <- 
  all_files %>% 
  unnest(cols = tbls) %>% 
  mutate(
    edition = str_sub(value, 4, 4) %>% as.numeric()
  ) %>% 
  left_join(
    y = 
      tibble(
        edition = c(2:9),
        year_s = seq(2004, 2018, 2)
      ),
    by = 
      "edition"
  )


# Regression Data & Complementaries ---------------------------------------

label_tbl <- 
  var_label(all_files$tbls[[8]]) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "colname",
    values_to = "explanations"
  )

core_countries <- 
  data %>%
  select(year_s, cntry) %>%
  distinct() %>%
  add_count(cntry) %>%
  filter(n == 8) %>%
  distinct(cntry) %>% 
  pull()

data_core <- 
  data %>% 
  filter(cntry %in% core_countries)

var_tbl <- 
  data_core %>% 
  group_by(euftf) %>% 
  summarise(n = n())

euftf_exp <- 
  var_tbl %>% 
  mutate(
    Europe_Integration = euftf %>% as.character(),
    Europe_Integration =
      case_when(
        Europe_Integration == "0" ~ "Too much integration",
        Europe_Integration == "10" ~ "We need more integration",
        is.na(Europe_Integration) ~ "I don't know",
        T ~ Europe_Integration
      ),
    Europe_Integration = fct_reorder(Europe_Integration, euftf)
  ) %>% 
  select(-n)

reg_data <- 
  data %>% 
  select(trstep, euftf, trstplt, trstprt, trstun, vote, 
         polintr, lrscale, stflife, stfeco, stfgov, 
         stfdem, stfedu, stfhlth, imsmetn, imdfetn, year_s,
         impcntr, imbgeco, imueclt, imwbcnt, cntry, pspwght) %>% 
  recipe(euftf ~ .) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  prep() %>% 
  juice() %>% 
  mutate(euftf1 = if_else(is.na(euftf), 5, as.numeric(euftf)))

reg_data_exp <- 
  label_tbl %>% 
  filter(
    colname %in% colnames(reg_data)
  )


# Estimations  ------------------------------------------------------------

all_countries_pooled <- 
  feols(
    fml = euftf ~ trstep+trstplt+trstprt+trstun+
      vote+polintr+lrscale+stflife+stfeco+stfgov+
      stfdem+stfedu+stfhlth+imsmetn+imdfetn+
      impcntr+imbgeco+imueclt+imwbcnt,
    data = reg_data,
    weights = ~pspwght,
    cluster = ~year_s + cntry
  )

all_countries_fe <- 
  feols(
    fml = euftf ~ trstep+trstplt+trstprt+trstun+
      vote+polintr+lrscale+stflife+stfeco+stfgov+
      stfdem+stfedu+stfhlth+imsmetn+imdfetn+
      impcntr+imbgeco+imueclt+imwbcnt | year_s + cntry,
    data = reg_data,
    weights = ~pspwght,
    cluster = ~year_s + cntry
  )

all_countries_fe_imp <- 
  feols(
    fml = euftf1 ~ trstep+trstplt+trstprt+trstun+
      vote+polintr+lrscale+stflife+stfeco+stfgov+
      stfdem+stfedu+stfhlth+imsmetn+imdfetn+
      impcntr+imbgeco+imueclt+imwbcnt | year_s + cntry,
    data = reg_data,
    weights = ~pspwght,
    cluster = ~year_s + cntry
  )

core_countries <- 
  feols(
    fml = euftf ~ trstep+trstplt+trstprt+trstun+
      vote+polintr+lrscale+stflife+stfeco+stfgov+
      stfdem+stfedu+stfhlth+imsmetn+imdfetn+
      impcntr+imbgeco+imueclt+imwbcnt | year_s + cntry,
    data = reg_data %>% 
      filter(cntry %in% core_countries),
    weights = ~pspwght,
    cluster = ~year_s + cntry
  )

etable(core_countries, all_countries_fe, 
       all_countries_pooled, all_countries_fe_imp, tex = T)


# Appendix Table & Plots ---------------------------------------------------------

## Total Observations
data %>% 
  group_by(year_s, cntry) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = year_s, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  left_join(
    WDI::WDI_data$country %>% 
      as_tibble() %>% 
      select(country, iso2c),
    by = c("cntry" = "iso2c")
  ) %>% 
  select(-cntry) %>% 
  rename(Country = country) %>% 
  relocate(Country) %>% 
  janitor::adorn_totals("row") %>% 
  janitor::adorn_totals("col") %>% 
  stargazer(summary = F, type = "latex")

## EI Dist Plots

desc_tbl <- 
  data %>% 
  select(cntry, year_s, euftf, pspwght)

weight_tbl <- 
  desc_tbl %>% 
  group_by(cntry, year_s) %>% 
  summarise(tot_weight = sum(pspwght)) %>% 
  ungroup()

ei_tbl <- 
  desc_tbl %>% 
  group_by(cntry, year_s, euftf) %>% 
  summarise(value = sum(pspwght)) %>% 
  ungroup() %>% 
  left_join(weight_tbl, by = c("cntry", "year_s")) %>%
  mutate(value = value / tot_weight) %>% 
  left_join(
    WDI::WDI_data$country %>% 
      as_tibble() %>% 
      select(country, iso2c),
    by = c("cntry" = "iso2c")
  ) %>% 
  mutate(Europe_Integration = euftf %>% as.character(),
         Europe_Integration =
           case_when(
             Europe_Integration == "0" ~ "Too much integration",
             Europe_Integration == "10" ~ "We need more integration",
             is.na(Europe_Integration) ~ "I don't know",
             T ~ Europe_Integration
           ),
         Europe_Integration = fct_reorder(Europe_Integration, euftf),
         value = round(value * 100, digits = 2)) %>% 
  group_by(cntry) %>% 
  nest() %>% 
  mutate(
    results = 
      map(data,
          function(tbl){
            tbl %>% 
              select(Country = country, value, 
                     Year = year_s, Europe_Integration) 
          }
      )
  ) %>% 
  ungroup() %>% 
  select(-data) %>% 
  unnest(cols = c(results)) %>% 
  select(-cntry)

core_countries <- ei_tbl %>% count(Country, sort = T) %>% 
  filter(n == 8) %>% pull(Country)

pop <- WDI(indicator = "SP.POP.TOTL", 
           start = 2004, end = 2018, extra = T) %>% 
  as_tibble()

pop1 <- 
  pop %>% 
  select(country, Population = SP.POP.TOTL, year) %>% 
  filter(country %in% core_countries) 

Tot_Pop <- 
  pop1 %>% 
  group_by(year) %>% 
  summarise(Tot_Pop = sum(Population))

pop2 <- 
  pop1 %>% 
  left_join(Tot_Pop, by = "year") %>% 
  mutate(rt = round(Population / Tot_Pop, digits = 3)) %>% 
  select(country, year, rt)

ei_tbl1 <- 
  ei_tbl %>% 
  filter(
    Country %in% core_countries
  ) %>% 
  left_join(pop2, 
            by = c("Country" = "country",
                   "Year" = "year")) %>% 
  mutate(value = value * rt) %>% 
  group_by(Year, Europe_Integration) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(Percent_Label = str_c(round(value,0), "%"))

plot1 <- ei_tbl1 %>% 
  filter(Year %in% c(2004, 2006)) %>% 
  ggplot(aes(Europe_Integration, value/100)) +
  geom_col(fill = "white", color = "black") +
  geom_text(aes(label = Percent_Label, y = (value/100 - 2/100)), color = "red") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_grid(~Year) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "")

plot2 <- ei_tbl1 %>% 
  filter(Year %in% c(2008, 2012)) %>% 
  ggplot(aes(Europe_Integration, value/100)) +
  geom_col(fill = "white", color = "black") +
  geom_text(aes(label = Percent_Label, y = (value/100 - 2/100)), color = "red") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_grid(~Year) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "")

plot3 <- ei_tbl1 %>% 
  filter(Year %in% c(2014, 2016, 2018)) %>% 
  ggplot(aes(Europe_Integration, value/100)) +
  geom_col(fill = "white", color = "black") +
  geom_text(aes(label = Percent_Label, y = (value/100 - 2/100)), color = "red") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_grid(~Year) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "")

## EI Grouped Change

opt_3_get <- function(tmp){ 
  
  var_tbl <- 
    tmp %>% 
    group_by(Opinion_EI) %>% 
    summarise(n = n())
  
  euftf_exp <- 
    var_tbl %>% 
    mutate(
      Opinion_EI_order =
        case_when(
          Opinion_EI == "Dont_Like" ~ 1,
          Opinion_EI == "Loves" ~ 3,
          T ~ 2
        ),
      Opinion_EI = fct_reorder(Opinion_EI, Opinion_EI_order)
    ) %>% 
    select(-n)
  
  euftf_stat <- 
    var_tbl %>% 
    mutate(prc = n / sum(n),
           Percent = round(100 * prc, 2)) %>%
    left_join(euftf_exp, by = "Opinion_EI") %>% 
    select(Opinion_EI, Percent) 
  
  res_tbl <- euftf_stat %>% 
    kable()
  
  plot <- euftf_stat %>% 
    mutate(Percent_Label = str_c("%", Percent)) %>% 
    ggplot(aes(Opinion_EI, Percent)) +
    geom_col(fill = "white", color = "black") +
    geom_text(aes(label = Percent_Label, y = (Percent - 2))) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(x = "", y = "", 
         title = "Position on Europe Integration?")
  
  list(res_tbl, plot, euftf_stat)
}

opt_3_res <- 
  data_core %>% 
  filter(!is.na(euftf)) %>% 
  mutate(
    Opinion_EI =
      case_when(
        euftf < 4 ~ "Dont_Like",
        euftf > 6 ~ "Loves",
        T ~ "Moderate"
      )
  ) %>% 
  group_by(year_s, cntry) %>% 
  nest() %>% 
  mutate(
    analysis_results_list = map(data, opt_3_get))

opt3_tbls <- 
  list()

for (x in c(1:nrow(ungroup(opt_3_res)))) {
  opt3_tbls[[x]] <- opt_3_res$analysis_results_list[[x]][[3]]
}

opt_3_tbl <- 
  opt_3_res %>% 
  select(cntry, year_s) %>% 
  ungroup() %>% 
  mutate(opt3_tbls) %>% 
  unnest(opt3_tbls) 

opt_3_tbl %>% 
  filter(year_s %in% c(2016, 2018)) %>% 
  ggplot(aes(year_s, Percent/100)) +
  geom_point(size = 2.5) +
  geom_line(aes(color = cntry), alpha = .4) +
  geom_smooth(method = "lm") +
  facet_wrap(~Opinion_EI) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = 0:2100, 
                     guide = guide_axis(angle = 45)) +
  labs(x = "", y = "", 
       title = "Opinions of Economics Integration")

