library(tidyverse)
library(haven)
library(labelled)

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

not_avialable_cols %>%
  filter(edition > 7) %>% 
  ggplot(aes(y = col_names, x = edition)) +
  geom_tile()




# pop correction

library(WDI)

pop <- WDI(indicator = "SP.POP.TOTL", start = 2004, end = 2018, extra = T) %>% as_tibble()

data <- 
  data %>% 
  left_join(
    y = pop,
    by = c("cntry" = "iso2c",
           "year_s" = "year")
  ) 

data <- rename(data, popul = SP.POP.TOTL)

# data %>% 
#   select(year_s, cntry) %>% 
#   distinct() %>% 
#   add_count(cntry) %>% 
#   filter(n == 8) %>% 
#   distinct(cntry)

# data %>% 
#   select(cntry, pspwght, year_s, popul)

tot_pop_yearly <- 
  data %>% 
  select(year_s, cntry, popul) %>% 
  group_by(year_s) %>% 
  distinct(cntry, year_s, .keep_all = T) %>% 
  summarise(tot_pop = sum(popul),
            cntry)

pop_adj_weights <- 
  data %>%
  select(cntry, pspwght, year_s, popul) %>% 
  group_by(cntry, year_s) %>% 
  summarise(tot_weight = sum(pspwght), popul) %>% 
  distinct() %>% 
  left_join(tot_pop_yearly) %>% 
  ungroup() %>% 
  mutate(rt_pop = popul / tot_pop) %>% 
  select(cntry, year_s, rt_pop)

data <- 
  data %>% 
  left_join(pop_adj_weights) %>% 
  mutate(pspwght1 = pspwght * rt_pop)

# analysis of leaving Europe ----------------------------------------------

raw <- read_dta(files_paths[7], encoding = "latin1")

label_tbl <- 
  var_label(raw) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "colname",
    values_to = "explanations"
  )

# library(survey)
# library(knitr)
# 
# data_surv <- 
#   svydesign(
#   ids = ~cntry,
#   weights = ~pspwght1,
#   data = data
# )
# 
# svymean(~euftf, data_surv, na.rm = T) %>% 
#   kable()
# 
# svyhist(~euftf, data_surv, )
# 
# tmp <- 
#   data %>% 
#   select(cntry, year_s, euftf, pspwght) %>% 
#   group_by(euftf) %>% 
#   summarise(
#     eutft_weighted = sum(pspwght)
#   ) %>% 
#   filter(!is.na(euftf))
# 
# sum(tmp$euftf * tmp$eutft_weighted, na.rm = T) / 
#   sum(tmp$eutft_weighted)
  
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

euftf_stat <- 
  var_tbl %>% 
  mutate(prc = n / sum(n),
         Percent = round(100 * prc, 2)) %>%
  left_join(euftf_exp) %>% 
  select(Europe_Integration, Percent) 

euftf_stat %>% 
  kable()

euftf_stat %>% 
  mutate(Percent_Label = str_c("%", Percent)) %>% 
  ggplot(aes(Europe_Integration, Percent)) +
  geom_col(fill = "white", color = "black") +
  geom_text(aes(label = Percent_Label, y = (Percent - 2))) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "", y = "", 
       title = "Did Europe Integration Went Too Far?",
       subtitle = "From whole dataset (unweighted), 2004-2018, Core 16 countries")

w_euftf <- 
  data_core %>% 
  select(year_s, cntry, euftf, pspwght1) %>% 
  group_by(euftf, year_s) %>% 
  summarise(w_euftf = sum(pspwght1))

get_fun_euftf <- function(tbl, yr){ 
  tbl <- 
    tbl %>% 
    mutate(
      Europe_Integration = euftf %>% as.character(),
      Europe_Integration =
        case_when(
          Europe_Integration == "0" ~ "Too much integration",
          Europe_Integration == "10" ~ "We need more integration",
          is.na(Europe_Integration) ~ "I don't know",
          T ~ Europe_Integration
        ),
      Europe_Integration = fct_reorder(Europe_Integration, euftf),
      Percent = round(100 * f_euftf, 2),
      Percent_Label = str_c("%", Percent)) 
  
  tbl %>% 
    ggplot(aes(Europe_Integration, Percent)) +
    geom_col(fill = tbl$clr[1], color = "black") +
    geom_text(aes(label = Percent_Label, y = (Percent - 2))) +
    geom_text(aes(x = 3, y = 15, label = str_c("Year of the data: ", yr))) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(x = "", y = "", 
         title = "Did Europe Integration Went Too Far?",
         subtitle = str_c("Core 16 countries")) +
    theme(legend.position = "none")
}

clr_tbl <- 
  data %>% distinct(year_s) %>% filter(year_s != 2010) %>%  
  mutate(clr = c("white", rep(c("red", "white"), 3)))

plots_euftf <- 
  w_euftf %>% 
  ungroup() %>% 
  group_by(year_s) %>% 
  summarise(tw_euftf = sum(w_euftf)) %>% 
  right_join(w_euftf, by = "year_s") %>% 
  mutate(f_euftf = w_euftf / tw_euftf) %>% 
  left_join(clr_tbl, by = "year_s") %>% 
  group_by(year_s) %>% 
  nest() %>% 
  filter(year_s != 2010) %>% 
  mutate(plots = map2(data, year_s, get_fun_euftf))

paths <- str_c("plot", plots_euftf$year_s)

map2(paths, plots_euftf$plots, 
     function(pth, plt){
       ggsave(filename = str_c(pth, ".png"), 
              plot = plt,
              path = here::here("plots"))
     }
     )

## applying to whole

euftf_fun <- function(tbl, year){
  var_tbl <- 
    tbl %>% 
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
  
  euftf_stat <- 
    var_tbl %>% 
    mutate(prc = n / sum(n),
           Percent = round(100 * prc, 2)) %>%
    left_join(euftf_exp, by = "euftf") %>% 
    select(Europe_Integration, Percent) 
  
  res_tbl <- euftf_stat %>% 
    kable()
  
  plot <- euftf_stat %>% 
    mutate(Percent_Label = str_c("%", Percent)) %>% 
    ggplot(aes(Europe_Integration, Percent)) +
    geom_col(fill = "white", color = "black") +
    geom_text(aes(label = Percent_Label, y = (Percent - 2))) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(x = "", y = "", 
         title = "Did Europe Integration Went Too Far?",
         subtitle = str_c("From whole dataset (unweighted), from year ", 
                          year, ", Core 16 countries"))
  
  list(res_tbl, plot, euftf_stat)
}

euftf_res <- 
  data_core %>% 
  group_by(year_s) %>% 
  nest() %>% 
  mutate(
    analysis_results_list = map2(data, year_s, euftf_fun))

euftf_res$analysis_results_list[[6]][[2]]

# looking at trends

euftf_res$analysis_results_list[[1]][[3]]

res_tbls <- 
  list()

for (x in c(1:8)) {
  res_tbls[[x]] <- euftf_res$analysis_results_list[[x]][[3]]
}

euftf_res %>% 
  select(year_s) %>% 
  ungroup() %>% 
  mutate(res_tbls) %>% 
  filter(year_s != 2010) %>% 
  unnest(res_tbls) %>% 
  filter(Europe_Integration == "Too much integration") %>% 
  mutate(Percent = Percent / 100) %>% 
  ggplot(aes(year_s, Percent)) +
  geom_point(color = "red", size = 3) +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(y = "", x = "", 
       title = "Percentage of people who think Europe Integration gone too far",
       subtitle = "From core 16 countries")

# apply every answer

get_plot_euftf_trend <- 
  function(chr){
    euftf_res %>% 
      select(year_s) %>% 
      ungroup() %>% 
      mutate(res_tbls) %>% 
      filter(year_s != 2010) %>% 
      unnest(res_tbls) %>% 
      filter(Europe_Integration == chr) %>% 
      mutate(Percent = Percent / 100) %>% 
      ggplot(aes(year_s, Percent)) +
      geom_point(color = "red", size = 3) +
      geom_line() +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(y = "", x = "", 
           title = str_c("Percentage of people who think, ", chr),
           subtitle = "From core 16 countries")
  }

euftf_answers_trend <- 
  euftf_res %>% 
  select(year_s) %>% 
  ungroup() %>% 
  mutate(res_tbls) %>% 
  filter(year_s != 2010) %>% 
  unnest(res_tbls) %>% 
  distinct(Europe_Integration) %>% 
  mutate(
    chrs = as.character(Europe_Integration),
    plots = map(chrs, get_plot_euftf_trend))


euftf_answers_trend$plots[[2]]

# country distributions

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
  
# Country Analysis

opt_3_tbl %>% 
  filter(year_s %in% c(2016, 2018)) %>% 
  pivot_wider(names_from = Opinion_EI, 
              values_from = Percent) %>% 
  select(-c(Loves, Moderate)) %>% 
  pivot_wider(names_from = year_s,
              values_from = Dont_Like, 
              names_prefix = "Dont_Like_") %>% 
  left_join(pop %>% 
              select(iso2c, country) %>% 
              distinct(),  
            by = c("cntry" = "iso2c")) %>% 
  mutate(
    Dont_Like_Change = (Dont_Like_2018 - Dont_Like_2016) / 
      Dont_Like_2016,
    Country = fct_reorder(country, desc(Dont_Like_Change))
  ) %>% 
  ggplot(aes(Country, Dont_Like_Change)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "", 
       title = "Change in Countries' Citizens Opinion Between 2016 and 2018",
       subtitle = "Those who don't like European Economic Integration")

opt_3_tbl %>% 
  filter(year_s %in% c(2016, 2018)) %>% 
  pivot_wider(names_from = Opinion_EI, 
              values_from = Percent) %>% 
  select(-c(Dont_Like, Moderate)) %>% 
  pivot_wider(names_from = year_s,
              values_from = Loves, 
              names_prefix = "Loves_") %>% 
  left_join(pop %>% 
              select(iso2c, country) %>% 
              distinct(),  
            by = c("cntry" = "iso2c")) %>% 
  mutate(
    Loves_Change = (Loves_2018 - Loves_2016) / 
      Loves_2016,
    Country = fct_reorder(country, Loves_Change)
  ) %>% 
  ggplot(aes(Country, Loves_Change)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "", 
       title = "Change in Countries' Citizens Opinion Between 2016 and 2018",
       subtitle = "Those who want further European Economic Integration")

# Characteristics of EU haters
# analysis_tbl <- 
library(tidymodels)

analysis_tbl_core <- 
  data_core %>% 
  select(trstep, euftf, trstplt, trstprt, trstun, vote, 
         polintr, lrscale, stflife, stfeco, stfgov, 
         stfdem, stfedu, stfhlth, imsmetn, imdfetn, year_s,
         impcntr, imbgeco, imueclt, imwbcnt, cntry, pspwght) %>% 
  recipe(euftf ~ .) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  prep() %>% 
  juice() %>% 
  mutate(Brexit = if_else(year_s > 2014, 1, 0))

analysis_tbl_wide <- 
  data %>% 
  select(trstep, euftf, trstplt, trstprt, trstun, vote, 
         polintr, lrscale, stflife, stfeco, stfgov, 
         stfdem, stfedu, stfhlth, imsmetn, imdfetn, year_s,
         impcntr, imbgeco, imueclt, imwbcnt, cntry, pspwght, pspwght1) %>% 
  recipe(euftf ~ .) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  prep() %>% 
  juice() %>% 
  mutate(Brexit = if_else(year_s > 2014, 1, 0))
  

label_tbl %>% 
  filter(
    colname %in% 
      c(
        "trstep", "euftf", "trstplt", "trstprt", "trstun", "vote", 
        "polintr", "lrscale", "stflife", "stfeco", "stfgov", 
        "stfdem", "stfedu", "stfhlth", "imsmetn", "imdfetn", "year_s",
        "impcntr", "imbgeco", "imueclt", "imwbcnt", "cntry", "pspwght"
      )
  )

library(fixest)
wide_estimation <- 
  feols(
    data = analysis_tbl_wide,
    fml = euftf ~ trstplt+trstprt+trstun+vote+polintr+
                    lrscale+stflife+stfeco+stfgov+stfdem+stfedu+stfhlth+
                    imsmetn+imdfetn+impcntr+imbgeco+imueclt+
                    imwbcnt | cntry+year_s,
    weights = ~pspwght1,
    cluster = ~cntry+year_s
)



between16_18_estimation_wide <- 
  feols(
    data = analysis_tbl_wide,
    fml = euftf ~ trstplt+trstprt+trstun+vote+polintr+
      lrscale+stflife+stfeco+stfgov+stfdem+stfedu+stfhlth+
      imsmetn+imdfetn+impcntr+imbgeco+imueclt+
      imwbcnt | cntry+year_s,
    weights = ~pspwght,
    cluster = ~cntry+year_s,
    subset = ~year_s %in% c(2016, 2018)
  )


wide_estimation_core <- 
  feols(
    data = analysis_tbl_core,
    fml = euftf ~ trstplt+trstprt+trstun+vote+polintr+
      lrscale+stflife+stfeco+stfgov+stfdem+stfedu+stfhlth+
      imsmetn+imdfetn+impcntr+imbgeco+imueclt+
      imwbcnt | cntry+year_s,
    weights = ~pspwght,
    cluster = ~cntry+year_s
  )

between16_18_estimation_core <- 
  feols(
    data = analysis_tbl_core,
    fml = euftf ~ trstplt+trstprt+trstun+vote+polintr+
      lrscale+stflife+stfeco+stfgov+stfdem+stfedu+stfhlth+
      imsmetn+imdfetn+impcntr+imbgeco+imueclt+
      imwbcnt | cntry+year_s,
    weights = ~pspwght,
    cluster = ~cntry+year_s,
    subset = ~year_s %in% c(2016, 2018)
  )

analysis_tbl_core


etable(wide_estimation,
       between16_18_estimation, 
       tex = F)

data_core

data_16_18 <- 
  data %>% 
  filter(year_s > 2014) %>% 
  select(-c(stratum, netinum, grspnum)) %>% 
  mutate(
    Int_Date = str_c(inwyys, "-", inwmms, "-", inwdds) %>% 
      lubridate::ymd(),
    ym = tsibble::yearmonth(Int_Date)
  ) 

# net effect of brexit

library(tsibble)
data_16_18 %>% 
  count(ym) %>% 
  ggplot(aes(ym, n)) +
  geom_col()

data_16_18 %>% 
  group_by(ym) %>% 
  summarise(avg_euftf = mean(euftf, na.rm = T)) %>% 
  ggplot(aes(ym, avg_euftf)) +
  geom_col()

data_16_18 %>% colnames()

data_16_18_analysis <- 
  data_16_18 %>% 
  recipe(euftf ~ .) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  prep() %>% 
  juice() %>% 
  select(- c(value, region.x, country:ym, dweight, pweight:inwyys))

est_16_18 <- 
  feols(
    data = data_16_18_analysis,
    fml = euftf ~ trstep+trstprl+trstlgl+trstplt+trstprt+
      trstun+vote+polintr+lrscale+stflife+stfeco+stfgov+
      stfdem+stfedu+stfhlth+imsmetn+imdfetn+impcntr+imbgeco+
      imueclt+imwbcnt+marsts+edulvlb+marstgb+atcherp+
      vteurmmb+psppsgva+actrolga+psppipla+cptppola+
      edition | cntry+year_s,
    weights = ~pspwght,
    cluster = ~cntry+year_s
  )

analysis_tbl_wide %>% 
  #select(starts_with("im")) %>% 
  select(-c(cntry, euftf)) %>% 
  cor() %>% 
  corrplot::corrplot()


set.seed(123)
data_pca <- 
  data %>% 
  select(trstep, euftf, trstplt, trstprt, trstun, vote, 
         polintr, lrscale, stflife, stfeco, stfgov, 
         stfdem, stfedu, stfhlth, imsmetn, imdfetn, year_s,
         impcntr, imbgeco, imueclt, imwbcnt, cntry, pspwght) %>% 
  recipe(euftf ~ .) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_normalize(starts_with("im"), starts_with("tr"),
                 starts_with("stf")) %>% 
  step_pca(imsmetn, imdfetn, impcntr,
           num_comp = 1, prefix = "pro_immigration") %>% 
  step_pca(imbgeco, imueclt, imwbcnt,
           num_comp = 1, prefix = "anti_immigration") %>% 
  step_pca(starts_with("tr"), 
           num_comp = 1, prefix = "trust") %>% 
  step_pca(starts_with("stf"), 
           num_comp = 1,
           prefix = "satisfaction") %>% 
  prep() %>% 
  juice()


psa_estimation <- 
  feols(
    data = data_pca,
    fml = euftf ~ vote +
      pro_immigration1 + anti_immigration1 +
      trust1 | cntry+year_s,
    weights = ~pspwght,
    cluster = ~cntry+year_s
  )

data %>% 
  group_by(cntry, year_s) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(cntry %in% core_countries) %>% 
  pivot_wider(names_from = year_s, 
              values_from = n) %>% 
  kable()

#######
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


all_countries_fe_only_eco <- 
  feols(
    fml = euftf ~ trstep+stfeco | year_s + cntry,
    data = reg_data,
    weights = ~pspwght,
    cluster = ~year_s + cntry
  )

###### tables
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



  
  












