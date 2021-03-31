library(tidyverse)


types <- c("ctp", "nchs", "cdc", "cdc_cases", "nchs_old")
df_list <- vector(mode = "list", length = length(types))
for (i in 1:length(types)){
  #incProgress(1/length(types), detail = paste("Loading data ", types[i]))
  df_list[[i]] <- load_data(type = types[i])
}
df_list <- df_list %>% reduce(full_join, by = c("abb", "date", "name"))
df_list

filt_string <- "tot_deaths"

old_dat <- load_data("nchs_old")
old_dat %>% filter(name == "Alabama") %>% ggplot(aes(x = date, y = nchs_2020_10_02_tot_deaths)) + geom_point()

df_list <- df_list %>% 
    pivot_longer(ends_with("tot_deaths"), names_to = "data_sets", values_to = "deaths") %>%
    select(date, abb, name, data_sets, deaths) %>% 
    mutate(data_sets = str_remove(data_sets, paste0("_", "tot_deaths"))) %>% 
    mutate(data_sets = str_to_upper(data_sets)) %>% 
    filter(name == "Alabama") %>% filter(!is.na(deaths))

ggplot(df_list, aes(x = date, y = deaths, col = data_sets)) + geom_line()


