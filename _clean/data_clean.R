
library(tidyverse)
library(readxl)


data <- read_excel("/Users/elenachechik/Desktop/Russian_studies/data/full_data_precise original.xlsx")


# цепляем из экселевского файла organisation_full_names новые organisation_full

organisation_full_names <- read_excel("/Users/elenachechik/Desktop/Russian_studies/data/organisation_full_names.xlsx") %>% 
  select(organisation, organisation_full) %>% 
  distinct(organisation, .keep_all = TRUE) %>% rename(organisation_full_new = organisation_full)

data <- data %>% #select(organisation, organisation_full) %>% 
  left_join(organisation_full_names) %>% 
  mutate(organisation_full = case_when(!is.na(organisation_full_new) ~ organisation_full_new,
                                       TRUE ~ organisation_full)) %>% select(-organisation_full_new)


UT_org_replace <- read_delim("/Users/elenachechik/Desktop/Russian_studies/data/UT_org_replace.csv", 
                             delim = ";", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE) 
UT_org_replace_ID <- read_delim("/Users/elenachechik/Desktop/Russian_studies/data/UT_org_replace.csv", 
                                delim = ";", escape_double = FALSE, col_names = FALSE, 
                                trim_ws = TRUE) %>% pull(X1)
ussr_replace <- read_excel("/Users/elenachechik/Desktop/Russian_studies/data/organisation_full_names.xlsx", 
                           sheet = "для замены ussr") %>% select(UT, country_unified) %>% distinct(UT, .keep_all = TRUE)
ussr_replace_ID <- read_excel("/Users/elenachechik/Desktop/Russian_studies/data/organisation_full_names.xlsx", 
                              sheet = "для замены ussr") %>% select(UT, country_unified) %>% distinct(UT, .keep_all = TRUE) %>% pull(UT)


data <- data %>% 
  mutate(year_group = case_when(year >= 1990 & year <= 2000 ~ "1990-2000",
                                year >= 2001 & year <= 2010 ~ "2001-2010",
                                year >= 2011 & year <= 2020 ~ "2011-2020",
                                TRUE ~ as.character(year))) %>% 
  mutate(year_group = factor(year_group, levels = c("1990-2000", "2001-2010","2011-2020"))) 

data <- data %>% mutate(field = gsub("\\['", "", field),
                        field = gsub("\\']", "", field),
                        field = gsub("\\', '", ";", field))

data <- data %>% mutate(region = case_when(region == "Africa"|
                                             region == "Americas" |
                                             region == "Oceania" ~ "Other",
                                           TRUE ~ region))




# 1 Чистка стран и организаций

data <- data %>% #select(UT, country, organisation, organisation_full) %>% 
  left_join(ussr_replace, by = c("UT"="UT")) %>% 
  mutate(country = case_when(is.na(country) ~ "Unknown",
                             TRUE ~ country)) %>% 
  mutate(country = case_when(!is.na(country_unified) & (country == "Ussr" | country == "Unknown") ~ country_unified, 
                             country == "Bosnia And Herzegovina" ~ "Bosnia and Herzegovina",
                             organisation_full == "University Belgrade" & country == "Yugoslavia" ~ "Serbia",
                             country == "England" | country == "North Ireland" |
                               country == "Wales" | country == "Scotland" ~ "United Kingdom", 
                             country == "Usa" ~ "United States", 
                             country == "Hong Kong" | country == "Peoples R China" ~ "China",
                             TRUE ~ country)) %>% select(-country_unified) %>% 
  filter(country != "Yugoslavia" & country != "Trinidad Tobago") 

data <- data %>% #select(UT, country, organisation_full) %>% 
  mutate(organisation_full = case_when(is.na(organisation_full) ~ "Unknown",
                                       TRUE ~ organisation_full)) %>% 
  left_join(UT_org_replace, by = c("UT"="X1")) %>% 
  mutate(organisation_full = case_when(UT == "WOS:000178643600036" | UT == "WOS:000455241900010" ~ "Unknown",
                                       !is.na(X2) & organisation_full == "War College" ~ X2,
                                       UT == "WOS:000337988000016" ~ "Unknown",
                                       UT == "WOS:A1990EF84700009" | UT == "WOS:A1990EF84700001" |
                                         UT == "WOS:A1991HK57400005" ~ "Unknown",
                                       TRUE ~ organisation_full)) %>% select(-X2) %>% 
  filter(organisation_full != "Independent Researcher") %>% 
  filter(organisation_full != "U.S. Naval War College") %>% 
  filter(organisation_full != "Us Naval War College")




# Страны норм, но есть три вида РЭШа

# Поэтому вменение моды будет бить не только универы тесок, но и микро задублированные универы. 

# Вменение моды стране через институцию


## Делаем таблицу с модами 

moda_table <- data %>% 
  group_by(organisation_full, country) %>% count() %>% ungroup() %>%
  group_by(organisation_full) %>% mutate(n_count = length(country)) %>% ungroup() %>%
  mutate(n = case_when(country == "Unknown" ~ as.numeric(0),
                       TRUE ~ as.numeric(n))) %>% 
  group_by(organisation_full, n_count) %>%
  mutate(country_2 = case_when(n == max(n) ~ country,
                               TRUE ~ NA_character_)) %>% ungroup() %>%
  filter(!is.na(country_2)) %>% select(organisation_full, country_2) %>% 
  distinct(organisation_full, .keep_all = TRUE)

## цепляем к нашему датасету data_website original
data <- data %>% left_join(moda_table)
data <- data %>% mutate(country_2 = case_when(organisation_full == "Unknown" ~ country,
                                              TRUE ~ country_2)) %>% 
  select(-country) %>% 
  rename(country = country_2)


## смотрим на "грязь" как в пункте (1) и сравниваем с новой переменной country_2

# df <- data_mod %>%
#   group_by(organisation_full) %>%
#   summarise(count = length(UT),
#             # country_2 = paste(sort(unique(country_2)), collapse="; "),
#             country = paste(sort(unique(country)), collapse="; ")) %>%
#   arrange(desc(count))


## Вмененеи моды Региону через страну


## Делаем таблицу с модами 

moda_table <- data %>% # select(country, region) %>% 
  mutate(region = case_when(is.na(region) ~ "Unknown",
                            TRUE ~ region)) %>% 
  group_by(country, region) %>% count() %>% ungroup() %>%
  group_by(country) %>% mutate(n_count = length(region)) %>% ungroup() %>%
  mutate(n = case_when(region == "Unknown" ~ as.numeric(0),
                       TRUE ~ as.numeric(n))) %>% 
  group_by(country, n_count) %>%
  mutate(region_2 = case_when(n == max(n) ~ region,
                              TRUE ~ NA_character_)) %>% ungroup() %>%
  filter(!is.na(region_2)) %>% select(country, region_2) %>% 
  distinct(country, .keep_all = TRUE)

## цепляем к нашему датасету data_website original
data <- data %>% # select(country, region) %>% 
  mutate(region = case_when(is.na(region) ~ "Unknown",
                            TRUE ~ region)) %>% 
  left_join(moda_table)

data <- data %>% mutate(region_2 = case_when(country == "Unknown" ~ region,
                                             TRUE ~ region_2))  %>% 
  select(-region) %>% 
  rename(region = region_2)

## смотрим на "грязь" 
# dt <- data2 %>% select(country, region) %>%
#   group_by(country) %>%
#   summarise(count = n(),
#             #region_2 = paste(sort(unique(region_2)), collapse="; "),
#             region = paste(sort(unique(region)), collapse="; ")) %>%
#   arrange(desc(count))

write_csv(data, 
          "/Users/elenachechik/Desktop/Russian_studies/data/data_clean.csv")


## Институции для катерины, оценка грязи


# df <- data %>% group_by(organisation_full) %>% count() 
# write_csv(df, 
#           "/Users/elenachechik/Desktop/Russian_studies/data/for_katerina_org.csv")
