#### The impact of website translation on access to biodiversity information - R Code ####
library(ggplot2)
library(dplyr)
library(readr)
library(rnaturalearth)
library(sf)
library(stringr)
library(scales)
library(tidyverse)
library(lubridate)
library(patchwork)
library(MASS)
library(FDRestimation)
library(broom)

#### Mapping ####
map_dat <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Mappingdaat.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

clean_text <- function(x) {
  x %>%
    str_replace_all("\u00A0", " ") %>%  
    str_replace_all("’", "'") %>%             
    str_trim() %>%
    stringi::stri_trans_general("Latin-ASCII")}

map_dat <- map_dat %>%
  mutate(Countries = clean_text(Countries))

world <- world %>%
  mutate(name_long = clean_text(name_long))

world_data <- world %>%
  left_join(map_dat, by = c("name_long" = "Countries"))

# Figure 1 - Number of Downloads and Visitors Map #
dl_map <- ggplot(world_data) +
  geom_sf(aes(fill = downloads)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1, 1000, 340000), 
                       labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(2, "cm"),  
                         barwidth = unit(0.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Downloads from GBIF\n website\n(log10 Transformed)", title = "") +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 9, face = "bold"),    
    legend.text = element_text(size = 8))

vis_map <- ggplot(world_data) +
  geom_sf(aes(fill = visitors)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1, 30, 10000, 3000000), 
                       labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(2, "cm"),  
                         barwidth = unit(0.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to enitre GBIF\n website\n(log10 Transformed)", title = "") +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 9, face = "bold"),    
    legend.text = element_text(size = 8))

dl_map / vis_map + plot_annotation(tag_levels = 'A')

## Figure 2 - Spanish and French Maps ##

# Spanish 
sp <- ggplot(world_data) +
  geom_sf(aes(fill = `/es/search`)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1, 10, 100, 1000, 10000, 110000), 
                       labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Spanish Search Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

# French 
fr <- ggplot(world_data) +
  geom_sf(aes(fill = `/fr/search`)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1, 40, 400, 4000, 42500), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to French Search Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

sp / fr

## Figure S1-7 - Other Language Maps ##

# Japanese 
ggplot(world_data) +
  geom_sf(aes(fill = `/ja/search`)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1, 20, 200, 2000, 22000), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Japanese Search Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

# Ukrainian 
ggplot(world_data) +
  geom_sf(aes(fill = `/uk/search`)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1, 6, 60, 600, 6000), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Ukrainian Search Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

# Simplified Chinese 
ggplot(world_data) +
  geom_sf(aes(fill = `/zh/search`)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1,40, 400, 4000, 40000), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Simplified Chinese\nSearch Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

# Traditional Chinese 
ggplot(world_data) +
  geom_sf(aes(fill = `/zh-tw/search`)) +
  scale_fill_viridis_c(trans = "log10",
                       breaks = c(1, 8, 85, 850, 8400), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Traditional Chinese\nSearch Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

# Russian 
ggplot(world_data) +
  geom_sf(aes(fill = `/ru/search`)) +
  scale_fill_viridis_c(trans = "log10",
                       breaks = c(1, 17, 170, 1700, 17000), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Russian Search Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

# Portuguese
ggplot(world_data) +
  geom_sf(aes(fill = `/pt/search`)) +
  scale_fill_viridis_c(trans = "log10",  
                       breaks = c(1, 27, 270, 2700, 27000), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Portuguese Search Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

# Arabic 
ggplot(world_data) +
  geom_sf(aes(fill = `/ar/search`)) +
  scale_fill_viridis_c(trans = "log10", 
                       breaks = c(1, 5, 50, 470), labels = scales::label_comma(accuracy = 1), 
                       na.value = "grey90",
                       guide = guide_colorbar(
                         barheight = unit(4.5, "cm"),  
                         barwidth = unit(1.5, "cm"),  
                         ticks.colour = NA)) +
  labs(fill = "Visits to Arabic Search Page\n(log10 Transformed)\n ", title = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.15),   
    legend.justification = c(0, 0),      
    legend.background = element_rect(fill = "white", color = "grey80"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

#### Visits to Specific Language Pages ####

## Japanese #
jp_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Japanese.csv")

jp_visit_long <- jp_visit %>%
  dplyr::select(date, `/search`, `/ja/search`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`/search`, `/ja/search`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("/search", "/ja/search"),
                           labels = c("English", "Non-English language")))

jp_plot <- ggplot(jp_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.474, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "", fill = "Language", title = "Japanese") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Ukrainian ##
uk_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Ukrainian.csv") 

uk_visit_long <- uk_visit %>%
  dplyr::select(date, `/search`, `/uk/search`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`/search`, `/uk/search`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("/search", "/uk/search"),
                           labels = c("English", "Non-English language")))

uk_plot <- ggplot(uk_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.419, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "Proportion", fill = "Language", title = "Ukrainian") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Traditional Chinese ##
zh_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Trad_Chinese.csv") 

zh_visit_long <- zh_visit %>%
  dplyr::select(date, `Taiwan_/search`, `Taiwan_/zh/search`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`Taiwan_/search`, `Taiwan_/zh/search`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()  %>%
  mutate(Language = factor(Language, levels = c("Taiwan_/search", "Taiwan_/zh/search"),
                           labels = c("English", "Non-English language")))

zh_plot <- ggplot(zh_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.381, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "", fill = "Language", title = "Traditional Chinese") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Spanish ##
es_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Spanish.csv") 

es_visit_long <- es_visit %>%
  dplyr::select(date, `/search_total`, `/es/search_total`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`/search_total`, `/es/search_total`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("/search_total", "/es/search_total"),
                           labels = c("English", "Non-English language")))

es_plot <- ggplot(es_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.570, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "Proportion", fill = "Language", title = "Spanish") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Simplified Chinese ##
simp_zh <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Simp_Chinese.csv") 

simp_zh_long <- simp_zh %>%
  dplyr::select(date, `China_/search`, `China_/zh/search`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`China_/search`, `China_/zh/search`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("China_/search", "China_/zh/search"),
                           labels = c("English", "Non-English language")))

simp_zh_plot <- ggplot(simp_zh_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.201, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Date", y = "", fill = "Language", title = "Simplified Chinese") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Russian ##
ru_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Russian.csv") 

ru_visit_long <- ru_visit %>%
  dplyr::select(date, `/search`, `/ru/search`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`/search`, `/ru/search`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("/search", "/ru/search"),
                           labels = c("English", "Non-English language")))

ru_plot <- ggplot(ru_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.289, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Date", y = "Proportion", fill = "Language", title = "Russian") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Portuguese ##
pt_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Portuguese.csv") 

pt_visit_long <- pt_visit %>%
  dplyr::select(date, `/search_total`, `/pt/search_total`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`/search_total`, `/pt/search_total`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("/search_total", "/pt/search_total"),
                           labels = c("English", "Non-English language")))

pt_plot <- ggplot(pt_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.208, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "", fill = "Language", title = "Portuguese") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## France ##
fr_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/France.csv") 

fr_visit_long <- fr_visit %>%
  dplyr::select(date, `total_/search`, `total_/fr/search`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`total_/search`, `total_/fr/search`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("total_/search", "total_/fr/search"),
                           labels = c("English", "Non-English language")))

fr_plot <- ggplot(fr_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.471, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "", fill = "Language", title = "French") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Arabic ##
ar_visit <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/Arabic.csv") 

ar_visit_long <- ar_visit %>%
  dplyr::select(date, `total_/search`, `total_/ar/search`) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  pivot_longer(cols = c(`total_/search`, `total_/ar/search`), names_to = "Language", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup() %>%
  mutate(Language = factor(Language, levels = c("total_/search", "total_/ar/search"),
                           labels = c("English", "Non-English language")))

ar_plot <- ggplot(ar_visit_long, aes(x = date, y = prop, fill = Language)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0.108, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("English" = "grey60", "Non-English language" = "#FF7F00")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Date", y = "", fill = "Language", title = "Arabic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Combining Plots ##
(es_plot | jp_plot | fr_plot ) /
  (uk_plot | zh_plot | pt_plot ) /
  (ru_plot | simp_zh_plot | ar_plot) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title.align = 0.5)

#### BACI - Visitors ####
lang_dat <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/All_Langs.csv")
release_dates <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/ImplementationDates.csv")

lang_dat_long <- lang_dat %>%
  pivot_longer(cols = -date, names_to = "language", values_to = "access") %>%
  mutate(date = dmy(date))

release_dates <- release_dates %>%
  mutate(Release_Date = dmy(Release_Date))

# Target language 
target_lang <- "Ukrainian" # Change depending on language assessing
comparison_lang <- "English" # Change depending on comparison language
  
years_before <- 1
years_after <- 4 # Change depend on time period assessing

release_date <- release_dates %>%
  filter(language == target_lang) %>%
  pull(Release_Date) %>%
  floor_date("month")

# Pre-Period Trend
lang_subset_pre <- lang_dat_long %>%
  filter(language %in% c(target_lang, comparison_lang)) %>% 
  mutate(
    language = factor(language, levels = c(comparison_lang, target_lang)),
    months = interval(release_date, date) %/% months(1)
  ) %>%
  filter(date >= (release_date - years(years_before)) & date < release_date)

pretrend_model_nb <- glm.nb(access ~ months * language, data = lang_subset_pre)
summary(pretrend_model_nb)

# BACI Analysis
lang_subset <- lang_dat_long %>%
  filter(language %in% c(target_lang, comparison_lang)) %>%
  mutate(
    BA = ifelse(date < release_date, 0, 1),  # 0 = before, 1 = after
    CI = ifelse(language == target_lang, 1, 0),  # 1 = treatment
    BA = factor(BA),
    CI = factor(CI)) %>%
  arrange(date) %>%
  filter(date >= (release_date - years(years_before)) &
           date <= (release_date + years(years_after))) %>%
  group_by(language) %>%
  mutate(
    time_index = row_number(),  
    first_after_index = min(time_index[date >= release_date]),
    Time = time_index - first_after_index) %>%
  ungroup()

baci_trend_model <- glm.nb(access ~ Time + BA + CI +
                             BA:CI + BA:Time + CI:Time + BA:CI:Time,
                           data = lang_subset)
summary(baci_trend_model)

# Family-Wise Error Rate
pvals <- c(0.964, 0.803, 0.948, 0.506) # Update p-values depending on language assessing with the p values from each time period assessed
fdr <- p.fdr(pvalues = pvals, adjust.method = "BH")
summary(fdr)
fdr$`Results Matrix`

#### BACI - Downloads ####
dl_dat <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/GBIF_downloads.csv")

dl_dat <- dl_dat %>%
  group_by(date, language) %>%
  summarise(totalDownloads = sum(totalDownloads, na.rm = TRUE), .groups = "drop") %>%
  complete(date, language, fill = list(totalDownloads = 0)) %>%
  mutate(date = dmy(date))

# Target language 
target_lang <- "Russian" # Change depending on language assessing
comparison_lang <- "English" # Change depending on comparison language

years_before <- 1
years_after <- 4 # Change depend on time period assessing

release_date <- release_dates %>%
  filter(language == target_lang) %>%
  pull(Release_Date) %>%
  floor_date("month")

# Pre-Period Trend
dl_subset_pre <- dl_dat %>%
  filter(language %in% c(target_lang, comparison_lang)) %>%
  mutate(
    language = factor(language, levels = c(comparison_lang, target_lang)),
    months = interval(release_date, date) %/% months(1)
  ) %>%
  filter(date >= (release_date - years(years_before)) & date < release_date)

pretrend_model_nb <- glm.nb(totalDownloads ~ months * language, data = dl_subset_pre)

summary(pretrend_model_nb)

# BACI Analysis
lang_subset <- dl_dat %>%
  filter(language %in% c(target_lang, comparison_lang)) %>%
  mutate(
    BA = ifelse(date < release_date, 0, 1),  # 0 = before, 1 = after
    CI = ifelse(language == target_lang, 1, 0),  # 1 = treatment
    BA = factor(BA),
    CI = factor(CI)) %>%
  arrange(date) %>%
  filter(date >= (release_date - years(years_before)) &
           date <= (release_date + years(years_after))) %>%
  group_by(language) %>%
  mutate(
    time_index = row_number(), 
    first_after_index = min(time_index[date >= release_date]),
    Time = time_index - first_after_index) %>%
  ungroup()

dl_baci_trend_model <- glm.nb(totalDownloads ~ Time + BA + CI +
                                BA:CI + BA:Time + CI:Time + BA:CI:Time,
                              data = lang_subset)
summary(dl_baci_trend_model)

# Family-Wise Error Rate
pvals <- c(0.964, 0.803, 0.948, 0.506) # Update p-values depending on language assessing with the p values from each time period assessed
fdr <- p.fdr(pvalues = pvals, adjust.method = "BH")
summary(fdr)
fdr$`Results Matrix`

#### BACI Download Figures  #### 
# Russian 
lang_dat_long <- lang_dat_long %>%
  mutate(date = as.Date(date))

date_range <- range(lang_subset$date)

newdata <- expand_grid(
  language = c("English", "Russian"),
  date = seq(date_range[1], date_range[2], by = "month")) %>%
  mutate(
    BA = factor(ifelse(date < release_date, 0, 1), levels = c(0, 1)),
    CI = factor(ifelse(language == target_lang, 1, 0), levels = c(0, 1))) %>%
  group_by(language) %>%
  arrange(date) %>%
  mutate(
    time_index = row_number(),
    first_after_index = min(time_index[date >= release_date]),
    Time = time_index - first_after_index) %>%
  ungroup()

newdata$fit <- predict(dl_baci_trend_model, newdata = newdata, type = "response")

ggplot() +
  geom_line(data = lang_subset, aes(x = date, y = totalDownloads, color = language, group = language), alpha = 0.3) +
  geom_line(data = newdata, aes(x = date, y = fit, color = language), size = 1.2) +
  geom_vline(xintercept = as.numeric(release_date), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("English" = "grey60", "Russian" = "#FF7F00")) +
  scale_y_log10(
    labels = scales::label_comma(),
    breaks = c(5, 15, 50,150, 500, 1500, 5000, 15000, 50000)) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y") +
  labs(
    x = "Date",
    y = "Total Monthly GBIF Downloads",
    color = "Language") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -45, hjust = 0),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none") +
  annotate("text", x = as.Date("2020-06-01"), y = 7500, label = "English", 
           color = "grey60", size = 5, fontface = "bold") +
  annotate("text", x = as.Date("2020-06-01"), y = 800, label = "Russian", 
           color = "#FF7F00", size = 5, fontface = "bold")

# Ukrainian
dl_dat <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/GBIF_downloads.csv")
release_dates <- read_csv("https://raw.githubusercontent.com/KHannah12/WebsiteTranslation/main/ImplementationDates.csv")

dl_dat <- dl_dat %>%
  group_by(date, language) %>%
  summarise(totalDownloads = sum(totalDownloads, na.rm = TRUE), .groups = "drop") %>%
  complete(date, language, fill = list(totalDownloads = 0)) %>%
  mutate(date = dmy(date))

release_dates <- release_dates %>%
  mutate(Release_Date = dmy(Release_Date))

target_lang <- "Ukrainian"
comparison_lang <- "Polish"
years_before <- 1
years_after <- 1

release_date <- release_dates %>%
  filter(language == target_lang) %>%
  pull(Release_Date) %>%
  floor_date("month")

full_subset <- dl_dat %>%
  filter(language %in% c(target_lang, comparison_lang)) %>%
  mutate(language = factor(language, levels = c(comparison_lang, target_lang))) %>%
  filter(date >= (release_date - years(1)) & date <= (release_date + years(4)))

lang_subset <- dl_dat %>%
  filter(language %in% c(target_lang, comparison_lang)) %>%
  mutate(
    period = ifelse(date < release_date, "before", "after"),
    period = factor(period, levels = c("before", "after")),
    language = factor(language, levels = c(comparison_lang, target_lang)),
    months = interval(floor_date(release_date, "month"), date) %/% months(1)) %>%
  filter(date >= (release_date - years(years_before)) & date <= (release_date + years(years_after)))

pre_trend_lines <- lang_subset %>%
  filter(date < release_date) %>%
  group_by(language) %>%
  nest() %>%
  mutate(model = map(data, ~lm(totalDownloads ~ months, data = .)))

post_trend_lines <- lang_subset %>%
  filter(date >= release_date & date <= (release_date + years(1))) %>%
  group_by(language) %>%
  nest() %>%
  mutate(model = map(data, ~lm(totalDownloads ~ months, data = .)))

pre_line_df <- pre_trend_lines %>%
  mutate(aug = map(model, augment)) %>%
  unnest(aug) %>%
  mutate(period = "before")

post_line_df <- post_trend_lines %>%
  mutate(aug = map(model, augment)) %>%
  unnest(aug) %>%
  mutate(period = "after")

line_df <- bind_rows(pre_line_df, post_line_df) %>%
  mutate(date = release_date + months(months)) %>%
  dplyr::select(date, access = .fitted, language, period)

ggplot() +
  geom_line(data = full_subset, aes(x = date, y = totalDownloads, color = language, group = language), alpha = 0.3) +
  geom_vline(xintercept = as.numeric(release_date), linetype = "dashed", color = "black") +
  geom_line(data = line_df, aes(x = date, y = access, color = language), size = 1) +
  labs(
    title = "",
    x = "Date", y = "Total Monthly GBIF Downloads") +
  scale_x_date(
    limits = as.Date(c("2019-09-01", "2024-08-31")),
    date_breaks = "3 months",
    date_labels = "%b %Y" ) +
  scale_y_log10(
    limits = c(1, 100000),
    breaks = c(1, 5, 25, 100, 500, 2000, 20000, 100000),
    labels = scales::label_comma()) +
  scale_color_manual(
    values = c("Polish" = "grey60", "Ukrainian" = "red"),
    labels = c("Polish" = "Polish", "Ukrainian" = "Ukrainian")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = -45, hjust = 0),
    legend.position = "none") + 
  annotate("text", x = as.Date("2021-06-01"), y = 12, label = "Ukrainian",
           color = "red", size = 6, fontface = "bold") +
  annotate("text", x = as.Date("2021-06-01"), y = 170, label = "Polish",
           color = "grey60", size = 6, fontface = "bold")
