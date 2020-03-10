########################################################################################
# 
# Analysis of Swiss news broadcasts
# David Zumbach, 20.01.2020, Update: 10.03.2020
#
########################################################################################

# Load Packages ------------------------------------------------------------------------

# Packages
pacman::p_load(dplyr, readxl, quanteda, tsibble, tidyr, tidytext, ggplot2, hrbrthemes,
               tibble, tokenizers)

# Helper functions
source("funs.R")

# Extract Subtitles --------------------------------------------------------------------

# Load Subtitle-URLs
srf <- readxl::read_excel("data/subtitles.xlsx", na = "NA", sheet = "srf")
rts <- readxl::read_excel("data/subtitles.xlsx", na = "NA", sheet = "rts")
rsi <- readxl::read_excel("data/subtitles.xlsx", na = "NA", sheet = "rsi")

# Extract Subtitles
srf$ut <- unlist(purrr::map(srf$url_ut, get_subtitle_SRF))
rts$ut <- unlist(purrr::map(rts$url_ut, get_subtitle_RTS))
rsi$ut <- unlist(purrr::map(rsi$url_ut, get_subtitle_RSI))

# Document-Feature Matrices ------------------------------------------------------------

# SRF
srf_d <- quanteda::corpus(
  srf$ut, 
  docnames = paste0(srf$datum, srf$sendung), 
  docvars = srf %>% select(-ut)
  ) %>%
  quanteda::dfm(
    tolower = FALSE,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove = quanteda::stopwords(language = "german"),
    stem = FALSE
  ) 

# RTS
rts_d <- quanteda::corpus(
  rts$ut, 
  docnames = paste0(rts$datum, rts$sendung), 
  docvars = rts %>% select(-ut)
  ) %>%
  quanteda::dfm(
    tolower = FALSE,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove = quanteda::stopwords(language = "french"),
    stem = FALSE
  ) 

# RSI
rsi_d <- quanteda::corpus(
  rsi$ut, 
  docnames = paste0(rsi$datum, rsi$sendung), 
  docvars = rsi %>% select(-ut)
  ) %>%
  quanteda::dfm(
    tolower = FALSE,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove = quanteda::stopwords(language = "italian"),
    stem = FALSE
  ) 

# Word Frequencies ---------------------------------------------------------------------

dt <- bind_rows(
  left_join(
    textstat_frequency(srf_d, groups = docvars(srf_d, "datum")) %>% 
      filter(stringr::str_detect(feature, "Corona|Virus")) %>% 
      group_by(group) %>% 
      summarise(Nennungen = sum(frequency)) %>% 
      mutate(Datum = as.Date(group)) %>% 
      select(-group) %>% 
      complete(Datum = seq.Date(as.Date("2020-01-01"), Sys.Date()-1, 1), fill = list(Nennungen = 0)) %>% 
      mutate(Sender = "SRF"),
    tibble(
      Sendung = names(ntoken(srf_d)),
      Wörter = ntoken(srf_d)
      ) %>% 
      mutate(Datum = stringr::str_extract(Sendung, "2020-[0-9]{2}-[0-9]{2}")) %>% 
      group_by(Datum) %>% 
      summarise(Wörter = sum(Wörter)) %>% 
      mutate(Datum = as.Date(Datum)),
    by = "Datum"
  ),
  left_join(
    textstat_frequency(rts_d, groups = docvars(rts_d, "datum")) %>% 
      filter(stringr::str_detect(feature, "[C|c]orona|[V|v]irus")) %>% 
      group_by(group) %>% 
      summarise(Nennungen = sum(frequency)) %>% 
      mutate(Datum = as.Date(group)) %>% 
      select(-group) %>% 
      complete(Datum = seq.Date(as.Date("2020-01-01"), Sys.Date()-1, 1), fill = list(Nennungen = 0)) %>% 
      mutate(Sender = "RTS"),
    tibble(
      Sendung = names(ntoken(rts_d)),
      Wörter = ntoken(rts_d)
    ) %>% 
      mutate(Datum = stringr::str_extract(Sendung, "2020-[0-9]{2}-[0-9]{2}")) %>% 
      group_by(Datum) %>% 
      summarise(Wörter = sum(Wörter)) %>% 
      mutate(Datum = as.Date(Datum)),
    by = "Datum"
  ),
  left_join(
    textstat_frequency(rsi_d, groups = docvars(rsi_d, "datum")) %>% 
      filter(stringr::str_detect(feature, "[C|c]orona|[V|v]irus")) %>% 
      group_by(group) %>% 
      summarise(Nennungen = sum(frequency)) %>% 
      mutate(Datum = as.Date(group)) %>% 
      select(-group) %>% 
      complete(Datum = seq.Date(as.Date("2020-01-01"), Sys.Date()-1, 1), fill = list(Nennungen = 0)) %>% 
      mutate(Sender = "RSI"),
    tibble(
      Sendung = names(ntoken(rsi_d)),
      Wörter = ntoken(rsi_d)
    ) %>% 
      mutate(Datum = stringr::str_extract(Sendung, "2020-[0-9]{2}-[0-9]{2}")) %>% 
      group_by(Datum) %>% 
      summarise(Wörter = sum(Wörter)) %>% 
      mutate(Datum = as.Date(Datum)),
    by = "Datum"
  )
)

# DataViz ------------------------------------------------------------------------------

dt %>%  
  tsibble::as_tsibble(key = Sender, index = Datum) %>% 
  index_by(Woche = yearweek(Datum)) %>% 
  group_by(Woche, Sender) %>% 
  summarise(
    Nennungen = sum(Nennungen),
    Wörter = sum(Wörter)
  ) %>% 
  ungroup() %>% 
  mutate(Rel_Freq = 1000 * Nennungen / Wörter) %>%
  mutate(Sender = factor(case_when(
    Sender == "SRF" ~ 1,
    Sender == "RTS" ~ 2,
    Sender == "RSI" ~ 3
    ), labels = c("SRF (German)", "RTS (French)", "RSI (Italian)"))) %>% 
  ggplot(aes(x = Woche, y = Rel_Freq, color = Sender)) +
  geom_line(size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = c("%W")) +
  scale_y_continuous(limits = c(0, 15)) +
  theme_ft_rc() +
  scale_color_manual(values = c(ft_cols$yellow, ft_cols$blue, ft_cols$white)) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.justification = "top",
    legend.title = element_blank()
    )  +
  labs(
    title = "COVID-19 in Swiss TV News", 
    subtitle = "Weekly Cumulative Frequency of 'Corona' and 'Virus' by Swiss Television Station",
    x = "Week", y = "Frequency per 1000 Words"
    ) 

ggsave("covid_freq.png", dpi = 1000, width = 8, height = 4.5)

# Associations ------------------------------------------------------------------------

# SRF
srf_assoc <- tibble::tibble(
  date = srf$datum,
  sentence = tokenizers::tokenize_sentences(srf$ut)
  ) %>% 
  unnest(sentence) %>%  
  mutate(covid = stringr::str_detect(sentence, "[C|c]orona|[V|v]irus")) %>% 
  filter(!is.na(covid)) %>% 
  mutate(id = c(1:n())) %>% 
  tsibble::as_tsibble(key = id, index = date) %>% 
  mutate(week = as.character(yearweek(date))) %>% 
  as_tibble() 

srf_assoc_d <- quanteda::corpus(srf_assoc$sentence, docvars = srf_assoc %>% select(-sentence)) %>%
  quanteda::dfm(
    tolower = FALSE,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove = quanteda::stopwords(language = "german"),
    stem = FALSE
  ) 

# Helper for weekly comparison
get_weekly_keyness <- function(x, dfm, station) {
  
  a <- quanteda::dfm_subset(dfm, week == x) 
  
  if (sum(docvars(a, "covid")) > 0) {
    
    textstat_keyness(a, docvars(a, "covid") == T) %>% 
      filter(!stringr::str_detect(feature, "[C|c]orona|[V|v]irus")) %>% 
      slice(1:10) %>% 
      mutate(
        week = x,
        channel = station
      )
    
  } else {
    
    tibble::tibble(
      feature = NA,
      chi2 = NA,
      p = NA,
      n_target = NA,
      n_reference = NA,
      week = x,
      channel = station
      )
    
  }

}

# Get Keywords
srf_keywords <- purrr::map_dfr(
  unique(srf_assoc$week), 
  get_weekly_keyness, 
  dfm = srf_assoc_d, 
  station = "SRF"
  )

# RTS
rts_assoc <- tibble::tibble(
  date = rts$datum,
  sentence = tokenizers::tokenize_sentences(rts$ut)
  ) %>% 
  unnest(sentence) %>%  
  mutate(covid = stringr::str_detect(sentence, "[C|c]orona|[V|v]irus")) %>% 
  filter(!is.na(covid)) %>% 
  mutate(id = c(1:n())) %>% 
  tsibble::as_tsibble(key = id, index = date) %>% 
  mutate(week = as.character(yearweek(date))) %>% 
  as_tibble() 

rts_assoc_d <- quanteda::corpus(rts_assoc$sentence, docvars = rts_assoc %>% select(-sentence)) %>%
  quanteda::dfm(
    tolower = FALSE,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove = quanteda::stopwords(language = "german"),
    stem = FALSE
  ) 

# Get Keywords
rts_keywords <- purrr::map_dfr(
  unique(rts_assoc$week), 
  get_weekly_keyness, 
  dfm = rts_assoc_d, 
  station = "RTS"
)

# RSI
rsi_assoc <- tibble::tibble(
  date = rsi$datum,
  sentence = tokenizers::tokenize_sentences(rsi$ut)
) %>% 
  unnest(sentence) %>%  
  mutate(covid = stringr::str_detect(sentence, "[C|c]orona|[V|v]irus")) %>% 
  filter(!is.na(covid)) %>% 
  mutate(id = c(1:n())) %>% 
  tsibble::as_tsibble(key = id, index = date) %>% 
  mutate(week = as.character(yearweek(date))) %>% 
  as_tibble() 

rsi_assoc_d <- quanteda::corpus(rsi_assoc$sentence, docvars = rsi_assoc %>% select(-sentence)) %>%
  quanteda::dfm(
    tolower = FALSE,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove = quanteda::stopwords(language = "german"),
    stem = FALSE
  ) 

# Get Keywords
rsi_keywords <- purrr::map_dfr(
  unique(rsi_assoc$week), 
  get_weekly_keyness, 
  dfm = rsi_assoc_d, 
  station = "RSI"
)
