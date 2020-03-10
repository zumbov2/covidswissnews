########################################################################################
# 
# Analysis of Swiss news broadcasts: Helper Functions
# David Zumbach, 20.01.2020, Update: 10.03.2020
#
########################################################################################

# Load Packages
pacman::p_load(httr, stringr, purrr, dplyr)

# Work horses
get_subtitle_wh_SRF <- function(x) {
  
  res <- httr::GET(x)
  
  if (httr::status_code(res) < 400) {
    
    httr::content(res) %>% 
      stringr::str_replace_all("\n\n[0-9]{1,}\n[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3} --> [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}\n", " ") %>% 
      stringr::str_remove_all("<font color.*?>") %>% 
      stringr::str_remove_all("</font>") %>% 
      stringr::str_replace_all("\n", " ") %>% 
      stringr::str_remove_all("\"") %>% 
      stringr::str_remove_all("WEBVTT") %>% 
      stringr::str_remove_all("1:1-Untertitelung.") %>% 
      stringr::str_remove_all("Wir untertiteln live:") %>% 
      stringr::str_remove_all("SWISS TXT AG / Access Services.*?") %>% 
      stringr::str_squish() %>% 
      stringr::str_remove_all("^.") %>% 
      stringr::str_replace_all("([a-zäöüéè]{1})-( |)(?!(und)|(oder)|(als)|[A-ZÄÖÜ])", "\\1") %>% 
      stringr::str_squish() %>% 
      stringr::str_remove_all("^Livepassagen.*?\\.") %>% 
      stringr::str_squish()
    
    }
  
}
get_subtitle_wh_RTS_old <- function(x) {
  
  res <- httr::GET(x)
  
  if (httr::status_code(res) < 400) {
    
    httr::content(res) %>% 
      stringr::str_replace_all("\n\n[0-9]{1,}\n[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3} --> [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}\n", " ") %>% 
      stringr::str_replace_all("\n", " ") %>% 
      stringr::str_remove_all("\"") %>% 
      stringr::str_remove_all("WEBVTT") %>% 
      stringr::str_replace_all("([\\.\\?\\!]{1}) -", "\\1 ") %>% 
      stringr::str_remove_all("Sous-titrage SWISS TXT.*?") %>% 
      stringr::str_squish() %>% 
      stringr::str_remove_all("^... ")
    
  }
  
}
get_subtitle_wh_RTS_new <- function(x) {
  
  res <- httr::GET(x)
  
  if (httr::status_code(res) < 400) {
    
    res <- httr::content(res, encoding = "UTF-8") %>%
      stringr::str_split("[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3} --> [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}") %>% 
      unlist() %>% 
      stringr::str_replace_all("\n", " ") %>% 
      stringr::str_replace_all("\r", " ") %>% 
      stringr::str_remove_all("\"") %>% 
      stringr::str_remove_all("Sous-titrage SWISS TXT.*?") %>% 
      stringr::str_squish() %>% 
      stringr::str_remove_all("^... ")
    
    res <- res[!res == "..."]
    res <- res[!stringr::str_detect(res, "WEBVTT X-TIMESTAMP-MAP=LOCAL")]
    
    return(res)
    
  }
  
}
get_subtitle_wh_RSI <- function(x) {
  
  res <- httr::GET(x)
  
  if (httr::status_code(res) < 400) {
    
    res <- suppressWarnings(readLines(x, encoding = "UTF-8"))
    
    res <- res %>% 
      stringr::str_extract_all(">(.*?)</span>") %>% 
      unlist() %>% 
      stringr::str_remove_all("^>") %>% 
      stringr::str_remove_all("</span>")
    
    res <- res[!res == "character(0)"] %>% 
      stringr::str_replace_all("&#39;", "'") %>% 
      stringr::str_remove_all("&#34;")
    
    paste0(res[!is.null(res)], collapse = " ") %>% 
      stringr::str_remove_all("sottotitoli SWISS TXT.*") %>% 
      stringr::str_remove_all("Trasmissione sottotitolata con parti in respeaking") %>% 
      stringr::str_remove_all("TG [0-9]{2}:[0-9]{2}.*?20[0-9]{2}") %>% 
      stringr::str_squish() %>% 
      stringr::str_remove_all("^TELEGIORNALE.*?20[0-9]{2}") %>% 
      stringr::str_squish()

  } else {
  
    return(NA)
  }
}

# Wrapper functions
get_subtitle_SRF <- function(x, n = 10) {
  
  cat(x, "\n")
  
  # Missing
  if (is.na(x)) return(NA)
  
  # Base
  base <- stringr::str_remove_all(x, "[0-9]{1}\\.vtt")
  uts <- paste0(base, 1:n, ".vtt")
    
  # Abfrage
  ut <- unlist(purrr::map(uts, get_subtitle_wh_SRF))
  res <- paste0(ut[!is.null(ut)], collapse = " ")
  return(res)
  
}
get_subtitle_RTS <- function(x, n = 40) {
  
  cat(x, "\n")
  
  # Missing
  if (is.na(x)) return(NA)
  if (stringr::str_detect(x, "akamaihd")) {
    
    # Base
    base <- stringr::str_remove_all(x, "[0-9]{1,}_fr_sbtl.webvtt")
    uts <- paste0(base, 1:n, "_fr_sbtl.webvtt")
    
    # Abfrage
    ut <- unlist(purrr::map(uts, get_subtitle_wh_RTS_new))
    ut <- ut[!duplicated(ut)]
    res <- paste0(ut[!is.null(ut)], collapse = " ")
    res <- stringr::str_replace_all(res, "([\\.\\?\\!]{1}) -", "\\1 ")
      
    return(res)
    
    } else {
    
    # Abfrage
    res <- get_subtitle_wh_RTS_old(x)
    return(res)
    
  }
  
}
get_subtitle_RSI <- function(x) {
  
  cat(x, "\n")
  
  # Missing
  if (is.na(x)) return(NA)
  
  # Abfrage
  res <- get_subtitle_wh_RSI(x)
  return(res)
    
    
  }

