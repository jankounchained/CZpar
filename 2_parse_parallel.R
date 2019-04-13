#################
# PARSE PSP HTML FILES - PARALLEL
#################

library(tidyverse)
library(furrr)
library(rvest)
library(zoo)
library(tictoc)
library(naniar)

file_list <- list.files(path = "data/2017ps/", pattern = "s", full.names = T)

###
# PARSING FUNCTION - SETUP A
###

parse_stenoskript <- function(html_doc) {
  
  link <- read_html(html_doc)
  
  # IMPORT DATA
  raw_xml <- link %>%
    html_nodes("p") %>%
    as.character() %>%
    as_tibble() %>%
    rename(xml = value)
  
  text <- link %>%
    html_nodes("p") %>%
    html_text() %>%
    as_tibble()
  
  doc <- link %>%
    html_node("title") %>%
    html_text() %>%
    as_tibble() %>%
    rename(doc = value)
  
  # EXTRACT INFORMATION
  prep1 <- cbind.data.frame(raw_xml, text, doc) %>%
    rename(text = value) %>%
    # extracting speaker
    mutate(speaker = str_extract(xml, "<a.*?</a>"),
           speaker = str_extract(speaker, ">.*?<"),
           speaker = str_remove_all(speaker, "<|>"),
           speaker_index = if_else(is.na(speaker), 0, 1)) %>%
    # extracting meeting point
    mutate(bod = ifelse(str_detect(xml, 'align="center'), text, NA),
           bod = ifelse(bod == " ", NA, bod),
           bod_index = if_else(is.na(bod), 0, 1)) %>%
    # extractking scenic notes
    mutate(com = str_extract_all(text, "\\((.*?)\\)"),
           com = paste0(com),
           com = ifelse(com == "character(0)", NA, com)) %>%
    # extract meeting time
    mutate(hhmm = ifelse(str_detect(com, "hodin"), 
                         str_extract(com, "\\d\\d\\.\\d\\d|\\d.\\d\\d"), NA))
  
  # EXTRACT LIST OF SPEAKERS
  # possible tweak: if speaker_index is 0, don't run these lines
  list_of_speakers <- enframe(table(prep1$speaker))$name
  list_of_speakers <- paste0(list_of_speakers, ":")
  
  
  # CLEANING INFORMATION
  prep2 <- prep1 %>%
    # clean the astrisks 
    mutate(text = str_remove(text, "\\*\\*\\*")) %>%
    # erasing empty rows (whitespace or nothing)
    mutate(text = trimws(text, which = "both"),
           text = ifelse(nchar(text) <= 1, NA, text)) %>%
    filter(!is.na(text)) %>%
    # erasing comments from text
    mutate(text = str_remove_all(text, "\\((.*?)\\)")) %>%
    # easting speaker name from text
    mutate(text = ifelse(speaker_index == 1, 
                         str_remove(text, paste(list_of_speakers, collapse = "|")), 
                         text))
  
  
  # GET READY FOR MERGING
  prep3 <- prep2 %>%
    # filling NAs for speaker
    mutate(speaker = na.locf(speaker, na.rm = F)) %>%
    # filling NAs for bod
    mutate(bod = na.locf(bod, na.rm = F)) %>%
    # correct variables
    mutate(xml = as.character(xml),
           text = as.character(text),
           doc = as.character(doc),
           speaker = as.character(speaker),
           speaker_index = as.numeric(speaker_index),
           bod = as.character(bod),
           bod_index = as.character(bod_index),
           com = as.character(com),
           hhmm = as.character(hhmm))
  
  return(prep3)
}



###
# LOAD DATA AND SETUP PARALLEL
###

plan(multiprocess)



###
# RUN
###

tictoc::tic()
pspr <- future_map_dfr(file_list, parse_stenoskript, .progress = TRUE)
tictoc::toc()


###
# TIDY
###

pspr <- pspr %>%
  # filling NAs for speaker
  mutate(speaker = na.locf(speaker, na.rm = F)) %>%
  # filling NAs for bod
  mutate(bod = na.locf(bod, na.rm = F)) %>%
  # filter bod rows
  filter(bod_index == 0) %>%
  # get rid of xml
  select(-xml) %>%
  # handling nas
  replace_with_na(replace = list(text = "")) %>%
  mutate(text = ifelse(is.na(text) & !is.na(hhmm) |
                         is.na(text) & !is.na(com), 0, text))

psp_tidy <- pspr %>%
  mutate(text_c = str_replace_all(text, "§", "paragraf"),
         text_c = str_remove_all(text_c, "[:punct:]"),
         text_c = tolower(text_c),
         text_c = trimws(text_c)) %>%
  rownames_to_column() 


###
# EXPORT
###
write_csv(psp_tidy, "data/csv/psp_tidy.csv")
