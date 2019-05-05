parse_stenoskript_2010 <- function(html_doc) {
  
  link <- read_html(html_doc)
  
  # IMPORT DATA
  raw_xml <- link %>%
    html_nodes("p") %>%
    as.character() %>%
    enframe(name = NULL) %>%
    rename(xml = value)
  
  text <- link %>%
    html_nodes("p") %>%
    html_text() %>%
    enframe(name = NULL) %>%
    rename(text = value)
  
  doc <- link %>%
    html_node("title") %>%
    html_text() %>%
    enframe(name = NULL) %>%
    rename(doc = value)
  
  # EXTRACT INFORMATION
  prep1 <- cbind.data.frame(raw_xml, text, doc) %>%
    # extracting speaker
    mutate(speaker = str_extract(xml, "<a.*?</a>"),
           speaker = str_extract(speaker, ">.*?<"),
           speaker = str_remove_all(speaker, "<|>"),
           speaker_index = if_else(is.na(speaker), 0, 1)) %>%
    # extract <b>speaker name</b> (psp2010 specific)
    mutate(speaker_b = ifelse(speaker_index == 1, str_extract(xml, "<a.*?</a>"), "nic"),
           speaker_b = str_extract(speaker_b, "b>.*?</b"),
           speaker_b = str_remove_all(speaker_b, "b>|</b")) %>%
    # merge speaker records
    mutate(speaker = ifelse(speaker == "" & speaker_index == 1, speaker_b, speaker)) %>%
    select(-speaker_b) %>%
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

s2010 <- list.files(path = "data/2010ps/", pattern = "s", full.names = T)
psp2010r <- map_df(s2010, parse_stenoskript_2010)
write_csv(psp2010r, "psp2010_rawish.csv")

# a_speaker <- psp2010r %>%
#   # extract <b>speaker name</b> (psp2010 specific)
#   mutate(speaker_b = ifelse(speaker_index == 1, str_extract(xml, "<a.*?</a>"), "nic"),
#          speaker_b = str_extract(speaker_b, "b>.*?</b"),
#          speaker_b = str_remove_all(speaker_b, "b>|</b")
#         ) %>%
#   filter(str_detect(text, "Prezident České republiky Václav Klaus"))
#   
# a_speaker$speaker_b[1]
# 
# a_slice <- psp2010r %>%
#   slice(2158)
# 
# a_slice$speaker[1]
# 
# na_speaker <- psp2010r %>%
#   filter(is.na(speaker))
