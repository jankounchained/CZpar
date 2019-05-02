lemmatize <- function(filename, udpipe_model) {
  
  part <- read_csv(paste0("data/to_udpipe/", filename))
  
  x <- udpipe_annotate(udpipe_model, x = part$text_c, doc_id = part$rowname)
  
  x <- as_tibble(x) %>%
    select(doc_id, token, lemma) 
  
  x <- x %>%
    mutate(doc_id = as.numeric(doc_id)) %>%
    filter(!is.na(lemma)) %>%
    group_by(doc_id) %>%
    summarise(text_c = str_c(lemma, collapse = " "))
  
  write_csv(x, path = paste0("data/from_udpipe/", filename))
  
}

