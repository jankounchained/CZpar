jmena <- psp_tidy %>%
  select(speaker) %>%
  mutate(speaker = word(speaker,-1)) %>%
  group_by(speaker) %>%
  summarise(n = n()) %>%
  mutate(speaker = str_remove_all(speaker, "[:punct:]"),
         speaker = tolower(speaker),
         speaker = trimws(speaker),
         nchar = nchar(speaker)) %>%
  mutate(root_stop = round(0.66 * nchar),
         speaker_s = substr(speaker, 1, root_stop))

surname_root <- paste(jmena$speaker_s, collapse = "|")

x2 <- x %>%
  select(doc_id, token, lemma) %>%
  mutate(znam = str_detect(token, surname_root),
         jmeno = ifelse(token %in% p2$prijmeni_s, p2$prijmeni, lemma ))