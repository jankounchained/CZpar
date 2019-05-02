p <- read_delim("data/misc/poslanci/osoby.unl", 
                delim = "|", escape_double = T,
                col_names = c("id_osoba", "pred", "prijmeni", "jmeno", "za",
                              "narozeni", "pohlavi", "zmena", "umrti"),
                locale = locale(encoding = 'windows-1250'))

p2 <- p %>%
  select(id_osoba, prijmeni) %>%
  mutate(prijmeni = str_remove_all(prijmeni, "[:punct:]"),
         prijmeni = tolower(prijmeni),
         prijmeni = trimws(prijmeni),
         nchar = nchar(prijmeni)) %>%
  mutate(root_stop = round(0.66 * nchar),
         prijmeni_s = substr(prijmeni, 1, root_stop))

surname_root <- paste(p2$prijmeni_s, collapse = "|")

x2 <- x %>%
  select(doc_id, token, lemma) %>%
  mutate(znam = str_detect(token, surname_root),
         jmeno = ifelse(token %in% p2$prijmeni_s, p2$prijmeni, lemma ))
