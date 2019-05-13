osoby <- read_delim("data/misc/osoby.unl", 
                delim = "|", escape_double = T,
                col_names = c("id_osoba", "pred", "prijmeni", "jmeno", "za",
                              "narozeni", "pohlavi", "zmena", "umrti"),
                locale = locale(encoding = 'windows-1250'))

osoba_extra <- read_delim("data/misc/osoba_extra.unl", 
                    delim = "|", escape_double = T,
                    col_names = c("id_osoba", "id_org", "typ", "obvod", "strana",
                                  "id_external"),
                    locale = locale(encoding = 'windows-1250'))

zarazeni <- read_delim("data/misc/zarazeni.unl", 
                    delim = "|", escape_double = T,
                    col_names = c("id_osoba", "id_of", "cl_funkce", "od_o", "do_o",
                                  "od_f", "do_f"),
                    locale = locale(encoding = 'windows-1250'))

poslanec <- read_delim("data/misc/poslanec.unl", 
                       delim = "|", escape_double = T,
                       col_names = c("id_poslanec", "id_osoba", "id_kraj", "id_kandidatka", 
                                     "id_obdobi", "web", "ulice", "obec", "psc", "email", 
                                     "telefon", "fax", "psp_telefon", "facebook", "foto"),
                       locale = locale(encoding = 'windows-1250'))

organy <- read_delim("data/misc/organy.unl", 
                       delim = "|", escape_double = T,
                       col_names = c("id_organ", "organ_id_organ", "id_typ_organu", 
                                     "zkratka", "nazev_organu_cz", "nazev_organu_en", 
                                     "od_organ", "do_organ", 
                                     "priorita", "cl_organ_base"),
                       locale = locale(encoding = 'windows-1250'))

funkce <- read_delim("data/misc/funkce.unl", 
                    delim = "|", escape_double = T,
                    col_names = c("id_funkce", "id_organ", "id_typ_funkce", 
                                  "nazev_funkce_cz", "priorita"),
                    locale = locale(encoding = 'windows-1250'))


obdobi_of_interest <- c("170", "171", "172")

p <- poslanec %>%
  filter(id_obdobi %in% obdobi_of_interest) %>%
  left_join(osoby, by = "id_osoba") %>%
  select(id_osoba, prijmeni, jmeno) %>%
  full_join(zarazeni, by = "id_osoba") %>%
  mutate(id_funkce = ifelse(cl_funkce == 1, id_of, NA),
         id_organ = ifelse(cl_funkce == 0, id_of, NA)) %>%
  left_join(organy, by = c("id_organ")) %>%
  filter(cl_funkce == 0)


p <- poslanec %>%
  filter(id_obdobi %in% obdobi_of_interest) %>%
  left_join(osoby, by = "id_osoba") %>%
  select(id_osoba, prijmeni, jmeno, id_kandidatka) %>%
  left_join(organy, by = c("id_kandidatka" = "id_organ")) %>%
  select(-organ_id_organ, -id_typ_organu, -nazev_organu_en,
         -od_organ, -do_organ, -priorita, -cl_organ_base)
