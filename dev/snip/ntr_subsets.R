### novelty - transience - resonance loop
### split by year -> count n/t/r for subsets

subset_year_new <- function(year, sub_range) {
  
  match <- str_c(
    paste0(
      as.character(year), "...., ", sub_range, "\\. ", "schůze"), 
    collapse = "|")
  
  return(match)
  
}

fli <- read_csv("data/csv/full_lemma_in.csv")

# 2010: 1-19, 20-40, 41-59
# 2013: 1-22, 23-38, 39-58
# 2017: 1-23, 24-29

ex101 <- c(paste0(0, 0, 1:9, "schuz"), paste0(0, 10:19, "schuz"))
ex101 <- str_c(ex101, collapse = "|")
s_101 <- fli %>%
  filter(str_detect(doc, ex101))
  
ex102 <- str_c(paste0(0, 20:40, "schuz"), collapse = "|")
s_102 <- fli %>%
  filter(str_detect(doc, ex102))

ex103 <- str_c(paste0(0, 41:59, "schuz"), collapse = "|")
s_103 <- fli %>%
  filter(str_detect(doc, ex103))


# psp2013
s_131 <- fli %>%
  filter(str_detect(doc, subset_year_new("2013", 1:22)))

s_132 <- fli %>%
  filter(str_detect(doc, subset_year_new("2013", 23:38)))

s_133 <- fli %>%
  filter(str_detect(doc, subset_year_new("2013", 39:58)))


# psp2017
s_171 <- fli %>%
  filter(str_detect(doc, subset_year_new("2017", 1:23)))

s_172 <- fli %>%
  filter(str_detect(doc, subset_year_new("2017", 24:29)))


# get all variables and write_csv
subsets <- grep("s_.*?", names(.GlobalEnv), value = T)

for (i in 1:8) {
  
  write_csv(get(subsets[i]), path = paste0("data/ntr/", subsets[i], ".csv"))
  
}
