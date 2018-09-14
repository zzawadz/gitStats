gstat_rename_authors <- function(data, authorMap) {
  authorMapFrame <- data_frame(Author = names(authorMap), to = authorMap)

  data2 <- left_join(data, authorMapFrame)
  data2 <- data2 %>% mutate(Author = ifelse(is.na(to), Author, to)) %>% select(-to)
}
