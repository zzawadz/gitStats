gstat_rename_authors <- function(data, authorMap) {
  authorMap <- c("zzawadzki" = "Zygmunt Zawadzki",
                 "zzawadz" = "Zygmunt Zawadzki",
                 "MarcinKosinski" = "Marcin Kosiński")
  authorMapFrame <- data_frame(Author = names(authorMap), to = authorMap)

  data2 <- left_join(data, authorMapFrame)
  data2 <- data2 %>% mutate(Author = ifelse(is.na(to), Author, to)) %>% select(-to)
}
