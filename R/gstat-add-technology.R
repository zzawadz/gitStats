gstat_add_technology <- function(data) {
  technology <- list(R = c("R", "r", "Rmd", "rmd"), "C/C++" = c("c", "cpp", "h", "hpp"), "Doc" = c("bib"))
  technologyMap <- mapply(function(x, y) {cbind(Technology = x, Ext = y)}, names(technology), technology)
  technologyMap <- as.data.frame(Reduce(rbind,technologyMap), stringsAsFactors = FALSE)
  byTechnology <- left_join(data, technologyMap)
  byTechnology <- byTechnology %>%
    mutate(Technology = ifelse(is.na(Technology), Ext, Technology))
  byTechnology
}
