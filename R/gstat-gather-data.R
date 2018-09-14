gstat_gather_data <- function(repoPath) {
  lines <- system(sprintf("cd %s && git log --pretty=fuller --numstat --date=iso", repoPath), TRUE)

  commits <- grep(lines, pattern = "^commit ")
  commits <- c(commits, length(lines) + 1)

  commitsList <- mapply(function(i, j) {
    lines[i:(j - 1)]
  }, head(commits, -1), tail(commits, -1))


  parse_commit <- function(cm) {
    author <- cm[grep(cm, pattern = "^Author: ")]
    author <- gsub(pattern = "(Author: +)|( <.*)", replacement = "", x = author)
    files <- cm[grep(cm, pattern = "[0-9]+\t[0-9]+\t")]
    date <- cm[grep(cm, pattern = "^CommitDate: ")]
    date <- stringi::stri_extract_first_regex(date, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}")

    if(length(files) == 0) return(NULL)
    res <- cbind(Date = date, Author = author, files)
    res
  }

  parsedCommitsList <- lapply(commitsList, parse_commit)
  commitsFrame <- as.data.frame(Reduce(rbind, parsedCommitsList), stringsAsFactors = FALSE)

  commitsFrame <- commitsFrame %>% separate(files, c("Added", "Removed", "File"), sep = "\t", remove = FALSE) %>%
    rename(FullPath = files) %>%
    mutate(File = basename(File), Ext = tools::file_ext(File)) %>%
    mutate(FullPath = gsub(pattern = "^[0-9]+\t[0-9]+\t", replacement = "", FullPath)) %>%
    mutate(Added = as.numeric(Added), Removed = as.numeric(Removed))

  commitsFrame
}
