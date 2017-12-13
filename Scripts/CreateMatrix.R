# Clear workspace
rm(list=ls())

# Load %>% from dplyr
library(dplyr)

# Load corpus and valid_corpora
load(file.path("Memory","corpus.r"))
load(file.path("Memory","valid_corpora.r"))

# Read in screenplays.txt from the Memory folder and subset valid documents
ref_tbl <- readr::read_delim(
  file.path("Memory","screenplays.txt"),
  delim = "\t"
) %>%
{.[valid_corpora,]}

# Create a document-term matrix
dtm <- tm::DocumentTermMatrix(corpus)

# Save the full matrix
full_dtm <- as.matrix(dtm)
save(full_dtm, file = file.path("Memory","full_dtm.r"))

# Save top n-words per movie
n <- 5
top_words <- ref_tbl %>%
  dplyr::select(title) %>%
  dplyr::bind_cols(
    .,
  (lapply(
  split(full_dtm, 1:nrow(full_dtm)),
  function(current_row){
    names(current_row) <- colnames(full_dtm)
    sort(current_row, decreasing = TRUE) %>%
    {
      item <- .
      item <- item / sum(item)
      item[!(names(item) %in% c(
        "content","language","meta","list","dmeta","aback"
      ))]
    } %>%
      head(., n) %>%
      names() %>%
      tibble::as_tibble()
  }
) %>%
  dplyr::bind_cols() %>%
  t() %>%
  tibble::as_data_frame())) %>%
  dplyr::filter(# Movies with the following data result from an error in import
    .,
    (V1 != "abl") &
      (V2 != "abort") &
      (V3 != "abov") &
      (V4 != "absolut") &
      (V5 != "accept")
  )

readr::write_delim(
  top_words, 
  path = file.path("Memory","top_words.txt"), 
  delim = "\t"
)


# Save a matrix with sparse terms removed
dtm <- tm::removeSparseTerms(dtm, 0.5)# Filter sparse terms

dtm <- as.matrix(dtm)
dtm <- dtm[ref_tbl$title %in% top_words$title, , drop = FALSE]
rownames(dtm) <- top_words$title
dtm <- apply(dtm, 1, function(item){item/sum(item)}) %>% t()

# Save normalized frequencies
save(dtm, file = file.path("Memory","dtm.r"))
