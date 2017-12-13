# Clear workspace
rm(list=ls())

# Load %>% from dplyr
library(dplyr)

# Read in screenplays.txt from the Memory folder
ref_tbl <- readr::read_delim(
  file.path("Memory","screenplays.txt"),
  delim = "\t"
)

corpora <- parallel::parLapply(
  parallel::makeCluster(parallel::detectCores()),
  split(ref_tbl,1:nrow(ref_tbl)),
  function(movie_row){
    # Load %>% from dplyr
    library(dplyr)
    
    output <- NULL
    try(
      {
        output <- pdftools::pdf_text(
          movie_row$path
        ) %>%
          .[-1] %>% # Remove page 1 (the title page)
          paste(., collapse = " ") %>% # Merge all pages into one element
          gsub("\\r\\n","",.) %>% # Remove carriage returns
          gsub("[[:punct:]]","",.) %>% # Remove punctuation
          gsub("[[:digit:]]","",.) %>% # Remove digits
          gsub("[A-Z]{2,}","",.) %>% # Remove things in all caps, but not 
          # capitalized words
          gsub("\\s+"," ",.) %>% # Remove extra white space
          tolower() %>% # Convert to lower case
          {
            item <- .
            tm::Corpus(tm::VectorSource(item)) # Create the corpus
          } %>%
          tm::tm_map(., tm::stemDocument) %>% # Stem words
          tm::tm_map(., tm::removeWords, tm::stopwords("en"))
      }
    )
    return(output)
  }
)

# Identify valid corpora
valid_corpora <- lapply(
  corpora,
  length
) %>%
  unlist() %>%
  {. == 1} %>%
  which()

# Subset valid corpora
corpora <- corpora[valid_corpora]

# Log progress
write(
  sprintf(
    "ExtractFromPDFs--Number of valid documents: %d",
    length(valid_corpora)
  ),
  file.path("Logs","log.txt"),
  append = TRUE
)

# Merge corpora
corpus <- tm::Corpus(tm::VectorSource(corpora))

save(corpus, file = file.path("Memory","corpus.r"))
save(valid_corpora, file = file.path("Memory","valid_corpora.r"))
