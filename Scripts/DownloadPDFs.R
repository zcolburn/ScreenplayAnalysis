# Clear workspace
rm(list=ls())

# Load %>% from dplyr
library(dplyr)

# Read in screenplays.txt from the Memory folder
ref_tbl <- readr::read_delim(
  file.path("Memory","screenplays.txt"),
  delim = "\t"
)

# Download PDFs
lapply(
  split(ref_tbl,1:nrow(ref_tbl)),
  function(item){
    download.file(
      url = item$URL[1],
      destfile = item$path,
      mode = "wb"
    )
  }
)
