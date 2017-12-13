main_webpage <- "http://www.scriptreaderpro.com/best-screenplays-to-read/"

# Load relevant packages
library(dplyr)

# Retrieve main webpage
webpage <- xml2::read_html(main_webpage)

# Extract links to PDFs
pdf_links <- rvest::html_nodes(webpage, "a") %>%
  rvest::html_attr(., "href") %>%
  {
    links <- .
    links[grepl("pdf$", links)]
  }

# Extract titles
titles <- rvest::html_nodes(webpage, "strong") %>%
  rvest::html_text(.) %>%
  {
    item <- .
    item[grepl("[[:digit:]]{1,2}\\.", item)]
  } %>%
  sub("[[:digit:]]{1,2}\\. ","",.)

# Create tibble of urls, titles, and fixed names
screenplays <- tibble::data_frame(
  URL = pdf_links,
  title = titles,
  name = make.names(titles),
  path = file.path("PDF_Downloads",paste0(name,".pdf")),
  id = 1:length(pdf_links)
)

readr::write_delim(
  screenplays,
  path = file.path("Memory","screenplays.txt"),
  delim = "\t"
)
