# Clear workspace
rm(list=ls())

main_webpage <- "http://www.imdb.com/"

# Load relevant packages
library(dplyr)

# Load valid_corpora
load(file.path("Memory","valid_corpora.r"))

# Read in screenplays.txt from the Memory folder and subset valid_corpora
ref_tbl <- readr::read_delim(
  file.path("Memory","screenplays.txt"),
  delim = "\t"
) %>%
{.[valid_corpora,]}


result <- lapply(
  split(ref_tbl,1:nrow(ref_tbl)),
  function(movie_row){
    rating <- NA
    votes <- NA
    
    try({
      webpage <- rvest::html_session(main_webpage)
      form <- rvest::html_form(webpage)[[1]]
      filled_form <- rvest::set_values(
        form = form,
        q = movie_row$title[1]
      )
      submitted <- rvest::submit_form(
        session = webpage,
        form = filled_form
      )
      movie_page <- rvest::follow_link(
        submitted,
        css = "td.result_text a"
      )
      ratings_node <- rvest::html_node(
        movie_page,
        css = "#title-overview-widget > div.vital > div.title_block > div > div.ratings_wrapper > div.imdbRating > div.ratingValue > strong"
      )
      text_result <- rvest::html_attr(ratings_node, "title")
      ratings_info <- gsub(",|[[a-zA-Z]]*","",text_result) %>%
        gsub("\\s+"," ",.) %>%
        strsplit(., " ") %>%
        unlist() %>%
        as.numeric()
      
      rating <- ratings_info[1]
      votes <- ratings_info[2]
    })
    
    movie_row %>% 
      dplyr::mutate(
        .,
        rating = rating,
        votes = votes
      )
  }
) %>%
  dplyr::bind_rows()

# Plot rating vs. votes
library(ggplot2)
ggplot(
  result,
  aes(rating, I(votes/10000))
)+
  geom_point()+
  xlab("Rating")+
  ylab(bquote("Votes ("*x*"10,000)"))+
  scale_x_continuous(
    expand=c(0,0),
    breaks = seq(0,10,by=2)
  )+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(
    xlim=c(0,10),
    ylim=c(0,200)
  )+
  theme_bw()+
  theme(
    axis.title = element_text(
      size = 12
    ),
    axis.text = element_text(
      size = 10,
      colour = "black"
    ),
    plot.margin = margin(0.25, 0.25, 0.1, 0.1, "in")
  )+
  geom_smooth(method = "loess", se = FALSE, span = 0.89, size = 0.7)
  
ggsave(
  filename = file.path("Figs","Votes_by_Ratings.tiff"),
  units = "in",
  height = 3,
  width = 4
)

# Write a new reference file with ratings and vote numbers in it
readr::write_delim(
  result,
  path = file.path("Memory","rated_movies.txt"),
  delim = "\t"
)
