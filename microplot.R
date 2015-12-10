library(ggplot2)
library(rvest)

plot_problems <- function() {
  data <- read.csv("cities.csv", colClasses = c("character", "integer", "character", "character", "factor"))
  # Sort in decreasing order of points, so that larger bubbles are in the
  # background.
  data <- data[order(data$points, decreasing=TRUE),]
  p <- ggplot(data, aes(x=hw, y=sw, size=points,label=city)) +
    xlab("hardware rev") +
    ylab("software rev") +
    ggtitle("microcorruption") +
    geom_point(color="white", fill="red", shape=21) +
    geom_text(size=4) +
    scale_size_area(max_size=50) +
    theme(legend.position="none",panel.grid.major = element_blank())
  print(p)
}

get_hall_of_fame <- function(use_cached = TRUE) {
  if (use_cached) {
    hall_of_fame <- 
      read.csv(
        "hof.csv", 
        colClasses = c("integer", "integer", "character", "integer"),
        row.names=1
      )
  } else {
    base <- "https://microcorruption.com"
    hall_of_fame <- 
      data.frame(rank = integer(0), username=character(0), score = integer(0))
    next_link <- paste0(base, "/hall_of_fame")

    while (length(next_link) > 0) {
      print(next_link)
      site_content <- read_html(next_link)
      rank <- html_text(
        html_nodes(site_content, ".green_table td:nth_child(1)")
      )
      username <- html_text(
        html_nodes(site_content, ".green_table td:nth_child(2) a:first-child")
      )

      score <- html_text(
        html_nodes(site_content, ".green_table td:nth_child(3)")
      )
      hall_of_fame <-
        rbind(hall_of_fame, data.frame(rank=rank, username=username, score=score))
      buttons <- html_nodes(site_content, "a.button.grey")
      last_button <- buttons[length(buttons)]
      next_path <- character(0) 
      next_link <- character(0)
      if (grepl("Next Page", html_text(last_button))) {
        next_path <- html_attr(last_button, "href")
        next_link <- paste0("https://microcorruption.com", next_path)
      }
    }
  }
  return(hall_of_fame)
}

test_default <- function(x = TRUE) {
  print(x)
}

