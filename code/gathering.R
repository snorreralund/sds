library("rvest")
link = paste0("http://en.wikipedia.org/",
              "wiki/Table_(information)")
link.data = link %>%
  read_html() %>% 
  html_node(".wikitable") %>% 
  # extract first node with class wikitable
  html_table() 
  # then convert the HTML table into a data frame

## ------------------------------------------------------------------------
css.selector = ".artTitle a"
link = "http://jyllands-posten.dk/"

jp.data = link %>% 
  read_html() %>% 
  html_nodes(css = css.selector) %>% 
  html_text()

## ---- echo = FALSE-------------------------------------------------------
jp.data[1:5]

## ------------------------------------------------------------------------
library("stringr")
jp.data1 = jp.data %>% 
  str_replace_all(pattern = "\\n|\\t|\\r" , 
                  replacement = "") 

## ------------------------------------------------------------------------
library("stringr")
jp.data2 = jp.data %>% 
  str_trim()

## ------------------------------------------------------------------------
jp.links = link %>% 
  read_html(encoding = "UTF-8") %>% 
  html_nodes(css = css.selector) %>%
  html_attr(name = 'href')

## ------------------------------------------------------------------------
jp.keep = jp.links %>%
  str_detect("politik|indland|international")
jp.links.clean = jp.links[jp.keep]
jp.remove.index = jp.links.clean %>% 
  str_detect("protected|premium|finans")
jp.links.clean = jp.links.clean[!jp.remove.index]

## ------------------------------------------------------------------------
first.link = jp.links.clean[1]
first.link.text = first.link %>% 
  read_html(encoding = "UTF-8") %>% 
  html_nodes("#articleText") %>% 
  html_text()

## ---- echo = FALSE-------------------------------------------------------
first.link.text

## ------------------------------------------------------------------------
first.link %>% 
  read_html(encoding = "UTF-8") %>% 
  html_nodes(".bylineAuthorName span") %>% 
  html_text()

## ------------------------------------------------------------------------
scrape_jp = function(link){
  my.link = link %>% 
    read_html(encoding = "UTF-8")
  author = my.link %>% 
    html_nodes(".bylineAuthorName span") %>% 
    html_text()
  if (length(author) == 0){ author = NA }
  link.text = my.link %>% 
    html_nodes("#articleText") %>% 
    html_text() 
  if (length(link.text) == 0){ link.text = NA }
  return(data.frame(author = author,
    link = link, text = link.text ))
}

## ------------------------------------------------------------------------
library("purrr")
jp.article.data = jp.links.clean[1:5] %>% 
  map_df(scrape_jp)

## ---- echo = FALSE-------------------------------------------------------
econ.link = "http://www.econ.ku.dk/ansatte/vip/"
links = econ.link %>% 
  read_html(encoding = "UTF-8") %>%
  html_nodes("td:nth-child(1) a")%>%
  html_attr(name = 'href')

long.links = paste(econ.link, links, sep = "")

scrape_econ = function(link){
  my.link = link %>% 
    read_html(encoding = "UTF-8")
  title = my.link %>% 
    html_nodes("#content .type") %>% 
    html_text()
  title = title[1]
  name = my.link %>% 
    html_nodes(".person") %>% 
    html_text()
  name = name[1]
  return( data.frame(name = name, 
                     title = title))
}

econ.data = long.links[1:5] %>% 
  map_df(scrape_econ)

## ------------------------------------------------------------------------
library("httr")
url = "https://api.github.com/repos/hadley/dplyr/issues"
get.1 = GET(url, query = list(state = "closed"))
get.2 = GET(url, query = list(state = "closed",
                       labels = "bug"))

## ------------------------------------------------------------------------
library("jsonlite")
get.1.parsed = content(get.1, as = "text")
get.1.data = fromJSON(get.1.parsed, flatten = TRUE)
get.2.parsed = content(get.2, as = "text")
get.2.data = fromJSON(get.2.parsed, flatten = TRUE)


## ---- eval = FALSE-------------------------------------------------------
## install.packages("gmapsdistance")

## ------------------------------------------------------------------------
library("gmapsdistance")
results = gmapsdistance(origin = "København", 
                        destination = "Roskilde", 
                        mode = "driving")

## ---- echo = FALSE-------------------------------------------------------
results

## ------------------------------------------------------------------------
results = gmapsdistance(
  origin = "38.1621328+24.0029257",
  destination = "37.9908372+23.7383394",
  mode = "walking")

## ---- echo = FALSE-------------------------------------------------------
results

## ------------------------------------------------------------------------
library("ggmap")
geocode("Økonomisk Institut, 
        Københavns Universitet, København")

## ------------------------------------------------------------------------
geocode("The White House")

## ---- eval = FALSE-------------------------------------------------------
## library("devtools")
## install_github("rOpenGov/dkstat")

