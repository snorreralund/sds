
# scraping IPaidaBribe ---------------------

# load libraries
library("rvest")
library("purrr")

# create links
base.link = "http://www.ipaidabribe.com/reports/paid?page="
pages = seq(0, 30, by = 10)

## define vector of links to be scraped
bribe.links = paste0(base.link, pages)

# helper function
helper_function = function(link, css.select){
  out = link %>% 
    html_nodes(css.select) %>% 
    html_text
  return(out)
}

# scraping function
scrape_bribes = function(link){
  parsed.link = link %>% 
    read_html
  header = parsed.link %>% 
    helper_function(".heading-3 a")
  views = parsed.link %>% 
    helper_function(".overview .views")
  location = parsed.link %>% 
    helper_function(".location")
  dept = parsed.link %>% 
    helper_function(".name a")
  return(
    data.frame(header = header, 
               views = views,
               location = location,
               department = dept)
        )
}

# iterate over links, extract data, bind rows
final.df = bribe.links %>% 
  map_df(scrape_bribes)
