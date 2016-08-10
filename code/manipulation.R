
## ---- message = FALSE, tidy=FALSE----------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "hadley/tidyr/"
branch = "master/"
link = "vignettes/pew.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df[1:3, 1:5])

## ------------------------------------------------------------------------
library("tidyr")
args(gather)

## ------------------------------------------------------------------------
df.gather = df %>% 
  gather(key = test, 
         value = frequency, 
         c(religion, variable1, variable2))

df.gather.2 = gather(data = df, 
         key = income, 
         value = frequency, 
         -religion)


## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.gather[1:5, ])

## ---- eval = FALSE-------------------------------------------------------
## df %>%
##   gather(key = income,
##          value = frequency,
##          2:11)

## ---- eval = FALSE-------------------------------------------------------
## df %>%
##   gather(key = income,
##          value = frequency,
##          -religion)

## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "hadley/tidyr/"
branch = "master/"
link = "vignettes/billboard.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df[1:5, 1:5])

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df[1:5, 6:10])

## ------------------------------------------------------------------------
billboard2 = df %>% 
  gather(key = week, 
         value = rank, wk1:wk76, 
         na.rm = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(billboard2[1:5, -3])

## ------------------------------------------------------------------------
library("dplyr")
billboard3 = billboard2 %>%
  mutate(
    week = extract_numeric(week),
    date = as.Date(date.entered) + 7 * (week - 1)) %>%
  select(-date.entered) %>% 
  arrange(artist, track, week)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(billboard3[1:5, 1:5])

## ---- message = FALSE----------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "hadley/tidyr/"
branch = "master/"
link = "vignettes/tb.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df[1:5, 1:8])

## ------------------------------------------------------------------------
tb2 = df %>% 
  gather(demo, n, -iso2, -year, na.rm = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(tb2[1:5, 1:4])

## ------------------------------------------------------------------------
tb3 = tb2 %>% 
  separate(demo, c("sex", "age"), 1)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(tb3[1:5, 1:5])

## ------------------------------------------------------------------------
tb3.wide = tb3 %>% spread(age, n)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(tb3.wide[1:5, 1:8])

## ---- message = FALSE, warning = FALSE-----------------------------------
library("readr")
library("dplyr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/finanslov_tidy.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ---- eval = FALSE-------------------------------------------------------
## View(df)
## glimpse(df)
## summary(df)
## head(df)

## ------------------------------------------------------------------------
df.max.udgift = df %>% 
  filter(udgift == max(udgift)) %>% 
  select(paragraf, aar, udgift)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.max.udgift)

## ------------------------------------------------------------------------
df.skat = df %>% 
  filter(paragraf == "Skatter og afgifter") %>%
  select(paragraf, aar, udgift) %>% 
  arrange(-udgift)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.skat[1:5, ])

## ------------------------------------------------------------------------
df.mutated = df %>%
  mutate(newVar = udgift/2) %>%
  select(newVar, udgift)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.mutated[1:5, ])

## ------------------------------------------------------------------------
df.sample.n = df %>% 
  select(paragraf, aar, udgift) %>%
  sample_n(3)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.sample.n)

## ------------------------------------------------------------------------
df.expense = df %>% 
  group_by(paragraf) %>% 
  summarise(sum.exp = sum(udgift, na.rm = TRUE)) %>% 
  arrange(-sum.exp)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.expense[1:5, ])

## ------------------------------------------------------------------------
df.2 = df %>% 
  group_by(paragraf) %>% 
  mutate(sum.exp = sum(udgift, na.rm = TRUE)) %>% 
  select(paragraf, udgift, sum.exp)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.2[1:5, ])

## ------------------------------------------------------------------------
df.expense.2 = df %>% 
  group_by(paragraf, aar) %>% 
  summarise(sum.exp = sum(udgift, na.rm = TRUE)) %>% 
  arrange(sum.exp)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.expense.2[1:5, ])

## ------------------------------------------------------------------------
df.expense.3 = df %>% 
  group_by(paragraf, aar) %>% 
  summarise(exp = sum(udgift, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(paragraf) %>%
  summarise(sum.exp = mean(exp, na.rm = TRUE))

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.expense.3[1:5, ])

## ---- echo = FALSE-------------------------------------------------------
superheroes = c("    name, alignment, gender,         publisher",
    " Magneto,       bad,   male,            Marvel",
    "   Storm,      good, female,            Marvel",
    "Mystique,       bad, female,            Marvel",
    "  Batman,      good,   male,                DC",
    "   Joker,       bad,   male,                DC",
    "Catwoman,       bad, female,                DC",
    " Hellboy,      good,   male, Dark Horse Comics")

superheroes = read.csv(text = superheroes, strip.white = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(superheroes)

## ---- echo = FALSE-------------------------------------------------------
publishers = c("publisher, yr_founded",
    "       DC,       1934",
    "   Marvel,       1939",
    "    Image,       1992")
publishers = read.csv(text = publishers, strip.white = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(publishers)

## ------------------------------------------------------------------------
ijsp = inner_join(superheroes, publishers)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(ijsp)

## ------------------------------------------------------------------------
ljsp = left_join(superheroes, publishers)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(ljsp)

## ---- message = TRUE-----------------------------------------------------
superheroes = superheroes %>% 
  mutate(seblikes = (publisher == "Marvel"))
publishers = publishers %>% 
  mutate(seb = (publisher == "Marvel"))
ij2 = inner_join(superheroes,publishers)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(ij2)

## ------------------------------------------------------------------------
ij2 = inner_join(superheroes, publishers,
                    by=c("publisher"="publisher",
                            "seblikes"="seb"))

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(ij2)

## ------------------------------------------------------------------------
fj = superheroes %>%
  full_join(publishers)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(fj[, -c(5, 7)])

## ---- eval = FALSE-------------------------------------------------------
## my_function = function(input1, input2, ..., inputN)
##   {
##   # define 'output' using input1,...,inputN
##   return(output)
##   }

## ------------------------------------------------------------------------
add_numbers = function(x, y){
  z = x + y
  return(z)
}

add_numbers(2, 4)
add_numbers(2, 6)

## ---- error = TRUE-------------------------------------------------------
add_numbers(2, "y")

## ------------------------------------------------------------------------
add_numbers = function(x, y){
  if ( !is.numeric(x) || !is.numeric(y)) {
    warning("either 'x' or 'y' is not numeric")
    return(NA)
  }
  else {
    z = x + y
    return(z)
  }
}

## ---- warning = TRUE-----------------------------------------------------
add_numbers(2, 4)
add_numbers(2, "y")

## ---- echo = FALSE-------------------------------------------------------
df = data.frame(
  a = rnorm(5),
  b = rnorm(5),
  c = runif(5),
  d = rchisq(5, 1)
)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df)

## ------------------------------------------------------------------------
output = vector()
for (i in 1:ncol(df)){
  output[[i]] = mean(df[, i], na.rm = TRUE)
}
output

## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "kosukeimai/qss/"
branch = "master/"
link = "PREDICTION/intrade08.csv"
data.link = paste0(gh.link, user.repo, branch, link)
intrade.08 = read_csv(data.link)

## ------------------------------------------------------------------------
link = "PREDICTION/intrade12.csv"
data.link = paste0(gh.link, user.repo, branch, link)
intrade.12 = read_csv(data.link)

## ------------------------------------------------------------------------
link = "PREDICTION/pres08.csv"
data.link = paste0(gh.link, user.repo, branch, link)
pres.08 = read_csv(data.link)

## ------------------------------------------------------------------------
link = "PREDICTION/pres12.csv"
data.link = paste0(gh.link, user.repo, branch, link)
pres.12 = read_csv(data.link)

## ------------------------------------------------------------------------
link = "PREDICTION/polls08.csv"
data.link = paste0(gh.link, user.repo, branch, link)
polls.08 = read_csv(data.link)

## ------------------------------------------------------------------------
link = "PREDICTION/polls12.csv"
data.link = paste0(gh.link, user.repo, branch, link)
polls.12 = read_csv(data.link)

## ---- eval = FALSE-------------------------------------------------------
## output = vector()
## for (i in 1:ncol(df)){
##   output[[i]] = median(df[, i], na.rm = TRUE)
## }

## ---- eval = FALSE-------------------------------------------------------
## output = vector()
## for (i in 1:ncol(df)){
##   output[[i]] = mean(df[, i], na.rm = TRUE)
## }

## ------------------------------------------------------------------------
library("purrr")
map_dbl(df, mean)
map_dbl(df, median)

## ------------------------------------------------------------------------
my_function = function(df){
  my.mean = mean(df, na.rm = TRUE)
  my.string = paste("mean is", 
                    round(my.mean, 2), 
                    sep = ":")
  return(my.string)
}

## ------------------------------------------------------------------------
map_chr(df, my_function)

## ------------------------------------------------------------------------
strings = c("a", "ab", "acb", "accb", 
            "acccb", "accccb")
grep("ac*b", strings, value = TRUE)
grep("ac+b", strings, value = TRUE)
grep("ac?b", strings, value = TRUE)

## ------------------------------------------------------------------------
grep("ac{2}b", strings, value = TRUE)
grep("ac{2,}b", strings, value = TRUE)
grep("ac{2,3}b", strings, value = TRUE)

## ------------------------------------------------------------------------
strings = c("^ab", "ab", "abc", "abd", "abe", "ab 12")
grep("ab.", strings, value = TRUE)
grep("ab[c-e]", strings, value = TRUE)
grep("ab[^c]", strings, value = TRUE)

## ------------------------------------------------------------------------
grep("^ab", strings, value = TRUE)
grep("\\^ab", strings, value = TRUE)
grep("abc|abd", strings, value = TRUE)

## ------------------------------------------------------------------------
library("stringr")

## ------------------------------------------------------------------------
x = c("apple", "banana", "pear", "213")
x %>% str_detect("e")
x %>% str_detect("[0-9]")
x %>% str_detect("e.r")

## ------------------------------------------------------------------------
x %>% str_extract("apple|[0-9]*")
x %>% str_extract("a.*")

## ------------------------------------------------------------------------
xx = c("apple 123", "banana", "pear", "213")
xx %>% str_extract("apple|[0-9]*")
xx %>% str_extract("a.*")

## ------------------------------------------------------------------------
xx %>% str_extract_all("apple|[0-9].")

## ------------------------------------------------------------------------
xx %>% str_replace("[0-9]", "\\?")
xx %>% str_replace_all("[0-9]", "\\?")
xx %>% str_replace_all(c("1" = "one", "2" = "x"))

## ------------------------------------------------------------------------
xx %>% str_split("a")

## ------------------------------------------------------------------------
xx %>% str_split("a", n = 2)

