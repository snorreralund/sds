
# Hollowood's Gender Divide ---------------------
## loading libraries
library("readr")
library("dplyr")

## read data
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/bechdel.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## generate variables
df = df %>% 
  mutate( # mean_male + female
    mean_male = count_male/count,
    mean_female = count_female/count) %>% 
  mutate( # status
    status = ifelse( mean_male == 1,
                     "all male",
                     ifelse(
                       mean_female == 1, 
                       "all female",
                       "mixed")
                     ))

## Alternative
df = df %>% 
  mutate( # mean_male + female
    mean_male = count_male/count,
    mean_female = count_female/count) %>% 
  mutate( # status
    status = "mixed",
    status = ifelse(mean_male == 1, 
                    "all male", status),
    status = ifelse(mean_female == 1, 
                    "all female", status)
    )

