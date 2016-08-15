
# Read data
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "kosukeimai/qss/"
branch = "master/"
link = "CAUSALITY/leaders.csv"
data.link = paste0(gh.link, user.repo, branch, link)
leaders = read_csv(data.link)
df = read_csv(data.link)

## Answer 1
library("dplyr")
df %>% 
  count(year) %>% 
  summarise(n = sum(n) / n())
            
## Answer 2
library("stringr")
df = df %>% 
  mutate(
    die_indicator = result %>% str_detect("dies"),
    success = if_else(die_indicator, 1, 0))
    
mean(df$success)    

## Answer 3
df %>% 
  group_by(success) %>% 
  summarise(
    m.pol = mean(politybefore),
    m.age = mean(age)
  )

## Answer 4
df = df %>% 
  mutate(
    warbefore = if_else(
      interwarbefore == 1 | 
        leaders$civilwarbefore == 1,
      1, 0)) %>% 
  mutate(
    warafter = if_else(
      interwarafter == 1 | 
        leaders$civilwarafter == 1,
      1, 0))

df %>% 
  group_by(warbefore, success) %>% 
  count()

## Answer 5
df = df %>% 
  group_by(success) %>% 
  summarise(pol.after = mean(polityafter),
            pol.before = mean(politybefore),
            war.after = mean(warafter),
            war.before = mean(warbefore)) %>% 
  mutate(
    d.pol.after = pol.after - pol.before,
    d.war.after = war.after - war.before
  )

# DiD
df$d.pol.after[2] - df$d.pol.after[1]
df$d.war.after[2] - df$d.war.after[1]


