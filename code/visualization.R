
install.packages("readr")
install.packages("ggplot2")


## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/polarization.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)
names(df)

https://raw.githubusercontent.com/sebastianbarfort/sds_summer/gh-pages/data/polarization.csv
https://raw.githubusercontent.com/sebastianbarfort/sds_summer/gh-pages/data/polarization.csv

## ------------------------------------------------------------------------
library("ggplot2")
p = ggplot(data = df, aes(x = x, y = pct))
p <- ggplot(data = df, aes(x = x, y = pct))

## ---- echo = FALSE-------------------------------------------------------
library("ggplot2")
theme_set(theme_gray() + theme(plot.background = element_rect(fill = "transparent")))

## ----base, echo = FALSE--------------------------------------------------
p

## ----line----------------------------------------------------------------
p = p + geom_line()

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
p = ggplot(data = df, 
           aes(x = x, y = pct, 
               fill = party, color = party))

## ----line2---------------------------------------------------------------
p = p + geom_line()

## ---- echo = FALSE-------------------------------------------------------
p

## ----ribbon1-------------------------------------------------------------
p = p + geom_ribbon(aes(ymin = 0, ymax = pct))

## ---- echo = FALSE-------------------------------------------------------
p

## ----ribbon2-------------------------------------------------------------
p = p + geom_ribbon(aes(ymin = 0, ymax = pct), 
                    alpha = .5)

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
p = p + scale_color_manual(
  name="", 
  values=c(Dem="blue",REP="red"),
  labels=c(Dem="Democrats", REP="Republicans")) +
  scale_fill_manual(
    name="", 
    values=c(Dem="#728ea2", REP="#cf6a5d"),
    labels=c(Dem="Democrats", REP="Republicans")) +
  guides(color="none", 
         fill=guide_legend(override.aes=list(alpha=1)))

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
p = p + 
  scale_x_continuous(
    breaks = c(-8, 0, 8),
    labels= c("Consistently\nliberal", 
              "Mixed",
              "Consistently\nconservative")) +
  scale_y_continuous(
    expand = c(0,0), limits = c(0, 12))

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
p = p + facet_wrap(~ year, ncol = 2, 
                   scales = "fixed")

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
p = p + labs(x = NULL, 
             y = NULL,
             title = "Polarization, 1994-2014")

## ------------------------------------------------------------------------
p = p + theme_minimal()

## ------------------------------------------------------------------------
p = p + 
  theme(legend.position = c(0.75, 0.1)) +
  theme(legend.direction = "horizontal") +
  theme(axis.text.y = element_blank())




## ---- echo = FALSE-------------------------------------------------------
ggsave(plot = p, file = "figures/polarization.pdf",
       width = 8, height = 6)

## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/polls_tidy.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ---- echo = FALSE-------------------------------------------------------
library("dplyr")
df = df %>% 
  mutate(party.ord = toupper(Parti),
         party.ord = reorder(party.ord, -predicted))

## ------------------------------------------------------------------------
p = ggplot(df, aes(x = predicted, y = density))

## ------------------------------------------------------------------------
p = p + geom_line()

## ------------------------------------------------------------------------
p = ggplot(df, aes(x = predicted, y = density, 
                   group = group, color = Parti))
p = p + geom_line()

## ------------------------------------------------------------------------
p = ggplot(df, aes(x = predicted, y = density, 
                   group = group, color = Parti))
p = p + 
  geom_line() + 
  facet_grid(party.ord ~ ., 
             scales = "free_y", 
             switch = "y") 

## ------------------------------------------------------------------------
df.1 = subset(df, date != max(date))
df.2 = subset(df, date == max(date))
p = ggplot()
p = p + 
  geom_line(data = df.1, 
            aes(x = predicted, y = density, 
                group = group),
            color = "grey50", alpha = .25) + 
  facet_grid(party.ord ~ ., 
             scales = "free_y", switch = "y") 

## ------------------------------------------------------------------------
p = p + geom_line(data = df.2, 
              aes(x = predicted, y = density, 
                  group = group), 
              colour = "red", size = 1)

## ------------------------------------------------------------------------
p = p + scale_x_continuous(
  breaks = scales::pretty_breaks(10)) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 180, 
                                    face = "bold", 
                                    vjust = 0.75,
                                    hjust = 1),
        axis.text.y = element_blank()) +
  labs(y = NULL, x = NULL)

## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/basket_tidy.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ------------------------------------------------------------------------
p = ggplot(data = df, 
           aes(x = games, 
               y = agg.three,
               color = as.factor(steph.ind),
               group = player))

## ------------------------------------------------------------------------
p = p + geom_step(size = 1) 

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
p = p + scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 160),
    breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(
    expand = c(0, 0)) +
  theme_minimal()

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
p = p +
  scale_color_manual(
    values = c("grey50", "grey50", "darkblue")) +
  labs(x = "Games", 
       y = "# of threes", 
       title = "Stephen Curry's 3-Point Record")

## ------------------------------------------------------------------------
p = p + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

## ---- echo = FALSE-------------------------------------------------------
p

## ------------------------------------------------------------------------
data.max = df %>% 
  filter(player == 
           "Stephen Curry, 2015-16") %>% 
  filter(agg.three == max(agg.three))

## ------------------------------------------------------------------------
library("ggrepel")
p = p +
  geom_text_repel(
    data = data.max,
    aes(label = player, 
        color = as.factor(steph.ind)),
    size = 3,
    nudge_x = 24,
    segment.size = 0
  ) 

## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/armslist.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ------------------------------------------------------------------------
p = ggplot(data = df, aes(x = price))

