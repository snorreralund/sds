
## ------------------------------------------------------------------------
library("readr")
library("dplyr")

infile = "../nopub/yelp_academic_dataset_review.json"
review_lines = read_lines(infile, 
                          n_max = 50000, 
                          progress = FALSE)

## ------------------------------------------------------------------------
library("stringr")
library("jsonlite")

reviews_combined = str_c("[", 
                         str_c(review_lines, 
                               collapse = ", "), 
                         "]")

reviews = fromJSON(reviews_combined) %>%
  flatten() %>%
  tbl_df()

## ------------------------------------------------------------------------
library("tidytext")
review_words = reviews %>%
  select(review_id, business_id, stars, text) %>%
  unnest_tokens(word, text) 

review_words %>% dim

## ------------------------------------------------------------------------
stopwords = stop_words$word

test = str_detect(review_words$word, "^[a-z']+$")
test[1:30]
review_words$word[1:30]
test2 = str_detect(review_words$word, "^[a-z]+")

review_words = review_words %>% 
  filter(!word %in% stop_words$word) %>% 
  filter(str_detect(word, "^[a-z']+$"))

## ------------------------------------------------------------------------
AFINN = sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)


## ------------------------------------------------------------------------
reviews_sentiment = review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(review_id, stars) %>%
  summarize(sentiment = mean(afinn_score))


## ------------------------------------------------------------------------
df.review = reviews_sentiment %>% 
  group_by(stars) %>% 
  summarise(m.sentiment = mean(sentiment))

## ---- echo = FALSE-------------------------------------------------------
library("ggplot2")
library("viridis")
library("ggrepel")
library("ggalt")
p = ggplot(df.review, aes(x = m.sentiment, y = stars,
                          color = as.factor(stars)))
p + geom_vline(xintercept = 0, size = 2, color = "white") +
  geom_lollipop(point.size=3, 
                  horizontal=TRUE) + 
  geom_text_repel(aes(label = stars),
                  color = "black",
                  nudge_y = .2) +
  scale_color_viridis(discrete = TRUE) +
  labs(x = "Sentiment Score (mean)",
       y = "Number of Stars on Yelp") +
  theme(legend.position = "none")

## ------------------------------------------------------------------------
review_words_counted = review_words %>%
  count(review_id, business_id, stars, word) %>%
  ungroup()

## ------------------------------------------------------------------------
word_summaries = review_words_counted %>%
  group_by(word) %>%
  summarize(businesses = n_distinct(business_id),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(stars)) %>%
  ungroup() %>% 
  arrange(reviews)

## ---- echo = FALSE-------------------------------------------------------
p = ggplot(word_summaries, 
           aes(x = reviews))
p + scale_x_log10() + 
  geom_freqpoly()

## ------------------------------------------------------------------------
word_summaries_filtered = word_summaries %>%
  filter(reviews >= 200, businesses >= 10)


## ---- echo = FALSE-------------------------------------------------------
p = ggplot(word_summaries_filtered, 
           aes(x = reviews))
p + scale_x_log10() + 
  geom_freqpoly()

## ------------------------------------------------------------------------
df.0 = word_summaries_filtered %>%
  arrange(-average_stars)

## ------------------------------------------------------------------------
df.1 = word_summaries_filtered %>%
  arrange(average_stars)

## ---- echo = FALSE-------------------------------------------------------
p = ggplot(sample_frac(word_summaries_filtered, 1), 
       aes(reviews, average_stars)) +
  geom_point(alpha = .35) +
  geom_text(
    data = word_summaries_filtered %>% 
      arrange(-average_stars) %>% 
  filter(row_number() <= 30), 
    aes(label = word), 
    check_overlap = TRUE, vjust = 1, hjust = 0.6) +
  geom_text(
    data = word_summaries_filtered %>% 
      arrange(average_stars) %>% 
  filter(row_number() <= 30), 
    aes(label = word), 
    check_overlap = TRUE, vjust = 1, hjust = 0.6) +
  geom_text(
    data = word_summaries_filtered %>% 
      arrange(-reviews) %>% 
  filter(row_number() <= 30), 
    aes(label = word), 
    check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_smooth(se = FALSE) + 
  xlab("# of reviews") +
  ylab("Average Stars")
ggsave(plot = p, file = "figures/reviews.pdf",
       width = 6, height = 4)

## ---- echo = FALSE-------------------------------------------------------
p = ggplot(reviews_sentiment,
           aes(x = sentiment, y = stars))
p + geom_jitter() +
  geom_smooth()

## ------------------------------------------------------------------------
library("purrr")
library("modelr")
gen_crossv = function(pol, 
                      data = reviews_sentiment){
  data %>% 
    crossv_mc(200) %>% 
    mutate(
      mod = map(train, 
                ~ lm(stars ~ poly(
                  sentiment, pol), 
                data = .)),
      rmse.test = map2_dbl(mod, test, rmse),
      rmse.train = map2_dbl(mod, train, rmse)
    )
}

## ---- eval = FALSE-------------------------------------------------------
## set.seed(3000)
## df.cv = 1:8 %>%
##   map_df(gen_crossv, .id = "degree")

## ---- echo = FALSE, eval = FALSE-----------------------------------------
## library("tidyr")
## df.cv.sum = df.cv %>%
##   group_by(degree) %>%
##   summarise(
##     m.rmse.test = mean(rmse.test),
##     m.rmse.train = mean(rmse.train)
##   )
## 
## df.cv.sum = df.cv.sum %>%
##   mutate(degree = as.numeric(degree)) %>%
##   gather(var, value, -degree) %>%
##   arrange(degree)
## 
## p = ggplot(df.cv.sum,
##            aes(x = degree, y = value,
##                color = var))
## p + geom_point() +
##   geom_line() +
##   scale_color_viridis(discrete = TRUE,
##                       name = NULL,
##                       labels = c("RMSE (test)",
##                                  "RMSE (train)")) +
##   theme(legend.position = "bottom") +
##   labs(x = "Degree", y = "RMSE") +
##   scale_x_continuous(breaks = 1:9)

## ------------------------------------------------------------------------
file = paste0("http://varianceexplained.org/",
              "files/",
              "trump_tweets_df.rda")
load(url(file))

## ------------------------------------------------------------------------
library("tidyr")

tweets = trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, 
          "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

## ------------------------------------------------------------------------
library("tidytext")
library("stringr")

reg = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words = tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = 
           str_replace_all(text,
  "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", 
                pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

## ---- echo = FALSE-------------------------------------------------------
df.tweet = tweet_words %>% 
  count(word) %>% 
  arrange(-n) %>% 
  filter(row_number() <= 10) %>% 
  ungroup() %>% 
  mutate(
    word = reorder(as.factor(word), n)
  )

p = ggplot(df.tweet, aes(x = word, 
                         y = n))
p + geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = NULL)

## ------------------------------------------------------------------------
android_iphone_ratios = tweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log2(Android / iPhone)) 

## ---- echo = FALSE-------------------------------------------------------
df.1 = android_iphone_ratios %>%
  arrange(-logratio) %>% 
  filter(row_number() <= 10) %>% 
  mutate(device = "Android")
df.2 = android_iphone_ratios %>%
  arrange(logratio) %>% 
  filter(row_number() <= 10) %>% 
  mutate(device = "iPhone")

df.trump = df.1 %>% 
  bind_rows(df.2) %>% 
  mutate(
    word = reorder(word, logratio)
  )

p = ggplot(df.trump, 
           aes(x = word, y = logratio,
               fill = device))
p + geom_bar(stat = "identity", color = "black") +
  coord_flip() + scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "below") +
  labs(x = NULL, y = NULL)

## ------------------------------------------------------------------------
nrc = sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)

## ---- echo = FALSE-------------------------------------------------------
p = android_iphone_ratios %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  geom_bar(stat = "identity", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Android / iPhone log ratio") +
  scale_fill_viridis(name = "", labels = c("Android", "iPhone"),
                    discrete = TRUE)

