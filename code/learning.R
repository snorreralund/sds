
## ---- echo = FALSE-------------------------------------------------------
set.seed(100)
library("modelr")
library("tidyr")
library("dplyr")
library("purrr")
library("viridis")
library("ggplot2")

## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "johnmyleswhite/ML_for_Hackers/"
branch = "master/"
link = "05-Regression/data/longevity.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)


## ------------------------------------------------------------------------
add_rmse = function(i){
  df %>% 
    mutate(sq.error = (AgeAtDeath - i)^2) %>% 
    summarise(mse = mean(sq.error),
              rmse = sqrt(mse),
              guess = i)
}

df.rmse = 63:83 %>% 
  map_df(add_rmse)

## ---- echo = FALSE-------------------------------------------------------
df.rmse.2 = df.rmse %>% filter(rmse == min(rmse))
ggplot(df.rmse, aes(x = guess, y = rmse)) +
  geom_point() +
  geom_line() +
  geom_point(data = df.rmse.2,
             color = "red") 

## ------------------------------------------------------------------------
df.rmse %>% 
  filter(rmse == min(rmse))

## ------------------------------------------------------------------------
df %>% 
  summarise(round(mean(AgeAtDeath), 0))

## ------------------------------------------------------------------------
true_model = function(x){
  2 + 8*x^4 + rnorm(length(x), sd = 1)
}

## ------------------------------------------------------------------------
df = data_frame(
  x = seq(0, 1, length = 50),
  y = true_model(x)
)

## ------------------------------------------------------------------------
my_model = function(pol, data = df){
  lm(y ~ poly(x, pol), data = data)
}

## ------------------------------------------------------------------------
model.1 = my_model(pol = 1)


## ------------------------------------------------------------------------
add_pred = function(mod, data = df){
  data %>% add_predictions(mod, var = "pred")
}

df.1 = add_pred(model.1)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(df.1[1:6, ])

## ---- echo = FALSE-------------------------------------------------------
p = ggplot(df.1)
p + geom_segment(aes(x=x, 
                   xend=x, 
                   y=y, yend=pred), 
                   color="red") +
  geom_point(aes(x = x, y = y),
               color = "black") +
  geom_line(aes(x = x, y = pred), 
            size = 1)

## ------------------------------------------------------------------------
# Estimate polynomial from 1 to 9
models = 1:9 %>% 
  map(my_model) %>% 
  map_df(add_pred, .id = "poly")

## ---- echo = FALSE-------------------------------------------------------
# plot
p = ggplot(data = models,
       aes(x, pred)) +
  geom_segment(aes(x=x, 
                   xend=x, 
                   y=y, yend=pred), 
                   color="red") +
  geom_point(data = df,
             aes(x, y),
             color = "grey50",
             fill = "white",
             shape = 21) +
  geom_line(aes( color = poly == 4),
            size = 1) +
  facet_wrap(~ poly, ncol = 3) +
  scale_color_manual(values = c("black", "blue")) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)


## ------------------------------------------------------------------------
models.rmse = models %>% 
  mutate(error = y - pred,
         sq.error = error^2) %>% 
  group_by(poly) %>% 
  summarise(
    mse = mean(sq.error),
    rmse = sqrt(mse)
  ) %>% 
  arrange(rmse)


## ------------------------------------------------------------------------
gen_crossv = function(pol, data = df){
  data %>% 
    crossv_kfold(10) %>% 
    mutate(
      mod = map(train, ~ lm(y ~ poly(x, pol), 
                            data = .)),
      rmse.test = map2_dbl(mod, test, rmse),
      rmse.train = map2_dbl(mod, train, rmse)
    )
}

## ------------------------------------------------------------------------
set.seed(3000)
df.cv = 1:10 %>% 
  map_df(gen_crossv, .id = "degree")


## ------------------------------------------------------------------------
df.cv.sum = df.cv %>% 
  group_by(degree) %>% 
  summarise(
    m.rmse.test = mean(rmse.test),
    m.rmse.train = mean(rmse.train)
  )

## ---- echo = FALSE-------------------------------------------------------
df.cv.sum = df.cv.sum %>% 
  mutate(degree = as.numeric(degree)) %>% 
  gather(var, value, -degree) %>% 
  arrange(degree)

p = ggplot(df.cv.sum, 
           aes(x = degree, y = value,
               color = var))
p + geom_point() +
  geom_line() +
  scale_color_viridis(discrete = TRUE,
                      name = NULL,
                      labels = c("RMSE (test)",
                                 "RMSE (train)")) +
  theme(legend.position = "bottom") +
  labs(x = "Degree", y = "RMSE") +
  scale_x_continuous(breaks = 1:11)

## ------------------------------------------------------------------------
lm.fit = my_model(pol = 1)
l2.norm = sum(coef(lm.fit)^2)
l1.norm = sum(abs(coef(lm.fit)))
print(paste0("l2.norm is ", l2.norm))
print(paste0("l1.norm is ", l1.norm))

## ---- message = FALSE, warning = FALSE-----------------------------------
library("glmnet")

## ---- echo = FALSE-------------------------------------------------------
set.seed(1)
df = data_frame(
  x = seq(0, 1, length = 50000),
  y = true_model(x)
)

## ------------------------------------------------------------------------
x = poly(df$x, 9)
y = df$y

out = glmnet(x, y)

## ------------------------------------------------------------------------
cal_rmse = function(prediction, truth){
  return(sqrt(mean( (prediction - truth) ^2)))
}

performance = function(i){
  prediction = predict(glmnet.fit, 
                       poly(test.df$x, 9), 
                       s = i)
  truth = test.df$y
  RMSE = cal_rmse(prediction, truth)
  return( data.frame(lambda = i, 
                     rmse = RMSE))
}

## ------------------------------------------------------------------------
n = nrow(df)
indices = sort(sample(1:n, round(.5*n)))
training.df = df[indices, ]
test.df = df[-indices, ]
glmnet.fit = glmnet(poly(training.df$x, 9), 
                    training.df$y) 
lambdas = glmnet.fit$lambda

perf.df = lambdas %>% 
  map_df(performance)

## ---- echo = FALSE-------------------------------------------------------
ggplot(perf.df, 
       aes( x = lambda, y = rmse)) + 
  geom_point() + geom_line() +
  geom_point(data = perf.df %>% 
               filter(rmse == min(rmse)),
             color = "red")

## ------------------------------------------------------------------------
best.lambda = perf.df %>% 
  filter(lambda == min(lambda))
glmnet.fit = glmnet(poly(df$x, 9), df$y)

## ------------------------------------------------------------------------
coef(glmnet.fit, s = best.lambda$lambda)

## ------------------------------------------------------------------------
library("jsonlite")
food = fromJSON("~/git/sds_summer/data/food.json")

## ------------------------------------------------------------------------
food$ingredients = lapply(food$ingredients, 
                          FUN=tolower)
food$ingredients = lapply(food$ingredients, 
                          FUN=function(x) 
                            gsub("-", "_", x))  
food$ingredients = lapply(food$ingredients, 
                          FUN=function(x) 
                            gsub("[^a-z0-9_ ]", "", x))

## ------------------------------------------------------------------------
library("tm")
combi_ingredients = c(Corpus(VectorSource(food$ingredients)),
                      Corpus(VectorSource(food$ingredients)))
combi_ingredients = tm_map(combi_ingredients, stemDocument,
                           language="english")
combi_ingredientsDTM = DocumentTermMatrix(combi_ingredients)
combi_ingredientsDTM = removeSparseTerms(combi_ingredientsDTM, 0.99)
combi_ingredientsDTM = as.data.frame(
  as.matrix(combi_ingredientsDTM))
combi = combi_ingredientsDTM
combi_ingredientsDTM$cuisine = as.factor(
  c(food$cuisine, rep("italian", nrow(food))))

## ------------------------------------------------------------------------
trainDTM  = combi_ingredientsDTM[1:nrow(food), ]
testDTM = combi_ingredientsDTM[-(1:nrow(food)), ]

## ------------------------------------------------------------------------
library("rpart")
set.seed(1)
model = rpart(cuisine ~ ., data = trainDTM, 
              method = "class")
cuisine = predict(model, newdata = testDTM, 
                  type = "class")

## ----warning=FALSE, message = FALSE--------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "johnmyleswhite/ML_for_Hackers/"
branch = "master/"
link = "08-PCA/data/stock_prices.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)


## ---- echo = FALSE-------------------------------------------------------
p = ggplot(df %>% filter(Stock %in% unique(df$Stock)[1:6]), 
           aes(x = Date, y = Close))
p + geom_point(alpha = .1, color = "yellow") +
  facet_wrap(~ Stock, ncol = 3) +
  geom_smooth()

## ---- message = FALSE, warning = FALSE-----------------------------------
library("tidyr")
df.wide = df %>% spread(Stock, Close)
df.wide = df.wide %>% na.omit

## ---- message = FALSE, warning = FALSE-----------------------------------
pca = princomp(select(df.wide, -Date))

## ------------------------------------------------------------------------
market.index = predict(pca)[, 1]
market.index = data.frame(
  market.index = market.index, 
  Date = df.wide$Date)


## ------------------------------------------------------------------------
library("lubridate")
link = "08-PCA/data/DJI.csv"
data.link = paste0(gh.link, user.repo, branch, link)
dj = read_csv(data.link)
dj = dj %>% 
  filter(ymd(Date) > ymd('2001-12-31')) %>% 
  filter(ymd(Date) != ymd('2002-02-01')) %>% 
  select(Date, Close)
market.data = inner_join(market.index, dj)


## ---- echo = FALSE-------------------------------------------------------
p = ggplot(market.data, aes(x = market.index * (-1), y = Close))
p + geom_point(alpha = .1) + 
  geom_smooth(method = "lm") 

## ---- warning = FALSE----------------------------------------------------
market.data = market.data %>% 
  mutate(
    market.index = scale(market.index * (-1)),
    Close = scale(Close))
market.data = market.data %>% 
  gather(index, value, -Date)

## ---- echo = FALSE-------------------------------------------------------
library("viridis")
p = ggplot(market.data, 
           aes(x = Date, y = value, group = index, colour = index))
p + geom_line() + 
  scale_color_viridis(discrete = TRUE)

