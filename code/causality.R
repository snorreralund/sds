
## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "kosukeimai/qss/"
branch = "master/"
link = "CAUSALITY/resume.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)


## ------------------------------------------------------------------------
library("dplyr")
df.table = df %>% 
  count(race, call)


## ------------------------------------------------------------------------
library("dplyr")
df.table = df %>% 
  group_by(race, call) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


## ------------------------------------------------------------------------
library("dplyr")
df.table = df %>% 
  group_by(race, sex, call) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


## ------------------------------------------------------------------------
lin.model = lm(call ~ race == "black", 
               data = df)

## ---- results = "asis", echo = FALSE-------------------------------------
stargazer::stargazer(lin.model, header = FALSE,
                     no.space = TRUE)

## ---- message = FALSE, warning = FALSE-----------------------------------
gh.link = "https://raw.githubusercontent.com/"
user.repo = "johnmyleswhite/ML_for_Hackers/"
branch = "master/02-Exploration/"
link = "data/01_heights_weights_genders.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)


## ---- echo = FALSE-------------------------------------------------------
library("ggplot2")
library("viridis")
ggplot(df, aes(x = Height, fill = Gender)) + 
  geom_density(alpha = .5) +
  scale_fill_viridis(discrete = TRUE)

## ---- echo = FALSE-------------------------------------------------------
ggplot(sample_frac(df, .5), aes(x = Weight, y = Height)) + 
  geom_point(alpha = .25) + 
  geom_smooth()

## ---- echo = FALSE-------------------------------------------------------
ggplot(sample_frac(df, .5), aes(x = Weight, y = Height, 
               colour = Gender)) + 
  geom_point(alpha = .1) +
  scale_color_viridis(discrete = TRUE)

## ------------------------------------------------------------------------
df = df %>% mutate(gender = Gender == "Male")
logit.model = glm(gender ~ Height + Weight, 
                  data = df, 
                  family = binomial(link = "logit"))

## ---- echo = FALSE-------------------------------------------------------
buildPoly <- function(xr, yr, slope = 1, intercept = 0, above = TRUE){
    #Assumes ggplot default of expand = c(0.05,0)
    xrTru <- xr + 0.05*diff(xr)*c(-1,1)
    yrTru <- yr + 0.05*diff(yr)*c(-1,1)

    #Find where the line crosses the plot edges
    yCross <- (yrTru - intercept) / slope
    xCross <- (slope * xrTru) + intercept

    #Build polygon by cases
    if (above & (slope >= 0)){
        rs <- data.frame(x=-Inf,y=Inf)
        if (xCross[1] < yrTru[1]){
            rs <- rbind(rs,c(-Inf,-Inf),c(yCross[1],-Inf))
        }
        else{
            rs <- rbind(rs,c(-Inf,xCross[1]))
        }
        if (xCross[2] < yrTru[2]){
            rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,Inf))
        }
        else{
            rs <- rbind(rs,c(yCross[2],Inf))
        }
    }
    if (!above & (slope >= 0)){
        rs <- data.frame(x= Inf,y= -Inf)
        if (xCross[1] > yrTru[1]){
            rs <- rbind(rs,c(-Inf,-Inf),c(-Inf,xCross[1]))
        }
        else{
            rs <- rbind(rs,c(yCross[1],-Inf))
        }
        if (xCross[2] > yrTru[2]){
            rs <- rbind(rs,c(yCross[2],Inf),c(Inf,Inf))
        }
        else{
            rs <- rbind(rs,c(Inf,xCross[2]))
        }
    }
    if (above & (slope < 0)){
        rs <- data.frame(x=Inf,y=Inf)
        if (xCross[1] < yrTru[2]){
            rs <- rbind(rs,c(-Inf,Inf),c(-Inf,xCross[1]))
        }
        else{
            rs <- rbind(rs,c(yCross[2],Inf))
        }
        if (xCross[2] < yrTru[1]){
            rs <- rbind(rs,c(yCross[1],-Inf),c(Inf,-Inf))
        }
        else{
            rs <- rbind(rs,c(Inf,xCross[2]))
        }
    }
    if (!above & (slope < 0)){
        rs <- data.frame(x= -Inf,y= -Inf)
        if (xCross[1] > yrTru[2]){
            rs <- rbind(rs,c(-Inf,Inf),c(yCross[2],Inf))
        }
        else{
            rs <- rbind(rs,c(-Inf,xCross[1]))
        }
        if (xCross[2] > yrTru[1]){
            rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,-Inf))
        }
        else{
            rs <- rbind(rs,c(yCross[1],-Inf))
        }
    }

    return(rs)
}

datPoly1 <- buildPoly(range(df$Height), range(df$Weight),
            slope= - coef(logit.model)[2] / coef(logit.model)[3],
            intercept= - coef(logit.model)[1] /coef(logit.model)[2],
            above = FALSE)
datPoly2 <- buildPoly(range(df$Height), range(df$Weight),
            slope= - coef(logit.model)[2] / coef(logit.model)[3],
            intercept= - coef(logit.model)[1] /coef(logit.model)[2],
            above = TRUE)
ggplot(sample_frac(df, .5), aes(x = Height, y = Weight)) + 
  geom_point(alpha = 0.2) +
  geom_abline(
    intercept = - coef(logit.model)[1] /coef(logit.model)[2],
    slope = - coef(logit.model)[2] / coef(logit.model)[3],
    color = "black") +
  geom_polygon(data=datPoly1, aes(x=x,y=y),alpha=0.2,fill="red") +
  geom_polygon(data=datPoly2, aes(x=x,y=y),alpha=0.2,fill="blue")

## ---- echo = FALSE-------------------------------------------------------
df$prediction = logit.model$fitted.values
df$prediction.cat = ifelse(df$prediction >= .5, "Male", "Female")
df$classified = ifelse(df$prediction.cat == df$Gender, "correct", "incorrect")

df.1 = df %>% filter(classified == "correct") %>% sample_frac(.2)
df.2 = df %>% filter(classified != "correct") %>% sample_frac(.75)

ggplot() + 
  geom_point(data = df.1,
             aes(x = Height, y = Weight),
             alpha = .05) +
  geom_point(data = df.2,
             aes(x = Height, y = Weight,
             fill = abs(prediction - .5)), 
             shape = 21, color = "black",
             alpha = .5) +
  geom_abline(
    intercept = - coef(logit.model)[1] / coef(logit.model)[2],
    slope = - coef(logit.model)[2] / coef(logit.model)[3],
    color = "black") + 
  scale_fill_viridis()

## ------------------------------------------------------------------------
df.class = df %>% 
  mutate(
    pred.prob = predict(logit.model),
    pred.cat = ifelse(pred.prob >= .5, 
                      "Male", "Female"),
    classified = ifelse(prediction.cat == Gender, 
                        "correct", "incorrect")
  ) %>% 
  group_by(classified) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


## ------------------------------------------------------------------------
df.class = df %>% 
  mutate(
    pred.prob = predict(logit.model),
    pred.cat = ifelse(pred.prob >= .5, 
                      "Male", "Female")
  ) %>% 
  group_by(Gender, pred.cat) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


## ------------------------------------------------------------------------
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/bball.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

## ------------------------------------------------------------------------
model.1 = lm(
  pts ~ height + weight + fg.pct + ft.pct, 
  data = df)

## ---- eval = FALSE-------------------------------------------------------
## library("stargazer")
## stargazer(model.1)

## ---- echo = FALSE, results = "asis"-------------------------------------
library("stargazer")
stargazer(model.1, header = FALSE, style = "ajps", no.space = TRUE,
          keep.stat = "n")

## ------------------------------------------------------------------------
library("broom")
output.1 = model.1 %>% tidy


## ---- echo = FALSE-------------------------------------------------------
output.1 = output.1 %>% 
  filter(term != "(Intercept)") 

## ------------------------------------------------------------------------
p = ggplot(output.1, aes(x = term, y = estimate))
p = p + geom_hline(aes(yintercept = 0),  size = 2, 
             colour = "white") +
  geom_point() +
  geom_errorbar(aes(ymin=estimate-2*std.error, 
                    ymax=estimate+2*std.error)) +
  coord_flip()

## ---- echo = FALSE-------------------------------------------------------
p 

## ------------------------------------------------------------------------
df = df %>% mutate(pts.high = pts > 13)
model.2 = glm(
  pts.high ~ height + weight + fg.pct + ft.pct, 
  data = df,
  family = binomial(link = "logit"))

## ------------------------------------------------------------------------
library("modelr")
df.plot = df %>% 
  data_grid(fg.pct = seq_range(fg.pct, 50), 
            .model = model.2) 
preds = predict(model.2,
                newdata = df.plot, 
                type = "response",
                se = TRUE)
df.plot$pred.full = preds$fit
df.plot$ymin = df.plot$pred.full - 2*preds$se.fit
df.plot$ymax = df.plot$pred.full + 2*preds$se.fit  


## ------------------------------------------------------------------------
p = ggplot(df.plot, aes(x = fg.pct, y = pred.full)) + 
    geom_ribbon(aes(y = pred.full, 
                    ymin = ymin, 
                    ymax = ymax),alpha = 0.25) +
    geom_line(color = "blue")

## ---- echo = FALSE-------------------------------------------------------
p +
  labs(x = "Field Goal Percentage",
       y = "Predicted Probability")

