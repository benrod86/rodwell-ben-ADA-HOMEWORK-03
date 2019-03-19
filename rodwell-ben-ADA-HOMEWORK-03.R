########## Module 13 and 14 ##########
##### Module 13 Hypothesis Testing #####
library(tidyverse)
library(readr)

## How do we calculate the p value?
## 1. Specify the statistic we want to evaluate (e.g., the mean)
## 2. Specify our null distribution and a test statistic (e.g., Z or T) 
## 3. Calculate the tail probability, i.e., the probability of
##    obtaining a statistic (e.g., a mean) as or more extreme
##    than was observed assuming that null distribution


## One sample means T and Z tests

f <- "https://raw.githubusercontent.com/difiore/ADA-2019/master/vervet-weights.csv"
d <- read_csv(f, col_names = TRUE)
head(d)

## 1. What is our H0?
## 2. What is our HA?
## 3. What is the hypothesis we want to test?
##    Is it two-tailed? Upper-tailed? Lower-tailed?
## 4. Calculate the mean, standard deviation, and SEM of the sample
## 5. Plot a histogram of the sample


mu <- 4.9 ## mean of comparison sample
x <- d$weight
m <- mean(x)
s <- sd(x)
n <- length(d$weight)
sem <- s/sqrt(n)
sem

hist(x)


## 1. Specify the statistic we want to evaluate (e.g., the mean)
mu <- 4.9 

## 2. Specify our null distribution and a test statistic (e.g., Z or T)
z <- (m-mu)/(s/sqrt(n)) 
z

## 3. Calculate the tail probability, i.e., the probability of
##    obtaining a statistic (e.g., a mean) as or more extreme
##    than was observed assuming that null distribution

p <- 1-pnorm(z)
p

## since we hypothesize that our sample is larger,
## we test to make sure it is outside of the lower tail
p <- pnorm(z, lower.tail = F)
p


## BUT since sample is less than 30 this is likely a T distribution
## have to use T dist to test, and take degrees of freedom into account
p <- 1 - pt(z, df = n-1)
p

## make sure outside of lower tail
p <- pt(z, df = n-1, lower.tail = F)
p

## Calculate 95% confidence intervals
crit <- qt(0.95, df = n - 1)
crit

ci <- m + c(-1, 1) * crit * sem
ci ## the comparative sample mean falls outside of this!
m

### Two tailed test
## Comparison population has mean of 7.3
## We want to know if our sample is likely to be of similar makeup
library(readr)
library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ADA-2019/master/woolly-weights.csv"
d <- read_csv(f, col_names = TRUE)
head(d)

mu <- 7.2
x <- d$weight
m <- mean(x)
s <- sd(x)
n <- length(x)
sem <- s/sqrt(n)

## Calculate T score
t <- (m-mu)/(s/sqrt(n))
t
t <- (m-mu)/sem
t

## Calculate P value
p.upper <- 1 - pt(abs(t), df = n-1)
p.lower <- pt(-1 * abs(t), df = n-1)
p <- p.upper + p.lower
p





##### Two Sample tests

## Unequal variance
f <- "https://raw.githubusercontent.com/difiore/ADA-2019/master/colobus-weights.csv"
d <- read_csv(f, col_names = TRUE)
head(d)

x <- d$weight[d$sex == "male"]
y <- d$weight[d$sex == "female"]
m1 <- mean(x)
m2 <- mean(y)
s1 <- sd(x)
s2 <- sd(y)
n1 <- length(x)
n2 <- length(y)
sem1 <- s1/sqrt(n1)
sem2 <- s2/sqrt(n1)

par(mfrow = c(1,2))
boxplot(x, ylim = c(4,8))
boxplot(y, ylim = c(4,8))
## variance is prob not equal

## Calculate T statistic
## need to know Degrees of Freedom
df <- (s1^2/n1 + s2^2/n2)^2/((s1^2/n1)^2/(n1 - 1) + (s2^2/n2)^2/(n2 - 1))
df


## Now the T score
t <- (m2-m1)/sqrt(s1^2/n1 + s2^2/n2)
t

## ID the critical values
alpha <- 0.05
crit <- qt(1-alpha/2, df = df)
crit





############# Homework ###############
############  PROBLEM 1 ##############
## Write a simple R function you call Z.prop.test(), that can
## perform one- or two-sample Z-tests for proportion data,
## using the following guidelines.

## Your function should take the following arguments: p1 and n1
## (no default) representing the estimated proportion and sample
## size (e.g., based on your sample data); p2 and n2 (both defaulting
## to NULL) that contain a second sample's proportion and sample
## size data in the event of a two-sample test; p0 (no default)
## as the expected value for the population proportion; and
## alternative (default "two.sided") and conf.level (default 0.95),
## to be used in the same way as in the function t.test().

## When conducting a two-sample test, it should be p1 that is
## tested as being smaller or larger than p2 when alternative="less"
## or alternative="greater", respectively, the same as in the use
## of x and y in the function t.test().

## The function should perform a one-sample Z-test using p1, n1,
## and p0 if either p2 or n2 (or both) is NULL.

## The function should contain a check for the rules of thumb we
## have talked about (n???>5 and n?(1?????)>5) to ensure the validity
## of assuming the normal distribution in both the one- and
## two-sample settings. If this is violated, the function should
## still complete, but it should also print an appropriate warning message.

## The function should return a list containing the following
## elements: Z (the test statistic), P (the appropriate p value),
## and CI (the two-sided CI with respect to "conf.level" around p1
##in the case of a one-sample test and around p2-p1 in the case of
## a two-sample test). For all test alternatives ("two.sided",
## "greater", "less"), calculate symmetric CIs based on quantiles
## of the normal distribution rather than worrying about calculating
## single-limit confidence bounds.


##### Start Problem 1 #####
Z.prop.test <- function(p1, n1, p2 = NULL,
                        n2 = NULL, p0, alternative = "two.sided",
                        conf.level = 0.95){
  m1 <- mean(p1)
  m2 <- mean(p2)
  s1 <-sd(p1)
  s2 <- sd(p2)
  sem1 <- s1/sqrt(n1)
  sem2 <- s2/sqrt(n2)
  return(sem1)
}






############  PROBLEM 2 ##############
## The comparative primate dataset we have used from Kamilar
## and Cooper has in it a large number of variables related to 
## life history and body size. For this exercise, the end aim is
## to fit a simple linear regression model to predict longevity
## (MaxLongevity_m) measured in months from species' brain size
## (Brain_Size_Species_Mean) measured in grams. Do the following 
## for both longevity~brain size and log(longevity)~log(brain size).

## Fit the regression model and, using {ggplot2}, produce a scatterplot
## with the fitted line superimposed upon the data. Append the the
## fitted model equation to your plot (HINT: use the function geom_text()).

## Identify and interpret the point estimate of the slope (??1),
## as well as the outcome of the test associated with the hypotheses
## H0: ??1=0; HA: ??1???0. Also, find a 90% CI for the slope (??1) parameter.

## Using your model, add lines for the 90% confidence and prediction
## interval bands on the plot, and add a legend to differentiate
## between the lines.

## Produce a point estimate and associated 90% prediction interval
## for the longevity of a species whose brain weight is 800 gm.
## Do you trust the model to predict observations accurately for
## this value of the explanatory variable? Why or why not?
## Looking at your two models, which do you think is better? Why?


##### START PROBLEM 2 #####
library(tidyverse)
library(readr)
f <- "https://raw.githubusercontent.com/difiore/ADA-2019/master/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE)
head(d)



## Create vectors for Longevity and Brain Size
x1 <- d$Brain_Size_Species_Mean
y1 <- d$MaxLongevity_m

## Create vectors for log(Longevity) and log(Brain size)
x2 <- log(x1)
y2 <- log(y1)

## Linear regression for Longevity and Brain Size
lb <- lm(data = d,  y1 ~ x1)
lb
## Linear regression forlog transformed Longevity and Brain Size
loglb <- lm(data = d, y2 ~ x2) 
loglb

## Fit the regression model and, using {ggplot2}, produce a scatterplot
## with the fitted line superimposed upon the data. Append the the
## fitted model equation to your plot (HINT: use the function geom_text()).

## Scatterplots of each model
### Raw data
g1 <- ggplot(data = d, aes(x = x1, y = y1))
g1 <- g1 + geom_point()
g1 <- g1 + ggtitle("Linear Model of Primate Longevity and Brain Size") +
  theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + xlab ("Species Mean Brain Size (g)")
g1 <- g1 + ylab ("Species Maximum Lifespan (months)")
g1 <- g1 + geom_smooth(method = "lm", formula = y ~ x, col = "purple")
g1 <- g1 + annotate("text", label = "y = 1.218 * x + 248.952", x = 100, y = 800, size = 6)
g1

g1 <- ggplot(data = d, aes(x = x1, y = y1)) + geom_point() +
  ggtitle("Linear Model of Primate Longevity and Brain Size") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Species Mean Brain Size (g)") +
  ylab ("Species Maximum Lifespan (months)") +
  geom_smooth(method = "lm", formula = y ~ x, col = "purple") +
  annotate("text", label = "y = 1.218 * x + 248.952", x = 100, y = 800, size = 6)
g1


### Log transformed data
g2 <- ggplot(data = d, aes(x = x2, y = y2))
g2 <- g2 + geom_point()
g2 <- g2 + ggtitle("Linear Model of log(Primate Longevity) and log(Brain Size)") +
  theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + xlab ("Species Mean Brain Size (log(g))")
g2 <- g2 + ylab ("Species Maximum Lifespan (log(months))")
g2 <- g2 + geom_smooth(method = "lm", formula = y ~ x, col = "magenta")
g2 <- g2 + annotate("text", label = "y = 0.2341 * x + 4.879", x = 1.5, y = 6.4, size = 6)
g2

g2 <- ggplot(data = d, aes(x = x2, y = y2)) + geom_point() +
  ggtitle("Linear Model of log(Primate Longevity) and log(Brain Size)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Species Mean Brain Size (log(g))") +
  ylab ("Species Maximum Lifespan (log(months))") +
  geom_smooth(method = "lm", formula = y ~ x, col = "magenta") +
  annotate("text", label = "y = 0.2341 * x + 4.879", x = 1.5, y = 6.4, size = 6)
g2

summary(lb)
summary(loglb)







## Identify and interpret the point estimate of the slope (??1),
## as well as the outcome of the test associated with the hypotheses
## H0: ??1=0; HA: ??1???0. Also, find a 90% CI for the slope (??1) parameter.

### Identify slope (??1) of first regression (raw data)
slope_lb <- lb$coefficients[2]
slope_lb


###Calculate 90% confidence intervals
t1 <- coef(summary(lb))
t1 <- data.frame(unlist(t1))
colnames(t1) <- c("Est", "SE", "t", "p")
t1


alpha <- 0.10
t1$lmCI <- confint(lb, level = 1 - alpha)  
attributes(t1$lmCI)



## Confidence Intervals of the point estimate
t1[, 5]

##########


### Identify slope (??1) of first regression (log transformed data)
slope_loglb <- loglb$coefficients[2]
slope_loglb


###Calculate 90% confidence intervals
t2 <- coef(summary(loglb))
t2 <- data.frame(unlist(t2))
colnames(t2) <- c("Est", "SE", "t", "p")
t2



alpha <- 0.10
t2$lmCI <- confint(loglb, level = 1 - alpha)  
attributes(t2$lmCI)



## Confidence Intervals for the point estimate
t2[, 5]





## Using your model, add lines for the 90% confidence and prediction
## interval bands on the plot, and add a legend to differentiate
## between the lines.


h_hatlb <- predict(lb, newdata = data.frame(brain = d$Brain_Size_Species_Mean))
dflb <- data.frame(cbind(d$Brain_Size_Species_Mean, d$MaxLongevity_m, h_hatlb))
names(dflb) <- c("x", "y", "yhat")
head(dflb)

cilb <- predict(lb, newdata = data.frame(brain = d$Brain_Size_Species_Mean), 
                interval = "confidence", level = 0.90)
head(cilb)


dflb <- cbind(dflb, cilb)
names(dflb) <- c("x", "y", "yhat", "CIfit", "CIlwr", "CIupr")
head(dflb)

pi <- predict(lb, newdata = data.frame(brain = d$Brain_Size_Species_Mean),
              interval = "prediction", 
              level = 0.90)  # for a vector of values
head(pi)
lwr <- pi[,2]
upr <- pi[,3]


g1 <- ggplot(data = d, aes(x = x1, y = y1))
g1 <- g1 + geom_point()
g1 <- g1 + ggtitle("Linear Model of Primate Longevity and Brain Size") +
  theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + xlab ("Species Mean Brain Size (g)")
g1 <- g1 + ylab ("Species Maximum Lifespan (months)")
g1 <- g1 + annotate("text", label = "y = 1.218 * x + 248.952", x = 125, y = 800, size = 6)
g1 <- g1 + geom_point(alpha = 0.5)
g1 <- g1 + geom_line(aes(x = x1, y = dflb$CIfit, color = "green"))
g1 <- g1 + geom_line(aes(x = x1, y = dflb$CIlwr, color = "blue"))
g1 <- g1 + geom_line(aes(x = x1, y = dflb$CIupr, color = "blue",))
g1 <- g1 + geom_line(aes(x = x1, y = lwr, color = "red"))
g1 <- g1 + geom_line(aes(x = x1, y = upr, color = "red"))
g1 <- g1 + theme(legend.position = "right")
g1 <- g1 + scale_fill_continuous(labels  = paste("Legend","Prediction Interval",
                                                 "Best Fit Line", "90% Confidence Intervals" ))
g1



h_hatloglb <- predict(loglb, newdata = data.frame(brain = d$Brain_Size_Species_Mean))
dfloglb <- data.frame(cbind(d$Brain_Size_Species_Mean, d$MaxLongevity_m, h_hatloglb))
names(dfloglb) <- c("x", "y", "yhat")
head(dfloglb)

ciloglb <- predict(loglb, newdata = data.frame(brain = d$Brain_Size_Species_Mean), 
                   interval = "confidence", level = 0.90)
head(ciloglb)


dfloglb <- cbind(dfloglb, ciloglb)
names(dfloglb) <- c("x", "y", "yhat", "CIfit", "CIlwr", "CIupr")
head(dfloglb)

pilog <- predict(loglb, newdata = data.frame(brain = d$Brain_Size_Species_Mean),
                 interval = "prediction", 
                 level = 0.90)  # for a vector of values
head(pilog)
lwrlog <- pilog[,2]
uprlog <- pilog[,3]



g2 <- ggplot(data = d, aes(x = x2, y = y2))
g2 <- g2 + geom_point()
g2 <- g2 + ggtitle("Linear Model of Primate log(Longevity) and log(Brain Size)") +
  theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + xlab ("log(Species Mean Brain Size (g))")
g2 <- g2 + ylab ("log(Species Maximum Lifespan (months))")
g2 <- g2 + annotate("text", label = "y = 0.2341 * x + 4.879", x = 1.8, y = 6.4, size = 6)
g2 <- g2 + geom_point(alpha = 0.5)
g2 <- g2 + geom_line(aes(x = x2, y = lwrlog, color = "red"))
g2 <- g2 + geom_line(aes(x = x2, y = uprlog, color = "red"))
g2 <- g2 + geom_line(aes(x = x2, y = dfloglb$CIfit, color = "green"))
g2 <- g2 + geom_line(aes(x = x2, y = dfloglb$CIlwr, color = "blue"))
g2 <- g2 + geom_line(aes(x = x2, y = dfloglb$CIupr, color = "blue"))
g2 <- g2 + theme(legend.position = "right")
g2 <- g2 + scale_fill_continuous(labels  = paste("Legend","Prediction Interval",
                                                 "Best Fit Line", "90% Confidence Intervals" ))
g2





## Produce a point estimate and associated 90% prediction interval
## for the longevity of a species whose brain weight is 800 gm.
## Do you trust the model to predict observations accurately for
## this value of the explanatory variable? Why or why not?
## Looking at your two models, which do you think is better? Why?

### Raw data preditction
longest1 <- t1$Est[2] * 800 + t1$Est[1]
longest1



piest1 <- predict(lb, newdata = data.frame(x1 = 800), interval = "prediction", 
                  level = 0.90)  # for a single value
piest1


### Log transformed data prediction
longest2 <- t2$Est[2] * log(800) + t2$Est[1]
longest2



piest2 <- predict(loglb, newdata = data.frame(x2 = log(800)), interval = "prediction", 
                  level = 0.90)  # for a single value
piest2
s


