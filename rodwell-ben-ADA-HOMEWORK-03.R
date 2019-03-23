
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
  if ((n1 * p1 < 5) & (n1 * (1 - p0)< 5)) {
    warning("Assumption of normal distribution is not valid")
  }
  if (is.null(p2) | is.null(n2)){
    phat <- p1
    pi <- p0
    n <- n1
    z <- (phat - pi)/sqrt(pi * (1 - pi)/n)
    z
  } else {
    sum1 <- n1*p1
    sum2 <- n2*p2
    pstar <- (sum1 + sum2)/(n1 + n2)
    phat1 <- p1
    phat2 <- p2
    z <- (phat2 - phat1)/sqrt((pstar * (1 - pstar)) * (1/n1 + 1/n2))
    z
  }
  if(alternative == "two.sided" | is.null(alternative)){
    p <- (1 - pnorm(z,lower.tail = F))
    p.lower <- pnorm(z, lower.tail = T)
     p <- p  
  }
  if (alternative == "less" & (is.null(p2) | is.null(n2))){
    p <- (pnorm(z, lower.tail = T))
  }
  if (alternative == "greater" & (is.null(p2) | is.null(n2))){
    p <- (pnorm(z, lower.tail = F))
  }
  if (alternative == "less" & (!is.null(p2) | !is.null(n2))){
    p <- 1-(pnorm(z, lower.tail = T))
  }
  if (alternative == "greater" & (!is.null(p2) | !is.null(n2))){
    p <- 1-(pnorm(z, lower.tail = F))
  }

  if (is.null(p2) | is.null(n2)){ ## Confidence intervals
    alpha <- conf.level
    lower <- phat - qnorm((1-alpha)/2) * sqrt(phat * (1 - phat)/n1)
    upper <- phat + qnorm((1-alpha)/2) * sqrt(phat * (1 - phat)/n1)
    ci <- c(lower, upper)
    ci ## this is only for the one sample test
  } else {
    lower.ci <- -1 *((p2 - p1) + 1.96 * sqrt(((p1 * (1 - p1))/n1) + ((p2 * (1 - p2))/n2))) 
    upper.ci <- -1 *((p2 - p1) - 1.96 * sqrt(((p1 * (1 - p1))/n1) + ((p2 * (1 - p2))/n2)))
        ci <- c(lower.ci,upper.ci)
  }
  STATS <- list(P = p,Z =  z, Confidence_intervals = ci)
  return(STATS)
}


## Testing the code with examples from the module
## Single sample lower tailed test

```{r}
v<- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
v1 <- c(1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 
        1, 0)
v2 <- c(1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 
        0, 1, 1, 0, 1, 1, 1)

p <- mean(v)
n <- length(v)
p1<- mean(v1)
n1 <- length(v1)
p2 <- mean(v2)
n2 = length(v2)

mytest <- Z.prop.test(p1 = p, n1 = n, p0 = 0.8, conf.level = .95, alternative = "less")
proptest <- prop.test(x = sum(v), n = length(v), conf.level = .95, p = .8, alternative = "less", correct = F)

mytest
proptest
```

## Single sample upper tailed test
```{r}
mytest <- Z.prop.test(p1 = p, n1 = n, p0 = 0.4, conf.level = .95, alternative = "greater")
proptest <- prop.test(x = sum(v), n = length(v), conf.level = .95, p = .4, alternative = "greater", correct = F)

mytest
proptest
```

## Single sample two tailed test
```{r}
mytest <- Z.prop.test(p1 = p1, n1 = n1,
                      p0 = .8, conf.level = .95, alternative = "two.sided")

proptest <- prop.test(x = sum(v), n = length(v), conf.level = .95, p = .8, alternative = "two.sided", correct = F)

mytest
proptest
```

## Two sample lower tailed test
```{r}
mytest <- Z.prop.test(p1 = p1, n1 = n1, p2 = p2, n2 = n2,
                      p0 = 0, conf.level = .95, alternative = "less")

proptest <- prop.test(x = c(sum(v1), sum(v2)), n = c(length(v1), length(v2)), alternative = "less", 
                      correct = FALSE)

mytest
proptest
```

## Two sample upper tailed test
```{r}
mytest <- Z.prop.test(p1 = mean(v1), n1 = length(v1),p2 = mean(v2), n2 = length(v2),
                      p0 = 0, conf.level = .95, alternative = "greater")

proptest <- prop.test(x = c(sum(v1), sum(v2)), n = c(length(v1), length(v2)), alternative = "greater",
                      correct = FALSE)

mytest
proptest
```



## Two Sample two tailed test
```{r}
mytest <- Z.prop.test(p1 = mean(v1), n1 = length(v1),p2 = mean(v2), n2 = length(v2),
                      p0 = 0, conf.level = .95, alternative = "two.sided")

proptest <- prop.test(x = c(sum(v1), sum(v2)), n = c(length(v1), length(v2)), alternative = "two.sided", 
                      correct = FALSE)

mytest
proptest
```




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


summary(loglb)







## Identify and interpret the point estimate of the slope (??1),
## as well as the outcome of the test associated with the hypotheses
## H0: ??1=0; HA: ??1???0. Also, find a 90% CI for the slope (??1) parameter.

## Identify slope (??1) of first regression (raw data)
slope_lb <- lb$coefficients[2]
slope_lb

## summarize in a data frame
t1 <- coef(summary(lb))
t1 <- data.frame(unlist(t1))
colnames(t1) <- c("Est", "SE", "t", "p")
t1

## ID the p value of BETA1
t1$p[2]


## Calculate 90% confidence intervals

alpha <- 0.10
t1$lmCI <- confint(lb, level = 1 - alpha)  
attributes(t1$lmCI)



## Confidence Intervals of the point estimate
t1[, 5]



## Identify slope of first regression (log transformed data)
slope_loglb <- loglb$coefficients[2]
slope_loglb


## summarize in a data frame
t2 <- coef(summary(loglb))
t2 <- data.frame(unlist(t2))
colnames(t2) <- c("Est", "SE", "t", "p")
t2

## ID p value of BETA1
t2$p[2]

## Calculate 90% confidence intervals
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




g1 <- ggplot(data = d, aes(x = x1, y = y1)) + geom_point() +
  ggtitle("Linear Model of Primate Longevity and Brain Size") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Species Mean Brain Size (g)") + ylab ("Species Maximum Lifespan (months)") +
  annotate("text", label = "y = 1.218 * x + 248.952", x = 125, y = 850, size = 6) +
  geom_point(alpha = 0.5) +
  geom_line(aes(x = x1, y = dflb$CIfit, color = "Line of best fit")) +
  geom_line(aes(x = x1, y = dflb$CIlwr, color = "90% CI")) +
  geom_line(aes(x = x1, y = dflb$CIupr, color = "90% CI",)) +
  geom_line(aes(x = x1, y = lwr, color = "90% PI")) +
  geom_line(aes(x = x1, y = upr, color = "90% PI")) +
  guides(color=guide_legend(title=""))
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



g2 <- ggplot(data = d, aes(x = x2, y = y2)) + geom_point() +
  ggtitle("Linear Model of Primate log(Longevity) and log(Brain Size)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("log(Species Mean Brain Size (g))") +
  ylab ("log(Species Maximum Lifespan (months))") +
  annotate("text", label = "y = 0.2341 * x + 4.879", x = 1.5, y = 6.4, size = 6) +
  geom_point(alpha = 0.5) + geom_line(aes(x = x2, y = lwrlog, color = "90% PI")) +
  geom_line(aes(x = x2, y = uprlog, color = "90% PI")) +
  geom_line(aes(x = x2, y = dfloglb$CIfit, color = "Line of Best Fit")) +
  geom_line(aes(x = x2, y = dfloglb$CIlwr, color = "90% CI")) +
  geom_line(aes(x = x2, y = dfloglb$CIupr, color = "90% CI")) +
  guides(color=guide_legend(title=""))
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




g1 <- ggplot(data = d, aes(x = x1, y = y1)) + geom_point() +
  ggtitle("Linear Model of Primate Longevity and Brain Size") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Species Mean Brain Size (g)") + ylab ("Species Maximum Lifespan (months)") +
  annotate("text", label = "y = 1.218 * x + 248.952", x = 125, y = 1100, size = 6) +
  geom_point(alpha = 0.5) +
  geom_line(aes(x = x1, y = dflb$CIfit, color = "Line of best fit")) +
  geom_line(aes(x = x1, y = dflb$CIlwr, color = "90% CI")) +
  geom_line(aes(x = x1, y = dflb$CIupr, color = "90% CI",)) +
  geom_line(aes(x = x1, y = lwr, color = "90% PI")) +
  geom_line(aes(x = x1, y = upr, color = "90% PI")) +
  guides(color=guide_legend(title="")) +
  xlim(0, 850) + ylim(0, 1300) +
  geom_point(x = 800, y = longest1, color = "purple", shape = 13)
g1




g2 <- ggplot(data = d, aes(x = x2, y = y2)) + geom_point() +
  ggtitle("Linear Model of Primate log(Longevity) and log(Brain Size)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("log(Species Mean Brain Size (g))") +
  ylab ("log(Species Maximum Lifespan (months))") +
  annotate("text", label = "y = 0.2341 * x + 4.879", x = 4, y = 6.8, size = 6) +
  geom_point(alpha = 0.5) + geom_line(aes(x = x2, y = lwrlog, color = "90% PI")) +
  geom_line(aes(x = x2, y = uprlog, color = "90% PI")) +
  geom_line(aes(x = x2, y = dfloglb$CIfit, color = "Line of Best Fit")) +
  geom_line(aes(x = x2, y = dfloglb$CIlwr, color = "90% CI")) +
  geom_line(aes(x = x2, y = dfloglb$CIupr, color = "90% CI")) +
  guides(color=guide_legend(title="")) +
  xlim(3, 7) + ylim (5, 7) +
  geom_point(x = log(800), y = longest2, color = "purple", shape = 13)
g2

summary(lb)
summary(loglb)
boxplot(d$Brain_Size_Species_Mean)
boxplot(d$MaxLongevity_m)

boxplot(log(d$Brain_Size_Species_Mean))
boxplot(log(d$MaxLongevity_m))





















## Code for prop test function
```{r}
Z.prop.test <- function(p1, n1, p2 = NULL,
                        n2 = NULL, p0, alternative = "two.sided",
                        conf.level = 0.95){
  if ((n1 * p1 < 5) & (n1 * (1 - p0)< 5)) {
    warning("Assumption of normal distribution is not valid")
  }
  if (is.null(p2) | is.null(n2)){
    phat <- p1
    pi <- p0
    n <- n1
    z <- (phat - pi)/sqrt(pi * (1 - pi)/n)
    z
  } else {
    sum1 <- n1*p1
    sum2 <- n2*p2
    pstar <- (sum1 + sum2)/(n1 + n2)
    phat1 <- p1
    phat2 <- p2
    z <- (phat2 - phat1)/sqrt((pstar * (1 - pstar)) * (1/n1 + 1/n2))
    z
  }
  if(alternative == "two.sided" | is.null(alternative) & !is.null(p2)){
    p.upper <- (1 - pnorm(z,lower.tail = T))
    p.lower <- pnorm(z, lower.tail = F)
    p <- p.upper + p.lower  
  }
  if (alternative == "less" & (is.null(p2) | is.null(n2))){
    p <- (pnorm(z, lower.tail = T))
  }
  if (alternative == "greater" & (is.null(p2) | is.null(n2))){
    p <- (pnorm(z, lower.tail = F))
  }
  if (alternative == "less" & (!is.null(p2) | !is.null(n2))){
    p <- (pnorm(z, lower.tail = T))
  }
  if (alternative == "greater" & (!is.null(p2) | !is.null(n2))){
    p <- (pnorm(z, lower.tail = F))
  }
  
  if (is.null(p2) | is.null(n2)){ ## Confidence intervals
    alpha <- conf.level
    lower <- phat + qnorm((1-alpha)/2) * sqrt(phat * (1 - phat)/n1)
    upper <- phat - qnorm((1-alpha)/2) * sqrt(phat * (1 - phat)/n1)
    ci <- c(lower, upper)
    ci ## this is only for the one sample test
  } else {
    lower.ci <- ((p2 - p1) - 1.96 * sqrt(((p1 * (1 - p1))/n1) + ((p2 * (1 - p2))/n2))) 
    upper.ci <- ((p2 - p1) + 1.96 * sqrt(((p1 * (1 - p1))/n1) + ((p2 * (1 - p2))/n2)))
    ci <- c(lower.ci,upper.ci)
  }
  STATS <- list(P_value = p,Z_score =  z, Confidence_intervals = ci)
  return(STATS)
}
```

## Testing the code with examples
```{r}
v<- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
v1 <- c(1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 
        1, 0)
v2 <- c(1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 
        0, 1, 1, 0, 1, 1, 1)

p <- mean(v)
n <- length(v)
p1<- mean(v1)
n1 <- length(v1)
p2 <- mean(v2)
n2 = length(v2)
```
## Single sample lower tailed test
```{r}
mytest <- Z.prop.test(p1 = p, n1 = n, p0 = 0.8, conf.level = .95, alternative = "less")
proptest <- prop.test(x = sum(v), n = length(v), conf.level = .95, p = .8, alternative = "less", correct = F)

mytest
proptest
```

## Single sample upper tailed test
```{r}
mytest <- Z.prop.test(p1 = p, n1 = n, p0 = 0.4, conf.level = .95, alternative = "greater")
proptest <- prop.test(x = sum(v), n = length(v), conf.level = .95, p = .4, alternative = "greater", correct = F)

mytest
proptest
```

## Single sample two tailed test
```{r}
mytest <- Z.prop.test(p1 = p1, n1 = n1, p0 = .8, conf.level = .95, alternative = "two.sided")

proptest <- prop.test(x = sum(v), n = length(v), conf.level = .95, p = .8,
                      alternative = "two.sided", correct = F)

mytest
proptest     #### These still do not match ????????
```

## Two sample lower tailed test
```{r}
mytest <- Z.prop.test(p1 = p1, n1 = n1, p2 = p2, n2 = n2,
                      p0 = 0, conf.level = .95, alternative = "less")

proptest <- prop.test(x = c(sum(v2), sum(v1)), n = c(length(v2), length(v1)), alternative = "less", 
                      correct = FALSE)

mytest
proptest  
```

## Two sample upper tailed test
```{r}
mytest <- Z.prop.test(p1 = mean(v1), n1 = length(v1),p2 = mean(v2), n2 = length(v2),
                      p0 = 0, conf.level = .95, alternative = "greater")

proptest <- prop.test(x = c(sum(v2), sum(v1)), n = c(length(v2), length(v1)), alternative = "greater",
                      correct = FALSE)

mytest
proptest
```

## Two Sample two tailed test
```{r}
mytest <- Z.prop.test(p1 = mean(v), n1 = length(v1),p2 = mean(v2), n2 = length(v2),
                      p0 = 0, conf.level = .95, alternative = "two.sided")

proptest <- prop.test(x = c(sum(v2), sum(v1)), n = c(length(v2), length(v1)), alternative =     "two.sided", correct = FALSE)

mytest
proptest
```
