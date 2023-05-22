pacman::p_load(tidyverse,magrittr,estimatr,modelsummary,stargazer,gt,gtsummary,broom)
options(digits=3,scientific = FALSE)
options(scipen=100) # 指数表記を回避するため

coin <- c(1,0)
z <- sample(coin,100,replace = TRUE)
sum(z)


S <- 100000
rec <- numeric(S)
coin <- c(1,0)
for(i in 1:S){
  z <- sample(coin,100,replace = TRUE)
  rec[i] <- sum(z)
}


hist(rec)

count <- (rec==50)
sum(count)
mean(count)

X <- rnorm(100000,50,10)
Y <- rnorm(100000,50,10)
cor(X,Y)

Z <- -((X-50)^2)/10
plot(X,Z)



curve(pnorm(x,50,10),0,100)


tempdata <- read_csv("R_EmpiricalAnalysis_csv/chap03/temperature.csv")
tempdata %<>% 
  mutate(test = temp*2)

tempdata <-
  mutate(tempdata, test4 = temp)

tempdata %<>% 
  filter(temp<5) %>% 
  mutate(test3 = temp*2) 


fdata <- Fatalities

a3 <- lm(fatal ~ income + drinkage, data = fdata) 

fdata %$% 
  lm_robust(fatal ~ income + drinkage) -> a2


fdata %$%
  lm_robust(fatal~income+drinkage,fixed_effects = state)  -> a1


tbl_regression(a1,a2)


models <- list(model1 = a1,
               model2 = a2, 
               model3 =a3)

modelsummary(models, 
             statistic = 'statistic',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             title = 'This is the title',
             gof_map = c("nobs", "r.squared",'adj.r.squared','fes'),
             add_rows = tibble("term" = "FE",
                               "value" = 'yes','no','no1'),
             )
f <- \(x) format(x$fes)
ar <- data.frame("FE", lapply(models, f))
models$fes

modelsummary(models, 
             statistic = 'statistic',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             title = 'This is the title',
             gof_map = c("nobs", "r.squared",'adj.r.squared','fes'),
             add_rows = ar)

models2 <- list()
fdata %$%
  lm_robust(fatal~income+drinkage,fixed_effects = state)  -> models2[['a11']] 

fdata %$% 
  lm_robust(fatal ~ income + drinkage) -> models2[['a22']] 

modelsummary(models2)


library(modelsummary)
library(estimatr)

set.seed(03222022)
N <- 10^4
x <- rnorm(N)
y <- 0.000002*x + rnorm(N)

clusters <- sample(1:1000, N, replace = TRUE)
z <- rnorm(N)
df <- cbind.data.frame(y, x, z, clusters)

models <- list(
  lm_robust(y ~ x, clusters = clusters, data = df),
  lm_robust(y ~ z, clusters = clusters, data = df))

f <- function(x) format(x$nclusters)
ar <- data.frame("Number of Clusters", lapply(models, f))
modelsummary(models,
             add_rows = ar,
             output = "markdown")

a1 %>% 
  tbl_regression()
