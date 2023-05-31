
rm(list=ls()); gc();  gc(); 
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr,estimatr ,modelsummary,broom) 

simdata <- read_csv("R_EmpiricalAnalysis_csv/chap05/distributions.csv")
simdata %$% 
  t.test(distA,distB)

simdata %$% 
  t.test(distB)

t.test(simdata$distB)


X<-rnorm(1000,0,1)
Y<-1+5*X+rnorm(1000,0,1)　#E[XY]=1+5Xが成立する変数X,Yのモデル

beta1<-lm(Y~X)$coefficients[2]　#OLS推定値

names(lm(Y~X))　#lm(Y~X)に含まれている項目をすべて見る

S<-10000　#この試行を10000回繰り返す
beta1<-numeric(S)　#結果を保存する
for(i in 1:S)
{x<-rnorm(1000,0,1)
y<-1+5*x+rnorm(1000,0,1)
beta1[i]<-lm(y~x)$coefficients[2]}　#繰り返しの範囲指定
summary(beta1)　#結果の要約
sd(beta1)　#標準偏差を求める
hist(beta1)　#結果をヒストグラムに表示する



install.packages("dplyr")　#パッケージのインストール
library(dplyr)
wagedata <- read_csv("R_EmpiricalAnalysis_csv/chap05/wage.csv")
wagedata %>%
  lm(log(wage)~educ+exper,data=.) %>%　　#重回帰分析を実施
  summary()          #分析結果を出力

result<-wagedata %>%
  lm(log(wage)~educ+exper,data=.) 　　#重回帰分析を実施
summary(result)$coefficients #回帰係数に関するものを出力

summary(result)$coefficients[,1:2] #1列目と2列目を出力

#95％信頼区間を作る方法その１
betahat<-summary(result)$coefficients[,1]
sigma<-summary(result)$coefficients[,2]　#文字の設定
lower<-betahat-1.96*sigma
upper<-betahat-1.96*sigma　#信頼区間を設定


install.packages("broom")　#パッケージのインストール
install.packages("lubridate")　#パッケージのインストール
library(dplyr)
library(lubridate)
library(broom)
tempdata<-readr::read_csv("temperature_aug.csv") #読み込むデータの選択

#テキストp90以降と同様の重回帰分析
tempdata<-tempdata%>%　#変数の作成・加工
  mutate(daytime=1*(18>=time&time>=9),
         date=ymd(date),
         dow=wday(date,label=TRUE),
         sunday=1*(dow=="日"),
         recess=1*("2014-8-16">=date&date
                   >="2014-8-11"))
result<-tempdata%>%
  lm(elec~temp+daytime+prec+sunday+recess,
     data=.)

