library(haven)
library(foreign)
library(tidyverse)
setwd("~/Desktop/Ma Economics/Spring 20/CI/Data Project/PISA")

CY07_MSU_STU_QQQ <- read_sav("CY07_MSU_STU_QQQ.sav")
CY07_MSU_SCH_QQQ <- read_sav("CY07_MSU_SCH_QQQ.sav")
CY07_MSU_TCH_QQQ <- read_sav("CY07_MSU_TCH_QQQ.sav")

a <- CY07_MSU_TCH_QQQ %>% group_by(CNT, CNTSCHID, TC001Q01NA) %>% count(CNT)
a1 <- CY07_MSU_TCH_QQQ %>% group_by(CNT, CNTSCHID) %>% count(CNT)
b <- CY07_MSU_STU_QQQ %>% group_by(CNT, CNTSCHID, ST004D01T) %>% count(CNT, ST004D01T)
c <- CY07_MSU_STU_QQQ %>% group_by(CNT) %>% count(CNT)
CY07_MSU_STU_QQQ %>% group_by(CNT) %>% count(ST004D01T)
CY07_MSU_TCH_QQQ %>% group_by(CNT) %>% count(TC001Q01NA)

total <- merge(a,c,by=c("CNT","CNTSCHID"))
total %>% group_by(CNT,CNTSCHID, TC001Q01NA, ST004D01T )%>% count(CNT)

total %>% group_by(CNT) %>% count(CNTSCHID)


