###########################################################################
## Libraries to use

library(haven)
library(foreign)
library(tidyverse)

#setwd("~/Desktop/Ma Economics/Spring 20/CI/Data Project/PISA")
setwd("~/Data")
rm (list = ls(all.names = TRUE)) #Clean the directory

####Importing The data
Students <- read_sav("CY07_MSU_STU_QQQ.sav")
Schools <- read_sav("CY07_MSU_SCH_QQQ.sav")
Teachers <- read_sav("CY07_MSU_TCH_QQQ.sav")


###########################################################################
## 1) MATCHING OBSERVATIONS: Teachers and Students from the same School  ##
###########################################################################

#Number of Schools per country.
Schools %>% group_by(CNT, CNTSCHID) %>% group_by(CNT) %>% count(CNT) 
#Number of Teachers per country.
Teachers %>% group_by(CNT, CNTSCHID, CNTTCHID) %>% group_by(CNT) %>% count(CNT)
#Number of Students per country.
Students %>% group_by(CNT, CNTSCHID, CNTSTUID) %>% group_by(CNT) %>% count(CNT)

## Sub-Sample
Teachers <- Teachers %>% filter(!is.na(TC001Q01NA)) ## Removing NAs
i <- unique(Teachers$CNTRYID)
j <- unique(Teachers$CNTSCHID)
Students <- Students %>% filter(CNTRYID %in% i) %>% filter(CNTSCHID %in% j) %>% filter(!is.na(ST004D01T))
Schools <- Schools %>% filter(CNTRYID %in% i)  %>% filter(CNTSCHID %in% j) %>% filter(!is.na(CNTSCHID))

### Gender
## Total Teacher Gender by Country 
Teachers %>% group_by(CNT, CNTSCHID, CNTTCHID, TC001Q01NA) %>% group_by(CNT) %>% count(TC001Q01NA)
## Total Students Gender by Country 
Students %>% group_by(CNT, CNTSCHID, CNTSTUID, ST004D01T) %>%  group_by(CNT) %>% count(ST004D01T)


write.foreign(Students, "Students.csv", "Students.sps",   package="SPSS")
write.foreign(Teachers, "Teachers.csv", "Teachers.sps",   package="SPSS")
write.foreign(Schools, "Schools.csv", "Schools.sps",   package="SPSS")

