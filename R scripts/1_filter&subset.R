##========================================================================##
# This script filter the complete observations (For our variable of interest 
# Teacher and Student gender)in the Three main data sets. 
# At the end it does a subsample of Colombia, to do all the initial analysis
# For computational limitations I am just using Colombia
# The idea is to use all Developing Countries aviable.
# Nixon. May 2/20

## Libraries to use
library(haven)
library(foreign)
library(tidyverse)

rm (list = ls(all.names = TRUE)) #Clean the directory
setwd("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015") # Set the Working Directory

####Importing The data PISA 2015 FILES
Students <- read_sav("CY6_MS_CMB_STU_QQQ.sav")
Schools <- read_sav("CY6_MS_CMB_SCH_QQQ.sav")
Teachers <- read_sav("CY6_MS_CMB_TCH_QQQ.sav")

##========================================================================##
## 1) MATCHING OBSERVATIONS: Teachers and Students from the same School   ##
##========================================================================##
#Number of Schools per country.
Schools %>% group_by(CNT, CNTSCHID) %>% group_by(CNT) %>% count(CNT) 
#Number of Teachers per country.
Teachers %>% group_by(CNT, CNTSCHID, CNTTCHID) %>% group_by(CNT) %>% count(CNT)
#Number of Students per country.
Students %>% group_by(CNT, CNTSCHID, CNTSTUID) %>% group_by(CNT) %>% count(CNT)


## Sub-Sample
Teachers2 <- Teachers %>% filter(!is.na(TC001Q01NA)) ## Removing NAs
i <- unique(Teachers2$CNTRYID)
j <- unique(Teachers2$CNTSCHID)
k <- unique(Teachers2$CNT)
Students2 <- Students %>% filter(CNTRYID %in% i) %>% filter(CNTSCHID %in% j) %>% filter(!is.na(ST004D01T))
Schools2 <- Schools %>% filter(CNTRYID %in% i) %>% filter(CNTSCHID %in% j) %>% filter(!is.na(CNTSCHID))

#Number of Schools per country.
a <- Schools2 %>% group_by(CNT, CNTSCHID) %>% group_by(CNT) %>% count(CNT) 
#Number of Teachers per country.
b <- Teachers2 %>% group_by(CNT, CNTSCHID, CNTTCHID) %>% group_by(CNT) %>% count(CNT)
#Number of Students per country. 
c <- Students2 %>% group_by(CNT, CNTSCHID, CNTSTUID) %>% group_by(CNT) %>% count(CNT)

d <- left_join(a,b, by="CNT")
Totales <- left_join(d,c, by='CNT')
rm (a,b,c,d)
colnames(Totales) <- c("CNT", "Schools", "Teachers", "Students")
Totales # Gives the taotal number of observation used in the study


### Gender
## Total Teacher Gender by Country 
Teachers2 %>% group_by(CNT, CNTSCHID, CNTTCHID, TC001Q01NA) %>% group_by(CNT) %>% count(TC001Q01NA)
## Total Students Gender by Country 
Students2 %>% group_by(CNT, CNTSCHID, CNTSTUID, ST004D01T) %>%  group_by(CNT) %>% count(ST004D01T)


##========================================================================##
## 2) Filtering the Data for COLOMBIA Only                                ##
##========================================================================##
col_teachers <- Teachers2 %>% filter(CNT=="COL")
col_students <- Students2 %>% filter(CNT=="COL")
col_schools <- Schools2 %>% filter(CNT=="COL")
save(col_schools, col_teachers, col_students , Totales, file = "col_sample_data.RData")

##========================================================================##
## 3) Filtering the Data for Lating America                               ##
##========================================================================##
latin_america <- c("BRA", "COL", "CHL", "PER")
lac_teachers <- Teachers2 %>% filter(CNT %in% latin_america)
lac_students <- Students2 %>% filter(CNT %in% latin_america)
lac_schools <- Schools2 %>% filter(CNT %in% latin_america)
save(lac_schools, lac_teachers, lac_students , Totales, file = "lat_sample_data.RData")

##========================================================================##
## 4) Filtering the Data for Lating America and Caribbean                 ##
##========================================================================##
latin_america <- c("BRA", "COL", "CHL", "PER", "DOM")
lac_teachers <- Teachers2 %>% filter(CNT %in% latin_america)
lac_students <- Students2 %>% filter(CNT %in% latin_america)
lac_schools <- Schools2 %>% filter(CNT %in% latin_america)
save(lac_schools, lac_teachers, lac_students , Totales, file = "lac_sample_data.RData")

##========================================================================##
## 3) Export the sub-sample To do the initial analysis                    ##
## (File is too large and the script takes too long)                      ##
##========================================================================##
save(Schools2, Teachers2, Students2 , Totales, file = "all_sample_data.RData")




#rm(i,j,k,Students, Students2, Schools, Schools2, Teachers, Teachers2) #Remove the data frame we are not going to use.
rm (list = ls(all.names = TRUE)) #Clean the directory