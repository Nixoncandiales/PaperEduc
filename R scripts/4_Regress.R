##========================================================================##
## This script runs OLS regressions and the Initial Model
# Produces regression in Table 2
## Nixon, May 13/20

# Libraries used
library(intsvy)
library(tidyverse)
library(ggpubr)
library(stargazer)
rm (list = ls(all.names = TRUE)) #Clean the directory
setwd("~/GitHub/PaperEduc/R scripts")#Set working Directory


##========================================================================##
# Call the Sample
##========================================================================##
#load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/col_treatment_data.RData")## Colombia Sample 
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lat_treatment_data.RData")## Latin America
#load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lac_treatment_data.RData")## Latin America & Caribbean
#load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/all_treatment_data.RData")## ALL pisa 2015


##========================================================================##  
##Base model Regression OLS 1
##========================================================================##
#Set School ID as a factor.

## Initial Reg, no controls. Just School and Country Fixed Effects.
students$CNTSCHID = factor(students$CNTSCHID)
reg1 <- pisa2015.reg.pv(x=c("Female", "prop_fem_50","T_50","ESCS", "CNTSCHID", "CNT" ), 
                        pvlabel="READ", data=students)

reg2 <- pisa2015.reg.pv(x=c("Female", "prop_fem_50","T_50","ESCS", "CNTSCHID", "CNT" ), 
                        pvlabel="MATH", data=students)

reg3 <- pisa2015.reg.pv(x=c("Female", "prop_fem_50","T_50","ESCS", "CNTSCHID", "CNT" ), 
                        pvlabel="SCIE", data=students)

setwd("~/GitHub/PaperEduc/Data 2015")#Set working Directory
save(reg1, reg2, reg3, file = "reg_result.RData")
rm (list = ls(all.names = TRUE)) #Clean the directory
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/reg_result.RData")

#Get the data frame
read_reg =reg1[["reg"]]
math_reg =reg2[["reg"]]
scie_reg =reg3[["reg"]]

#========Set the Fromat into table 2 =====#
#Upper panel
i <- round(t(read_reg %>% select(1:2) %>% slice(1:5,1374)),3)
i[2,] = paste0("(",format(unlist(i[2,])),")")
i <- data.frame(Reading = c(i)) %>% slice(1:11)
j <- round(t(math_reg %>% select(1:2) %>% slice(1:5,1374)),3)
j[2,] = paste0("(",format(unlist(j[2,])),")")
j <- data.frame(Math = c(j)) %>% slice(1:11)
k <- round(t(scie_reg %>% select(1:2) %>% slice(1:5,1374)),3)
k[2,] = paste0("(",format(unlist(k[2,])),")")
k <- data.frame(Science = c(k)) %>% slice(1:11)
labels <- c("Constant","", "Female Student","", "Female School","", "Female Student x Female School","", "ESCS", "", "R2")
tab2 <- cbind(labels, i, j, k)
colnames(tab2) <- c("", "Reading", "Math", "Science")
rm(i,j,k, labels)

#Bottom Panel
tab2b <- data.frame(rbind(c("Observations",rep("43,678",3)),
                          c("Shool FE", rep("ding{51}",3)),
                          c("Country FE", rep("ding{51}",3))))
colnames(tab2b) <- c("", "Reading", "Math", "Science")

tab2 = rbind(tab2,tab2b)
rm(tab2b)

#========Print the table 2 =====#
stargazer(tab2, type="text", summary = FALSE, rownames = FALSE)
stargazer(tab2, type="latex", summary = FALSE,
          column.separate = c(2,2),rownames = FALSE,
          align = FALSE, title = "Main Results", style = "aer",
          out = "C:/Users/16083/Documents/GitHub/PaperEduc/Tables/table2.tex", 
          #out.header = TRUE, 
          digits = 3, digits.extra = 1, initial.zero = FALSE, 
          label="Tab2", notes	= "Each column represents a separate regression where the dependt variable is the test reading, math or science. Each model present  school fixed affect and country fixed effects, as well", notes.align="l", font.size = "scriptsize")

### P values .... I calculete them manuel, given the intsvy does not provide p values.
t1 = round(read_reg %>% slice(1:5) %>% select(3),3)
t1 = t1[,1]
p1 = round(2*pnorm(-abs(t1)),3)
star1 = ifelse(p1<0.001,"***",
       (ifelse(p1<0.01,"**",
               (ifelse(p1<0.05,"*","")))))

t2 = round(math_reg %>% slice(1:5) %>% select(3),3)
t2 = t2[,1]
p2 = round(2*pnorm(-abs(t2)),3)
star2 = ifelse(p2<0.001,"***",
               (ifelse(p2<0.01,"**",
                       (ifelse(p2<0.05,"*","")))))

t3 = round(scie_reg %>% slice(1:5) %>% select(3),3)
t3 = t3[,1]
p3 = round(2*pnorm(-abs(t3)),3)
star3 = ifelse(p3<0.001,"***",
               (ifelse(p3<0.01,"**",
                       (ifelse(p3<0.05,"*","")))))
cbind(t1,p1,star1,t2,p2,star2,t3,p3,star3)
rm(t1,t2,t3,p1,p2,p3,star1,star2,star3)

