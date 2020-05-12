##========================================================================##
## This script runs Descriptives statistics and graphs.
## Nixon, May 3/20
library(intsvy)
library(tidyverse)
library(ggpubr)
library(stargazer)
rm (list = ls(all.names = TRUE)) #Clean the directory
setwd("~/GitHub/PaperEduc/R scripts")#Set working Directory
table_dir <- "C:/Users/16083/Documents/GitHub/PaperEduc/Tables"

##========================================================================##
# Call the Sample
##========================================================================##
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/col_treatment_data.RData")## Colombia Sample 
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lat_treatment_data.RData")## Latin America
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lac_treatment_data.RData")## Latin America & Caribbean
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/all_treatment_data.RData")## ALL pisa 2015


##========================================================================##
## Figure 1 : Mean comparison across countries and tests by Gender
##========================================================================##
{
read <- plot(pisa2015.mean.pv(pvlabel = "READ", by = c("CNT", "Gender"), 
                              data = students), sort=TRUE)  + 
                                                                  theme_pubr() #Reading

math <- plot(pisa2015.mean.pv(pvlabel = "MATH", by = c("CNT", "Gender"), 
                              data = students), sort=TRUE)  + 
                                                                  theme_pubr() #Math

scie <- plot(pisa2015.mean.pv(pvlabel = "SCIE", by = c("CNT", "Gender"), 
                              data = students), sort=TRUE)  + 
                                                                  theme_pubr() #Science

figure1 <- ggarrange(read, math, scie, labels = c("R", "M", "S"), 
                     common.legend = TRUE, ncol = 3,pointsize = 2, legend = "bottom")  #Pooling them in one graph
}
#Export the figure to the folder.
setwd("~/GitHub/PaperEduc/Figures")
figure1 %>% 
  ggexport(filename = "fig1b.png", pointsize = 2, width = 1200, height = 600)



##========================================================================##
## Table 1
# comparisson on mean characteristics by gender and treatment. 
##========================================================================##
{
  
  ###Socioeconomics characteristhics
x1 = pisa2015.mean(variable ="PARED", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Gender, prop_fem_50, Mean, SD) #%>% mutate(id = row_number())
x1[,4] = paste0("(",x1[,4], ")")

x2 = pisa2015.mean(variable ="WEALTH", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
x2[,2] = paste0("(",x2[,2], ")")

x3 = pisa2015.mean(variable ="ESCS", by=c("Gender", "prop_fem_50"), data=students)  %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
x3[,2] = paste0("(",x3[,2], ")")

x4 = pisa2015.mean(variable ="HOMEPOS", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
x4[,2] = paste0("(",x4[,2], ")")

x5 = pisa2015.table(variable ="FISCED", by = c("Gender", "prop_fem_50"), data = students)  %>% 
  filter(FISCED==6) %>% select(Percentage, `Std.err.`) %>% mutate(Percentage = Percentage/100)
x5[,2] = paste0("(",x5[,2], ")")

x6 = pisa2015.table(variable ="MISCED", by = c("Gender", "prop_fem_50"), data = students) %>% 
  filter(MISCED==6) %>% select( Percentage, `Std.err.`) %>% mutate(Percentage = Percentage/100)
x6[,2] = paste0("(",x6[,2], ")")

###Students aptitue characteristics
z1 = pisa2015.mean(variable ="BELONG", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
z1[,2] = paste0("(",z1[,2], ")")

z2 = pisa2015.mean(variable ="OUTHOURS", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
z2[,2] = paste0("(",z2[,2], ")")

z3 = pisa2015.mean(variable ="COOPERATE", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
z3[,2] = paste0("(",z3[,2], ")")

z4 = pisa2015.mean(variable ="CPSVALUE", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
z4[,2] = paste0("(",z4[,2], ")")

z5 = pisa2015.mean(variable ="ENVAWARE", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
z5[,2] = paste0("(",z5[,2], ")")

z6 = pisa2015.mean(variable ="ENVOPT", by=c("Gender", "prop_fem_50"), data=students) %>% 
  select(Mean, SD) #%>% mutate(id = row_number())
z6[,2] = paste0("(",z6[,2], ")")

##test sCORES
y1 <- pisa2015.mean.pv(pvlabel = "READ", by = c("Gender", "prop_fem_50"), data = students)  %>% select(Mean, SD)
y1[,2] = paste0("(",y1[,2], ")")

y2 <- pisa2015.mean.pv(pvlabel = "MATH", by = c("Gender", "prop_fem_50"), data = students)  %>% select(Mean, SD)
y2[,2] = paste0("(",y2[,2], ")")

y3 <- pisa2015.mean.pv(pvlabel = "SCIE", by = c("Gender", "prop_fem_50"), data = students)  %>% select(Mean, SD)
y3[,2] = paste0("(",y3[,2], ")")
}
table1 <- data.frame(rbind(t(x1),t(x2),t(x3),t(x4),t(x5),t(x6),
                           t(z1),t(z2),t(z3),t(z4),t(z5),t(z6),
                           t(y1),t(y2),t(y3))) #Set the table
student_controls <- c("","","PARED","","WEALTH","","ESCS","","HOMEPOS","","FISCED","", "MISCED","",
                      "BELONG", "", "OUTHOURS", "", "COOPERATE", "", "CPSVALUE", "", "ENVAWARE", "", "ENVOPT", "",
                      "Reading", "", "Math", "", "Science", "") #Names
table1 = cbind(student_controls,table1)


stargazer(table1, type="latex", summary = FALSE,
          column.labels=c("Control","Treatment","Female","Male"), 
          column.separate = c(2,2),rownames = FALSE,
          align = TRUE, title = "Comparison of Mean Characteristics", style = "aer",
          out = "C:/Users/16083/Documents/GitHub/PaperEduc/Tables/table1.tex", 
          out.header = TRUE, 
          digits = 3, digits.extra = 0, initial.zero = TRUE, 
          label="Tab1", notes	= "TBD", notes.align="l")



.#labels <- pisa.var.label(folder = "C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015",  
#                    name = "Variable labels", school.file="CY6_MS_CMB_SCH_QQQ.sav", 
#                    student.file="CY6_MS_CMB_STU_QQQ.sav")


plot(pisa2015.mean(variable ="OUTHOURS", by=c("CNT","Gender", "prop_fem_50"), data=students), sort=TRUE) 

##========================================================================##  
##Base model Regression OLS 1
##========================================================================##
reg1 <- pisa2015.reg.pv(x=c("Female", "prop_fem_100","T_100","ESCS", "CNT"), 
                        pvlabel="MATH", data=students)


