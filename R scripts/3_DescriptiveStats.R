##========================================================================##
## This script runs Descriptives statistics and graphs.
# Produces FIgure 1 and Table 1
## Nixon, May 11/20
# Libraries used
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
#load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/col_treatment_data.RData")## Colombia Sample 
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lat_treatment_data.RData")## Latin America
#load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lac_treatment_data.RData")## Latin America & Caribbean
#load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/all_treatment_data.RData")## ALL pisa 2015


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
setwd("~/GitHub/PaperEduc/R scripts")
rm(math,read,scie)


##========================================================================##
## Table 1
# comparisson on mean characteristics by gender and treatment. 
##========================================================================##
##========================================================================##


##PANEL A. Control and means
##========================================================================##
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
        
        ##Test Scores
        y1 <- pisa2015.mean.pv(pvlabel = "READ", by = c("Gender", "prop_fem_50"), data = students)  %>% 
          select(Mean, SD)
        y1[,2] = paste0("(",y1[,2], ")")
        
        y2 <- pisa2015.mean.pv(pvlabel = "MATH", by = c("Gender", "prop_fem_50"), data = students)  %>% 
          select(Mean, SD)
        y2[,2] = paste0("(",y2[,2], ")")
        
        y3 <- pisa2015.mean.pv(pvlabel = "SCIE", by = c("Gender", "prop_fem_50"), data = students)  %>% 
          select(Mean, SD)
        y3[,2] = paste0("(",y3[,2], ")")
##========================================================================##


### PANEL B
##========================================================================##
  ## Adding the Treatment variable to the Teachers data set.
    a = students %>% select(CNTSCHID, prop_fem_50, prop_fem_75, prop_fem_100) %>% 
      group_by(CNTSCHID) %>% summarise(prop_fem_50 = mean(prop_fem_50), prop_fem_75 = mean(prop_fem_75), 
                                       prop_fem_100 = mean(prop_fem_100))
    
    teachers = left_join(teachers,a,by="CNTSCHID")
    rm(a)
## Adding Teachers Controls and Means

        #Age
        t1 <- teachers %>% group_by(TC001Q01NA, prop_fem_50 ) %>% 
          summarize(Mean= mean(TC002Q01NA, na.rm = TRUE), SD=sd(TC002Q01NA, na.rm = TRUE)) %>% round(digits = 2) %>% as.data.frame()
        t1[,4] = paste0("(",t1[,4], ")")
        
        #Exp
        t2 <- teachers %>% group_by(TC001Q01NA, prop_fem_50 ) %>% 
          summarize(Mean=mean(TC007Q02NA, na.rm = TRUE), SD=sd(TC002Q01NA, na.rm = TRUE)) %>% 
          as.data.frame() %>% select(Mean, SD) %>% round(digits = 2)
        t2[,2] = paste0("(",t2[,2], ")")
        
        #Porportion of Bachelors
        t3 = teachers %>% select(TC001Q01NA, prop_fem_50, TC012Q01NA) %>%  
          group_by(TC001Q01NA, prop_fem_50, TC012Q01NA ) %>% tally %>%  mutate(p = n/sum(n), 
                                                                               se = sqrt((p*(1-p)/sum(n)))) %>% 
          filter(TC012Q01NA ==3) %>% round(digits = 2) %>%
          as.data.frame() %>% select(p, se)
        t3[,2] = paste0("(",t3[,2], ")")
        
        #Porportion of Masters
        t4 = teachers %>% select(TC001Q01NA, prop_fem_50, TC012Q01NA) %>%  
          group_by(TC001Q01NA, prop_fem_50, TC012Q01NA ) %>% tally %>%  mutate(p = n/sum(n), 
                                                                               se = sqrt((p*(1-p)/sum(n)))) %>% 
          filter(TC012Q01NA ==4) %>% round(digits = 2) %>%
          as.data.frame() %>% select(p, se)
        t4[,2] = paste0("(",t4[,2], ")")
        
        #Porportion of Ph.D.
        t5 = teachers %>% select(TC001Q01NA, prop_fem_50, TC012Q01NA) %>%  
          group_by(TC001Q01NA, prop_fem_50, TC012Q01NA ) %>% tally %>%  mutate(p = n/sum(n), 
                                                                               se = sqrt((p*(1-p)/sum(n)))) %>% 
          filter(TC012Q01NA ==5) %>% round(digits = 2) %>%
          as.data.frame() %>% select(p, se)
        t5[,2] = paste0("(",t5[,2], ")")
        
        #Porportion of Full-Time Teachers.
        t6 = teachers %>% select(TC001Q01NA, prop_fem_50, TC005Q01NA) %>%  
          group_by(TC001Q01NA, prop_fem_50, TC005Q01NA ) %>% tally %>%  mutate(p = n/sum(n), 
                                                                               se = sqrt((p*(1-p)/sum(n)))) %>% 
          filter(TC005Q01NA ==1) %>% round(digits = 2) %>%
          as.data.frame() %>% select(p, se)
        t6[,2] = paste0("(",t6[,2], ")")
        
        #Porportion of Originally ttrained as teaches.
        t7 = teachers %>% select(TC001Q01NA, prop_fem_50, OTT1) %>%  
          group_by(TC001Q01NA, prop_fem_50, OTT1 ) %>% tally %>%  mutate(p = n/sum(n), 
                                                                               se = sqrt((p*(1-p)/sum(n)))) %>% 
          filter(OTT1 ==1) %>% round(digits = 2) %>%
          as.data.frame() %>% select(p, se)
        t7[,2] = paste0("(",t7[,2], ")") 
##========================================================================##

### Print The Panel A
      table1a <- data.frame(rbind(t(x1),t(x2),t(x3),t(x4),t(x5),t(x6),
                                 t(z1),t(z2),t(z3),t(z4),t(z5),t(z6),
                                 t(y1),t(y2),t(y3))) #Set the table
      student_controls <- c("","","Highest education of parents in years ","","Family wealth","",
                            "Index of economic, social and cultural status (ESCS)","",
                            "Home possessions","","Proportion of Dad w/ College education",
                            "", "Proportion of Mom w/ College education","",
                            "Sense of Belonging to School", "", "Out-of-school study time per week", 
                            "", "Enjoy cooperation", "", "Value cooperation", "", "Environmental awareness", 
                            "", "Environmental optimism", "",
                            "Reading", "", "Math", "", "Science", "") #Names
      table1a = cbind(student_controls,table1a)
      stargazer(table1a, type="latex", summary = FALSE,
                column.labels=c("Control","Treatment","Female","Male"), 
                column.separate = c(2,2),rownames = FALSE,
                align = FALSE, title = "Comparison of Mean Characteristics", style = "aer",
                out = "C:/Users/16083/Documents/GitHub/PaperEduc/Tables/table1a.tex", 
                #out.header = TRUE, 
                digits = 3, digits.extra = 0, initial.zero = TRUE, 
                label="Tab1", notes	= "TBD", notes.align="l", font.size = "scriptsize")

### Print The Panel B
      table1b <- data.frame(rbind(t(t1),t(t2),t(t3),t(t4),t(t5),t(t6), t(t7)))
      teacher_controls <- c("","", "Teacher Age", "", "Teacher Experience (years)", "",
                            "Teachers w/ Bachelor's", "", "Teachers w/ Master's", "", 
                            "Teachers w/ Ph.D.'s", "", "Full-Time Teachers", "", 
                            "Originally ttrained as teaches", "")
      table1b = cbind(teacher_controls,table1b)
      stargazer(table1b, type="latex", summary = FALSE,
                column.labels=c("Control","Treatment","Female","Male"), 
                column.separate = c(2,2),rownames = FALSE,
                align = FALSE, title = "Comparison of Mean Characteristics", style = "aer",
                out = "C:/Users/16083/Documents/GitHub/PaperEduc/Tables/table1b.tex", 
                #out.header = TRUE, 
                digits = 3, digits.extra = 1, initial.zero = FALSE, 
                label="Tab1", notes	= "TBD", notes.align="l", font.size = "scriptsize")
      
rm(table_dir,student_controls,teacher_controls,x1,x2,x3,x4,x5,x6,x7,y1,y2,y3,z1,z2,z3,z4,z5,z6,t1,t2,t3,t4,t5,t6,t7)      
      
##========================================================================##  
##Base model Regression OLS 1
##========================================================================##
#reg1 <- pisa2015.reg.pv(x=c("Female", "prop_fem_100","T_100","ESCS", "CNT"), 
                        #pvlabel="MATH", data=students)

#plot(pisa2015.mean(variable ="OUTHOURS", by=c("CNT","Gender", "prop_fem_50"), data=students), sort=TRUE) 


