##========================================================================##
## This script Defines treatment and controls and set the intial OLS
## Nixon, May 2/20

library(tidyverse)
rm (list = ls(all.names = TRUE)) #Clean the directory
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lat_sample_data.RData")

##========================================================================##
## 1) Define Treatment and Control Groups                                 ##
## Treatment: Teacher_female & Student_female = 1
## Control: Teacher_female & Student_male = 0
## Control: Teacher_male & Student_female = 0
## Treatment 2?: Teacher_male & student_male = 1 or 2 ??
## Note: I am not sure if we need to include a double treatment

## Teacher Gender Variable: TC001Q01NA
## Student Gender Variable: ST004D01T???
##========================================================================##
lac_teachers %>% select(CNTSCHID, CNTTCHID, TC001Q01NA) %>% group_by(CNTSCHID) %>% count(TC001Q01NA)
lac_students %>% select(CNTSCHID, CNTSTUID, ST004D01T) %>% group_by(CNTSCHID) %>% count(ST004D01T)
# I ran into a big problem. I couldn't disentangle if the professor who filled the survey
# was a proffesor of and especific student. I can't match them at the individual level
# can only match them at school level. Also, I am not sure which class they teach
# So the initial idea of comparing MATH FEMALE proffesors and Female Students
# might not be plausible. Instead I would consider the proportion of Female teacher in the School
# as the criteria to be in the treatment or not. 
##========================================================================##

# Calculate the gender proportion school-wise
prop_female_teach <- data.frame(100*prop.table(table(lac_teachers$CNTSCHID, 
                                                     lac_teachers$TC001Q01NA),1)) %>% 
  spread(Var2, Freq )%>% mutate(Var1 = as.numeric(as.character(Var1)))

colnames(prop_female_teach) <- c("CNTSCHID", "Prop_Teach_female", "Prop_Teach_male") #rename the ID variable to be equal in both data set to proceed with the merge
head(prop_female_teach)

# Compute the school treatments, three levels: 1) Prop>50 , 2) prop>75, 3) prop=100
school_treated <- as.data.frame(100*prop.table(table(lac_teachers$CNTSCHID, 
                                                     lac_teachers$TC001Q01NA),1)) %>%
  mutate(f_1 = if_else((Var2==1 & Freq>50),1,0),
         f_2 = if_else((Var2==1 & Freq>75),1,0), 
         f_3 = if_else((Var2==1 & Freq==100),1,0)) %>%
  select(Var1, f_1, f_2, f_3) %>%  mutate(Var1 = as.numeric(as.character(Var1))) %>% group_by(Var1) %>% summarise(prop_fem_50 = sum(f_1),
                                                                                                                  prop_fem_75 = sum(f_2),
                                                                                                                  prop_fem_100 = sum(f_3)) # Compute the school treatments, three levels: 1) Prop>50 , 2) prop>75, 3) prop=100
colnames(school_treated) <- c("CNTSCHID","prop_fem_50","prop_fem_75","prop_fem_100") #rename the ID variable to be equal in both data set to proceed with the merge

#Merge Treatment Variables with School Data With School Data
school_treated <-left_join(prop_female_teach, school_treated, by="CNTSCHID")
lac_students <-left_join(lac_students, school_treated, by="CNTSCHID") ## Merge The school Terated variables in Students Data.

# Generete the treatment for the female students.
lac_students <- lac_students %>% mutate(T_50 = ifelse((prop_fem_50==1 & ST004D01T==1),1,0),
                                        T_75 = ifelse((prop_fem_75==1 & ST004D01T==1),1,0),
                                        T_100 = ifelse((prop_fem_100==1 & ST004D01T==1),1,0)) 

## Total Schools Treated and Total Students treated.
school_treated %>%  summarise(T_50 = sum(prop_fem_50),
                              T_75 = sum(prop_fem_75),
                              T_100 = sum(prop_fem_100))

lac_students %>%   summarise(T_50 = sum(T_50),
                             T_75 = sum(T_75),
                             T_100 = sum(T_100))
rm(prop_female_teach)

## TOTAL OBSERVATIONS OF THE SUBSAMPLE
a <- lac_schools %>% group_by(CNT, CNTSCHID) %>% group_by(CNT) %>% count(CNT) 
#Number of Teachers per country.
b <- lac_teachers %>% group_by(CNT, CNTSCHID, CNTTCHID) %>% group_by(CNT) %>% count(CNT)
#Number of Students per country. 
c <- lac_students %>% group_by(CNT, CNTSCHID, CNTSTUID) %>% group_by(CNT) %>% count(CNT)
d <- left_join(a,b, by="CNT")
Totales_lac <- left_join(d,c, by='CNT')
rm (a,b,c,d)
colnames(Totales_lac) <- c("CNT", "Schools", "Teachers", "Students")

##Create a new variable Gender as a FACTOR and string, to easier graphs.
lac_students <- lac_students %>% mutate(Gender = ifelse(ST004D01T==1,"Female","Male"),
                                        Female = ifelse(ST004D01T==1,1,0))
schools = lac_schools
students = lac_students
teachers = lac_teachers
save(schools, teachers, students, file = "lat_treatment_data.RData")
rm (list = ls(all.names = TRUE))
load("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015/lat_treatment_data.RData")
print("The Latin America Sample has been Imported")