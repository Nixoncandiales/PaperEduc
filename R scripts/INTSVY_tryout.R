
library(haven)
library(foreign)
library(tidyverse)
library(intsvy)
library(ggthemes)

rm (list = ls(all.names = TRUE)) #Clean the directory
setwd("C:/Users/16083/Documents/GitHub/PaperEduc/Data 2015") # Set the Working Directory
data <- getwd()
latin_america <- c("BRA", "COL", "CHL", "COL", "DOM", "PER")

a <- pisa.var.label(folder = file.path(getwd()),
               school.file = "CY6_MS_CMB_SCH_QQQ.sav",
               student.file = "CY6_MS_CMB_STU_QQQ.sav")

a <- intsvy.select.merge(data, latin_america)

pisa <- pisa.select.merge(folder= data,
                          school.file="CY6_MS_CMB_SCH_QQQ.sav", 
                          student.file="CY6_MS_CMB_STU_QQQ.sav",
                          student= c("ST01Q01", "ST04Q01", "ESCS", "PARED"), 
                          school = c("CLSIZE", "TCSHORT"), 
                          countries =  c("BRA", "COL", "CHL", "COL", "DOM", "PER"))  


pisa2015.table(variable="ST004D01T", by="CNT",data=lac_students)
pisa2015.ben.pv(pvlabel="MATH", by="CNT", data=lac_students)
pisa2015.log(y="Prop_Teach_female", x="ST008Q01TA", by="CNT", data = lac_students)
a <- pisa2015.mean(variable = "AGE", by="CNT", data=lac_students)
pisa2015.reg(y="AGE", x=c("ST004D01T", "T_50"), by="CNT", data=lac_students)
pisa2015.reg.pv(pvlabel="READ", x=c("ST008Q01TA", "prop_fem_100","T_100","ESCS", "CNT", "CNTSCHID"), data=lac_students)

pmeansNC <- piaac.mean.pv(pvlabel = "NUM", by = "CNTRYID",
                          + data = piaac, export = FALSE)
plot(a, sort = TRUE)


b <- pisa2015.mean.pv(pvlabel = "READ", by = c("CNT", "ST004D01T"), data = lac_students)
p <- plot(b, sort = TRUE)



theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
