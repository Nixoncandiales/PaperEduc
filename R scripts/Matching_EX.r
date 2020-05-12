#### ================ From NICK website
#Coursing Exact Matching - require a exact match.
library(tidyverse)
library(Ecdat)
data(Wages)

#Coarsen
Wages <- Wages %>% mutate(ed.coarse = cut(ed,breaks=3),
                          exp.coarse = cut(exp,breaks=3))
#Split up the treated and untreated
union <- Wages %>% filter(union=='yes')
nonunion <- Wages %>% filter(union=='no') %>%
  #For every potential complete-match, let's get the average Y
  group_by(ed.coarse,exp.coarse,bluecol,
           ind,south,smsa,married,sex,black) %>%
  summarize(untreated.lwage = mean(lwage))

union %>% inner_join(nonunion) %>%
  summarize(union.mean = mean(lwage),nonunion.mean=mean(untreated.lwage))

#Original union and nonunion counts, and matched union count
c(sum(Wages$union=='yes'),sum(Wages$union=='no'),nrow(union %>% inner_join(nonunion)))


#### ================ From NICK website
library(atus)
library(tidyverse)

data(atusresp)
data(atusact)
eating <- atusact %>% filter(tiercode==110101) %>% inner_join(atusresp) %>% ungroup() %>%
  select(dur, hh_child, labor_status, student_status, work_hrs_week, partner_hh, weekly_earn, tuyear) %>%
  na.omit() %>%
  mutate(hrs.c = cut(work_hrs_week,breaks=5),earn.c = cut(weekly_earn,breaks=5),year.c = cut(tuyear,breaks=5))

kids <- filter(eating,hh_child=='yes')
nokids <- eating %>% filter(hh_child=='no') %>%
  group_by(hrs.c,earn.c,year.c,labor_status,student_status,partner_hh) %>%
  summarize(nokids.dur = mean(dur))
kids %>% inner_join(nokids) %>% summarize(kids.dur=mean(dur),nokids.dur=mean(nokids.dur))
