# packages ----
library(dplyr)
library(tidyr)
library(here)
library(stringr)
library(epiR)
library(ggplot2)
library(purrr)
library(epitools)
library(scales)
# patient characteristics ----- 
data.files<-list.files(here("data"))
# dbs
db.names<-data.files[str_detect(data.files, "Patient.characteristcis_")]
db.names<-str_replace(db.names, "Patient.characteristcis_","")
db.names<-str_replace(db.names, ".RData","" )

Network.patient.characteristcis<-list()
for(i in 1:length(db.names)){
 load(paste0(here("data"),"/Patient.characteristcis_", db.names[i], ".RData"))

for(l in 1:length(Patient.characteristcis)){
    Patient.characteristcis[[l]]$db<- db.names[i]
  }
 Network.patient.characteristcis[[db.names[i]]]<-bind_rows(Patient.characteristcis, .id = "id") 
 rownames(Network.patient.characteristcis[[db.names[i]]])<-1:nrow(Network.patient.characteristcis[[db.names[i]]])
 
 rm(Patient.characteristcis)
}
Network.patient.characteristcis<-bind_rows(Network.patient.characteristcis) %>% 
  mutate(pop=ifelse(pop=="Vaccinated with Pfizer-Biontech",
                    "Vaccinated with BNT162b2", pop)) %>% 
  mutate(pop=ifelse(pop=="Vaccinated with AstraZeneca",
                    "Vaccinated with ChAdOx1", pop))  %>%
  #  mutate(pop=ifelse(pop=="Vaccinated with Moderna",
  #                   "Vaccinated with mRNA-1273", pop)) %>% 
  # mutate(pop=ifelse(pop=="Vaccinated with Janssen",
  #                    "Vaccinated with Ad26.COV2.S", pop)) %>% 
  mutate(pop=ifelse(pop=="General population 2017",
                    "General population (index date: 1st December)", pop))   %>% 
  mutate(pop=ifelse(pop=="General population with visit 2017 to 2019",
                    "General population (index date: first visit/ contact)", pop)) 
table(Network.patient.characteristcis$pop)
#
names(Network.patient.characteristcis)<-str_replace(names(Network.patient.characteristcis),
                                                    "Overall","Study population")
table(Network.patient.characteristcis$pop.type)
Network.patient.characteristcis<-Network.patient.characteristcis %>% 
  mutate(pop.type=ifelse(pop.type=="before.march",
                    "Before March 1st", pop.type)) %>% 
  mutate(pop.type=ifelse(pop.type=="from.march",
                    "From March 1st", pop.type)) 

# incidence rates -----

db.names<-data.files[str_detect(data.files, "IR.summary_")]
db.names<-str_replace(db.names, "IR.summary_","")
db.names<-str_replace(db.names, ".RData","" )
# db.names<-db.names[2:2]
Network.IR<-list()
for(i in 1:length(db.names)){
load(paste0(here("data"),"/IR.summary_", db.names[i], ".RData"))
Network.IR[[db.names[i]]]<-IR.summary %>%
  select(-outcome) %>% 
  filter(events!="<5") %>% 
  mutate(events=as.numeric(events))
 rm(IR.summary)
}
Network.IR<-bind_rows(Network.IR)%>% 
  mutate(pop=ifelse(pop=="General population 2017",
                    "General population (index date: 1st December)", pop))  # %>% 
#mutate(pop=ifelse(pop=="General population with visit 2017 to 2019",# BERTA
#  "General population (index date: first visit/ contact)", pop)) 




# add CIs
IR.conf<-epi.conf(as.matrix(cbind(Network.IR$events, Network.IR$years)),
         ctype = "inc.rate", method = "exact", conf.level = 0.95)
Network.IR$ir_100000_lower<-IR.conf$lower* 100000
Network.IR$ir_100000_upper<-IR.conf$upper* 100000
rm(IR.conf)

Network.IR<-Network.IR %>% # BERTA
  mutate(outcome.name=ifelse(outcome.name=="BellsPalsy" ,"Bell's Palsy",  outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="GuillainBarreSyndrome" ,"Guillain Barre Syndrome",  outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="TransverseMyelitis" ,"Transverse Myelitis" ,  outcome.name))

table(Network.IR$time.window)
#BERTA: canvi time windows (tenim dos cohorts covid: 21d i 90d)
#Covid 21d: talls 7, 14, 21
#Covid 90d: talls 14,21, 90
#Ens quedem amb talls 21d (igual que cohort vacunats + 90d)
time_window_excl <- Network.IR %>%filter(pop=="COVID19 PCR positive test 90d")%>%
  filter(time.window!="Full")

Network.IR <- Network.IR %>% anti_join(time_window_excl)
Network.IR<-Network.IR %>%
  mutate(time.window=as.character(time.window)) %>% 
  mutate(time.window=
                    ifelse(time.window=="Full", "Full (vaccinated: 1 to 21 days, SARS-CoV-2: 1 to 21 or 90 days)",
          #         ifelse(((time.window=="Full") & (pop=="COVID19 PCR positive test 21d")),"Full (vaccinated: 1 to 21 days, SARS-CoV-2: 1 to 21 days)",
          #         ifelse(((time.window=="Full") & (pop=="COVID19 PCR positive test 90d")),"Full (vaccinated: 1 to 21 days, SARS-CoV-2: 1 to 90 days)", 
                    ifelse(time.window=="w1", "#1 (vaccinated: 1 to 7 days, SARS-CoV-2: 1 to 7 days)",
                    ifelse(time.window=="w2", "#2 (vaccinated: 7 to 14 days, SARS-CoV-2: 7 to 14 days)",
                    ifelse(time.window=="w3", "#3 (vaccinated: 14 to 21 days, SARS-CoV-2: 14 to 21 days)",
                    time.window)))))%>%
  mutate(time.window=factor(time.window,
                            levels=c("Full (vaccinated: 1 to 21 days, SARS-CoV-2: 1 to 21 or 90 days)",
                                     "#1 (vaccinated: 1 to 7 days, SARS-CoV-2: 1 to 7 days)",
                                      "#2 (vaccinated: 7 to 14 days, SARS-CoV-2: 7 to 14 days)",
                                     "#3 (vaccinated: 14 to 21 days, SARS-CoV-2: 14 to 21 days)")))
table(Network.IR$time.window)

table(Network.IR$pop)

table(Network.IR$pop.type)

# incidence ------
# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
         big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
         big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
         big.mark=",", nsmall = 0, digits=0, scientific=FALSE))}


# dbs
db.names<-data.files[str_detect(data.files, "Survival.summary_")]
db.names<-str_replace(db.names, "Survival.summary_","")
db.names<-str_replace(db.names, ".RData","")


Network.Survival.summary<-list()
for(i in 1:length(db.names)){
 load(paste0(here("data"),"/Survival.summary_", db.names[i], ".RData"))
 Network.Survival.summary[[db.names[i]]]<-Survival.summary 
 rm(Survival.summary)
}
Network.Survival.summary<-bind_rows(Network.Survival.summary) %>% 
  mutate(total_events=nice.num.count(cum.n.event)) %>% 
  mutate(inc.est=
           ifelse(!is.na(surv),
           paste0(nice.num2((1-surv)*100),
                    "% (",
                    nice.num2((1-upper)*100),
                    "% to ",
                    nice.num2((1-lower)*100), "%)"
                    ), NA))
Network.Survival.summary<-Network.Survival.summary  %>% 
  mutate(total_events=ifelse(time==0, NA, total_events)) %>% 
  mutate(inc.est=ifelse(time==0, NA, inc.est))%>% 
  mutate(pop=ifelse(pop=="General population 2017",
                    "General population (index date: 1st December)", pop))   %>% 
  mutate(pop=ifelse(pop=="General population with visit 2017 to 2019",
                    "General population (index date: first visit/ contact)", pop)) 
# 

Network.Survival.summary<-Network.Survival.summary %>% 
   mutate(group=str_replace(group, "age_gr2=", "Age: " )) %>% 
   mutate(group=str_replace(group, "age_gr=", "Age: " )) %>% 
   mutate(group=str_replace(group, "age_gr3=", "Age: " )) %>% 
   mutate(group=str_replace(group, ", gender=", "; Sex: " ))%>% 
   mutate(group=str_replace(group, "cond.drug.comp=0", "Without condition or medication of interest" )) %>% 
   mutate(group=str_replace(group, "cond.drug.comp=1", "With condition or medication of interest" ))  %>% 
   mutate(group=str_replace(group, "cond.comp=0", "Without condition of interest" )) %>% 
   mutate(group=str_replace(group, "cond.comp=1", "With condition of interest" )) %>% 
   mutate(group=str_replace(group, "drug.comp=0", "Without medication of interest" )) %>% 
   mutate(group=str_replace(group, "drug.comp=1", "With medication of interest" )) 

table(Network.Survival.summary$group)


Network.Survival.summary<-Network.Survival.summary %>% 
   mutate(n.risk=nice.num.count(n.risk)) %>% 
   mutate(total_events=
            ifelse(total_events=="<5", 
                   total_events,
            nice.num.count(as.numeric(total_events)))) %>% 
   rename("Strata"="group")%>% 
   rename("Number at risk"="n.risk")%>% 
   rename("Events"="total_events")%>% 
   rename("Cumulative incidence"="inc.est") 

Network.Survival.summary<-Network.Survival.summary %>%  #BERTA
  mutate(outcome.name=ifelse(outcome.name=="BellsPalsy" ,"Bell's Palsy",  outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="GuillainBarreSyndrome" ,"Guillain Barre Syndrome",  outcome.name)) %>%
  mutate(outcome.name=ifelse(outcome.name=="TransverseMyelitis" ,"Transverse Myelitis" ,  outcome.name))


# IRRs ----  
get.IRR.df<-function(target.name, comparator.name){
target<-Network.IR %>% 
  filter(pop=={{target.name}}) %>%  
  select(pop, pop.type, time.window, n, years, events, 
         strata, outcome.name,
         ir_100000, ir_100000_lower,ir_100000_upper,
         prior.obs.required, gender, age_gr, age_gr2, age_gr3, 
         cond.comp, cond.drug.comp,
         drug.comp, db)  %>%
  rename("target.pop"="pop") %>% 
  rename("target.pop.type"="pop.type") %>% 
  rename("target.time.window"="time.window") %>% 
   rename("n_target"="n") %>% 
   rename("years_target"="years") %>% 
   rename("events_target"="events") %>% 
   rename("ir_100000_target"="ir_100000")%>% 
   rename("ir_100000_lower_target"="ir_100000_lower")%>% 
   rename("ir_100000_upper_target"="ir_100000_upper")

comparator<-Network.IR %>% 
  filter(pop=={{comparator.name}}) %>%
  select(pop, n, years, events, strata, outcome.name,
         ir_100000, ir_100000_lower,ir_100000_upper,
         prior.obs.required, gender, age_gr, age_gr2, age_gr3, 
         cond.comp, cond.drug.comp,
         drug.comp, db) %>% 
   rename("comparator"="pop") %>% 
   rename("n_comparator"="n") %>% 
   rename("years_comparator"="years") %>% 
   rename("events_comparator"="events") %>% 
   rename("ir_100000_comparator"="ir_100000")%>% 
   rename("ir_100000_lower_comparator"="ir_100000_lower")%>% 
   rename("ir_100000_upper_comparator"="ir_100000_upper")

IRR<-left_join(target, comparator)
IRR<-IRR %>% 
  filter(!is.na(events_comparator) & !is.na(events_target))
IRR<-IRR %>% 
  filter(events_comparator!=0 & events_target!=0)
IRR<-IRR %>% 
  filter(events_target>=5)

IRR$rrr.est<-NA
IRR$rrr.lower<-NA
IRR$rrr.upper<-NA

working.IRR<-vector(mode = "list", length = nrow(IRR))
working.IRR.lower<-vector(mode = "list", length = nrow(IRR))
working.IRR.upper<-vector(mode = "list", length = nrow(IRR))

for(i in 1:nrow(IRR)){
if(i %in% seq(1,nrow(IRR), 1000)){
print(paste0(i, " of ", nrow(IRR)))  
}
  
r<-rateratio(x=c(IRR$events_comparator[i],
              IRR$events_target[i]), 
          y = c(IRR$years_comparator[i],
                IRR$years_target[i]),
          conf.level = 0.95)
working.IRR[[i]] <-r$measure[2,1]
working.IRR.lower[[i]] <-r$measure[2,2]
working.IRR.upper[[i]] <-r$measure[2,3]
}

IRR$rrr.est<-unlist(working.IRR)
IRR$rrr.lower<-unlist(working.IRR.lower)
IRR$rrr.upper<-unlist(working.IRR.upper) 


IRR}

##BErta: COVID_diagnosis_background.pop.IRR
COVID_diagnosis_21d_background.pop.IRR<-get.IRR.df(target.name="COVID19 positive test 21d", 
                                  comparator.name="General population (index date: 1st December)")

COVID_diagnosis_90d_background.pop.IRR<-get.IRR.df(target.name="COVID19 positive test 90d", #BERTA
                                               comparator.name="General population (index date: 1st December)")

AZ1_background.pop.IRR<-get.IRR.df(target.name="ChAdOx1 first-dose",
                                  comparator.name="General population (index date: 1st December)") 
AZ1.no.covid_background.pop.IRR<-get.IRR.df(target.name="ChAdOx1 first-dose (no prior covid)",
                                  comparator.name="General population (index date: 1st December)") 

AZ2_background.pop.IRR<-get.IRR.df(target.name="ChAdOx1 second-dose",
                                  comparator.name="General population (index date: 1st December)") 
AZ2.no.covid_background.pop.IRR<-get.IRR.df(target.name="ChAdOx1 second-dose (no prior covid)",
                                           comparator.name="General population (index date: 1st December)") 

Pf1_background.pop.IRR<-get.IRR.df(target.name="BNT162b2 first-dose",
                                  comparator.name="General population (index date: 1st December)") 
Pf1.no.covid_background.pop.IRR<-get.IRR.df(target.name="BNT162b2 first-dose (no prior covid)",
                                  comparator.name="General population (index date: 1st December)") 

Pf2_background.pop.IRR<-get.IRR.df(target.name="BNT162b2 second-dose",
                                  comparator.name="General population (index date: 1st December)") 
Pf2.no.covid_background.pop.IRR<-get.IRR.df(target.name="BNT162b2 second-dose (no prior covid)",
                                  comparator.name="General population (index date: 1st December)") 

#Any1_background.pop.IRR<-get.IRR.df(target.name="Any first-dose",# Berta
 #                                  comparator.name="General population (index date: 1st December)") 
#Any1.no.covid_background.pop.IRR<-get.IRR.df(target.name="Any first-dose (no prior covid)",
#                                            comparator.name="General population (index date: 1st December)") 
# Any1_background.pop.visit.IRR<-get.IRR.df(target.name="Any first-dose",
#                                   comparator.name="General population (index date: first visit/ contact)")


Jnj_background.pop.IRR<-get.IRR.df(target.name="Ad26.COV2.S first-dose",
                                  comparator.name="General population (index date: 1st December)") 
Jnj.no.covid_background.pop.IRR<-get.IRR.df(target.name="Ad26.COV2.S first-dose (no prior covid)",
                                            comparator.name="General population (index date: 1st December)") 


Mod1_background.pop.IRR<-get.IRR.df(target.name="mRNA-1273 first-dose",
                                   comparator.name="General population (index date: 1st December)")
Mod1.no.covid_background.pop.IRR<-get.IRR.df(target.name="mRNA-1273 first-dose (no prior covid)",
                                  comparator.name="General population (index date: 1st December)") 


Mod2_background.pop.IRR<-get.IRR.df(target.name="mRNA-1273 second-dose",
                                   comparator.name="General population (index date: 1st December)") 
# Mod2.no.covid_background.pop.IRR<-get.IRR.df(target.name="mRNA-1273 second-dose (no prior covid)",
#                                   comparator.name="General population (index date: 1st December)") 


# IRS -----
get.IRS.df<-function(target.name, comparator.name){ 

target<-Network.IR %>% 
  filter(pop=={{target.name}}) %>%  
  filter(strata=="age_gr3") %>% 
  select(pop, pop.type, n, time.window, years, events, strata, outcome.name,
         prior.obs.required, age_gr3, db) %>% 
  rename("target.pop"="pop") %>% 
  rename("target.pop.type"="pop.type") %>% 
  rename("target.time.window"="time.window") %>% 
   rename("n_target"="n") %>% 
   rename("years_target"="years") %>% 
   rename("events_target"="events") 

comparator<-Network.IR %>% 
  filter(pop=={{comparator.name}}) %>%
  filter(strata=="age_gr3") %>% 
  select(pop, pop.type, n, time.window, years, events, strata, outcome.name,
         prior.obs.required, age_gr3, db) %>% 
   rename("comparator"="pop") %>% 
   rename("n_comparator"="n") %>% 
   rename("years_comparator"="years") %>% 
   rename("events_comparator"="events")




# for each outcome
# for each target.pop.type
# for each time window
# for each prior.obs.required

db.to.get<-unique(target$db)
outcome.names.to.get<- unique(target$outcome.name)
target.pop.type.to.get<- unique(target$target.pop.type)
target.time.window.to.get<- unique(target$target.time.window)
prior.obs.required.to.get<- unique(target$prior.obs.required)

# d<-1
# o<-1
# pt<-1
# tw<-1
# po<-1

ISR<-list()
for(d in 1:length(db.to.get)){ 
for(o in 1:length(outcome.names.to.get)){ 
for(pt in 1:length(target.pop.type.to.get)){ 
for(tw in 1:length(target.time.window.to.get)){ 
for(po in 1:length(prior.obs.required.to.get)){ 

 print(paste0("DB ", d))
 print(paste0("-- Outcome ", o))

a<-target %>% 
  filter(db==db.to.get[d]) %>% 
  filter(target.pop.type==target.pop.type.to.get[pt]) %>% 
  filter(outcome.name==outcome.names.to.get[o])  %>% 
  filter(target.time.window==target.time.window.to.get[tw]) %>% 
  filter(prior.obs.required==prior.obs.required.to.get[po])
  
b<-comparator %>% 
  filter(db==db.to.get[d]) %>% 
  filter(outcome.name==outcome.names.to.get[o]) %>% 
  filter(prior.obs.required==prior.obs.required.to.get[po])


c<-suppressMessages(a %>% 
  full_join(b))
c<-c %>% # if zero count of persons in an age group
  mutate(events_target=ifelse(is.na(n_target),
                  0,
                  events_target))%>% 
  mutate(years_target=ifelse(is.na(years_target),
                  0,
                  years_target))

isr<-ageadjust.indirect(count=c$events_target,
                        pop=c$years_target,
                        stdcount =c$events_comparator,
                        stdpop = c$years_comparator)


ISR[[paste(db.to.get[d],
           target.pop.type.to.get[pt],
           outcome.names.to.get[o],
           target.time.window.to.get[tw],
           prior.obs.required.to.get[po], sep = ";")]]<-
  data.frame(
    n_target=sum(a$n_target),
   years_target= sum(a$years_target),
   events_target= sum(a$events_target),
   n_comparator= sum(b$n_comparator),
    years_comparator=sum(b$years_comparator),
   events_comparator= sum(b$events_comparator),
data.frame(isr.observed=data.frame(isr$sir)[1,1]),
data.frame(isr.expected=data.frame(isr$sir)[2,1]),
data.frame(isr.sir=data.frame(isr$sir)[3,1]),
data.frame(isr.sir_lower=data.frame(isr$sir)[4,1]),
data.frame(isr.sir_upper=data.frame(isr$sir)[5,1]),
data.frame(ir_100000.crude=data.frame(100000*isr$rate)[1,1]),
data.frame(ir_100000.stand=data.frame(100000*isr$rate)[2,1]),
data.frame(ir_100000_lower.stand=data.frame(100000*isr$rate)[3,1]),
data.frame(ir_100000_upper.stand=data.frame(100000*isr$rate)[4,1])
) %>% 
  mutate(target.pop.type=target.pop.type.to.get[pt]) %>% 
  mutate(outcome.name=outcome.names.to.get[o])  %>% 
  mutate(target.time.window=target.time.window.to.get[tw]) %>% 
  mutate(prior.obs.required=prior.obs.required.to.get[po])%>% 
  mutate(db=db.to.get[d])
}}}}}

ISR<-bind_rows(ISR)

ISR
}

COVID_diagnosis_21d_background.pop.IRS<-get.IRS.df(target.name="COVID19 positive test 21d",
                                  comparator.name="General population (index date: 1st December)")
COVID_diagnosis_90d_background.pop.IRS<-get.IRS.df(target.name="COVID19 positive test 90d", #BERTA
                                               comparator.name="General population (index date: 1st December)")

AZ1_background.pop.IRS<-get.IRS.df(target.name="ChAdOx1 first-dose",
                                  comparator.name="General population (index date: 1st December)")
AZ1.no.covid_background.pop.IRS<-get.IRS.df(target.name="ChAdOx1 first-dose (no prior covid)",
                                  comparator.name="General population (index date: 1st December)")

AZ2_background.pop.IRS<-get.IRS.df(target.name="ChAdOx1 second-dose",
                                   comparator.name="General population (index date: 1st December)")
AZ2.no.covid_background.pop.IRS<-get.IRS.df(target.name="ChAdOx1 second-dose (no prior covid)",
                                            comparator.name="General population (index date: 1st December)")

Pf1_background.pop.IRS<-get.IRS.df(target.name="BNT162b2 first-dose",
                                  comparator.name="General population (index date: 1st December)") 
Pf1.no.covid_background.pop.IRS<-get.IRS.df(target.name="BNT162b2 first-dose (no prior covid)",
                                  comparator.name="General population (index date: 1st December)") 

Pf2_background.pop.IRS<-get.IRS.df(target.name="BNT162b2 second-dose",
                                  comparator.name="General population (index date: 1st December)") 
Pf2.no.covid_background.pop.IRS<-get.IRS.df(target.name="BNT162b2 second-dose (no prior covid)",
                                  comparator.name="General population (index date: 1st December)") 

Jnj_background.pop.IRS<-get.IRS.df(target.name="Ad26.COV2.S first-dose",
                                   comparator.name="General population (index date: 1st December)")
Jnj.no.covid_background.pop.IRS<-get.IRS.df(target.name="Ad26.COV2.S first-dose (no prior covid)",
                                             comparator.name="General population (index date: 1st December)") 

Mod1_background.pop.IRS<-get.IRS.df(target.name="mRNA-1273 first-dose",
                                   comparator.name="General population (index date: 1st December)") 
Mod2_background.pop.IRS<-get.IRS.df(target.name="mRNA-1273 second-dose",
                                  comparator.name="General population (index date: 1st December)") 

Mod1.no.covid_background.pop.IRS<-get.IRS.df(target.name="mRNA-1273 first-dose (no prior covid)",
                                             comparator.name="General population (index date: 1st December)") 
Mod2.no.covid_background.pop.IRS<-get.IRS.df(target.name="mRNA-1273 second-dose (no prior covid)",
                                            comparator.name="General population (index date: 1st December)")

 
# # export as csv -----
# library(openxlsx)
# wb <- createWorkbook()
# addWorksheet(wb=wb, sheetName = "All")
# Network.IR.for.xlsx<-Network.IR %>% 
#   select(db, n,years,events , strata,outcome.name,
#          prior.obs.required,
#          pop.type,
#          gender,
#          age_gr,
#          age_gr2,
#          age_gr3,
#          ir_100000,
#          ir_100000_lower,
#          ir_100000_upper) %>% 
#   filter(strata %in% c("overall", "age_gr2_gender","age_gr_gender", "age_gr3_gender")) %>% 
#   arrange(db, outcome.name)
# writeData(wb, sheet = "All", Network.IR.for.xlsx,rowNames = FALSE)
# saveWorkbook(wb, 
#              here("data", paste0("Network.IR", ".xlsx")), overwrite = TRUE) 


# library(openxlsx)
# wb <- createWorkbook()
# addWorksheet(wb=wb, sheetName = "All")
# a<-Network.IR %>% 
#   filter(strata=="age_gr3_gender") %>% 
#   select(pop, pop.type,
#          n, years, events, ir_100000, age_gr3, gender, outcome.name,
#          time.window, 
#          prior.obs.required)
# writeData(wb, sheet = "All", a,rowNames = FALSE)
# saveWorkbook(wb,
#              here("data", paste0("IncidenceRatesToStandardise", ".xlsx")), overwrite = TRUE)
# 
# 
# wb <- createWorkbook()
# addWorksheet(wb=wb, sheetName = "All")
# a<-Network.IR
# # %>% 
# #   select(pop, pop.type,
# #          n, years, events, ir_100000, age_gr3, gender, outcome.name,
# #          time.window, 
# #          prior.obs.required)
# writeData(wb, sheet = "All", a,rowNames = FALSE)
# saveWorkbook(wb,
#              here("data", paste0("IncidenceRates", ".xlsx")), overwrite = TRUE)




# BERTA (ALL EVENTS)
Network.IR_all<-Network.IR
COVID_diagnosis_21d_background.pop.IRR.all<-COVID_diagnosis_21d_background.pop.IRR 
COVID_diagnosis_90d_background.pop.IRR.all <-COVID_diagnosis_90d_background.pop.IRR 
# COVID_PCR_background.pop.visit.IRR<-COVID_PCR_background.pop.visit.IRR %>% 
AZ1_background.pop.IRR.all <-AZ1_background.pop.IRR 
AZ1.no.covid_background.pop.IRR.all <-AZ1.no.covid_background.pop.IRR 
# AZ1_background.pop.visit.IRR<-AZ1_background.pop.visit.IRR 
AZ2_background.pop.IRR.all <-AZ2_background.pop.IRR 
AZ2.no.covid_background.pop.IRR.all <-AZ2.no.covid_background.pop.IRR 
# AZ2_background.pop.visit.IRR<-AZ2_background.pop.visit.IRR 


Pf1_background.pop.IRR.all <-Pf1_background.pop.IRR
Pf1.no.covid_background.pop.IRR.all <-Pf1.no.covid_background.pop.IRR 
# Pf1_background.pop.visit.IRR<-Pf1_background.pop.visit.IRR %>% 
Pf2_background.pop.IRR.all<-Pf2_background.pop.IRR 
Pf2.no.covid_background.pop.IRR.all<-Pf2.no.covid_background.pop.IRR
#Any1_background.pop.IRR.all<-Any1_background.pop.IRR
#Any1.no.covid_background.pop.IRR.all <-Any1.no.covid_background.pop.IRR 

Mod1_background.pop.IRR.all<-Mod1_background.pop.IRR 
Mod1.no.covid_background.pop.IRR.all<-Mod1.no.covid_background.pop.IRR

# Mod1_background.pop.visit.IRR<-Mod1_background.pop.visit.IRR 
Mod2_background.pop.IRR.all<-Mod2_background.pop.IRR
Mod2.no.covid_background.pop.IRR.all<-Mod2.no.covid_background.pop.IRR

# Mod2_background.pop.visit.IRR<-Mod2_background.pop.visit.IRR
Jnj_background.pop.IRR.all<-Jnj_background.pop.IRR 
Jnj.no.covid_background.pop.IRR.all<-Jnj.no.covid_background.pop.IRR

# Jnj_background.pop.visit.IRR<-Jnj_background.pop.visit.IRR 
# 

COVID_diagnosis_21d_background.pop.IRS.all<-COVID_diagnosis_21d_background.pop.IRS
COVID_diagnosis_90d_background.pop.IRS.all<-COVID_diagnosis_90d_background.pop.IRS
# COVID_PCR_background.pop.visit.IRS<-COVID_PCR_background.pop.visit.IRS 
AZ1_background.pop.IRS.all<-AZ1_background.pop.IRS
AZ1.no.covid_background.pop.IRS.all<-AZ1.no.covid_background.pop.IRS
# AZ1_background.pop.visit.IRS<-AZ1_background.pop.visit.IRS 
AZ2_background.pop.IRS.all<-AZ2_background.pop.IRS
AZ2.no.covid_background.pop.IRS.all<-AZ2.no.covid_background.pop.IRS
# AZ2_background.pop.visit.IRS<-AZ2_background.pop.visit.IRS 
Pf1_background.pop.IRS.all<-Pf1_background.pop.IRS 
Pf1.no.covid_background.pop.IRS.all<-Pf1.no.covid_background.pop.IRS
# Pf1_background.pop.visit.IRS<-Pf1_background.pop.visit.IRS
Pf2_background.pop.IRS.all<-Pf2_background.pop.IRS 
Pf2.no.covid_background.pop.IRS.all<-Pf2.no.covid_background.pop.IRS
#Any1_background.pop.IRS.all<-Any1_background.pop.IRS # BERTA
#Any1.no.covid_background.pop.IRS.all<-Any1.no.covid_background.pop.IRS 
Mod1_background.pop.IRS.all<-Mod1_background.pop.IRS
Mod1.no.covid_background.pop.IRS.all<-Mod1.no.covid_background.pop.IRS
Mod2_background.pop.IRS.all<-Mod2_background.pop.IRS
Mod2.no.covid_background.pop.IRS.all<-Mod2.no.covid_background.pop.IRS
Jnj_background.pop.IRS.all<-Jnj_background.pop.IRS 
Jnj.no.covid_background.pop.IRS.all<-Jnj.no.covid_background.pop.IRS


# obscure counts less than 5 -----
Network.IR<-Network.IR %>% 
  filter(events>=5)
COVID_diagnosis_21d_background.pop.IRR<-COVID_diagnosis_21d_background.pop.IRR %>% 
  filter(events_target>=5)
COVID_diagnosis_90d_background.pop.IRR<-COVID_diagnosis_90d_background.pop.IRR %>% 
  filter(events_target>=5)
# COVID_diagnosis_background.pop.visit.IRR<-COVID_diagnosis_background.pop.visit.IRR %>% 
#   filter(events_target>=5)

AZ1_background.pop.IRR<-AZ1_background.pop.IRR %>% 
  filter(events_target>=5)
AZ1.no.covid_background.pop.IRR<-AZ1.no.covid_background.pop.IRR %>% 
  filter(events_target>=5)
# AZ1_background.pop.visit.IRR<-AZ1_background.pop.visit.IRR %>% 
#   filter(events_target>=5)
AZ2_background.pop.IRR<-AZ2_background.pop.IRR %>% 
  filter(events_target>=5)
AZ2.no.covid_background.pop.IRR<-AZ2.no.covid_background.pop.IRR %>% 
  filter(events_target>=5)
# AZ2_background.pop.visit.IRR<-AZ2_background.pop.visit.IRR %>% 
#   filter(events_target>=5)

Pf1_background.pop.IRR<-Pf1_background.pop.IRR %>% 
  filter(events_target>=5)
Pf1.no.covid_background.pop.IRR<-Pf1.no.covid_background.pop.IRR %>% 
  filter(events_target>=5)
# Pf1_background.pop.visit.IRR<-Pf1_background.pop.visit.IRR %>% 
#   filter(events_target>=5)
Pf2_background.pop.IRR<-Pf2_background.pop.IRR %>% 
  filter(events_target>=5)
Pf2.no.covid_background.pop.IRR<-Pf2.no.covid_background.pop.IRR %>% 
  filter(events_target>=5)

#Any1_background.pop.IRR<-Any1_background.pop.IRR %>% # BERTA
#  filter(events_target>=5)
#Any1.no.covid_background.pop.IRR<-Any1.no.covid_background.pop.IRR %>% 
#  filter(events_target>=5)

# Pf2_background.pop.visit.IRR<-Pf2_background.pop.visit.IRR %>% 
#   filter(events_target>=5)
Mod1_background.pop.IRR<-Mod1_background.pop.IRR %>% 
   filter(events_target>=5)
Mod1.no.covid_background.pop.IRR<-Mod1.no.covid_background.pop.IRR %>% 
   filter(events_target>=5)
 
# Mod1_background.pop.visit.IRR<-Mod1_background.pop.visit.IRR %>% 
#   filter(events_target>=5)
Mod2_background.pop.IRR<-Mod2_background.pop.IRR %>% 
   filter(events_target>=5)
Mod2.no.covid_background.pop.IRR<-Mod2.no.covid_background.pop.IRR %>% 
  filter(events_target>=5)

# Mod2_background.pop.visit.IRR<-Mod2_background.pop.visit.IRR %>% 
#   filter(events_target>=5)
 Jnj_background.pop.IRR<-Jnj_background.pop.IRR %>% 
   filter(events_target>=5)
# Jnj_background.pop.visit.IRR<-Jnj_background.pop.visit.IRR %>% 
#   filter(events_target>=5)
# 
#


COVID_diagnosis_21d_background.pop.IRS<-COVID_diagnosis_21d_background.pop.IRS %>%
  filter(events_target>=5)
COVID_diagnosis_90d_background.pop.IRS<-COVID_diagnosis_90d_background.pop.IRS %>%
  filter(events_target>=5)
# COVID_PCR_21d_background.pop.visit.IRS<-COVID_PCR_21d_background.pop.visit.IRS %>% 
#   filter(events_target>=5)
# COVID_PCR_90d_background.pop.visit.IRS<-COVID_PCR_90d_background.pop.visit.IRS %>% 
#   filter(events_target>=5)

AZ1_background.pop.IRS<-AZ1_background.pop.IRS %>%
  filter(events_target>=5)
AZ1.no.covid_background.pop.IRS<-AZ1.no.covid_background.pop.IRS %>%
  filter(events_target>=5)
# AZ1_background.pop.visit.IRS<-AZ1_background.pop.visit.IRS %>% 
#   filter(events_target>=5)
AZ2_background.pop.IRS<-AZ2_background.pop.IRS %>%
  filter(events_target>=5)
AZ2.no.covid_background.pop.IRS<-AZ2.no.covid_background.pop.IRS %>%
  filter(events_target>=5)
# AZ2_background.pop.visit.IRS<-AZ2_background.pop.visit.IRS %>% 
#   filter(events_target>=5)


Pf1_background.pop.IRS<-Pf1_background.pop.IRS %>%
  filter(events_target>=5)
Pf1.no.covid_background.pop.IRS<-Pf1.no.covid_background.pop.IRS %>%
  filter(events_target>=5)
# Pf1_background.pop.visit.IRS<-Pf1_background.pop.visit.IRS %>% 
#   filter(events_target>=5)
Pf2_background.pop.IRS<-Pf2_background.pop.IRS %>%
  filter(events_target>=5)
Pf2.no.covid_background.pop.IRS<-Pf2.no.covid_background.pop.IRS %>%
  filter(events_target>=5)

#Any1_background.pop.IRS<-Any1_background.pop.IRS %>% # BERTA
# filter(events_target>=5)
#Any1.no.covid_background.pop.IRS<-Any1.no.covid_background.pop.IRS %>% 
#  filter(events_target>=5)

# Pf2_background.pop.visit.IRS<-Pf2_background.pop.visit.IRS %>% 
#   filter(events_target>=5)
# 
 Jnj_background.pop.IRS<-Jnj_background.pop.IRS %>% 
   filter(events_target>=5)
Jnj.no.covid_background.pop.IRS<-Jnj.no.covid_background.pop.IRS %>%
   filter(events_target>=5)
 
 Mod1_background.pop.IRS<-Mod1_background.pop.IRS %>% 
   filter(events_target>=5)
 Mod1.no.covid_background.pop.IRS<-Mod1.no.covid_background.pop.IRS %>%
   filter(events_target>=5)
 
# Mod1_background.pop.visit.IRS<-Mod1_background.pop.visit.IRS %>% 
#   filter(events_target>=5)
 Mod2_background.pop.IRS<-Mod2_background.pop.IRS %>% 
   filter(events_target>=5)
 Mod2.no.covid_background.pop.IRS<-Mod2.no.covid_background.pop.IRS %>%
   filter(events_target>=5)
 
# Mod2_background.pop.visit.IRS<-Mod2_background.pop.visit.IRS %>% 
#   filter(events_target>=5)



# limit to TTS events -----

table(Network.IR$outcome.name)


tts_events<-c(
  "Bell's Palsy",
  "Encephalomyelitis",
  "Guillain Barre Syndrome",
  "Transverse Myelitis")

# Network.IR1<-Network.IR %>%
#   filter(outcome.name %in% tts_events)
#
# a<-data.frame(table(Network.IR$outcome.name))
# b<-data.frame(table(Network.IR1$outcome.name))
# c<-a %>% left_join(b %>% rename("Var2"="Var1"))
# data.frame(table(Network.IR1$outcome.name))

Network.IR<-Network.IR %>%
  filter(outcome.name %in% tts_events)

Network.Survival.summary<-Network.Survival.summary %>%
  filter(outcome.name %in% tts_events)
COVID_diagnosis_21d_background.pop.IRR<-COVID_diagnosis_21d_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
COVID_diagnosis_90d_background.pop.IRR<-COVID_diagnosis_90d_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
# COVID_diagnosis_background.pop.visit.IRR<-COVID_PCR_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)

AZ1_background.pop.IRR<-AZ1_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
AZ1.no.covid_background.pop.IRR<-AZ1.no.covid_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
# AZ1_background.pop.visit.IRR<-AZ1_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)
AZ2_background.pop.IRR<-AZ2_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
AZ2.no.covid_background.pop.IRR<-AZ2.no.covid_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
# AZ2_background.pop.visit.IRR<-AZ2_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)


Pf1_background.pop.IRR<-Pf1_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
Pf1.no.covid_background.pop.IRR<-Pf1.no.covid_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
Pf2_background.pop.IRR<-Pf2_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
Pf2.no.covid_background.pop.IRR<-Pf2.no.covid_background.pop.IRR %>%
  filter(outcome.name %in% tts_events)
# Pf_background.pop.visit.IRR<-Pf_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)
#Any1_background.pop.IRR<-Any1_background.pop.IRR %>% # BERTA
#  filter(outcome.name %in% tts_events)
#Any1.no.covid_background.pop.IRR<-Any1.no.covid_background.pop.IRR %>% 
 # filter(outcome.name %in% tts_events)
Jnj_background.pop.IRR<-Jnj_background.pop.IRR %>% # BERTA
  filter(outcome.name %in% tts_events)
Jnj.no.covid_background.pop.IRR<-Jnj.no.covid_background.pop.IRR %>% 
  filter(outcome.name %in% tts_events)
Mod1_background.pop.IRR<-Mod1_background.pop.IRR %>% # BERTA
  filter(outcome.name %in% tts_events)
Mod1.no.covid_background.pop.IRR<-Mod1.no.covid_background.pop.IRR %>% 
  filter(outcome.name %in% tts_events)
Mod2_background.pop.IRR<-Mod2_background.pop.IRR %>% # BERTA
  filter(outcome.name %in% tts_events)
# Mod2.no.covid_background.pop.IRR<-Mod2.no.covid_background.pop.IRR %>% 
#   filter(outcome.name %in% tts_events)






COVID_diagnosis_21d_background.pop.IRS<-COVID_diagnosis_21d_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
COVID_diagnosis_90d_background.pop.IRS<-COVID_diagnosis_90d_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)

# COVID_PCR_background.pop.visit.IRS<-COVID_PCR_background.pop.visit.IRS %>%
#   filter(outcome.name %in% tts_events)
AZ1_background.pop.IRS<-AZ1_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
AZ1.no.covid_background.pop.IRS<-AZ1.no.covid_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
# AZ1_background.pop.visit.IRS<-AZ1_background.pop.visit.IRS %>%
#   filter(outcome.name %in% tts_events)
AZ2_background.pop.IRS<-AZ2_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
AZ2.no.covid_background.pop.IRS<-AZ2.no.covid_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
# AZ2_background.pop.visit.IRS<-AZ2_background.pop.visit.IRS %>%
#   filter(outcome.name %in% tts_events)

Pf1_background.pop.IRS<-Pf1_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
Pf1.no.covid_background.pop.IRS<-Pf1.no.covid_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
Pf2_background.pop.IRS<-Pf2_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
Pf2.no.covid_background.pop.IRS<-Pf2.no.covid_background.pop.IRS %>%
  filter(outcome.name %in% tts_events)
#Any1_background.pop.IRS<-Any1_background.pop.IRS %>% # BERTA
 # filter(outcome.name %in% tts_events)
#Any1.no.covid_background.pop.IRS<-Any1.no.covid_background.pop.IRS %>% 
#  filter(outcome.name %in% tts_events)
Jnj_background.pop.IRS<-Jnj_background.pop.IRS %>% # BERTA
  filter(outcome.name %in% tts_events)
Jnj.no.covid_background.pop.IRS<-Jnj.no.covid_background.pop.IRS %>% 
  filter(outcome.name %in% tts_events)
Mod1_background.pop.IRR<-Mod1_background.pop.IRR %>% # BERTA
  filter(outcome.name %in% tts_events)
Mod1.no.covid_background.pop.IRS<-Mod1.no.covid_background.pop.IRS %>% 
  filter(outcome.name %in% tts_events)
Mod2_background.pop.IRS<-Mod2_background.pop.IRS %>% # BERTA
  filter(outcome.name %in% tts_events)
Mod2.no.covid_background.pop.IRS<-Mod2.no.covid_background.pop.IRS %>% 
  filter(outcome.name %in% tts_events)



##Berta (all events) -----
Network.IR.all<-Network.IR_all %>%
  filter(outcome.name %in% tts_events)

COVID_diagnosis_21d_background.pop.IRR.all<-COVID_diagnosis_21d_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
COVID_diagnosis_90d_background.pop.IRR.all<-COVID_diagnosis_90d_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
# COVID_diagnosis_background.pop.visit.IRR<-COVID_PCR_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)

AZ1_background.pop.IRR.all<-AZ1_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
AZ1.no.covid_background.pop.IRR.all<-AZ1.no.covid_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
# AZ1_background.pop.visit.IRR<-AZ1_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)
AZ2_background.pop.IRR.all<-AZ2_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
AZ2.no.covid_background.pop.IRR.all<-AZ2.no.covid_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
# AZ2_background.pop.visit.IRR<-AZ2_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)

Pf1_background.pop.IRR.all<-Pf1_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
Pf1.no.covid_background.pop.IRR.all<-Pf1.no.covid_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
Pf2_background.pop.IRR.all<-Pf2_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
Pf2.no.covid_background.pop.IRR.all<-Pf2.no.covid_background.pop.IRR.all %>%
  filter(outcome.name %in% tts_events)
# Pf_background.pop.visit.IRR<-Pf_background.pop.visit.IRR %>%
#   filter(outcome.name %in% tts_events)
#Any1_background.pop.IRR.all<-Any1_background.pop.IRR.all %>% # BERTA
#  filter(outcome.name %in% tts_events)
#Any1.no.covid_background.pop.IRR.all<-Any1.no.covid_background.pop.IRR.all %>% 
 # filter(outcome.name %in% tts_events)

Jnj_background.pop.IRR.all<-Jnj_background.pop.IRR.all %>% # BERTA
  filter(outcome.name %in% tts_events)
Jnj.no.covid_background.pop.IRR.all<-Jnj.no.covid_background.pop.IRR.all %>% 
  filter(outcome.name %in% tts_events)
Mod1_background.pop.IRR.all<-Mod1_background.pop.IRR.all %>% # BERTA
  filter(outcome.name %in% tts_events)
Mod1.no.covid_background.pop.IRR.all<-Mod1.no.covid_background.pop.IRR.all %>% 
  filter(outcome.name %in% tts_events)
Mod2_background.pop.IRR.all<-Mod2_background.pop.IRR.all %>% # BERTA
  filter(outcome.name %in% tts_events)
Mod2.no.covid_background.pop.IRR.all<-Mod2.no.covid_background.pop.IRR.all %>% 
  filter(outcome.name %in% tts_events)








COVID_diagnosis_21d_background.pop.IRS.all<-COVID_diagnosis_21d_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
COVID_diagnosis_90d_background.pop.IRS.all<-COVID_diagnosis_90d_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)


# COVID_PCR_background.pop.visit.IRS<-COVID_PCR_background.pop.visit.IRS %>%
#   filter(outcome.name %in% tts_events)
AZ1_background.pop.IRS.all<-AZ1_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
AZ1.no.covid_background.pop.IRS.all<-AZ1.no.covid_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
# AZ1_background.pop.visit.IRS<-AZ1_background.pop.visit.IRS %>%
#   filter(outcome.name %in% tts_events)
AZ2_background.pop.IRS.all<-AZ2_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
AZ2.no.covid_background.pop.IRS.all<-AZ2.no.covid_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
# AZ2_background.pop.visit.IRS<-AZ2_background.pop.visit.IRS %>%
#   filter(outcome.name %in% tts_events)

Pf1_background.pop.IRS.all<-Pf1_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
Pf1.no.covid_background.pop.IRS.all<-Pf1.no.covid_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
Pf2_background.pop.IRS.all<-Pf2_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
Pf2.no.covid_background.pop.IRS.all<-Pf2.no.covid_background.pop.IRS.all %>%
  filter(outcome.name %in% tts_events)
#Any1_background.pop.IRS.all<-Any1_background.pop.IRS.all %>% # BERTA
  #filter(outcome.name %in% tts_events)
#Any1.no.covid_background.pop.IRS.all<-Any1.no.covid_background.pop.IRS.all %>% 
 # filter(outcome.name %in% tts_events)
# Pf_background.pop.visit.IRS<-Pf_background.pop.visit.IRS %>%
#   filter(outcome.name %in% tts_events)
Jnj_background.pop.IRS.all<-Jnj_background.pop.IRS.all %>% # BERTA
  filter(outcome.name %in% tts_events)
Jnj.no.covid_background.pop.IRS.all<-Jnj.no.covid_background.pop.IRS.all %>% 
  filter(outcome.name %in% tts_events)
Mod1_background.pop.IRR.all<-Mod1_background.pop.IRR.all %>% # BERTA
  filter(outcome.name %in% tts_events)
Mod1.no.covid_background.pop.IRS.all<-Mod1.no.covid_background.pop.IRS.all %>% 
  filter(outcome.name %in% tts_events)
Mod2_background.pop.IRS.all<-Mod2_background.pop.IRS.all %>% # BERTA
  filter(outcome.name %in% tts_events)
Mod2.no.covid_background.pop.IRS.all<-Mod2.no.covid_background.pop.IRS.all %>% 
  filter(outcome.name %in% tts_events)

# save -----

save(Network.patient.characteristcis, file = here("data", "Network.patient.characteristcis.RData"))
save(Network.IR, file = here("data", "Network.IR.RData"))
save(Network.Survival.summary, file = here("data", "Network.Survival.summary.RData"))

save(COVID_diagnosis_21d_background.pop.IRR, file = here("data", "COVID_diagnosis_21d_background.pop.IRR.RData"))
save(COVID_diagnosis_90d_background.pop.IRR, file = here("data", "COVID_diagnosis_90d_background.pop.IRR.RData"))

# save(COVID_diagnosis_background.pop.visit.IRR, file = here("data", "COVID_diagnosis_background.pop.visit.IRR.RData"))

save(AZ1_background.pop.IRR, file = here("data", "AZ1_background.pop.IRR.RData"))
save(AZ1.no.covid_background.pop.IRR, file = here("data", "AZ1.no.covid_background.pop.IRR.RData"))
# save(AZ1_background.pop.visit.IRR, file = here("data", "AZ1_background.pop.visit.IRR.RData"))
save(AZ2_background.pop.IRR, file = here("data", "AZ2_background.pop.IRR.RData"))
save(AZ2.no.covid_background.pop.IRR, file = here("data", "AZ2.no.covid_background.pop.IRR.RData"))
# save(AZ2_background.pop.visit.IRR, file = here("data", "AZ2_background.pop.visit.IRR.RData"))

save(Pf1_background.pop.IRR, file = here("data", "Pf1_background.pop.IRR.RData"))
save(Pf1.no.covid_background.pop.IRR, file = here("data", "Pf1.no.covid_background.pop.IRR.RData"))
# save(Pf1_background.pop.visit.IRR, file = here("data", "Pf1_background.pop.visit.IRR.RData"))
save(Pf2_background.pop.IRR, file = here("data", "Pf2_background.pop.IRR.RData"))
save(Pf2.no.covid_background.pop.IRR, file = here("data", "Pf2.no.covid_background.pop.IRR.RData"))
# save(Pf2_background.pop.visit.IRR, file = here("data", "Pf2_background.pop.visit.IRR.RData"))

#save(Any1_background.pop.IRR, file = here("data", "Any1_background.pop.IRR.RData"))
#save(Any1.no.covid_background.pop.IRR, file = here("data", "Any1.no.covid_background.pop.IRR.RData"))
save(Mod1_background.pop.IRR, file = here("data", "Mod1_background.pop.IRR.RData"))
save(Mod1.no.covid_background.pop.IRR, file = here("data", "Mod1.no.covid_background.pop.IRR.RData"))
save(Mod2_background.pop.IRR, file = here("data", "Mod2_background.pop.IRR.RData"))
save(Mod2.no.covid_background.pop.IRR, file = here("data", "Mod2.no.covid_background.pop.IRR.RData"))
save(Jnj_background.pop.IRR, file = here("data", "Jnj_background.pop.IRR.RData"))
save(Jnj.no.covid_background.pop.IRR, file = here("data", "Jnj.no.covid_background.pop.IRR.RData"))

save(COVID_diagnosis_21d_background.pop.IRS, file = here("data", "COVID_diagnosis_21d_background.pop.IRS.RData"))
save(COVID_diagnosis_90d_background.pop.IRS, file = here("data", "COVID_diagnosis_90d_background.pop.IRS.RData"))
# save(COVID_diagnosis_21d_background.pop.visit.IRS, file = here("data", "COVID_diagnosis_21d_background.pop.visit.IRS.RData"))
# save(COVID_diagnosis_90d_background.pop.visit.IRS, file = here("data", "COVID_diagnosis_90d_background.pop.visit.IRS.RData"))


save(AZ1_background.pop.IRS, file = here("data", "AZ1_background.pop.IRS.RData"))
save(AZ1.no.covid_background.pop.IRS, file = here("data", "AZ1.no.covid_background.pop.IRS.RData"))
# save(AZ1_background.pop.visit.IRS, file = here("data", "AZ1_background.pop.visit.IRS.RData"))
save(AZ2_background.pop.IRS, file = here("data", "AZ2_background.pop.IRS.RData"))
save(AZ2.no.covid_background.pop.IRS, file = here("data", "AZ2.no.covid_background.pop.IRS.RData"))
# save(AZ1_background.pop.visit.IRS, file = here("data", "AZ2_background.pop.visit.IRS.RData"))



save(Pf1_background.pop.IRS, file = here("data", "Pf1_background.pop.IRS.RData"))
save(Pf1.no.covid_background.pop.IRS, file = here("data", "Pf1.no.covid_background.pop.IRS.RData"))
# save(Pf1_background.pop.visit.IRS, file = here("data", "Pf1_background.pop.visit.IRS.RData"))
save(Pf2_background.pop.IRS, file = here("data", "Pf2_background.pop.IRS.RData"))
save(Pf2.no.covid_background.pop.IRS, file = here("data", "Pf2.no.covid_background.pop.IRS.RData"))
# save(Pf2_background.pop.visit.IRS, file = here("data", "Pf2_background.pop.visit.IRS.RData"))
#save(Any1_background.pop.IRS, file = here("data", "Any1_background.pop.IRS.RData"))#BERTA
#save(Any1.no.covid_background.pop.IRS, file = here("data", "Any1.no.covid_background.pop.IRS.RData"))

save(Mod1_background.pop.IRS, file = here("data", "Mod1_background.pop.IRS.RData"))
save(Mod1.no.covid_background.pop.IRS, file = here("data", "Mod1.no.covid_background.pop.IRS.RData"))
#save(Mod1_background.pop.visit.IRS, file = here("data", "Mod1_background.pop.visit.IRS.RData"))
save(Mod2_background.pop.IRS, file = here("data", "Mod2_background.pop.IRS.RData"))
save(Mod2.no.covid_background.pop.IRS, file = here("data", "Mod2.no.covid_background.pop.IRS.RData"))

# save(Mod2_background.pop.visit.IRS, file = here("data", "Mod2_background.pop.visit.IRS.RData"))
# 
save(Jnj_background.pop.IRS, file = here("data", "Jnj_background.pop.IRS.RData"))
save(Jnj.no.covid_background.pop.IRS, file = here("data", "Jnj.no.covid_background.pop.IRS.RData"))
# save(Jnj_background.pop.visit.IRS, file = here("data", "Jnj_background.pop.visit.IRS.RData"))


# save: Berta (all events)-----
save(Network.IR.all, file = here("data", "Network.IR.all.RData"))
save(COVID_diagnosis_21d_background.pop.IRR.all, file = here("data", "COVID_diagnosis_21d_background.pop.IRR.all.RData"))
save(COVID_diagnosis_90d_background.pop.IRR.all, file = here("data", "COVID_diagnosis_90d_background.pop.IRR.all.RData"))
# save(COVID_diagnosis_21d_background.pop.visit.IRR, file = here("data", "COVID_diagnosis_21d_background.pop.visit.IRR.RData"))
# save(COVID_diagnosis_90d_background.pop.visit.IRR, file = here("data", "COVID_diagnosis_90d_background.pop.visit.IRR.RData"))

save(AZ1_background.pop.IRR.all, file = here("data", "AZ1_background.pop.IRR.all.RData"))
save(AZ1.no.covid_background.pop.IRR.all, file = here("data", "AZ1.no.covid_background.pop.IRR.all.RData"))
# save(AZ1_background.pop.visit.IRR, file = here("data", "AZ1_background.pop.visit.IRR.RData"))
save(AZ2_background.pop.IRR.all, file = here("data", "AZ2_background.pop.IRR.all.RData"))
save(AZ2.no.covid_background.pop.IRR.all, file = here("data", "AZ2.no.covid_background.pop.IRR.all.RData"))
# save(AZ2_background.pop.visit.IRR, file = here("data", "AZ2_background.pop.visit.IRR.RData"))


save(Pf1_background.pop.IRR.all, file = here("data", "Pf1_background.pop.IRR.all.RData"))
save(Pf1.no.covid_background.pop.IRR.all, file = here("data", "Pf1.no.covid_background.pop.IRR.all.RData"))
# save(Pf1_background.pop.visit.IRR, file = here("data", "Pf1_background.pop.visit.IRR.RData"))
save(Pf2_background.pop.IRR.all, file = here("data", "Pf2_background.pop.IRR.all.RData"))
save(Pf2.no.covid_background.pop.IRR.all, file = here("data", "Pf2.no.covid_background.pop.IRR.all.RData"))
#save(Any1_background.pop.IRR.all, file = here("data", "Any1_background.pop.IRR.all.RData"))#BERTA
#save(Any1.no.covid_background.pop.IRR.all, file = here("data", "Any1.no.covid_background.pop.IRR.all.RData")) 
# save(Pf2_background.pop.visit.IRR, file = here("data", "Pf2_background.pop.visit.IRR.RData"))
 save(Mod1_background.pop.IRR.all, file = here("data", "Mod1_background.pop.IRR.all.RData"))
 save(Mod1.no.covid_background.pop.IRR.all, file = here("data", "Mod1.no.covid_background.pop.IRR.all.RData"))
# save(Mod1_background.pop.visit.IRR, file = here("data", "Mod1_background.pop.visit.IRR.RData"))
 save(Mod2_background.pop.IRR.all, file = here("data", "Mod2_background.pop.IRR.all.RData"))
 save(Mod2.no.covid_background.pop.IRR.all, file = here("data", "Mod2.no.covid_background.pop.IRR.all.RData"))
# save(Mod2_background.pop.visit.IRR, file = here("data", "Mod2_background.pop.visit.IRR.RData"))
 save(Jnj_background.pop.IRR.all, file = here("data", "Jnj_background.pop.IRR.all.RData"))
 save(Jnj.no.covid_background.pop.IRR.all, file = here("data", "Jnj.no.covid_background.pop.IRR.all.RData"))
# save(Jnj_background.pop.visit.IRR, file = here("data", "Jnj_background.pop.visit.IRR.RData"))

save(COVID_diagnosis_21d_background.pop.IRS.all, file = here("data", "COVID_diagnosis_21d_background.pop.IRS.all.RData"))
save(COVID_diagnosis_90d_background.pop.IRS.all, file = here("data", "COVID_diagnosis_90d_background.pop.IRS.all.RData"))
# save(COVID_diagnosis_21d_background.pop.visit.IRS, file = here("data", "COVID_diagnosis_21d_background.pop.visit.IRS.RData"))
# save(COVID_diagnosis_90d_background.pop.visit.IRS, file = here("data", "COVID_diagnosis_90d_background.pop.visit.IRS.RData"))


save(AZ1_background.pop.IRS.all, file = here("data", "AZ1_background.pop.IRS.all.RData"))
save(AZ1.no.covid_background.pop.IRS.all, file = here("data", "AZ1.no.covid_background.pop.IRS.all.RData"))
# save(AZ1_background.pop.visit.IRS, file = here("data", "AZ1_background.pop.visit.IRS.RData"))
save(AZ2_background.pop.IRS.all, file = here("data", "AZ2_background.pop.IRS.all.RData"))
save(AZ2.no.covid_background.pop.IRS.all, file = here("data", "AZ2.no.covid_background.pop.IRS.all.RData"))
# save(AZ2_background.pop.visit.IRS, file = here("data", "AZ2_background.pop.visit.IRS.RData"))

save(Pf1_background.pop.IRS.all, file = here("data", "Pf1_background.pop.IRS.all.RData"))
save(Pf1.no.covid_background.pop.IRS.all, file = here("data", "Pf1.no.covid_background.pop.IRS.all.RData"))
# save(Pf1_background.pop.visit.IRS, file = here("data", "Pf1_background.pop.visit.IRS.RData"))
save(Pf2_background.pop.IRS.all, file = here("data", "Pf2_background.pop.IRS.all.RData"))
save(Pf2.no.covid_background.pop.IRS.all, file = here("data", "Pf2.no.covid_background.pop.IRS.all.RData"))
# save(Pf2_background.pop.visit.IRS, file = here("data", "Pf2_background.pop.visit.IRS.RData"))
#save(Any1_background.pop.IRS.all, file = here("data", "Any1_background.pop.IRS.all.RData"))#BERTA
#save(Any1.no.covid_background.pop.IRS.all, file = here("data", "Any1.no.covid_background.pop.IRS.all.RData"))
 save(Mod1_background.pop.IRS.all, file = here("data", "Mod1_background.pop.IRS.all.RData"))
 save(Mod1.no.covid_background.pop.IRS.all, file = here("data", "Mod1.no.covid_background.pop.IRS.all.RData"))
 
# save(Mod1_background.pop.visit.IRS, file = here("data", "Mod1_background.pop.visit.IRS.RData"))
 save(Mod2_background.pop.IRS.all, file = here("data", "Mod2_background.pop.IRS.all.RData"))
 save(Mod2.no.covid_background.pop.IRS.all, file = here("data", "Mod2.no.covid_background.pop.IRS.all.RData"))
# save(Mod2_background.pop.visit.IRS, file = here("data", "Mod2_background.pop.visit.IRS.RData"))
# 
 save(Jnj_background.pop.IRS.all, file = here("data", "Jnj_background.pop.IRS.all.RData"))
 save(Jnj.no.covid_background.pop.IRS.all, file = here("data", "Jnj.no.covid_background.pop.IRS.all.RData"))
# save(Jnj_background.pop.visit.IRS, file = here("data", "Jnj_background.pop.visit.IRS.RData"))
