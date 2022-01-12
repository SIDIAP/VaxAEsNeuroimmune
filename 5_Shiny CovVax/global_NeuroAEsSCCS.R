
# SCCS MDDR and results -----
tosOfInterest <- read_delim("C:/Users/eburn/Dropbox/OHDSI/covid/Coagulopathy/sidiap github vax incidence general code/VaxAEsNeuroimmune/3_SccsAnalysis/Settings/tosOfInterest.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE) 
tosOfInterest<-tosOfInterest[,2:6]

# outcome.ids<-tosOfInterest %>% 
#   select(outcomeId, outcomeName) %>% 
#   distinct()
# exposure.ids<-tosOfInterest %>% 
#   select(exposureId, exposureName ) %>% 
#   distinct()

analysis.ids<-data.frame(
  analysisId=c(1,2,3,4,5),
  analysisName=c("Simplest model", 
                 "age and season model 21d split",
                 "age and season model with 1yr pre-vax",
                 "age and season model post-covid 90d",
                 "age and season model 21 d without split"))


MDRR_sidiap <- read_csv("C:/Users/eburn/Dropbox/OHDSI/covid/Coagulopathy/sidiap github vax incidence general code/VaxAEsNeuroimmune/3_SccsAnalysis/output.folder/MDRR_all_sidiap.csv")



MDRR_sidiap <-MDRR_sidiap %>% 
  mutate(db="SIDIAP") %>% 
  mutate(history.req=ifelse(str_detect(ExposureGroup, "w_history"),
                                   "Yes", "No"))

# add cprd here

MDRR<-bind_rows(MDRR_sidiap) %>% # add cprd here
  left_join(tosOfInterest) %>% 
  left_join(analysis.ids)

table(MDRR$exposureName)
MDRR<-MDRR %>% 
  filter(str_detect(exposureName,"second-dose", negate = TRUE))
table(MDRR$exposureName)

save(MDRR, file = here("data", "MDRR.RData"))



sccsSummary_all_sidiap <- read_csv("C:/Users/eburn/Dropbox/OHDSI/covid/Coagulopathy/sidiap github vax incidence general code/VaxAEsNeuroimmune/3_SccsAnalysis/output.folder/sccsSummary_all_sidiap.csv")

sccsSummary_all_sidiap <-sccsSummary_all_sidiap %>% 
  mutate(db="SIDIAP")%>% 
  mutate(history.req=ifelse(str_detect(ExposureGroup, "w_history"),
                                   "Yes", "No"))

# add cprd here

nrow(sccsSummary_all_sidiap %>% 
       select(analysisId,exposureId,
              outcomeId, ExposureGroup) %>% 
       distinct())/
  nrow(sccsSummary_all_sidiap)


sccsSummary<-bind_rows(sccsSummary_all_sidiap) %>%  # add cprd here
  left_join(analysis.ids) %>%
  left_join(tosOfInterest) 


# split up, and join - tidy format
sccsSummary.covid.90d <-sccsSummary %>% 
  filter(!is.na(`rr(post-covid-19 90d)`)) %>% 
  rename("rr"="rr(post-covid-19 90d)") %>% 
  rename("ci95lb"="ci95lb(post-covid-19 90d)") %>% 
  rename("ci95ub"="ci95ub(post-covid-19 90d)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req,rr, ci95lb,ci95ub) %>% 
  filter(exposureName=="COVID19 PCR positive test 90d")%>% 
  mutate(time="full")

sccsSummary.21d.no.split <-sccsSummary %>% 
  filter(!is.na(`rr(post-vaccine 21d no split)`)) %>% 
  rename("rr"="rr(post-vaccine 21d no split)") %>% 
  rename("ci95lb"="ci95lb(post-vaccine 21d no split)") %>% 
  rename("ci95ub"="ci95ub(post-vaccine 21d no split)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req, rr, ci95lb,ci95ub) %>% 
  mutate(time="full")

sccsSummary.day0 <-sccsSummary %>% 
  filter(!is.na(`rr(vaccine 0)`)) %>% 
  rename("rr"="rr(vaccine 0)") %>% 
  rename("ci95lb"="ci95lb(vaccine 0)") %>% 
  rename("ci95ub"="ci95ub(vaccine 0)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req,rr, ci95lb,ci95ub)  %>% 
  mutate(time="Day 0")

sccsSummary.day1_7 <-sccsSummary %>% 
  filter(!is.na(`rr(post-vaccine 21d day 1-7)`)) %>% 
  rename("rr"="rr(post-vaccine 21d day 1-7)") %>% 
  rename("ci95lb"="ci95lb(post-vaccine 21d day 1-7)") %>% 
  rename("ci95ub"="ci95ub(post-vaccine 21d day 1-7)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req,rr, ci95lb,ci95ub)  %>% 
  mutate(time="Days 1 to 7")

sccsSummary.day8_14 <-sccsSummary %>% 
  filter(!is.na(`rr(post-vaccine 21d day 8-14)`)) %>% 
  rename("rr"="rr(post-vaccine 21d day 8-14)") %>% 
  rename("ci95lb"="ci95lb(post-vaccine 21d day 8-14)") %>% 
  rename("ci95ub"="ci95ub(post-vaccine 21d day 8-14)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req,rr, ci95lb,ci95ub)  %>% 
  mutate(time="Days 8 to 14")

sccsSummary.day15_21 <-sccsSummary %>% 
  filter(!is.na(`rr(post-vaccine 21d day 15-)`)) %>% 
  rename("rr"="rr(post-vaccine 21d day 15-)") %>% 
  rename("ci95lb"="ci95lb(post-vaccine 21d day 15-)") %>% 
  rename("ci95ub"="ci95ub(post-vaccine 21d day 15-)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req,rr, ci95lb,ci95ub)  %>% 
  mutate(time="Days 15 to 21")

sccsSummary.day.pre_1y<-sccsSummary %>% 
  filter(!is.na(`rr(Pre-exposure 1yr)`)) %>% 
  rename("rr"="rr(Pre-exposure 1yr)") %>% 
  rename("ci95lb"="ci95lb(Pre-exposure 1yr)") %>% 
  rename("ci95ub"="ci95ub(Pre-exposure 1yr)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req,rr, ci95lb,ci95ub)  %>% 
  mutate(time="Prior year")

sccsSummary.day.pre_21d<-sccsSummary %>% 
  filter(!is.na(`rr(Pre-exposure 21d)`)) %>% 
  rename("rr"="rr(Pre-exposure 21d)") %>% 
  rename("ci95lb"="ci95lb(Pre-exposure 21d)") %>% 
  rename("ci95ub"="ci95ub(Pre-exposure 21d)") %>%
  select(db,analysisName,exposureName, outcomeName,history.req,rr, ci95lb,ci95ub)  %>% 
  mutate(time="Prior 21 days")



sccsSummary.tidy<-bind_rows(sccsSummary.covid.90d,
          sccsSummary.21d.no.split,
          sccsSummary.day.pre_1y,
          sccsSummary.day.pre_21d,
          sccsSummary.day0,
          sccsSummary.day1_7,
          sccsSummary.day8_14,
          sccsSummary.day15_21)

save(sccsSummary.tidy, file = here("data", "sccsSummary.tidy.RData"))

