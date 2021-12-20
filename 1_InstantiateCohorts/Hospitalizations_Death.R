
conn <- connect(connectionDetails)

#Tables
cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                               results_database_schema,
                               ".", cohortTableExposures)))  

outcome_db<-tbl(db, sql(paste0("SELECT * FROM ",
                               results_database_schema,
                              ".", cohortTableOutcomes)))    
concept_ancestor_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept_ancestor")))

condition_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                            cdm_database_schema,
                                            ".condition_occurrence")))

visit_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        cdm_database_schema,
                                        ".visit_occurrence")))
exitus_db<-tbl(db, sql(paste0("SELECT * FROM ",
                              vocabulary_database_schema,
                              ".death")))

#DEATH-----
death <- exitus_db %>%
  filter(death_date >= working.date) %>%
  select(person_id, death_date) %>%
  collect()


outcome_death <- outcome %>%
  left_join(death, by=c("subject_id"="person_id"))

# index death
# between zero to seven days after

index_death<-outcome_death %>%
  mutate(dtime=as.numeric(difftime(cohort_start_date, death_date, units="days"))) %>%
  filter(dtime >= 0)%>%
  filter(dtime <= 7) %>%
  select(-dtime) %>%
  mutate(death_7d =1)

death_cases <- outcome %>%
  left_join(index_death)

n <- outcome_death%>%group_by(cohort_definition_id)%>%count(cohort_definition_id) %>% rename("all_cases"= "n")
deaths <- death_cases%>% group_by(cohort_definition_id)%>%count(death_7d) %>% filter(death_7d=="1") %>% select(n)%>%rename("death_7d"="n")
table_death <- merge(n, deaths, all.x=TRUE, by="cohort_definition_id")

#HOSPITALISATION (only for SIDIAP)-----
#Outcomes
outcome <- outcome_db %>% collect()

cohort.sql<-list.files(here("1_InstantiateCohorts/OutcomeCohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
outcome.cohorts<-tibble(id=1:length(cohort.sql),
                        file=cohort.sql,
                        name=str_replace(cohort.sql, ".sql", ""))  

# drop any outcome cohorts with less than 5 people
outcome.cohorts<-outcome.cohorts %>% 
  inner_join(outcome_db %>% 
               group_by(cohort_definition_id) %>% 
               tally() %>% 
               collect() %>% 
               filter(n>5) %>% 
               select(cohort_definition_id),
             by=c("id"="cohort_definition_id"))  

#filter events after 2017
outcome <- outcome %>%
  filter(cohort_start_date >= dmy("01-01-2017"))

#first outcome
outcome <- outcome %>% group_by(subject_id, cohort_definition_id)%>% 
  arrange(subject_id,cohort_start_date) %>% 
  mutate(seq=1:length(subject_id))%>%
  filter(seq =="1")

#Hospitalization
ip.codes<-c(9201, 262) #hospitalization codes
# add all descendents
ip.codes.w.desc<-concept_ancestor_db %>%
  filter(ancestor_concept_id %in% ip.codes ) %>%
  collect() %>%
  select(descendant_concept_id) %>%
  distinct() %>%
  pull()

# hospitalisations, from study start onwards
working.date<-as.Date("01/01/2017", "%d/%m/%Y")
hospitalisations<-visit_occurrence_db %>%
  filter(visit_start_date >= working.date) %>%
  filter(visit_concept_id %in% c(ip.codes.w.desc)) %>%
  select(person_id, visit_start_date, visit_end_date) %>%
  collect()
hospitalisations<-hospitalisations %>%
  distinct()

#Hospitalisations among people with the event
#the number of rows equals the number of hospitalisations for each person (>2017)
hospitalisations<- outcome %>% 
  left_join(hospitalisations, by=c("subject_id"="person_id"))

# index hospitalisation
# between zero to three days after

index_hosptialisation_day0_3<-hospitalisations %>%
  mutate(dtime=as.numeric(difftime(cohort_start_date, visit_start_date, units="days"))) %>%
  filter(dtime >= 0)%>%
  filter(dtime <= 3) %>%
  select(-dtime) %>%
  arrange(subject_id, visit_start_date) %>%
  mutate(hosp_3d =1)

index_hosptialisation_day0<-hospitalisations %>%
  mutate(dtime=as.numeric(difftime(cohort_start_date, visit_start_date, units="days"))) %>%
  filter(dtime == 0)%>%
  select(-dtime) %>%
  arrange(subject_id, visit_start_date) %>%
  mutate(hosp_0d =1)

cases <- outcome %>%
  left_join(index_hosptialisation_day0_3)

cases <-cases%>%
  left_join(index_hosptialisation_day0)

n <- outcome%>%group_by(cohort_definition_id)%>%count(cohort_definition_id) %>% rename("all_cases"= "n")
cases_3d <- cases%>% group_by(cohort_definition_id)%>%count(hosp_3d)%>%filter(hosp_3d=="1")%>% 
            select(n)%>%rename("hosp_3d"= "n")
cases_0d <- cases%>% group_by(cohort_definition_id)%>%count(hosp_0d)%>%filter(hosp_0d=="1")%>%
            select(n)%>% rename("hosp_0d"= "n")

table <- merge(n,cases_0d, by="cohort_definition_id")
table <- merge(table,cases_3d, by="cohort_definition_id")

rm(n, index_hosptialisation_day0,index_hosptialisation_day0_3,ip.codes,
   ip.codes.w.desc)
