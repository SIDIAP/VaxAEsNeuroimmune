# instantiate outcome tables -----
cohort.sql<-list.files(here("1_InstantiateCohorts","OutcomeCohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
outcome.cohorts<-tibble(id=1:length(cohort.sql),
                        file=cohort.sql,
                        name=str_replace(cohort.sql, ".sql", ""))  
if(create.outcome.cohorts=="FALSE"){
  print(paste0("- Skipping creating outcome cohorts"))
} else { 
  print(paste0("- Getting outcomes"))

# create empty cohorts table
print(paste0("Create empty cohort table")) 
sql<-readSql(here("1_InstantiateCohorts","OutcomeCohorts", "sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                            sql,
                            cohort_database_schema =  results_database_schema,
                            cohort_table = cohortTableOutcomes)
rm(sql)
  
for(cohort.i in 1:length(outcome.cohorts$id)){  
    working.id<-outcome.cohorts$id[cohort.i]
    
    print(paste0("- Getting outcome: ", 
                 outcome.cohorts$name[cohort.i],
                 " (", cohort.i, " of ", length(outcome.cohorts$name), ")"))
    sql<-readSql(here("1_InstantiateCohorts","OutcomeCohorts", "sql",
                      outcome.cohorts$file[cohort.i])) 
    sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
    
    sql<-SqlRender::translate(sql, targetDialect = targetDialect)
    renderTranslateExecuteSql(conn=conn, 
                              sql, 
                              cdm_database_schema = cdm_database_schema,
                              vocabulary_database_schema = vocabulary_database_schema,
                              target_database_schema = results_database_schema,
                              results_database_schema = results_database_schema,
                              target_cohort_table = cohortTableOutcomes,
                              target_cohort_id = working.id)  
  }
} 
# link 
outcome_db<-tbl(db, sql(paste0("SELECT * FROM ",
                               results_database_schema,
                               ".", cohortTableOutcomes)))    

# instantiate COVID tables -----
#Definició COVID-19 amplia (criteri exclusió)
cohort.sql<-list.files(here("1_InstantiateCohorts","CovidCohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
cohort.sql <- cohort.sql[cohort.sql!="COVID19 PCR positive test.sql"]
covid.cohorts<-tibble(id=1:length(cohort.sql),
                      file=cohort.sql,
                      name=str_replace(cohort.sql, ".sql", ""))  

if(create.covid.cohorts=="FALSE"){
  print(paste0("- Skipping creating COVID cohorts"))
} else { 
  print(paste0("- Getting COVID cohorts"))
  # conn <- connect(connectionDetails)
  
  # create empty cohorts table
  print(paste0("Create empty cohort table")) 
  sql<-readSql(here("1_InstantiateCohorts","CovidCohorts", "sql","CreateCohortTable.sql"))
  sql<-SqlRender::translate(sql, targetDialect = targetDialect)
  renderTranslateExecuteSql(conn=conn, 
                            sql,
                            cohort_database_schema = results_database_schema,
                            cohort_table = cohortTableCOVID_excl)
  
  rm(sql)
  
  for(cohort.i in 1:length(covid.cohorts$id)){
    
    working.id<-covid.cohorts$id[cohort.i]
    
    print(paste0("- Getting outcome: ", 
                 covid.cohorts$name[cohort.i],
                 " (", cohort.i, " of ", length(covid.cohorts$name), ")"))
    sql<-readSql(here("1_InstantiateCohorts","CovidCohorts", "sql",
                      covid.cohorts$file[cohort.i])) 
    sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
    
    sql<-SqlRender::translate(sql, targetDialect = targetDialect)
    renderTranslateExecuteSql(conn=conn, 
                              sql, 
                              cdm_database_schema = cdm_database_schema,
                              vocabulary_database_schema = vocabulary_database_schema,
                              target_database_schema = results_database_schema,
                              results_database_schema = results_database_schema,
                              target_cohort_table = cohortTableCOVID_excl,
                              target_cohort_id = working.id) 
  }
}
covid_db_excl <-tbl(db, sql(paste0("SELECT * FROM ",
                                   results_database_schema,
                                   ".", cohortTableCOVID_excl)))   




#Definició cohort COVID-19

#PCR o TEST ANTIGEN positiu
cohort.sql<-list.files(here("1_InstantiateCohorts","CovidCohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
cohort.sql <- cohort.sql[cohort.sql!="COVID19 PCR positive test.sql"]
cohort.sql<-cohort.sql[cohort.sql!="COVID19 diagnosis broad.sql"]

covid.cohorts<-tibble(id=1:length(cohort.sql),
                      file=cohort.sql,
                      name=str_replace(cohort.sql, ".sql", ""))  


if(create.covid.cohorts=="FALSE"){
} else { 
  print(paste0("- Getting extra COVID cohorts"))
  
  # create empty cohorts table
  print(paste0("Create empty cohort table")) 
  sql<-readSql(here("1_InstantiateCohorts","CovidCohorts", "sql","CreateCohortTable.sql"))
  sql<-SqlRender::translate(sql, targetDialect = targetDialect)
  renderTranslateExecuteSql(conn=conn, 
                            sql,
                            cohort_database_schema = results_database_schema,
                            cohort_table = cohortTableCOVID)
  
  rm(sql)
  
  for(cohort.i in 1:length(covid.cohorts$id)){
    
    working.id<-covid.cohorts$id[cohort.i]
    
    print(paste0("- Getting outcome: ", 
                 covid.cohorts$name[cohort.i],
                 " (", cohort.i, " of ", length(covid.cohorts$name), ")"))
    sql<-readSql(here("1_InstantiateCohorts","CovidCohorts", "sql",
                      covid.cohorts$file[cohort.i])) 
    sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
    
    sql<-SqlRender::translate(sql, targetDialect = targetDialect)
    renderTranslateExecuteSql(conn=conn, 
                              sql, 
                              cdm_database_schema = cdm_database_schema,
                              vocabulary_database_schema = vocabulary_database_schema,
                              target_database_schema = results_database_schema,
                              results_database_schema = results_database_schema,
                              target_cohort_table = cohortTableCOVID,
                              target_cohort_id = working.id) 
  }
}

covid_db<-tbl(db, sql(paste0("SELECT * FROM ",
                             results_database_schema,
                             ".", cohortTableCOVID)))   


# instantiate study populations -----
# create empty cohorts table
if(create.exposure.cohorts=="FALSE"){
  print(paste0("- Skipping creating COVID cohorts"))
} else { 
  
print(paste0("Create empty cohort tables")) 
sql<-readSql(here("1_InstantiateCohorts","VaccinatedCohorts", "sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
# Gpop
renderTranslateExecuteSql(conn=conn, 
                            sql,
                            cohort_database_schema = results_database_schema,
                            cohort_table = cohortTableExposuresGpop)
# Cov
renderTranslateExecuteSql(conn=conn, 
                            sql,
                            cohort_database_schema = results_database_schema,
                            cohort_table = cohortTableExposuresCov)
# Vax
renderTranslateExecuteSql(conn=conn, 
                            sql,
                            cohort_database_schema = results_database_schema,
                            cohort_table = cohortTableExposuresVax)
rm(sql)


for(i in 1:length(study.cohorts$id)){ 
# study population
working.study.cohort<-  study.cohorts %>% 
    filter(id==i) %>% 
    select(name) %>% 
    pull()
working.study.cohort.type<-  study.cohorts %>% 
    filter(id==i) %>% 
    select(type) %>% 
    pull()
# print(paste0("Running analysis for ", working.study.cohort))  

  
# get current exposure population
if(working.study.cohort.type=="VaccinatedCohorts"){
#exclude people with more than one type of vaccine 
if(db.name=="CPRD AURUM"){
      all_vacc_db <- drug_exposure_db %>%  
        filter((drug_concept_id=="37003436")|
                 (drug_source_concept_id=="35891522")) %>% 
        group_by(person_id) %>% 
        summarise(unique=n_distinct(drug_concept_id)) %>% 
        filter(unique>="1")  #at least one vaccine
    } else {
      all_vacc_db <- drug_exposure_db %>%  
        filter((drug_concept_id=="37003436")|
                 (drug_concept_id=="724905")|
                 (drug_concept_id=="37003518")|
                 (drug_concept_id=="739906")) %>% 
        group_by(person_id) %>% 
        summarise(unique=n_distinct(drug_concept_id)) %>% 
        filter(unique>="1") #at least one vaccine 
    }
    
mixed_doses_db <- all_vacc_db %>%
      filter(unique>="2")  #with two different vaccines
# mixed_doses_db %>%  count()
    
if(str_detect(working.study.cohort, "BNT162b2")){
cohort.to.instantiate_db<-drug_exposure_db %>% 
   filter(drug_concept_id=="37003436")%>% 
   anti_join(mixed_doses_db) 
}
if(str_detect(working.study.cohort, "ChAdOx1")){
      if(db.name=="CPRD AURUM"){
        cohort.to.instantiate_db<-drug_exposure_db %>% 
          filter(drug_source_concept_id=="35891522")%>% 
          anti_join(mixed_doses_db) 
      } else{
        cohort.to.instantiate_db<-drug_exposure_db %>% 
          filter(drug_concept_id=="724905")%>% 
          anti_join(mixed_doses_db) 
      }
    } 
if(str_detect(working.study.cohort, "mRNA-1273")){
      cohort.to.instantiate_db<-drug_exposure_db %>% 
        filter(drug_concept_id=="37003518") %>% 
        anti_join(mixed_doses_db) 
    }
if(str_detect(working.study.cohort, "Ad26.COV2.S")){
      cohort.to.instantiate_db<-drug_exposure_db %>% 
        filter(drug_concept_id=="739906")%>% 
        anti_join(mixed_doses_db) 
    } 
    
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
  group_by(person_id)%>% 
  select(person_id,drug_exposure_start_date) %>%   
  window_order(person_id,drug_exposure_start_date) %>%
  distinct() %>%  
  mutate(seq = row_number()) %>% 
  select(person_id,drug_exposure_start_date, seq)
    
# get first or second dose depending on working cohort
if(str_detect(working.study.cohort, "first-dose")){
# nb keep date of second dose so we can use it for censoring
# keep first two records and get time between doses      
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
        filter(drug_exposure_start_date>=start_date_vacc) %>% 
        filter(seq %in% c(1,2)) %>% 
        mutate(second.dose.date=lead(drug_exposure_start_date,
                         order_by = c("person_id"))) %>% # lead - second dose
        mutate(dose1_dose2= second.dose.date- drug_exposure_start_date)%>% 
        filter(seq==1) 
    }
    
if(str_detect(working.study.cohort, "second-dose")){
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
        filter(drug_exposure_start_date>=start_date_vacc) %>% 
        filter(seq %in% c(1,2)) %>% 
        mutate(first.dose.date=lag(drug_exposure_start_date,
                         order_by = c("person_id"))) %>% # lag - first dose
        mutate(dose1_dose2= drug_exposure_start_date-first.dose.date)%>% 
        filter(seq==2)   

 #3) exclude if second dose record is outside window
      # sum(cohort.to.instantiate$dose1_dose2<18)
      # sum(cohort.to.instantiate$dose1_dose2>60)
      if(str_detect(working.study.cohort, "BNT162b2 second-dose")){      
        cohort.to.instantiate_db<-cohort.to.instantiate_db  %>% 
          filter(dose1_dose2>=14) %>% 
          filter(dose1_dose2<=180) 
        
        # prop.table(table(cohort.to.instantiate$dose1_dose2==21))
        # prop.table(table(cohort.to.instantiate$dose1_dose2<21))
        # prop.table(table(cohort.to.instantiate$dose1_dose2>21))
      }
      
      if(str_detect(working.study.cohort, "ChAdOx1 second-dose")){      
        cohort.to.instantiate_db<-cohort.to.instantiate_db  %>% 
          filter(dose1_dose2>=14) %>% 
          filter(dose1_dose2<=180) 
      }      
      
      if(str_detect(working.study.cohort, "mRNA-1273 second-dose")){      
        cohort.to.instantiate_db<-cohort.to.instantiate_db  %>% 
          filter(dose1_dose2>=14) %>% 
          filter(dose1_dose2<=180)    
      }    
      
      
    }
    
    
 # exclude if prior covid
if(str_detect(working.study.cohort, "no prior covid")){
cohort.to.instantiate_db <- compute(cohort.to.instantiate_db)   

covid_excl_db <- covid_db_excl %>% #Exclusió definició amplia covid
        select(subject_id, cohort_start_date) %>%
        window_order(subject_id,cohort_start_date) %>% 
        group_by(subject_id) %>% 
        mutate(seq = row_number()) %>% 
        filter(seq==1) %>% 
        select(-seq) %>% 
        ungroup() %>% 
        compute()

# Covid after vaccination (for descriptive purposes)
# after_covid <- cohort.to.instantiate_db %>% 
#         select(person_id,drug_exposure_start_date) %>% 
#         inner_join(covid_db_excl %>%
#                      rename("person_id"="subject_id")%>%
#                      rename("condition_start_date" ="cohort_start_date"), 
#                    by="person_id")  %>%
#         filter(drug_exposure_start_date<=condition_start_date) %>% 
#        collect()
      
# drop if covid prior to vaccinated
prior_covid_db <- cohort.to.instantiate_db %>% 
    select(person_id,drug_exposure_start_date) %>% 
    ungroup() %>% 
    inner_join(covid_excl_db %>%
    rename("person_id"="subject_id")%>%
    rename("condition_start_date" ="cohort_start_date"), 
                   by="person_id")  %>%
    filter(drug_exposure_start_date>=condition_start_date) %>% 
    select(person_id)
      
# vacc_table <- rbind(vacc_table, c(prior_covid_db %>% tally() %>% pull(), 
#                                         working.study.cohort,
#                                         feature="COVID-19 prior to vaccination", 
#                                         excluded= "YES"))
      
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
        anti_join(prior_covid_db)
# vacc_table <- rbind(vacc_table, c(nrow(cohort.to.instantiate_db), 
#                                         working.study.cohort,feature="COVID-19 after vaccination", 
#                                         excluded ="NO"))
    }
    
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
      mutate(cohort_definition_id=!!study.cohorts$id[i]) %>% 
      filter(drug_exposure_start_date<= !!end_date_vacc_covid) %>% 
      rename("subject_id"="person_id") %>% 
      rename("cohort_start_date"="drug_exposure_start_date") %>%
      mutate("cohort_end_date"= as.Date(end_date_vacc_covid)) %>% 
      select(cohort_definition_id, subject_id,
             cohort_start_date,cohort_end_date, dose1_dose2)

# insert into tmp table
print(paste0("-- Inserting ", working.study.cohort, " into database")) 
start.insert<-Sys.time()
sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableExposurestmp}', 'U') IS NOT NULL\n",
	"DROP TABLE {results_database_schema}.{cohortTableExposurestmp};")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, as.character(sql_query))

sql_query <- glue::glue("SELECT * \n",
                          "INTO {results_database_schema}.{cohortTableExposurestmp}\n",
                          " FROM (\n",
                          dbplyr::sql_render(cohort.to.instantiate_db),
                          "\n) AS from_table")
DBI::dbExecute(db, as.character(sql_query))
# add to exposures
# tbl(db, sql(paste0("SELECT * FROM ",  results_database_schema,
#                     ".", cohortTableExposurestmp)))

sql_query <- glue::glue("INSERT ",
                          "INTO {results_database_schema}.{cohortTableExposuresVax} ",
                          "SELECT * FROM {results_database_schema}.{cohortTableExposurestmp}")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, sql_query)

duration <- Sys.time()-start.insert
print(paste("--- Inserting", working.study.cohort, "took", 
            round(duration[[1]], 2),  units(duration)))

} else if (working.study.cohort.type=="CovidCohorts"){
    
if(str_detect(working.study.cohort, "COVID19 positive")){
      
cohort.to.instantiate_db <- person_db %>%
        select(person_id) %>%
        inner_join(covid_db %>% 
                     select(subject_id, cohort_start_date) %>% 
                     rename("person_id"="subject_id"),
                   by = "person_id") 
      
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
        filter(cohort_start_date>=!!start_date_covid) %>% 
        filter(cohort_start_date<=!!end_date_vacc_covid)  
      
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
        mutate(cohort_definition_id=!!study.cohorts$id[i]) %>% 
        rename("subject_id"="person_id") %>% 
        rename("cohort_start_date"="cohort_start_date")%>% 
        mutate("cohort_end_date"=as.Date(end_date_vacc_covid)) %>%  
        select(cohort_definition_id, subject_id, cohort_start_date,cohort_end_date)
      
# drop if vaccinated prior to COVID 
if(db.name=="CPRD AURUM"){
        vaccines_db <-drug_exposure_db %>% 
          filter(drug_concept_id=="37003436"|
                   drug_source_concept_id=="35891522") %>%
          select(person_id, drug_exposure_start_date) %>% 
          distinct() # drop any duplicates from same day
      } else{
        vaccines_db <-drug_exposure_db %>% 
          filter(drug_concept_id=="37003436"|drug_concept_id=="724905"|
                   drug_concept_id=="37003518"|drug_concept_id=="739906") %>% 
          select(person_id, drug_exposure_start_date) %>% 
          distinct()
      }
      
vaccines_db <- vaccines_db %>%
   filter(drug_exposure_start_date >=start_date_vacc)%>%
   window_order(person_id,drug_exposure_start_date) %>% 
   group_by(person_id) %>%   
   mutate(seq = row_number()) %>% 
        filter(seq==1) # keep first record

# drop
vaccinated_covid_db <-cohort.to.instantiate_db %>% 
        inner_join(vaccines_db %>% 
                     select(person_id,drug_exposure_start_date)%>%
                     rename("subject_id"="person_id")) %>% 
        filter(cohort_start_date>= drug_exposure_start_date)
      
cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
        anti_join(vaccinated_covid_db) 

cohort.to.instantiate_db<-cohort.to.instantiate_db %>% 
  mutate(dose1_dose2=as.integer(NA))

# insert into tmp table
print(paste0("-- Inserting ", working.study.cohort, " into database")) 
start.insert<-Sys.time()
sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableExposurestmp}', 'U') IS NOT NULL\n",
	"DROP TABLE {results_database_schema}.{cohortTableExposurestmp};")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, as.character(sql_query))

sql_query <- glue::glue("SELECT * \n",
                          "INTO {results_database_schema}.{cohortTableExposurestmp}\n",
                          " FROM (\n",
                          dbplyr::sql_render(cohort.to.instantiate_db),
                          "\n) AS from_table")
DBI::dbExecute(db, as.character(sql_query))
# add to exposures
# tbl(db, sql(paste0("SELECT * FROM ",  results_database_schema,
#                     ".", cohortTableExposurestmp)))

sql_query <- glue::glue("INSERT ",
                          "INTO {results_database_schema}.{cohortTableExposuresCov} ",
                          "SELECT * FROM {results_database_schema}.{cohortTableExposurestmp}")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, sql_query)

duration <- Sys.time()-start.insert
print(paste("--- Inserting", working.study.cohort, "took", 
            round(duration[[1]], 2),  units(duration)))




      
# # insert into database
# print(paste0("-- Inserting ", working.study.cohort, " into database")) 
# start.insert<-Sys.time()
# sql_query <- glue::glue("INSERT \n",
#                           "INTO {results_database_schema}.{cohortTableExposurestmp}\n",
#                           "SELECT * FROM (\n",
#                           dbplyr::sql_render(cohort.to.instantiate_db),
#                           "\n) AS from_table")
# DBI::dbExecute(db, as.character(sql_query))
# duration <- Sys.time()-start.insert
# print(paste("--- Inserting", working.study.cohort, "took", 
#             round(duration[[1]], 2),  units(duration)))

      
    }
    
    
  } else { #gpop
    
# create empty cohorts table
print(paste0("-- Inserting ", working.study.cohort, " into database")) 
start.insert<-Sys.time()
print(paste0("Create empty cohort table"))
sql<-readSql(here("1_InstantiateCohorts","VaccinatedCohorts", "sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn,
                            sql,
                            cohort_database_schema =  results_database_schema,
                            cohort_table = cohortTableExposurestmp)
rm(sql)

sql<-readSql(here("1_InstantiateCohorts",working.study.cohort.type, "sql",
                      study.cohorts$file[i]))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
renderTranslateExecuteSql(conn=conn,
                              sql,
                              cdm_database_schema = cdm_database_schema,
                              target_database_schema = results_database_schema,
                              target_cohort_table = cohortTableExposurestmp,
                              # results_database_schema = results_database_schema,
                              # vocabulary_database_schema=vocabulary_database_schema,
                              target_cohort_id = study.cohorts$id[i])

# add to exposures
sql_query <- glue::glue("INSERT ",
                          "INTO {results_database_schema}.{cohortTableExposuresGpop} ",
                          "SELECT * FROM {results_database_schema}.{cohortTableExposurestmp}")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, sql_query)


print(paste("--- Inserting", working.study.cohort, "took", 
            round(duration[[1]], 2),  units(duration)))
}

}
  
}

cohortTableExposuresVax_db<-tbl(db, sql(paste0("SELECT * FROM ",
         results_database_schema,
          ".", cohortTableExposuresVax))) 

cohortTableExposuresCov_db<-tbl(db, sql(paste0("SELECT * FROM ",
         results_database_schema,
          ".", cohortTableExposuresCov))) 

cohortTableExposuresGpop_db<-tbl(db, sql(paste0("SELECT * FROM ",
         results_database_schema,
          ".", cohortTableExposuresGpop)))

# Additional study populations with year of history required ----
# Cov
if(create.exposure.cohorts =="FALSE"){
  print(paste0("- Skipping creating SCCS cohorts with year of history"))
} else { 
cohortTableExposuresCov_w_history_db<-cohortTableExposuresCov_db %>% 
  left_join(observation_period_db %>%
              select(person_id,observation_period_start_date) %>%
              rename("subject_id"= "person_id")) %>%
  filter((cohort_start_date %- interval% '12 months')>
           observation_period_start_date) %>% 
  select(-observation_period_start_date)

# insert into  table
print(paste0("-- Inserting ", "cohortTableExposuresCov_w_history_db", " into database")) 
start.insert<-Sys.time()
sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableExposuresCov_w_history}', 'U') IS NOT NULL\n",
	"DROP TABLE {results_database_schema}.{cohortTableExposuresCov_w_history};")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, as.character(sql_query))

sql_query <- glue::glue("SELECT * \n",
                          "INTO {results_database_schema}.{cohortTableExposuresCov_w_history}\n",
                          " FROM (\n",
                          dbplyr::sql_render(cohortTableExposuresCov_w_history_db),
                          "\n) AS from_table")
DBI::dbExecute(db, as.character(sql_query))

duration <- Sys.time()-start.insert
print(paste("--- Inserting", "cohortTableExposuresCov_w_history_db", "took", 
            round(duration[[1]], 2),  units(duration)))


# Vax
cohortTableExposuresVax_w_history_db<-cohortTableExposuresVax_db %>% 
  left_join(observation_period_db %>%
              select(person_id,observation_period_start_date) %>%
              rename("subject_id"= "person_id")) %>%
  filter((cohort_start_date %- interval% '12 months')>
           observation_period_start_date) %>% 
  select(-observation_period_start_date)

# insert into  table
print(paste0("-- Inserting ", "cohortTableExposuresVax_w_history_db", " into database")) 
start.insert<-Sys.time()
sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableExposuresVax_w_history}', 'U') IS NOT NULL\n",
	"DROP TABLE {results_database_schema}.{cohortTableExposuresVax_w_history};")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, as.character(sql_query))

sql_query <- glue::glue("SELECT * \n",
                          "INTO {results_database_schema}.{cohortTableExposuresVax_w_history}\n",
                          " FROM (\n",
                          dbplyr::sql_render(cohortTableExposuresVax_w_history_db),
                          "\n) AS from_table")
DBI::dbExecute(db, as.character(sql_query))

duration <- Sys.time()-start.insert
print(paste("--- Inserting", "cohortTableExposuresCov_w_history_db", "took", 
            round(duration[[1]], 2),  units(duration)))
}



# # check
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresVax))) %>% 
#   tally() %>% collect() %>% pull()) -
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresVax_w_history))) %>% 
#   tally() %>% collect() %>% pull())
# 
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresCov))) %>% 
#   tally() %>% collect() %>% pull()) -
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresCov_w_history))) %>% 
#   tally() %>% collect() %>% pull())

# Additional study populations with full potential 21 day follow up  ----
if(create.exposure.cohorts =="FALSE"){
  print(paste0("- Skipping creating SCCS cohorts with 21 day potential follow up"))
} else { 
# Cov
cohortTableExposuresCov_w_21days_db<-cohortTableExposuresCov_db %>% 
        filter(cohort_start_date<=!!(db.end.date-days(21)))

# insert into  table
print(paste0("-- Inserting ", "cohortTableExposuresCov_w_21days_db", " into database")) 
start.insert<-Sys.time()
sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableExposuresCov_w_21days}', 'U') IS NOT NULL\n",
	"DROP TABLE {results_database_schema}.{cohortTableExposuresCov_w_21days};")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, as.character(sql_query))

sql_query <- glue::glue("SELECT * \n",
                          "INTO {results_database_schema}.{cohortTableExposuresCov_w_21days}\n",
                          " FROM (\n",
                          dbplyr::sql_render(cohortTableExposuresCov_w_21days_db),
                          "\n) AS from_table")
DBI::dbExecute(db, as.character(sql_query))

duration <- Sys.time()-start.insert
print(paste("--- Inserting", "cohortTableExposuresCov_w_21days_db", "took", 
            round(duration[[1]], 2),  units(duration)))


# Vax
cohortTableExposuresVax_w_21days_db<-cohortTableExposuresVax_db %>% 
        filter(cohort_start_date<=!!(db.end.date-days(21)))

# insert into  table
print(paste0("-- Inserting ", "cohortTableExposuresVax_w_21days_db", " into database")) 
start.insert<-Sys.time()
sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableExposuresVax_w_21days}', 'U') IS NOT NULL\n",
	"DROP TABLE {results_database_schema}.{cohortTableExposuresVax_w_21days};")
sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
DBI::dbExecute(db, as.character(sql_query))

sql_query <- glue::glue("SELECT * \n",
                          "INTO {results_database_schema}.{cohortTableExposuresVax_w_21days}\n",
                          " FROM (\n",
                          dbplyr::sql_render(cohortTableExposuresVax_w_21days_db),
                          "\n) AS from_table")
DBI::dbExecute(db, as.character(sql_query))

duration <- Sys.time()-start.insert
print(paste("--- Inserting", "cohortTableExposuresVax_w_21days_db", "took", 
            round(duration[[1]], 2),  units(duration)))
}



# # check
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresVax))) %>% 
#   tally() %>% collect() %>% pull()) -
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresVax_w_21days))) %>% 
#   tally() %>% collect() %>% pull())
# 
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresCov))) %>% 
#   tally() %>% collect() %>% pull()) -
# (tbl(db, sql(paste0("SELECT * FROM ",
#          results_database_schema,
#           ".", cohortTableExposuresCov_w_21days))) %>% 
#   tally() %>% collect() %>% pull())

# add indexes ----
# sql_query <- glue::glue( "CREATE INDEX CovVaxOutcomes_idx ON {results_database_schema}.{cohortTableOutcomes}",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# sql_query <- glue::glue( "CREATE INDEX Cov_idx ON {results_database_schema}.{cohortTableExposuresCov}",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# sql_query <- glue::glue( "CREATE INDEX Cov_w_history_idx ON {results_database_schema}.{cohortTableExposuresCov_w_history}",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# sql_query <- glue::glue( "CREATE INDEX Cov_w_21days_idx ON {results_database_schema}.{cohortTableExposuresCov_w_21days}",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# 
# sql_query <- glue::glue( "CREATE INDEX Vax_idx ON {results_database_schema}.{cohortTableExposuresVax}",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# sql_query <- glue::glue( "CREATE INDEX Vax_w_history_idx ON {results_database_schema}.{cohortTableExposuresVax_w_history}",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# sql_query <- glue::glue( "CREATE INDEX Vax_w_21days_idx ON {results_database_schema}.{cohortTableExposuresVax_w_21days}",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# sql_query <- glue::glue( "CREATE INDEX Gpop_idx ON {results_database_schema}.{cohortTableExposuresGpop} ",
#                           "(cohort_definition_id, subject_id)")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)


