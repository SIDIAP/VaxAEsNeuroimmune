# open connection to db ----
conn <- connect(connectionDetails)
# helper functions -----
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

# tidy up tableone output
TidyTableOne<-function(working.table){
# format 
for(i in 1:ncol({{working.table}})) {

  cur_column <- working.table[, i]
  cur_column <- str_extract(cur_column, '[0-9.]+\\b') %>% 
                as.numeric() 
  cur_column <-nice.num.count(cur_column)
  # add back in
  working.table[, i] <- str_replace(string={{working.table}}[, i], 
                              pattern='[0-9.]+\\b', 
                              replacement=cur_column)    
}

rownames(working.table)<-str_to_sentence(rownames(working.table))
rownames(working.table)<-str_replace(rownames(working.table),
                                      "Prior_obs_years", "Years of prior observation time")
rownames(working.table)<-str_replace(rownames(working.table),
                                      "Age_gr", "Age group")
rownames(working.table)<-str_replace(rownames(working.table),
                                      "Gender", "Sex")
rownames(working.table)<-str_replace(rownames(working.table),
                                      "Hrfs_gr", "Hospital Frailty Risk Score")
rownames(working.table)<-str_replace(rownames(working.table),
                                      "Copd", "COPD")
rownames(working.table)<-str_replace(rownames(working.table) , "_", " ")
rownames(working.table)<-str_replace(rownames(working.table) , "_", " ")
rownames(working.table)<-str_replace(rownames(working.table) , " = 1 ", " ")
rownames(working.table)<-str_replace(rownames(working.table) , "iqr", "IQR")
#return
working.table}

# get tableone
get.patient.characteristics<-function(Population, vars, factor.vars){
summary.table<-data.frame(print(CreateTableOne(
  vars =  vars,
  factorVars=factor.vars,
  includeNA=T,
  data = Population,
  test = F), 
  showAllLevels=F,smd=F,
  nonnormal = vars,
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE)) 
TidyTableOne(summary.table)
}

# general formatting for figures
gg.general.format<-function(plot){
  plot+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=12)) }
gg.general.format.facet<-function(plot){
  plot+
  theme_bw()+
  scale_y_continuous(label=label_comma(accuracy= 1), position = "right", limits=c(0,NA))+
  theme(panel.spacing = unit(0.6, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12), 
        legend.position = "top") }
gg.general.format.facet.perc<-function(plot){
  plot+
  theme_bw()+
  scale_y_continuous(label=label_percent(accuracy= 1), position = "right", limits=c(0,NA))+
  theme(panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=14), 
        legend.position = "top") }

# link to db tables -----
person_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".person")))
observation_period_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".observation_period")))
observation_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".observation")))
visit_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".visit_occurrence")))
condition_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".condition_occurrence")))
drug_exposure_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".drug_exposure")))
drug_era_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       cdm_database_schema,
                       ".drug_era")))
concept_ancestor_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       vocabulary_database_schema,
                       ".concept_ancestor")))
concept_db<-tbl(db, sql(paste0("SELECT * FROM ",
                       vocabulary_database_schema,
                       ".concept")))
exitus_db<-tbl(db, sql(paste0("SELECT * FROM ",
                               vocabulary_database_schema,
                               ".death")))

# get db end date ---
db.end.date<-as.Date(observation_period_db %>% 
  select(observation_period_end_date) %>% 
  summarise(max(observation_period_end_date, na.rm=TRUE)) %>% 
  pull())
db.end.date<-dmy(format(db.end.date, "%d/%m/%Y"))

# last date for cohort entry 
end_date_vacc_covid<-db.end.date-days(7)

# get hospitalisation codes -----
ip.codes<-c(9201, 262) 
# add all descendents
ip.codes.w.desc<-concept_ancestor_db %>%
  filter(ancestor_concept_id %in% ip.codes ) %>%
  collect() %>%
  select(descendant_concept_id) %>%
  distinct() %>%
  pull()
 




# extra table names ----
cohortTableExposurestmp<-paste0(cohortTableExposures, "tmp")
cohortTableExposuresGpop<-paste0(cohortTableExposures, "Gpop")
cohortTableExposuresCov<-paste0(cohortTableExposures, "Cov")
cohortTableExposuresVax<-paste0(cohortTableExposures, "Vax")

cohortTableExposuresCov_w_history<-paste0(cohortTableExposures, "Cov_w_history")
cohortTableExposuresVax_w_history<-paste0(cohortTableExposures, "Vax_w_history")
cohortTableExposuresCov_w_21days<-paste0(cohortTableExposures, "Cov_w_21days")
cohortTableExposuresVax_w_21days<-paste0(cohortTableExposures, "Vax_w_21days")

# specifications ----
# get study cohorts
study.cohorts<-list()

if(run.vax.cohorts=="TRUE"){
study.cohorts[["vaccinated"]]<-tibble(id=1:14,
                        file=NA,
                        name=c("BNT162b2 first-dose", 
                               "BNT162b2 second-dose", 
                               "ChAdOx1 first-dose",
                               "ChAdOx1 second-dose", 
                               "mRNA-1273 first-dose",
                               "mRNA-1273 second-dose",
                               "Ad26.COV2.S first-dose",
                               "BNT162b2 first-dose (no prior covid)", 
                               "BNT162b2 second-dose (no prior covid)", 
                               "ChAdOx1 first-dose (no prior covid)",
                               "ChAdOx1 second-dose (no prior covid)",
                               "mRNA-1273 first-dose (no prior covid)",
                               "mRNA-1273 second-dose (no prior covid)",
                               "Ad26.COV2.S first-dose (no prior covid)"
                               )) %>% 
  mutate(type="VaccinatedCohorts")
}

if(run.covid.cohorts=="TRUE"){
  study.cohorts.sql.covid<-list.files(here("1_InstantiateCohorts","CovidCohorts", "sql"))
  study.cohorts.sql.covid<-study.cohorts.sql.covid[study.cohorts.sql.covid!="CreateCohortTable.sql"]
  #drop the two covid cohorts we only use for exclusions 
  study.cohorts.sql.covid<-study.cohorts.sql.covid[study.cohorts.sql.covid!="COVID19 PCR positive test.sql"]
  study.cohorts.sql.covid<-study.cohorts.sql.covid[study.cohorts.sql.covid!="COVID19 diagnosis broad.sql"]
  study.cohorts[["covid"]]<-tibble(id=1:length(study.cohorts.sql.covid),
                                   file=study.cohorts.sql.covid,
                                   name=str_replace(study.cohorts.sql.covid, ".sql", " 21d"))%>%  #BERTA
    mutate(type="CovidCohorts")
  study.cohorts[["covid"]] <-  study.cohorts[["covid"]] %>%  #BERTA
    add_row(id=2, file = study.cohorts.sql.covid, name= str_replace(study.cohorts.sql.covid, ".sql", " 90d"), type="CovidCohorts")
  
}

if(run.general.pop.cohorts=="TRUE"){
  study.cohorts.sql.general.pop<-list.files(here("1_InstantiateCohorts","GeneralPopCohorts", "sql"))
  study.cohorts.sql.general.pop<-study.cohorts.sql.general.pop[study.cohorts.sql.general.pop!="CreateCohortTable.sql"]
  study.cohorts[["general.pop"]]<-tibble(id=1:length(study.cohorts.sql.general.pop),
                                   file=study.cohorts.sql.general.pop,
                                   name=str_replace(study.cohorts.sql.general.pop, ".sql", ""))%>% 
    mutate(type="GeneralPopCohorts")
  
   study.cohorts[["general.pop"]]<-head(study.cohorts[["general.pop"]],1)
}

study.cohorts<- bind_rows(study.cohorts) %>%  mutate(id=1:nrow(.))
# study.cohorts<-study.cohorts %>% filter(id==8)%>%  mutate(id=1:nrow(.))

vacc_table <- tibble(N_current=NA, cohort=NA, 
                     feature=NA, excluded=NA)
events_table <- tibble(N=NA, cohort=NA, 
                       outcome=NA, setting= NA)

# these are the conditions we´ll extract for our table 1
cond.codes<-c("434621", 
                  "443392", 
                  "201820",
                  "433736",
                  "321588",
                  "316866",
                  "4030518")
cond.names<-c("autoimmune_disease",
              "malignant_neoplastic_disease",
              "diabetes_mellitus",
              "obesity",
              "heart_disease",
              "hypertensive_disorder",
              "renal_impairment" )
            
# these are the medications we´ll extract for our table 1
drug.codes<-c("21602722",
                  "21600961",
                  "21601853",
                   "21601386" )
drug.names<-c("corticosteroids",
                  "antithrombotic",
                  "lipid_modifying",
                  "antineoplastic_immunomodulating")


# instantiate cohorts -----
source(here("1_InstantiateCohorts","InstantiateCohorts.R"))

# drop any outcome cohorts with less than 5 people
study.cohorts<-study.cohorts %>% 
  inner_join(
bind_rows(
cohortTableExposuresVax_db  %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect(),
cohortTableExposuresCov_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect(),
cohortTableExposuresGpop_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect()) %>% 
  filter(n>5) %>% 
  select(cohort_definition_id),
  by=c("id"="cohort_definition_id"))


outcome.cohorts<-outcome.cohorts %>% 
  inner_join(outcome_db %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  filter(n>5) %>% 
  select(cohort_definition_id),
  by=c("id"="cohort_definition_id")) 

# run background rates analysis -----
source(here("2_BackgroundRatesAnalysis" ,"RunBackgroundRatesAnalysis.R"))
# run SCCS -----
# to add
# close conn ----
disconnect(conn)

# save ----
if (!dir.exists(output.folder)){
dir.create(output.folder)
} 
save(IR.summary, file = paste0(output.folder, "/IR.summary_", db.name, ".RData"))
save(Patient.characteristcis, file = paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData"))
write.csv(vacc_table, file = paste0(output.folder, "/vac_table_", db.name, ".csv"))
write.csv(events_table, file = paste0(output.folder, "/events_table_", db.name, ".csv"))
# write.csv(table_vac, file = paste0(output.folder, "/mixed_doses_", db.name, ".csv"))

if(run.vax.cohorts==TRUE | run.covid.cohorts==TRUE){
  save(Survival.summary, file = paste0(output.folder, "/Survival.summary_", db.name, ".RData"))  
}


# # zip results
print("Zipping results to output folder")
unlink(paste0(output.folder, "/OutputToShare_", db.name, ".zip"))
zipName <- paste0(output.folder, "/OutputToShare_", db.name, ".zip")

if(run.vax.cohorts==TRUE | run.covid.cohorts==TRUE){
files<-c(paste0(output.folder, "/IR.summary_", db.name, ".RData"),
         paste0(output.folder, "/Survival.summary_", db.name, ".RData"),
         paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData")
         )
} else {
  files<-c(paste0(output.folder, "/IR.summary_", db.name, ".RData"),
           paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData")
           ) 
}
files <- files[file.exists(files)==TRUE]
createZipFile(zipFile = zipName,
              rootFolder=output.folder,
              files = files)

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")

