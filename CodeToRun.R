
#install.packages("renv") # if not already installed, install renv from CRAN
#renv::restore() # this should prompt you to install the various packages required for the study
# renv::activate()

# packages -----
library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(RSQLite)
library(rmarkdown)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(RPostgreSQL)
# please load the above packages 
# you should have them all available, with the required version, after
# having run renv::restore above

# set up for running study code ----
# The name/ acronym for your database (to be used in the titles of reports, etc) 
# IMPORTANT - currently needs to be "SIDIAP" or "CPRD AURUM"
db.name<-"SIDIAP"

# the path to a folder (that exists) where the results from this analysis will be saved
output.folder<-here::here("4_StudyOutput")

# Database details
oracleTempSchema<-NULL

# If you haven´t already, save database details to .Renviron by running:
# usethis::edit_r_environ()
server<-Sys.getenv("SERVER_postgres21t2")
server_dbi<-Sys.getenv("SERVER_DBI_postgres21t2")

user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT") 
host<-Sys.getenv("DB_HOST") 

connectionDetails <-DatabaseConnector::downloadJdbcDrivers("postgresql", here::here())
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server =server,
                                                                user = user,
                                                                password = password,
                                                                port = port ,
                                                                pathToDriver = here::here())
db <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# sql dialect used with the OHDSI SqlRender package
targetDialect <-"postgresql" 
# schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"omop21t2_cmbd"
# schema that contains the vocabularie
vocabulary_database_schema<-"omop21t2_cmbd" 
# schema where a results table will be created 
results_database_schema<-"results21t2_cmbd_v2"

# Tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten
cohortTableExposures<-"CovVaxExposures" #nb- name also used as stem for additional tables
cohortTableOutcomes <-"CovVaxOutcomes"
cohortTableProfiles<-"CovVaxProfiles"
cohortTableCOVID <- "CovVaxCOVID" 
cohortTableCOVID_excl <- "CovVaxCOVID_excl" 

# create cohorts ------
# if you have already created the outcome cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.outcome.cohorts<- FALSE
create.covid.cohorts <- FALSE 
create.exposure.cohorts <- FALSE 

# choose cohorts to instantiate -----
run.vax.cohorts<-TRUE
run.covid.cohorts<-TRUE
run.general.pop.cohorts<-TRUE

# dates for vaccination and SARS-CoV-2 cohorts -----
start_date_vacc <- dmy("27/12/2020") #start date for vaccinated cohorts
start_date_covid <- dmy("01/09/2020") #start date for covid-19 cohorts

# run the analysis ----
run.analysis.with.sampling<-FALSE
start<-Sys.time()
source(here("2_BackgroundRatesAnalysis" ,"RunBackgroundRatesAnalysis.R"))
Sys.time()-start





