
#install.packages("renv") # if not already installed, install renv from CRAN
# renv::restore() # this should prompt you to install the various packages required for the study
# renv::activate()
# renv::init()
# renv::status()
# renv::snapshot()

# packages
library(SqlRender)
# install.packages("remotes")
library(remotes)
# remotes::install_github("ohdsi/DatabaseConnector", ref = "develop")
# remotes::install_github("ohdsi/DatabaseConnector")

library(DatabaseConnector)
library(FeatureExtraction)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
# library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
# remotes::install_github("rstats-db/RSQLite")
# install.packages("RSQLite")
library(RSQLite)
#library(rmarkdown)
#library(tableone)
library(scales)
library(forcats)
#library(epiR)
library(RPostgreSQL)
library(SelfControlledCaseSeries)
library(Andromeda)

# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "~/andromedaTemp")

## Get the number of available gigabytes:
getAndromedaTempDiskSpace()/ 1024^3

# connect to db

oracleTempSchema<-NULL

DBMS = "postgresql"
DB_SERVER = "..."
DB_PORT = ...
DB_SERVER_p20_000211_cdm_aurum = "..."
DB_HOST = "..."
DB_SERVER_p20_059_cdm_aurum = "..."
DB_SERVER_cdmgold202007 = "..."


connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server = "...",
                                                                user = "...",
                                                                password = "...",
                                                                port = 0000 ,
                                                                pathToDriver = "...")

conn <- DatabaseConnector::connect(connectionDetails)
disconnect(conn)

db <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = "postgres21",
                port = port,
                host = host,
                user = user,
                password = password)

#dbListTables(db)

outputFolder <- here::here("output")
cdmDatabaseSchema <- "public"
cohortDatabaseSchema <- "results"
cdmVersion <- "5"
maxCores <- 4


targetDialect <-"postgresql"

cohortTableOutcomes <-"CovVaxOutcomes"
cohortTableExposures_Vax<-"CovVaxExposuresVax"
cohortTableExposures_Cov<-"CovVaxExposuresCov"

cohortTableExposures_Vax_21d<-"CovVaxExposuresVax_w_21days"
cohortTableExposures_Cov_21d<-"CovVaxExposuresCov_w_21days"

cohortTableExposures_Vax_w_history<-"CovVaxExposuresVax_w_history"
cohortTableExposures_Cov_w_history<-"CovVaxExposuresCov_w_history"


# remove temp files
tempdir()
file.remove(list.files(tempdir(), full.names = TRUE) )
