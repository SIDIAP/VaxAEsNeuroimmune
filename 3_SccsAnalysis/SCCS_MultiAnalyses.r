## running multiple analyses ##

# the analysis assume the exposures and outcomes have already been created.

#---- COHORTS FROM ED -----------------
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
vax.cohorts <- study.cohorts$vaccinated

#--- creat tos list ------
# tos.vax <- data.frame( exposureId=rep(c(1:4,8:11),each=4),
#             outcomeId=rep(c(1:4),8),
#             outcomeName= rep(c("BellsPalsy","Encephalomyelitis","GuillainBarreSyndrome","TransverseMyelitis"),8),
#             group = "main_vax"
#             )
# tos.vax <- tos.vax %>% left_join(rename(vax.cohorts,exposureId = id), by="exposureId") %>% select(-file,-type)
#
# tos.cov <- data.frame( exposureId=rep(c(15,16),each=4),
#                        outcomeId=rep(c(1:4),2),
#                        outcomeName= rep(c("BellsPalsy","Encephalomyelitis","GuillainBarreSyndrome","TransverseMyelitis"),2),
#                        group = "main_cov",
#                        exposureName = rep(c("COVID19 PCR positive test 21d","COVID19 PCR positive test 90d"),each=4)
# )
# tos <- bind_rows(rename(tos.vax, exposureName= name),tos.cov)
# write.csv(tos,here::here("Settings", "tosOfInterest.csv"))

#===================================== START HERE ===========================================

createTos <- function(outputFolder,group) {
  pathToCsv <- here::here("Settings", "tosOfInterest.csv")
  tosOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  tosOfInterest <- tosOfInterest[tosOfInterest$group==group,]
  tos <- unique(rbind(tosOfInterest[, c("exposureId", "outcomeId")]))

  createTo <- function(i) {
    exposureOutcome <- SelfControlledCaseSeries::createExposureOutcome(exposureId = tos$exposureId[i],
                                                                       outcomeId = tos$outcomeId[i])
    return(exposureOutcome)
  }
  tosList <- lapply(1:nrow(tos), createTo)
  return(tosList)
}

# eoList <- createTos(outputFolder = outputFolder,group="main_vax")

sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList("./Settings/sccsAnalysisList.json")

sccsAnalysisList_Cov <- SelfControlledCaseSeries::loadSccsAnalysisList("./Settings/sccsAnalysisList_Cov.json")

# Execute analysis ------------------------------------------------
if (!file.exists(file.path(outputFolder)))
  dir.create(file.path(outputFolder))

outputFolder_Vax <- file.path(outputFolder,"Vax")
outputFolder_Cov <- file.path(outputFolder,"Cov")

if (!file.exists(file.path(outputFolder_Vax)))
  dir.create(file.path(outputFolder_Vax))
if (!file.exists(file.path(outputFolder_Cov)))
  dir.create(file.path(outputFolder_Cov))

ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
ParallelLogger::addDefaultErrorReportLogger()

on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)

start <- Sys.time()

#-- VAX
eoList <- createTos(outputFolder = outputFolder,group="main_vax")

sccsResult_Vax <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTableExposures_Vax,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTableOutcomes,
  cdmVersion = cdmVersion,
  outputFolder = outputFolder_Vax,
  # outputFolder = file.path(outputFolder,"single"), ############
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  # exposureOutcomeList = eoList_single, ##########
  sccsAnalysisList = sccsAnalysisList,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 4,
  createSccsIntervalDataThreads  = min(5, maxCores),
  fitSccsModelThreads = max(1, floor(maxCores/4)),
  cvThreads =  min(4, maxCores)
  )

t1 <- Sys.time() - start
ParallelLogger::logInfo(paste("Completed SCCS analyses of vaccine cohort in", signif(t1, 3), attr(t1, "units")))

#-- COV
eoList <- createTos(outputFolder = outputFolder,group="main_cov")
t1 <- Sys.time()
sccsResult_Cov <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTableExposures_Cov,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTableOutcomes,
  cdmVersion = cdmVersion,
  outputFolder = outputFolder_Cov,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList_Cov,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 4,
  createSccsIntervalDataThreads  = min(5, maxCores),
  fitSccsModelThreads = max(1, floor(maxCores/4)),
  cvThreads =  min(4, maxCores)
)


t2 <- Sys.time() - t1
ParallelLogger::logInfo(paste("Completed SCCS analyses of Covid cohort in", signif(t2, 3), attr(t2, "units")))

# ------------- SENSITIVITY ANALYSIS ----------
# sensitivity 1 --------------
ParallelLogger::logInfo("Conducting sensitivity analysis: require 21 days follow-up after index date")
outputFolder_Vax_21d <- file.path(outputFolder,"Vax_21d")
outputFolder_Cov_21d <- file.path(outputFolder,"Cov_21d")

if (!file.exists(file.path(outputFolder_Vax_21d)))
  dir.create(file.path(outputFolder_Vax_21d))
if (!file.exists(file.path(outputFolder_Cov_21d)))
  dir.create(file.path(outputFolder_Cov_21d))

#-- VAX
eoList <- createTos(outputFolder = outputFolder,group="main_vax")
t2 <- Sys.time()
sccsResult_Vax_21d <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTableExposures_Vax_21d,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTableOutcomes,
  cdmVersion = cdmVersion,
  outputFolder = outputFolder_Vax_21d,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 4,
  createSccsIntervalDataThreads  = min(5, maxCores),
  fitSccsModelThreads = max(1, floor(maxCores/4)),
  cvThreads =  min(4, maxCores)
)

t3 <- Sys.time() - t2
ParallelLogger::logInfo(paste("Completed SCCS analyses of vaccine cohort in", signif(t3, 3), attr(t3, "units")))

#-- COV
t4 <- Sys.time()
eoList <- createTos(outputFolder = outputFolder,group="main_cov")

sccsResult_Cov_21d <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTableExposures_Cov_21d,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTableOutcomes,
  cdmVersion = cdmVersion,
  outputFolder = outputFolder_Cov_21d,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList_Cov,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 4,
  createSccsIntervalDataThreads  = min(5, maxCores),
  fitSccsModelThreads = max(1, floor(maxCores/4)),
  cvThreads =  min(4, maxCores)
)

t5<- Sys.time() - t4
ParallelLogger::logInfo(paste("Completed SCCS analyses of Covid cohort in", signif(t5, 3), attr(t5, "units")))

# sensitivity 2 ---------------
ParallelLogger::logInfo("Conducting sensitivity analysis: require 365 days observation prior index date")
outputFolder_Vax_w_history <- file.path(outputFolder,"Vax_w_history")
outputFolder_Cov_w_history <- file.path(outputFolder,"Cov_w_history")

if (!file.exists(file.path(outputFolder_Vax_w_history)))
  dir.create(file.path(outputFolder_Vax_w_history))
if (!file.exists(file.path(outputFolder_Cov_w_history)))
  dir.create(file.path(outputFolder_Cov_w_history))

#-- VAX
eoList <- createTos(outputFolder = outputFolder,group="main_vax")
t2 <- Sys.time()
sccsResult_Vax_w_history <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTableExposures_Vax_w_history,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTableOutcomes,
  cdmVersion = cdmVersion,
  outputFolder = outputFolder_Vax_w_history,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 4,
  createSccsIntervalDataThreads  = min(5, maxCores),
  fitSccsModelThreads = max(1, floor(maxCores/4)),
  cvThreads =  min(4, maxCores)
)

t3 <- Sys.time() - t2
ParallelLogger::logInfo(paste("Completed SCCS analyses of vaccine cohort in", signif(t3, 3), attr(t3, "units")))

#-- COV
t4 <- Sys.time()
eoList <- createTos(outputFolder = outputFolder,group="main_cov")

sccsResult_Cov_w_history <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTableExposures_Cov_w_history,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTableOutcomes,
  cdmVersion = cdmVersion,
  outputFolder = outputFolder_Cov_w_history,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList_Cov,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 4,
  createSccsIntervalDataThreads  = min(5, maxCores),
  fitSccsModelThreads = max(1, floor(maxCores/4)),
  cvThreads =  min(4, maxCores)
)


t5<- Sys.time() - t4
ParallelLogger::logInfo(paste("Completed SCCS analyses of Covid cohort in", signif(t5, 3), attr(t5, "units")))


# Analysis summary --------------
outputFolderNames <- c(outputFolder_Vax,outputFolder_Vax_21d,outputFolder_Vax_w_history,outputFolder_Cov,outputFolder_Cov_21d,outputFolder_Cov_w_history)

for (outputFolderName in outputFolderNames){
  outcomeModelReference <- readRDS(file.path(outputFolderName, "outcomeModelReference.rds"))
  sccsSummary <- SelfControlledCaseSeries::summarizeSccsAnalyses(outcomeModelReference, outputFolderName)
  sccsSummaryFile <- file.path(outputFolderName, "sccsSummary.csv")
  readr::write_csv(sccsSummary, sccsSummaryFile)
  saveRDS(sccsSummary,file.path(outputFolderName, "sccsSummary.RDS"))
}

sccsSummary_all <- list()
for (outputFolderName in outputFolderNames){
  sccsSummary <- readRDS(file.path(outputFolderName, "sccsSummary.RDS"))
  AnaName <- gsub("(.*/\\s*(.*$))", "\\2", outputFolderName)
  sccsSummary_all[[AnaName]] <- sccsSummary
  sccsSummary_all[[AnaName]]$ExposureGroup <- AnaName
}
sccsSummary_all <- bind_rows(sccsSummary_all)
readr::write_csv(sccsSummary_all, file.path(outputFolder, "sccsSummary_all.csv"))
saveRDS(sccsSummary_all,file.path(outputFolder, "sccsSummary_all.RDS"))

# Statistical power -----------------------------------------------------------------

t1 <- Sys.time()
for (outputFolderName in outputFolderNames){
  reference <- readRDS(file.path(outputFolderName, "outcomeModelReference.rds"))
  Power_mdrr <- list()
  for (i in 1 : nrow(reference)){
    sccsIntervalData <- SelfControlledCaseSeries::loadSccsIntervalData(file.path(outputFolderName,reference$sccsIntervalDataFile[i]))
    mdrr <-  SelfControlledCaseSeries::computeMdrr(sccsIntervalData,1000)
    mdrr$exposureId <- reference$exposureId[i]
    mdrr$outcomeId  <- reference$outcomeId [i]
    mdrr$analysisId  <- reference$analysisId [i]
    Power_mdrr[[i]] <- mdrr
  }

  Power_mdrr <- bind_rows(Power_mdrr)
  readr::write_csv(Power_mdrr, file.path(outputFolderName, "Power_mdrr.csv"))
  saveRDS(Power_mdrr,file.path(outputFolderName, "Power_mdrr.RDS"))
}

t2 <- Sys.time() - t1
ParallelLogger::logInfo(paste("Completed compute MDRR in", signif(t3, 3), attr(t3, "units")))

# all mdrr into one table --------
MDRR_all <- list()
for (outputFolderName in outputFolderNames){
  Power_mdrr <- readRDS(file.path(outputFolderName, "Power_mdrr.RDS"))
  AnaName <- gsub("(.*/\\s*(.*$))", "\\2", outputFolderName)
  MDRR_all[[AnaName]] <- Power_mdrr
  MDRR_all[[AnaName]]$ExposureGroup <- AnaName
}
MDRR_all <- bind_rows(MDRR_all)
readr::write_csv(MDRR_all, file.path(outputFolder, "MDRR_all.csv"))
saveRDS(MDRR_all,file.path(outputFolder, "MDRR_all.RDS"))

ParallelLogger::clearLoggers()

#====================== END =======================================

# diagnostics plot ------------------------------------------------
library(dplyr)
library(tidyr)
library(here)
library(stringr)
#library(epiR)
library(ggplot2)
library(purrr)
#library(popEpi)
#library(Epi)
#library(splines)
#library(epitools)
library(scales)

options(scipen = 999)
studyPopFile <- unique(reference[reference$outcomeId==1,]$studyPopFile)
studyPop <- readRDS(file.path(outputFolder, studyPopFile))
sccsDataFile <- unique(reference$sccsDataFile)
sccsData <- loadSccsData(file.path(outputFolder, sccsDataFile[1]))

SelfControlledCaseSeries::plotExposureCentered(studyPop,sccsData,exposureEraId=1)
SelfControlledCaseSeries::plotExposureCentered(studyPop,sccsData,exposureEraId=2)

SelfControlledCaseSeries::plotAgeSpans(studyPop)
SelfControlledCaseSeries::plotEventObservationDependence(studyPop)
SelfControlledCaseSeries::plotEventToCalendarTime(studyPop)


#======== create analysis setting (no need to run) =====================



# Specifying analyses --------------------------------------------------
getDbSccsDataArgs1 <- SelfControlledCaseSeries::createGetDbSccsDataArgs(
  useCustomCovariates = FALSE,
  deleteCovariatesSmallCount = 100,
  studyStartDate = "20170101",
  # studyEndDate = "",
  # exposureIds = c(),
  maxCasesPerOutcome =0)

createStudyPopulationArgs1 <- SelfControlledCaseSeries::createCreateStudyPopulationArgs(
  firstOutcomeOnly = TRUE)

# Defining  model setting
covarVax <- createEraCovariateSettings(
  label = "post-vaccine 21d",
  includeEraIds = "exposureId",
  startAnchor = "era start" ,
  start = 1,
  end = 21,
  endAnchor = "era start",
  splitPoints = c(7,14))

sameday <- createEraCovariateSettings(label = "vaccine 0",
                                      includeEraIds = "exposureId",
                                      #    includeEraIds = VaxAZ,
                                      startAnchor = "era start" ,
                                      start = 0,
                                      end = 0,
                                      endAnchor = "era start")

covarPreVax <- createEraCovariateSettings(label = "Pre-exposure 21d",
                                          includeEraIds = "exposureId",
                                          startAnchor = "era start" ,
                                          start = -21,
                                          end = -1,
                                          endAnchor = "era start")

covarPre1yVax <- createEraCovariateSettings(label = "Pre-exposure 1yr",
                                            includeEraIds = "exposureId",
                                            start = -365,
                                            end = -1,
                                            endAnchor = "era start")

covarCov90 <- createEraCovariateSettings(
  label = "post-covid-19 90d",
  includeEraIds = "exposureId",
  startAnchor = "era start" ,
  start = 1,
  end = 90,
  endAnchor = "era start",
  splitPoints = c(21))


createSccsIntervalDataArgs1 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(sameday,
                              covarVax,
                              covarPreVax))

## Including age and seasonality
ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)
seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 4)

createSccsIntervalDataArgs2 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(sameday,
                              covarVax,
                              covarPreVax),
  ageCovariateSettings = ageCovariateSettings,
  seasonalityCovariateSettings = seasonalityCovariateSettings,
  eventDependentObservation = TRUE)

createSccsIntervalDataArgs3 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(sameday,
                              covarVax,
                              covarPre1yVax),
  ageCovariateSettings = ageCovariateSettings,
  seasonalityCovariateSettings = seasonalityCovariateSettings,
  eventDependentObservation = TRUE)

createSccsIntervalDataArgs4 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(sameday,
                              covarCov90,
                              covarPreVax),
  ageCovariateSettings = ageCovariateSettings,
  seasonalityCovariateSettings = seasonalityCovariateSettings,
  eventDependentObservation = TRUE)


fitSccsModelArgs <- createFitSccsModelArgs(control = createControl(threads = parallel::detectCores()-1))


sccsAnalysis1 <- createSccsAnalysis(analysisId = 1,
                                    description = "Simplest model",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createStudyPopulationArgs = createStudyPopulationArgs1,
                                    createSccsIntervalDataArgs = createSccsIntervalDataArgs1,
                                    fitSccsModelArgs = fitSccsModelArgs)

sccsAnalysis2 <- createSccsAnalysis(analysisId = 2,
                                    description = "age and season model",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createStudyPopulationArgs = createStudyPopulationArgs1,
                                    createSccsIntervalDataArgs = createSccsIntervalDataArgs2,
                                    fitSccsModelArgs = fitSccsModelArgs)

sccsAnalysis3 <- createSccsAnalysis(analysisId = 3,
                                    description = "age and season model with 1yr pre-vax",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createStudyPopulationArgs = createStudyPopulationArgs1,
                                    createSccsIntervalDataArgs = createSccsIntervalDataArgs3,
                                    fitSccsModelArgs = fitSccsModelArgs)

sccsAnalysis4 <- createSccsAnalysis(analysisId = 4,
                                    description = "age and season model post-covid 90d",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createStudyPopulationArgs = createStudyPopulationArgs1,
                                    createSccsIntervalDataArgs = createSccsIntervalDataArgs4,
                                    fitSccsModelArgs = fitSccsModelArgs)

sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2,sccsAnalysis3)
sccsAnalysisList_Cov <- list(sccsAnalysis1, sccsAnalysis2,sccsAnalysis3,sccsAnalysis4)

SelfControlledCaseSeries::saveSccsAnalysisList(sccsAnalysisList, "./Settings/sccsAnalysisList.json")
sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList("./Settings/sccsAnalysisList.json")

SelfControlledCaseSeries::saveSccsAnalysisList(sccsAnalysisList_Cov, "./Settings/sccsAnalysisList_Cov.json")
sccsAnalysisList_Cov <- SelfControlledCaseSeries::loadSccsAnalysisList("./Settings/sccsAnalysisList_Cov.json")
