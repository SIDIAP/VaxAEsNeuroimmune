# bring in model settings ----
# nb these have been created in the SccsSettings.R file
sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList(
                        here("3_SccsAnalysis", "Settings", "sccsAnalysisList.json"))
sccsAnalysisList_Cov <- SelfControlledCaseSeries::loadSccsAnalysisList(
                        here("3_SccsAnalysis", "Settings", "sccsAnalysisList_Cov.json"))

# comparisons to make
createTos <- function(outputFolder,group) {
  pathToCsv <- here::here("3_SccsAnalysis","Settings", "tosOfInterest.csv")
  tosOfInterest <- suppressMessages(read_delim("3_SccsAnalysis/Settings/tosOfInterest.csv", 
     delim = ";", escape_double = FALSE, trim_ws = TRUE))# Berta: changed delim to ,
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

# set up logging -----
ParallelLogger::addDefaultFileLogger(file.path(output.folder, "log.txt"))
ParallelLogger::addDefaultErrorReportLogger()

on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)

# run for vax ----
eoList <- createTos(outputFolder = output.folder,group="main_vax")
start <- Sys.time()
sccsResult_Vax <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdm_database_schema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = results_database_schema,
  exposureTable = cohortTableExposuresVax,
  outcomeDatabaseSchema = results_database_schema,
  outcomeTable = cohortTableOutcomes,
  outputFolder = outputFolder_SccsVax,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = (detectCores()-2),
  createSccsIntervalDataThreads  = (detectCores()-2),
  fitSccsModelThreads = (detectCores()-2),
  cvThreads =  (detectCores()-2))
t1 <- Sys.time() - start
ParallelLogger::logInfo(paste("Completed SCCS analyses of vaccine cohort in", 
                              signif(t1, 3), attr(t1, "units")))

# run for covid ----- 
eoList <- createTos(outputFolder = outputFolder,group="main_cov")
t1 <- Sys.time()
sccsResult_Cov <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdm_database_schema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = results_database_schema,
  exposureTable = cohortTableExposuresCov,
  outcomeDatabaseSchema = results_database_schema,
  outcomeTable = cohortTableOutcomes,
  outputFolder = outputFolder_SccsCov,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList_Cov,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = (detectCores()-2),
  createSccsIntervalDataThreads  = (detectCores()-2),
  fitSccsModelThreads = (detectCores()-2),
  cvThreads =  (detectCores()-2))
t2 <- Sys.time() - t1
ParallelLogger::logInfo(paste("Completed SCCS analyses of Covid cohort in", 
                              signif(t2, 3), attr(t2, "units")))

# run for vax (requiring year of prior history) ----
ParallelLogger::logInfo("Conducting sensitivity analysis: require 365 days observation prior index date")

eoList <- createTos(outputFolder = outputFolder,group="main_vax")
t2 <- Sys.time()
sccsResult_Vax_w_history <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdm_database_schema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = results_database_schema,
  exposureTable = cohortTableExposuresVax_w_history,
  outcomeDatabaseSchema = results_database_schema,
  outcomeTable = cohortTableOutcomes,
  outputFolder = outputFolder_SccsVax_w_history,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = (detectCores()-2),
  createSccsIntervalDataThreads  = (detectCores()-2),
  fitSccsModelThreads = (detectCores()-2),
  cvThreads =  (detectCores()-2))
t3 <- Sys.time() - t2
ParallelLogger::logInfo(paste("Completed SCCS analyses of vaccine cohort with history in", 
                              signif(t3, 3), attr(t3, "units")))


# run for covid (requiring year of prior history)  -------
eoList <- createTos(outputFolder = outputFolder,group="main_cov")
t4 <- Sys.time()
sccsResult_Cov_w_history <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdm_database_schema,
  oracleTempSchema = NULL,
  exposureDatabaseSchema = results_database_schema,
  exposureTable = cohortTableExposuresCov_w_history,
  outcomeDatabaseSchema = results_database_schema,
  outcomeTable = cohortTableOutcomes,
  outputFolder = outputFolder_SccsCov_w_history,
  combineDataFetchAcrossOutcomes = TRUE,
  exposureOutcomeList = eoList,
  sccsAnalysisList = sccsAnalysisList_Cov,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = (detectCores()-2),
  createSccsIntervalDataThreads  = (detectCores()-2),
  fitSccsModelThreads = (detectCores()-2),
  cvThreads =  (detectCores()-2))
t5<- Sys.time() - t4
ParallelLogger::logInfo(paste("Completed SCCS analyses of Covid cohort with history in", 
                              signif(t5, 3), attr(t5, "units")))

# analysis summary --------------
outputFolderNames <- c(outputFolder_SccsVax,
                       outputFolder_SccsVax_w_history,
                       outputFolder_SccsCov,
                       outputFolder_SccsCov_w_history)

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
readr::write_csv(sccsSummary_all, file.path(output.folder, "sccsSummary_all.csv"))
saveRDS(sccsSummary_all,file.path(output.folder, "sccsSummary_all.RDS"))

# result table new ------------------------------------------------

outputFolder_Vax <- file.path(output.folder,"SccsVax") # Berta
outputFolder_Cov <- file.path(output.folder,"SccsCov") # Berta
# Berta outputFolder_Vax_21d <- file.path(output.folder,"Vax_21d")
# BertaoutputFolder_Cov_21d <- file.path(output.folder,"Cov_21d")
outputFolder_Vax_w_history <- file.path(output.folder,"Vax_w_history")
outputFolder_Cov_w_history <- file.path(output.folder,"Cov_w_history")

outputFolderNames <- c(outputFolder_Vax,outputFolder_Vax_w_history,outputFolder_Cov,outputFolder_Cov_w_history)#outputFolder_Vax_21d,outputFolder_Cov_21d,
for (outputFolderName in outputFolderNames){
  reference <- readRDS(file.path(outputFolderName, "outcomeModelReference.rds"))
  reference_InUse <- reference %>% filter(analysisId %in% c(2,4,5) & exposureId %in% c(1,3,8,10,15,16) & outcomeId ==1 )
  
  results <- list()
  for (i in 1 : nrow(reference_InUse)){
    sccsModel <- readRDS(file.path(outputFolderName, reference_InUse$sccsModelFile[i]))
    estimates <-getModel(sccsModel)
    estimates <- estimates %>% filter(originalEraType!= "hoi") %>% distinct() %>% rename(covariateId = id)
    
    event_num <- sccsModel[["metaData"]][["covariateStatistics"]]
    
    row <- estimates %>% inner_join(event_num, by=c("covariateId"))
    
    row$exposureId <- reference_InUse$exposureId[i]
    row$outcomeId  <- reference_InUse$outcomeId [i]
    row$analysisId  <- reference_InUse$analysisId [i]
    
    results[[i]] <- row
  }
  results <- bind_rows(results)
  readr::write_csv(results, file.path(outputFolderName, "results.csv"))
  saveRDS(results,file.path(outputFolderName, "results.RDS"))
}

results_all <- list()
for (outputFolderName in outputFolderNames){
  results <- readRDS(file.path(outputFolderName, "results.RDS"))
  AnaName <- gsub("(.*/\\s*(.*$))", "\\2", outputFolderName)
  results_all[[AnaName]] <- results
  results_all[[AnaName]]$ExposureGroup <- AnaName
}
results_all <- bind_rows(results_all)
readr::write_csv(sccsSummary_all, file.path(output.folder, "results_all.csv"))
saveRDS(sccsSummary_all,file.path(output.folder, "results_all.RDS"))


# get mdrrs -----
t1 <- Sys.time()
for (outputFolderName in outputFolderNames){
  reference <- readRDS(file.path(outputFolderName, "outcomeModelReference.rds"))
  Power_mdrr <- list()
  for (i in 1 : nrow(reference)){
    sccsIntervalData <- SelfControlledCaseSeries::loadSccsIntervalData(file.path(outputFolderName,reference$sccsIntervalDataFile[i]))
    mdrr <-  SelfControlledCaseSeries::computeMdrr(sccsIntervalData,1001)
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
readr::write_csv(MDRR_all, file.path(output.folder, "MDRR_all.csv"))
saveRDS(MDRR_all,file.path(output.folder, "MDRR_all.RDS"))

ParallelLogger::clearLoggers()

MDRR_filtered <- MDRR_all %>% filter(mdrr<2)
readr::write_csv(MDRR_filtered, file.path(output.folder, "MDRR_filtered.csv"))
# diagnostics plot ------------------------------------------------

options(scipen = 999)
studyPopFile <- unique(reference[reference$outcomeId==1,]$studyPopFile)
studyPop <- readRDS(file.path(outputFolder_SccsVax, studyPopFile))
sccsDataFile <- unique(reference$sccsDataFile)
sccsData <- loadSccsData(file.path(outputFolder_SccsVax, sccsDataFile[1]))

SelfControlledCaseSeries::plotExposureCentered(studyPop,sccsData,exposureEraId=1)
SelfControlledCaseSeries::plotExposureCentered(studyPop,sccsData,exposureEraId=2)

SelfControlledCaseSeries::plotAgeSpans(studyPop)
SelfControlledCaseSeries::plotEventObservationDependence(studyPop)
SelfControlledCaseSeries::plotEventToCalendarTime(studyPop)

