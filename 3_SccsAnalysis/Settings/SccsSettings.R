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

covarVax21noSplit <- createEraCovariateSettings(
  label = "post-vaccine 21d no split",
  includeEraIds = "exposureId",
  startAnchor = "era start" ,
  start = 1,
  end = 21,
  endAnchor = "era start")

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

covarCov90 <- createEraCovariateSettings(label = "post-covid-19 90d",
                                         includeEraIds = "exposureId",
                                         startAnchor = "era start" ,
                                         start = 1,
                                         end = 90,
                                         endAnchor = "era start")


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

createSccsIntervalDataArgs5 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(sameday,
                              covarVax21noSplit,
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
                                    description = "age and season model 21d split",
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

sccsAnalysis5 <- createSccsAnalysis(analysisId = 5,
                                    description = "age and season model 21 d without split",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createStudyPopulationArgs = createStudyPopulationArgs1,
                                    createSccsIntervalDataArgs = createSccsIntervalDataArgs5,
                                    fitSccsModelArgs = fitSccsModelArgs)

sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2,sccsAnalysis3,sccsAnalysis5)
sccsAnalysisList_Cov <- list(sccsAnalysis1, sccsAnalysis2,sccsAnalysis3,sccsAnalysis4,sccsAnalysis5)

SelfControlledCaseSeries::saveSccsAnalysisList(sccsAnalysisList,
                                                here("3_SccsAnalysis", "Settings", "sccsAnalysisList.json"))
# sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList("./Settings/sccsAnalysisList.json")

SelfControlledCaseSeries::saveSccsAnalysisList(sccsAnalysisList_Cov, 
                                               here("3_SccsAnalysis", "Settings", "sccsAnalysisList_Cov.json"))
# sccsAnalysisList_Cov <- SelfControlledCaseSeries::loadSccsAnalysisList("./Settings/sccsAnalysisList_Cov.json")
