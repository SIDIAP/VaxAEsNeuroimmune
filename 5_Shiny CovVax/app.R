#### CLEAR WORKSPACE -----
# rm(list=ls())

#### PACKAGES -----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
# library(ggthemes)
library(plotly)
library(here)
library(scales)
library(dplyr)
library(stringr)
library(tidyr)
library(epitools)

#### Load data -----
load(here("data", "Network.patient.characteristcis.RData")) 
# Network.patient.characteristcis<-Network.patient.characteristcis%>% 
#   filter(str_detect(pop, "ChAdOx1 second", negate=TRUE))
load(here("data", "Network.Survival.summary.RData"))
# Network.Survival.summary<-Network.Survival.summary%>% 
#   filter(str_detect(pop, "ChAdOx1 second", negate=TRUE))
Network.Survival.summary<-Network.Survival.summary %>% 
  mutate(`Cumulative incidence`=ifelse(Events %in% c(0,1,2,3,4), " ", 
                                       `Cumulative incidence`)) %>% 
  mutate(Events=ifelse(Events %in% c(0,1,2,3,4), "<5",Events))

load(here("data", "Network.IR.RData"))
# Network.IR<-Network.IR%>% 
#   filter(str_detect(pop, "ChAdOx1 second", negate=TRUE))

load(here("data", "COVID_diagnosis_21d_background.pop.IRR.RData"))
load(here("data", "COVID_diagnosis_90d_background.pop.IRR.RData"))
# load(file = here("data", "COVID_diagnosis_background.pop.visit.IRR.RData"))
load(file = here("data", "AZ1_background.pop.IRR.RData"))
load(file = here("data", "AZ1.no.covid_background.pop.IRR.RData"))
# load(file = here("data", "AZ_background.pop.visit.IRR.RData"))
load(file = here("data", "AZ2_background.pop.IRR.RData"))
load(file = here("data", "AZ2.no.covid_background.pop.IRR.RData"))
# load(file = here("data", "AZ2.no.covid_background.pop.IRR.RData"))
load(file = here("data", "Pf1_background.pop.IRR.RData"))
load(file = here("data", "Pf1.no.covid_background.pop.IRR.RData"))
# load(file = here("data", "Pf1_background.pop.visit.IRR.RData"))
load(file = here("data", "Pf2_background.pop.IRR.RData"))
load(file = here("data", "Pf2.no.covid_background.pop.IRR.RData"))
# load(file = here("data", "Pf2_background.pop.visit.IRR.RData"))
load(file = here("data", "Mod1_background.pop.IRR.RData"))
load(file = here("data", "Mod1.no.covid_background.pop.IRR.RData"))

load(file = here("data", "Mod2_background.pop.IRR.RData"))
# load(file = here("data", "Mod1_background.pop.visit.IRR.RData"))
# load(file = here("data", "Mod2_background.pop.IRR.RData"))
# load(file = here("data", "Mod2_background.pop.visit.IRR.RData"))
load(file = here("data", "Jnj_background.pop.IRR.RData"))
load(file = here("data", "Jnj.no.covid_background.pop.IRR.RData"))

load(here("data", "COVID_diagnosis_21d_background.pop.IRS.RData"))
load(here("data", "COVID_diagnosis_21d_background.pop.IRS.RData"))
load(here("data", "COVID_diagnosis_90d_background.pop.IRS.RData"))
load(here("data", "COVID_diagnosis_90d_background.pop.IRS.RData"))
# load(here("data", "COVID_diagnosis_background.pop.visit.IRS.RData"))
load(here("data", "AZ1_background.pop.IRS.RData"))
load(here("data", "AZ1.no.covid_background.pop.IRS.RData"))
load(here("data", "AZ2_background.pop.IRS.RData"))
load(here("data", "AZ2.no.covid_background.pop.IRS.RData"))
# load(here("data", "AZ_background.pop.visit.IRS.RData"))
load(here("data", "Pf1_background.pop.IRS.RData"))
load(here("data", "Pf1.no.covid_background.pop.IRS.RData"))
# load(here("data", "Pf1_background.pop.visit.IRS.RData"))
load(here("data", "Pf2_background.pop.IRS.RData"))
load(here("data", "Pf2.no.covid_background.pop.IRS.RData"))
# load(here("data", "Pf2_background.pop.visit.IRS.RData"))
 
load(here("data", "Mod1_background.pop.IRS.RData"))
load(here("data", "Mod1.no.covid_background.pop.IRS.RData"))
# load(here("data", "Mod1_background.pop.visit.IRS.RData"))
load(here("data", "Mod2_background.pop.IRS.RData"))
# load(here("data", "Mod2_background.pop.visit.IRS.RData"))
# 
load(here("data", "Jnj_background.pop.IRS.RData"))
load(here("data", "Jnj.no.covid_background.pop.IRS.RData"))
# load(here("data", "Jnj_background.pop.visit.IRS.RData"))

load(here("data", "MDRR.RData"))
load(here("data", "sccsSummary.tidy.RData"))

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
#### UI -----
ui <-  fluidPage(theme = shinytheme("spacelab"),
                 
# title ------ 
# shown across tabs
titlePanel("Incidence of immune-mediated neurological events
           among persons vaccinated against or infected with SARS-CoV-2"),
               
# set up: pages along the side -----  
                 navlistPanel(
                   
                   
## Introduction  -----  
tabPanel("Background", 
  tags$h3("Background"),
     tags$hr(),
   tags$h4(tags$strong("Please note, the results presented here should be considered as 
                       preliminary and subject to change.")),
   tags$hr(),
   # tags$h5("This app is a companion to the study ...."),
  HTML('<br>'),
  tags$h4("Abstract"),
  tags$hr()
), 
## Patient profiles ------ 
tabPanel("Patient Profiles",	  
    tags$h3("Patient Profiles"),
    tags$h5("A summary of study participant characteristics are shown below, for the study population as
    a whole and for those with a specific outcome of interest. Select the study population used,
    whether a year of prior history was required for individuals, 
    a population strata, and which populations are to be summarised."),
    tags$hr(),
    
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "CohortProfileDatabaseSelector",
  label = "Database",
  choices = unique(Network.patient.characteristcis$db),
  selected = unique(Network.patient.characteristcis$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)
          ),
  
           div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "CohortProfileStudyPopulationTypeSelector", 
  label = "Study population", 
  choices = unique(Network.patient.characteristcis$pop[which(Network.patient.characteristcis$pop!="Vaccinated")]),
  selected = unique(Network.patient.characteristcis$pop[which(Network.patient.characteristcis$pop!="Vaccinated")])[1],
  options = list(
    `actions-box` = TRUE, 
    size = 10,
    `selected-text-format` = "count > 3"), 
  multiple = TRUE)),
  
  # div(style="display: inline-block;vertical-align:top; width: 250px;",
  #               pickerInput(inputId = "CohortProfileStudyPopulationSelector", 
  # label = "Study month (vaccinated)", 
  # choices = unique(Network.patient.characteristcis$pop.type[which(!is.na(Network.patient.characteristcis$pop.type))]),
  # selected = unique(Network.patient.characteristcis$pop.type[which(!is.na(Network.patient.characteristcis$pop.type))])[1],
  # options = list(
  #   `actions-box` = TRUE, 
  #   size = 10,
  #   `selected-text-format` = "count > 3"), 
  # multiple = FALSE)),
    
  div(style="display: inline-block;vertical-align:top; width: 250px;",
    pickerInput(inputId = "CohortProfilePriorHistorySelector", 
  label = "Year of prior history required", 
  choices = rev(unique(Network.patient.characteristcis$prior.obs.required)), 
  selected = rev(unique(Network.patient.characteristcis$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE, 
    size = 10,
    `selected-text-format` = "count > 3"), 
  multiple = FALSE)),
  
      
  div(style="display: inline-block;vertical-align:top; width: 250px;",
    pickerInput(inputId = "CohortProfilePriorAgeSelector", 
  label = "Age strata", 
  choices = unique(Network.patient.characteristcis$age_gr2), 
  selected = unique(Network.patient.characteristcis$age_gr2)[1],
  options = list(
    `actions-box` = TRUE, 
    size = 10,
    `selected-text-format` = "count > 3"), 
  multiple = FALSE)),
  
  div(style="display: inline-block;vertical-align:top; width: 150px;",
  pickerInput(inputId = "CohortProfileOutcomeSelector",
  label = "Cohort",
  choices = c("Study population", sort(names(Network.patient.characteristcis)[!names(Network.patient.characteristcis) %in% c("id","var","db","pop", "study.year", "end.year","prior.obs.required" , "pop.type","Study population", "age_gr2")])),
  selected = c("Study population", sort(names(Network.patient.characteristcis)[!names(Network.patient.characteristcis) %in% c("id","var","db","pop", "study.year", "end.year","prior.obs.required" , "pop.type","Study population", "age_gr2")]))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),
  
  tags$hr(),
  DTOutput('tbl')
                            )
,
                   
                   
## Incidence ----
 tabPanel("Cumulative Incidence",

    tags$h3("Cumulative Incidence"),
    tags$h5("The cumulative incidence of events over time. Select the
            study populations,
            whether a year of prior history was required for individuals
            to be included into the study, the statification of interest, and
            the outcome of interest. "),
    tags$hr(),

     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "IncDatabaseSelector",
  label = "Database",
  choices = unique(Network.Survival.summary$db),
  selected = unique(Network.Survival.summary$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

             div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "IncStudyPopulationTypeSelector",
  label = "Study population",
  choices = unique(Network.Survival.summary$pop),
  selected = unique(Network.Survival.summary$pop)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

  # div(style="display: inline-block;vertical-align:top; width: 250px;",
  #     pickerInput(inputId = "IncPopulationSelector",
  # label = "Study month (vaccinated)",
  # choices = unique(Network.Survival.summary$pop.type),
  # selected = unique(Network.Survival.summary$pop.type)[1],
  #   # c("Date anchored: 1st January 2017",
  #   #           "Visit anchored",
  #   #           "Visit anchored (28 days follow up)"),
  # options = list(
  #   `actions-box` = TRUE,
  #   size = 10,
  #   `selected-text-format` = "count > 3"),
  # multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "IncStudyPopulationSelector",
  label = "Year of prior history required",
  choices = rev(unique(Network.Survival.summary$prior.obs.required)),
  selected = rev(unique(Network.Survival.summary$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IncStrataSelector",
  label = "Strata",
  choices =
  c("Overall",
              "Sex",
              "Age group (<=44, 45-64, >=65)",
              "Age group (<=44, 45-64, >=65) and sex",
              "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)",
              "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex",
              "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)",
              "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex",
    "Condition of interest",
    "Medication of interest",
    "Condition or medication of interest") ,
  selected = "Overall",
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IncOutcomeSelector",
  label = "Outcome",
  choices = sort(unique(Network.Survival.summary$outcome.name)) ,
  selected = sort(unique(Network.Survival.summary$outcome.name))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IncDaysSelector",
  label = "Time point",
  choices = c("All", sort(unique(Network.Survival.summary$time))) ,
  selected = c("All", sort(unique(Network.Survival.summary$time)))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),
  DTOutput('inc.tbl')

 ),
          
## Incidence Rates ------ 
 tabPanel("Incidence Rates",

    tags$h3("Incidence Rates"),
    tags$h5("Incidence rates per 100,000 person-years are shown below. Select the type of study population used, whether a year of prior
    history was required,Select the study population used,
    whether a year of prior history was required for individuals, the statification of interest, and
            the outcome and time point of interest."),
    tags$hr(),
     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "IRsDatabaseSelector",
  label = "Database",
  choices = unique(Network.IR$db),
  selected = unique(Network.IR$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

               div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "IRsStudyPopulationTypeSelector",
  label = "Study population",
  choices = unique(Network.IR$pop),
  selected = unique(Network.IR$pop)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),


  # div(style="display: inline-block;vertical-align:top; width: 250px;",
  #     pickerInput(inputId = "IRsPopulationSelector",
  # label = "Study month (vaccinated)",
  # choices = unique(Network.IR$pop.type[which(!is.na(Network.IR$pop.type))]),
  # selected = unique(Network.IR$pop.type[which(!is.na(Network.IR$pop.type))])[1],
  #   # c("Date anchored: 1st January 2017",
  #   #           "Visit anchored",
  #   #           "Visit anchored (28 days follow up)"),
  # options = list(
  #   `actions-box` = TRUE,
  #   size = 10,
  #   `selected-text-format` = "count > 3"),
  # multiple = FALSE)),



    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "IRsStudyPopulationSelector",
  label = "Year of prior history required",
  choices = rev(unique(Network.IR$prior.obs.required)),
  selected = rev(unique(Network.IR$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "IRsTimeWindowSelector",
  label = "Time window relative to index date",
  choices = as.character(unique(Network.IR$time.window[which(!is.na(Network.IR$time.window))])),
  selected = as.character(unique(Network.IR$time.window[which(!is.na(Network.IR$time.window))]))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IRsStrataSelector",
  label = "Strata",
  choices = c("Overall (standardised to General population (index date: 1st December))",
              # "Overall (standardised to General population (index date: first visit/ contact))",
              "Overall",
              "Sex",
              "Age group (<=44, 45-64, >=65)",
              "Age group (<=44, 45-64, >=65) and sex",
              "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)",
              "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex",
              "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)",
              "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex",
    "Condition of interest",
    "Medication of interest",
    "Condition or medication of interest",
     "Age group (<=44, 45-64, >=65), sex, and condition of interest",
     "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition of interest",
      "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition of interest",
      "Age group (<=44, 45-64, >=65), sex, and medication of interest",
      "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and medication of interest",
      "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and medication of interest",
      "Age group (<=44, 45-64, >=65), sex, and medication of interest",
      "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition or medication of interest",
      "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition or medication of interest"
    ) ,
  selected = "Overall (standardised to General population (index date: 1st December))",
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IRsOutcomeSelector",
  label = "Outcome",
  choices = sort(unique(Network.IR$outcome.name)) ,
  selected = sort(unique(Network.IR$outcome.name))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),
  tags$hr(),
prettyCheckbox(
  inputId = "log_scale", label = "Plot y axis on log scale", icon = icon("check")
),
  plotlyOutput("IR.plot.overall"))
   ,
# Incidence rate ratios ------

 tabPanel("Incidence Rate Ratios",

    tags$h3("Incidence Rate Ratios"),
    tags$h5("Incidence rates ratios. Select the target and comparator populations,
    whether a year of prior
    history was required, Select the study population used,
    whether a year of prior history was required for individuals, the time window,
    the statification of interest, and
            the outcome of interest."),
    tags$hr(),
     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "IRRDatabaseSelector",
  label = "Database",
  choices = unique(Network.IR$db),
  selected = unique(Network.IR$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

               div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "IRRTargetStudyPopulationTypeSelector",
  label = "Target",
  choices = unique(Network.IR$pop[which(!is.na(Network.IR$pop))]),
  selected = unique(Network.IR$pop[which(!is.na(Network.IR$pop))])[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "IRRComparatorStudyPopulationTypeSelector",
  label = "Comparator",
  choices = c("General population (index date: 1st December)"
              # ,
              # "General population (index date: first visit/ contact)"
              ),
  selected = c("General population (index date: 1st December)"),
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),



  # div(style="display: inline-block;vertical-align:top; width: 150px;",
  #     pickerInput(inputId = "IRRPopulationSelector",
  # label = "Target study period",
  # choices = unique(Network.IR$pop.type[which(!is.na(Network.IR$pop.type))]),
  # selected = unique(Network.IR$pop.type[which(!is.na(Network.IR$pop.type))])[1],
  #   # c("Date anchored: 1st January 2017",
  #   #           "Visit anchored",
  #   #           "Visit anchored (28 days follow up)"),
  # options = list(
  #   `actions-box` = TRUE,
  #   size = 10,
  #   `selected-text-format` = "count > 3"),
  # multiple = FALSE)),



    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "IRRStudyPopulationSelector",
  label = "Year of prior history required",
  choices = rev(unique(Network.IR$prior.obs.required)),
  selected = rev(unique(Network.IR$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "IRRTimeWindowSelector",
  label = "Time window relative to index date",
  choices = as.character(unique(Network.IR$time.window[which(!is.na(Network.IR$time.window))])),
  selected = as.character(unique(Network.IR$time.window[which(!is.na(Network.IR$time.window))]))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IRRStrataSelector",
  label = "Strata",
  choices = c("Overall (standardised)",
              "overall",
               "Sex",
               "Age group (<=44, 45-64, >=65)",
              "Age group (<=44, 45-64, >=65) and sex",
               "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)",
              "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex",
               "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)",
              "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex",
    "Condition of interest",
    "Medication of interest",
    "Condition or medication of interest",
     "Age group (<=44, 45-64, >=65), sex, and condition of interest",
     "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition of interest",
      "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition of interest",
      "Age group (<=44, 45-64, >=65), sex, and medication of interest",
      "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and medication of interest",
      "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and medication of interest",
      "Age group (<=44, 45-64, >=65), sex, and medication of interest",
      "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition or medication of interest",
      "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition or medication of interest") ,
  selected = "Overall (standardised)",
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IRROutcomeSelector",
  label = "Outcome",
  choices = sort(unique(c(
    # unique(COVID_diagnosis_background.pop.IRR$outcome.name),
# unique(COVID_diagnosis_background.pop.visit.IRR$outcome.name),
# unique(AZ_background.pop.IRR$outcome.name),
# unique(AZ_background.pop.IRR$outcome.name),
unique(Pf1_background.pop.IRR$outcome.name),
unique(Pf1_background.pop.IRR$outcome.name)))) ,
  selected = sort(unique(c(
    # unique(COVID_diagnosis_background.pop.IRR$outcome.name),
# unique(COVID_diagnosis_background.pop.visit.IRR$outcome.name),
# unique(AZ_background.pop.IRR$outcome.name),
# unique(AZ_background.pop.IRR$outcome.name),
unique(Pf1_background.pop.IRR$outcome.name),
unique(Pf1_background.pop.IRR$outcome.name))))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),
  tags$hr(),
#   prettyCheckbox(
#   inputId = "IRR_log_scale", label = "Plot x axis on log scale", icon = icon("check")
# ),
  plotlyOutput("IRR.plot.overall")
  # ,
  # tags$hr(),
  # DTOutput('tbl.IRR')
),
# sccs -----
 tabPanel("SCCS",

    tags$h3("SCCS"),
    # tags$h5("Incidence rates ratios. Select the target and comparator populations,
    # whether a year of prior
    # history was required, Select the study population used,
    # whether a year of prior history was required for individuals, the time window,
    # the statification of interest, and
    #         the outcome of interest."),
    tags$hr(),
     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "SCCSDatabaseSelector",
  label = "Database",
  choices = unique(MDRR$db),
  selected = unique(MDRR$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

               div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "SccsExposureNameSelector",
  label = "Study population",
  choices = unique(MDRR$exposureName),
  selected = unique(MDRR$exposureName)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

               div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "SCCSHistory.req",
  label = "History required",
  choices = unique(MDRR$history.req),
  selected = unique(MDRR$history.req)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),
  
      div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "SCCSOutcomeSelector",
  label = "Outcome",
  choices = unique(MDRR$outcomeName),
  selected = unique(MDRR$outcomeName)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

      div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "SCCSAnalysisSelector",
  label = "Outcome",
  choices = unique(MDRR$analysisName),
  selected = unique(MDRR$analysisName)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  
    tags$hr(),
    tags$h4("MDDR"),
   DTOutput('tbl.sccs.mddr'),
  
      tags$hr(),
    tags$h4("Results"),
   DTOutput('tbl.sccs.results')

)


# close -----
                                                   ))

#### SERVER ------
server <-	function(input, output, session) {
  
get.patient.characteristcis<-reactive({
    # browser()
table<-Network.patient.characteristcis %>%
      filter(db %in% input$CohortProfileDatabaseSelector ) %>% 
      filter(pop %in% input$CohortProfileStudyPopulationTypeSelector) %>% 
      # filter(pop.type %in% input$CohortProfileStudyPopulationSelector |
      #        is.na(pop.type)) %>% 
      filter(pop.type=="All") %>% 
      filter(prior.obs.required%in%input$CohortProfilePriorHistorySelector) %>% 
      filter(age_gr2%in%input$CohortProfilePriorAgeSelector) %>% 
      select(-"id") %>% 
      select(var, pop, input$CohortProfileOutcomeSelector, db)
  
# drop medea
table<-table %>% 
  filter(str_detect(var, "Med", negate=TRUE))

table<-table %>% 
  mutate(pop_db=paste0(pop, " [", db, "]")) %>% 
  select(-c(pop, db))

    
table<-table  %>%
  pivot_wider(names_from = pop_db,
             names_glue = "{pop_db}: {.value}",
              values_from = input$CohortProfileOutcomeSelector) 

 #    browser()
 # table<-bind_rows(
 #     table[c(1:31),],
 #    table[c(1),]  %>%
 #      mutate_at(vars(names(table)), ~ replace(., !is.na(.), NA)) %>%
 #      mutate(var="Comorbidities"),
 #     table[c(18:27),],
 #    table[c(1),]  %>%
 #      mutate_at(vars(names(table)), ~ replace(., !is.na(.), NA)) %>%
 #      mutate(var="Medication use (183 days prior to four days prior)"),
 #    table[c(28:31),]
 #    )  
 
 table<- table %>%
     mutate(var=ifelse(var=="Copd", "COPD", var)) %>%
     mutate(var=ifelse(var=="Antiinflamatory and antirheumatic", "Non-steroidal anti-inflammatory drugs ", var)) %>%
     mutate(var=ifelse(var=="Coxibs", "Cox2 inhibitors ", var)) %>%
     mutate(var=ifelse(var=="Corticosteroids", "Systemic corticosteroids ", var)) %>%
     mutate(var=ifelse(var=="Antithrombotic", "Antithrombotic and anticoagulant therapies", var)) %>%
     mutate(var=ifelse(var=="Lipid modifying", "Lipid modifying agents ", var)) %>%
     mutate(var=ifelse(var=="Antineoplastic immunomodulating", "Antineoplastic and immunomodulating agents ", var)) %>%
     mutate(var=ifelse(var=="Hormonal contraceptives", "Hormonal contraceptives for systemic use ", var)) %>%
     mutate(var=ifelse(var=="Sex hormones modulators", "Sex hormones and modulators of the genital system", var))%>%
     mutate(var=ifelse(var=="Age.18 29", "Age: 18 to 29", var))  %>%
   mutate(var=ifelse(var=="Age.30 39", "Age: 30 to 39", var))  %>%
     mutate(var=ifelse(var=="Age.40 49", "Age: 40 to 49", var))  %>%
     mutate(var=ifelse(var=="Age.50 59", "Age: 50 to 59", var))  %>%
     mutate(var=ifelse(var=="Age.60 69", "Age: 60 to 69", var))  %>%
     mutate(var=ifelse(var=="Age.70 79", "Age: 70 to 79", var))  %>%
     mutate(var=ifelse(var=="Age.80u", "Age: 80 or older", var)) 
 
 # table<- table %>%
 #     mutate(var=str_replace_all(var, "[.]g", " group")) %>% 
 #     mutate(var=str_replace(var, "Medea.u", "MEDEA: ")) %>%
 #     mutate(var=str_replace(var, "Medea.miss", "MEDEA: Missing"))   %>%
 #     mutate(var=str_replace(var, "Medea.r", "MEDEA: Rural"))   %>%
 #     mutate(var=str_replace(var, "MEDEA: 1", "MEDEA: 1 (least deprived)"))    %>%
 #     mutate(var=str_replace(var, "MEDEA: 5", "MEDEA: 5 (most deprived)"))  
table<-apply(table, 2,
                     function(x)
                       ifelse(str_sub(x, 1, 8) %in%  c("0 (0.0%)"),
             "<5",x))
table<-data.frame(table,check.names = FALSE)

   # 
  table  <-table[,!is.na(table[1,]), drop = F]
  table  <-table[,(table[1,])!="0", drop = F]
  table  <-table[,(table[1,])!="1", drop = F]
  table  <-table[,(table[1,])!="2", drop = F]
  table  <-table[,(table[1,])!="3", drop = F]
  table  <-table[,(table[1,])!="4", drop = F]
  # table  <-table[,(table[1,])!="5", drop = F]
  

    table
  }) 
 
output$tbl<-  renderDataTable({
  validate(need(length(input$CohortProfileDatabaseSelector)>0,
                "No results for selected inputs"))
    validate(need(length(input$CohortProfileStudyPopulationTypeSelector)>0,
                "No results for selected inputs"))
    validate(need(length(input$CohortProfileOutcomeSelector)>0,
                "No results for selected inputs"))
  
  
  table<-get.patient.characteristcis() 
   validate(need(ncol(table)>1, 
                "No results for selected inputs"))
  table<-table%>% select_if(~!is.na(.[1L]))
  table<-table[,  which(!table[1,] %in% c("1","2", "3", "4", "5"))]
 
  
  validate(need(ncol(table)>1, 
                "No results for selected inputs"))
  
   datatable(table
    ,rownames= FALSE,
    colnames = c('Variable' = 1),
     extensions = 'Buttons',
    # options = list(lengthChange = FALSE,
    #                pageLength = 40,
    #                buttons = c('copy', 'csv', 'excel')))
    options = list(lengthChange = FALSE,
                    pageLength = 40,
                                dom = 'tB',
                               buttons = list(list(extend = "excel", 
                                             text = "Download table as excel",
                                             filename = "PatientProfiles.csv"))
                            ))
    } )
 
get.inc.summary<-reactive({
   
  table<-Network.Survival.summary %>% 
  filter(db %in% input$IncDatabaseSelector ) %>% 
  filter(pop %in% input$IncStudyPopulationTypeSelector) %>% 
  # filter(pop.type %in% input$IncPopulationSelector) %>% 
  filter(pop.type %in% "All") %>% 
  filter(prior.obs.required%in% input$IncStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IncOutcomeSelector)
  
  if(input$IncDaysSelector!="All"){
  table<-table %>% 
   filter(time %in%  input$IncDaysSelector)
  }
  
  
    # browser()
  
  if(input$IncStrataSelector=="Overall"){
 table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("overall")) %>%  
   select(db, pop, time, "Number at risk", "Events","Cumulative incidence") 
 }
  
      if(input$IncStrataSelector=="Sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("gender")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
      }
  
        if(input$IncStrataSelector=="Condition of interest"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("cond.comp")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
        }
  
          if(input$IncStrataSelector=="Medication of interest"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("drug.comp")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
          if(input$IncStrataSelector=="Condition or medication of interest"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("cond.drug.comp")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
  
    if(input$IncStrataSelector=="Age group (<=44, 45-64, >=65) and sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr2_gender")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
      if(input$IncStrataSelector=="Age group (<=44, 45-64, >=65)"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr2")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
      if(input$IncStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr_gender")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
      }
  
        if(input$IncStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
  
      if(input$IncStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr3_gender")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
      }
  
        if(input$IncStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr3")) %>%  
   select(db,pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  

  table
  })
  
output$inc.tbl<- renderDataTable({
      table<-get.inc.summary() 

 
  
   datatable(table
    ,rownames= FALSE,
    colnames = c('Database' = 1,
                 'Study population' = 2,
                 'Days since index date' = 3),
     extensions = 'Buttons',
    # options = list(lengthChange = FALSE,
    #                pageLength = 40,
    #                buttons = c('copy', 'csv', 'excel')))
    options = list(lengthChange = FALSE,
                    pageLength = 40,
                                dom = 'tB',
                               buttons = list(list(extend = "excel", 
                                             text = "Download table as excel",
                                             filename = "Inc.csv"))
                            ))
    
  } )
    
get.IR.plot.data<- reactive({
   
 Network.IR  %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
  filter(time.window%in% input$IRsTimeWindowSelector|
             is.na(time.window)) %>%  
  filter(pop %in% input$IRsStudyPopulationTypeSelector) %>% 
  filter(pop.type %in% "All"|
             is.na(pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector)
    
  }) 

output$IR.plot.overall<- renderPlotly({
   
#   validate(
#   need(input$IRsDatabaseSelector != "", "Please select a database")
# )
  
  
working.data<-get.IR.plot.data() 

if(input$IRsStrataSelector=="Overall (standardised to General population (index date: 1st December))"){
    
  # browser()
 # Background.pop
 working.data<-list()
   if(any(input$IRsStudyPopulationTypeSelector=="General population (index date: 1st December)")){
 working.data[["working.data.gpop"]] <- Network.IR  %>%  
  filter(db %in% input$IRsDatabaseSelector) %>% 
  filter(time.window%in% input$IRsTimeWindowSelector|
             is.na(time.window)) %>%  
  filter(pop=="General population (index date: 1st December)") %>% 
  filter(pop.type %in% "All"|
             is.na(pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  filter(strata=="overall")
   }
  
   if(any(input$IRsStudyPopulationTypeSelector=="COVID19 positive test 21d")){
   working.data[["working.data.cov"]] <- COVID_diagnosis_21d_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="COVID-19 positive test (21 days)") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
   }
 
   if(any(input$IRsStudyPopulationTypeSelector=="COVID19 positive test 90d")){
   working.data[["working.data.cov"]] <- COVID_diagnosis_90d_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="COVID-19 positive test (90 days)") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
   }
 
if(any(input$IRsStudyPopulationTypeSelector=="ChAdOx1 first-dose")){
 
   working.data[["working.data.az"]] <- AZ1_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="ChAdOx1 first-dose") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
}
 
 if(any(input$IRsStudyPopulationTypeSelector=="ChAdOx1 first-dose (no prior covid)")){
 
   working.data[["working.data.az.no.covid"]] <- AZ1.no.covid_background.pop.IRS %>%
  filter(db %in% input$IRsDatabaseSelector) %>%  
   mutate(pop="ChAdOx1 first-dose (no prior covid)") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
    }

 if(any(input$IRsStudyPopulationTypeSelector=="ChAdOx1 second-dose")){
 
   working.data[["working.data.az2"]] <- AZ2_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="ChAdOx1 second-dose") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
}
 
 if(any(input$IRsStudyPopulationTypeSelector=="ChAdOx1 second-dose (no prior covid)")){
 
   working.data[["working.data.az2.no.covid"]] <- AZ2.no.covid_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="ChAdOx1 second-dose (no prior covid)") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
    }

 
if(any(input$IRsStudyPopulationTypeSelector=="BNT162b2 first-dose")){
   working.data[["working.data.pf1"]] <- Pf1_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="BNT162b2 first-dose") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
}  
 
 if(any(input$IRsStudyPopulationTypeSelector=="BNT162b2 first-dose (no prior covid)")){
   working.data[["working.data.pf1.no.covid"]] <- Pf1_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="BNT162b2 first-dose (no prior covid)") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
      }  

 
 if(any(input$IRsStudyPopulationTypeSelector=="BNT162b2 second-dose")){
   working.data[["working.data.pf2"]] <- Pf2_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="BNT162b2 second-dose") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
 }
 
  if(any(input$IRsStudyPopulationTypeSelector=="BNT162b2 second-dose (no prior covid)")){
   working.data[["working.data.pf2.no.covid"]] <- Pf2_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="BNT162b2 second-dose (no prior covid)") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
 }  

 
  if(any(input$IRsStudyPopulationTypeSelector=="Ad26.COV2.S first-dose")){
   working.data[["working.data.Jnj"]] <- Jnj_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="Ad26.COV2.S first-dose") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
  }  
 
   if(any(input$IRsStudyPopulationTypeSelector=="mRNA-1273 first-dose")){
   working.data[["working.data.Mod1"]] <- Mod1_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="mRNA-1273 first-dose") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
      } 

    if(any(input$IRsStudyPopulationTypeSelector=="mRNA-1273 second-dose")){
   working.data[["working.data.Mod2"]] <- Mod2_background.pop.IRS %>% 
  filter(db %in% input$IRsDatabaseSelector) %>% 
   mutate(pop="mRNA-1273 second-dose") %>% 
  filter(target.time.window%in% input$IRsTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All"|
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand") 
      } 
 
 working.data<-bind_rows(working.data)
 
  validate(
  need(!is.null(nrow(working.data)), "No results for selected inputs")
) 
  validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
  plot<-  working.data %>% 
  ggplot(aes(pop,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
             text =paste0(outcome.name,
                          "\n", pop,
                          "\nSIR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Study population")+
    ylab("Standardised IR per 100,000 PYs\n")
}

# if(input$IRsStrataSelector=="Overall (standardised to General population (index date: first visit/ contact))"){
#     # browser()
# 
#  # Background.pop
#  working.data<-list()
#    if(any(input$IRsStudyPopulationTypeSelector=="General population (index date: 1st December)")){
#  working.data[["working.data.gpop"]] <- Network.IR  %>% 
#   filter(time.window%in% input$IRsTimeWindowSelector|
#              is.na(time.window)) %>%  
#   filter(pop=="General population (index date: first visit/ contact)") %>% 
#   filter(pop.type %in% input$IRsPopulationSelector|
#              is.na(pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   filter(strata=="overall")
#    }
#   
#    if(any(input$IRsStudyPopulationTypeSelector=="COVID-19 diagnosis")){
#    working.data[["working.data.cov"]] <- COVID_diagnosis_background.pop.visit.IRS %>% 
#    mutate(pop="COVID-19 diagnosis") %>% 
#   filter(target.time.window%in% input$IRsTimeWindowSelector|
#              is.na(target.time.window)) %>%  
#   filter(target.pop.type %in% input$IRsPopulationSelector|
#              is.na(target.pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   rename("ir_100000"="ir_100000.stand") %>% 
#   rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
#   rename("ir_100000_upper"="ir_100000_upper.stand") 
#    }
# 
# if(any(input$IRsStudyPopulationTypeSelector=="ChAdOx1 first-dose")){
#  
#    working.data[["working.data.az"]] <- AZ_background.pop.visit.IRS %>% 
#    mutate(pop="ChAdOx1 first-dose") %>% 
#   filter(target.time.window%in% input$IRsTimeWindowSelector|
#              is.na(target.time.window)) %>%  
#   filter(target.pop.type %in% input$IRsPopulationSelector|
#              is.na(target.pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   rename("ir_100000"="ir_100000.stand") %>% 
#   rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
#   rename("ir_100000_upper"="ir_100000_upper.stand") 
#     }
#  
# if(any(input$IRsStudyPopulationTypeSelector=="BNT162b2 first-dose")){
#    working.data[["working.data.pf1"]] <- Pf1_background.pop.visit.IRS %>% 
#    mutate(pop="BNT162b2 first-dose") %>% 
#   filter(target.time.window%in% input$IRsTimeWindowSelector|
#              is.na(target.time.window)) %>%  
#   filter(target.pop.type %in% input$IRsPopulationSelector|
#              is.na(target.pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   rename("ir_100000"="ir_100000.stand") %>% 
#   rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
#   rename("ir_100000_upper"="ir_100000_upper.stand") 
#       }  
#  
#  if(any(input$IRsStudyPopulationTypeSelector=="BNT162b2 second-dose")){
#    working.data[["working.data.pf2"]] <- Pf2_background.pop.visit.IRS %>% 
#    mutate(pop="BNT162b2 second-dose") %>% 
#   filter(target.time.window%in% input$IRsTimeWindowSelector|
#              is.na(target.time.window)) %>%  
#   filter(target.pop.type %in% input$IRsPopulationSelector|
#              is.na(target.pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   rename("ir_100000"="ir_100000.stand") %>% 
#   rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
#   rename("ir_100000_upper"="ir_100000_upper.stand") 
#  }  
#  
#   if(any(input$IRsStudyPopulationTypeSelector=="Ad26.COV2.S first-dose")){
#    working.data[["working.data.Jnj"]] <- Jnj_background.pop.visit.IRS %>% 
#    mutate(pop="Ad26.COV2.S first-dose") %>% 
#   filter(target.time.window%in% input$IRsTimeWindowSelector|
#              is.na(target.time.window)) %>%  
#   filter(target.pop.type %in% input$IRsPopulationSelector|
#              is.na(target.pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   rename("ir_100000"="ir_100000.stand") %>% 
#   rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
#   rename("ir_100000_upper"="ir_100000_upper.stand") 
#   }  
#  
#    if(any(input$IRsStudyPopulationTypeSelector=="mRNA-1273 first-dose")){
#    working.data[["working.data.Mod1"]] <- Mod1_background.pop.visit.IRS %>% 
#    mutate(pop="mRNA-1273 first-dose") %>% 
#   filter(target.time.window%in% input$IRsTimeWindowSelector|
#              is.na(target.time.window)) %>%  
#   filter(target.pop.type %in% input$IRsPopulationSelector|
#              is.na(target.pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   rename("ir_100000"="ir_100000.stand") %>% 
#   rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
#   rename("ir_100000_upper"="ir_100000_upper.stand") 
#       } 
# 
#     if(any(input$IRsStudyPopulationTypeSelector=="mRNA-1273 second-dose")){
#    working.data[["working.data.Mod2"]] <- Mod2_background.pop.visit.IRS %>% 
#    mutate(pop="mRNA-1273 second-dose") %>% 
#   filter(target.time.window%in% input$IRsTimeWindowSelector|
#              is.na(target.time.window)) %>%  
#   filter(target.pop.type %in% input$IRsPopulationSelector|
#              is.na(target.pop.type)) %>% 
#   filter(prior.obs.required%in% input$IRsStudyPopulationSelector)%>% 
#   filter(outcome.name %in%  input$IRsOutcomeSelector) %>% 
#   rename("ir_100000"="ir_100000.stand") %>% 
#   rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
#   rename("ir_100000_upper"="ir_100000_upper.stand") 
#       } 
#  
#  working.data<-bind_rows(working.data)
#  
#   validate(
#   need(!is.null(nrow(working.data)), "No results for selected inputs")
# ) 
#   validate(
#   need(nrow(working.data)>0, "No results for selected inputs")
# )
#  
#   plot<-  working.data %>% 
#   ggplot(aes(pop,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
#              text =paste0(outcome.name,
#                           "\n", pop,
#                           "\nSIR: ", round(ir_100000,1),
#                           " (", round(ir_100000_lower,1),
#                           " to ",round(ir_100000_upper,1), ")"
#                           )))+
#     xlab("Study population")+
#     ylab("Standardised IR per 100,000 PYs\n")
# }



if(input$IRsStrataSelector=="Overall"){
   working.data<-working.data %>% 
  filter(strata %in% c("overall"))
 
  validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
  plot<-  working.data %>% 
  ggplot(aes(pop,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
             text =paste0(outcome.name,
                          "\n", pop,
                          "\nIR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Study population")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Sex"){
  # browser() 
 working.data<-working.data %>%
  filter(strata %in% c("gender"))
 plot<-  working.data %>%
  ggplot(aes(gender,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender,pop), shape=gender,
               text =paste0(outcome.name, 
                          "\n", pop,"; ", gender,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Database")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Condition of interest"){
  # browser() 
 working.data<-working.data %>%
  filter(strata %in% c("cond.comp"))
 plot<-  working.data %>%
   # mutate(cond.comp=as.character(cond.comp)) %>% 
   mutate(cond.comp=ifelse(cond.comp=="0",
                           "Without conidition\nof interest",
                           "With conidition\nof interest" )) %>% 
  ggplot(aes(cond.comp,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(cond.comp,pop), shape=cond.comp,
               text =paste0(outcome.name, 
                          "\n", pop,"; ", cond.comp,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab(" ")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Medication of interest"){
  # browser() 
 working.data<-working.data %>%
  filter(strata %in% c("drug.comp"))
 plot<-  working.data %>%
   mutate(drug.comp=ifelse(drug.comp=="0",
                           "Without medication\nof interest",
                           "With medication\nof interest" )) %>% 
  ggplot(aes(drug.comp,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(drug.comp,pop), shape=drug.comp,
               text =paste0(outcome.name, 
                          "\n", pop,"; ", drug.comp,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab(" ")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Condition or medication of interest"){
  # browser() 
 working.data<-working.data %>%
  filter(strata %in% c("cond.drug.comp"))
 plot<-  working.data %>%
   mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "Without conidition\nor medication\nof interest",
                           "With conidition\nor medication\nof interest" )) %>% 
  ggplot(aes(cond.drug.comp,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(cond.drug.comp,pop), shape=cond.drug.comp,
               text =paste0(outcome.name, 
                          "\n", pop,"; ", cond.drug.comp,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab(" ")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (<=44, 45-64, >=65) and sex"){
 working.data<-working.data %>% 
  filter(strata %in% c("age_gr2_gender"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
 plot<-  working.data %>% 
  ggplot(aes(age_gr2,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender,pop), shape=gender,linetype=gender,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr2,"; ", gender,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}


if(input$IRsStrataSelector=="Age group (<=44, 45-64, >=65)"){
 working.data<-working.data %>% 
  filter(strata %in% c("age_gr2"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
 plot<-  working.data %>% 
  ggplot(aes(age_gr2,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(age_gr2,pop), shape=pop,linetype=pop,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr2,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex"){
 working.data<-working.data %>% 
  filter(strata %in% c("age_gr_gender")) 
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
 plot<-  working.data %>% 
  ggplot(aes(age_gr,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender,pop), shape=gender,linetype=gender,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr,"; ", gender,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)"){
 working.data<-working.data %>% 
  filter(strata %in% c("age_gr"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
 plot<-  working.data %>% 
  ggplot(aes(age_gr,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=pop, shape=pop,linetype=pop,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex"){
 working.data<-working.data %>% 
  filter(strata %in% c("age_gr3_gender"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
 plot<-  working.data %>% 
  ggplot(aes(age_gr3,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender,pop), shape=gender,linetype=gender,
             text =paste0(outcome.name,
                          "\n", pop,"; ",age_gr3,"; ", gender,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
 }

if(input$IRsStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)"){
 working.data<-working.data %>% 
  filter(strata %in% c("age_gr3"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 
 plot<-  working.data %>% 
  ggplot(aes(age_gr3,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=pop, shape=pop,linetype=pop,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr3,
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}



if(input$IRsStrataSelector=="Age group (<=44, 45-64, >=65), sex, and condition of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr2_gender_cond.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(cond.comp=ifelse(cond.comp=="0",
                           "\nWithout condition\nof interest",
                           "\nWith condition\nof interest" )) %>% 
   mutate(gender_cond.comp=paste0(gender, ";", cond.comp)) %>% 
  ggplot(aes(age_gr2,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_cond.comp, ";", pop), shape=gender_cond.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr2,"; ", gender,"; ", cond.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr_gender_cond.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(cond.comp=ifelse(cond.comp=="0",
                           "\nWithout condition\nof interest",
                           "\nWith condition\nof interest" )) %>% 
   mutate(gender_cond.comp=paste0(gender, ";", cond.comp)) %>% 
  ggplot(aes(age_gr,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_cond.comp, pop), shape=gender_cond.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr,"; ", gender,"; ", cond.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr3_gender_cond.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(cond.comp=ifelse(cond.comp=="0",
                           "\nWithout condition\nof interest",
                           "\nWith condition\nof interest" )) %>% 
   mutate(gender_cond.comp=paste0(gender, ";", cond.comp)) %>% 
  ggplot(aes(age_gr3,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_cond.comp, pop), shape=gender_cond.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr3,"; ", gender,"; ", cond.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (<=44, 45-64, >=65), sex, and medication of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr2_gender_drug.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(drug.comp=ifelse(drug.comp=="0",
                           "\nWithout medication\nof interest",
                           "\nWith medication\nof interest" )) %>% 
   mutate(gender_drug.comp=paste0(gender, "; ", drug.comp)) %>% 
  ggplot(aes(age_gr2,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_drug.comp, pop), shape=gender_drug.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr2,"; ", gender,"; ", drug.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and medication of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr_gender_drug.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(drug.comp=ifelse(drug.comp=="0",
                           "\nWithout medication\nof interest",
                           "\nWith medication\nof interest" )) %>% 
   mutate(gender_drug.comp=paste0(gender, "; ", drug.comp)) %>% 
  ggplot(aes(age_gr,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_drug.comp, pop), shape=gender_drug.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr,"; ", gender,"; ", drug.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and medication of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr3_gender_drug.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(drug.comp=ifelse(drug.comp=="0",
                           "\nWithout medication\nof interest",
                           "\nWith medication\nof interest" )) %>% 
   mutate(gender_drug.comp=paste0(gender, "; ", drug.comp)) %>% 
  ggplot(aes(age_gr3,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_drug.comp, pop), shape=gender_drug.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr3,"; ", gender,"; ", drug.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (<=44, 45-64, >=65), sex, and condition or medication of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr2_gender_cond.drug.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "\nWithout conidition or\nmedication of interest",
                           "\nWith conidition or\nmedication of interest" )) %>% 
   mutate(gender_cond.drug.comp=paste0(gender, "; ", cond.drug.comp)) %>% 
  ggplot(aes(age_gr2,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_cond.drug.comp, pop), shape=gender_cond.drug.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr2,"; ", gender,"; ", cond.drug.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition or medication of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr_gender_cond.drug.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "\nWithout conidition or\nmedication of interest",
                           "\nWith conidition or\nmedication of interest" )) %>% 
   mutate(gender_cond.drug.comp=paste0(gender, "; ", cond.drug.comp)) %>% 
  ggplot(aes(age_gr,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_cond.drug.comp, pop), shape=gender_cond.drug.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr,"; ", gender,"; ", cond.drug.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}

if(input$IRsStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition or medication of interest"){
  
  working.data<-working.data %>% 
  filter(strata %in% c("age_gr3_gender_cond.drug.comp"))  
 
   validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()
 plot<-  working.data %>% 
   mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "\nWithout conidition or\nmedication of interest",
                           "\nWith conidition or\nmedication of interest" )) %>% 
   mutate(gender_cond.drug.comp=paste0(gender, "; ", cond.drug.comp)) %>% 
  ggplot(aes(age_gr3,ir_100000, ymin=ir_100000_lower,ymax=ir_100000_upper,
                 colour=pop, group=paste0(gender_cond.drug.comp, pop), shape=gender_cond.drug.comp,
             text =paste0(outcome.name, 
                          "\n", pop,"; ",age_gr3,"; ", gender,"; ", cond.drug.comp,"; ",
                          "\nIRR: ", round(ir_100000,1),
                          " (", round(ir_100000_lower,1),
                          " to ",round(ir_100000_upper,1), ")"
                          )))+
    xlab("Age group")+
    ylab("IR per 100,000 PYs\n")
}


 validate(
  need(any(!is.na(plot$data$ir_100000_upper)==TRUE)==TRUE, "No results for selected inputs")
)

max.lim<-ceiling(max(plot$data$ir_100000_upper))
# browser()
if(input$log_scale==TRUE){ 
plot<-plot+
  geom_point(position=position_dodge(width=0.5), size=1.5)+
  scale_y_continuous(label=label_comma(accuracy= 1), trans='log2',
                    limits=c(0.1,max.lim) )} 
if(input$log_scale==FALSE){ 
plot<-plot+
  geom_point(position=position_dodge(width=0.5), size=1.5)+
  scale_y_continuous(label=label_comma(accuracy= 1), 
                    limits=c(0,NA) )} 

  ggplotly(plot +
  geom_errorbar(position=position_dodge(width=0.5), width=0,)+     
  theme_bw()+
   scale_color_manual(values=c("ChAdOx1 first-dose" = "#e41a1c",
                               "ChAdOx1 first-dose (no prior covid)" = "#e41a1c",
                                 "BNT162b2 first-dose" = "blue",
                                 "BNT162b2 first-dose (no prior covid)" = "blue",
                                 "BNT162b2 second-dose" = "#377eb8",
                                 "BNT162b2 second-dose (no prior covid)" = "#377eb8",
                               "Ad26.COV2.S first-dose" = "black",
                               "Ad26.COV2.S first-dose (no prior covid)"   = "black",
                               "mRNA-1273 first-dose" = "grey",
                               "mRNA-1273 first-dose (no prior covid)" = "grey",
                               "mRNA-1273 second-dose" = "grey",
                               "mRNA-1273 second-dose (no prior covid)" = "grey",
                               "General population (index date: 1st December)" = "#252525",
                               "COVID-19 positive test (21 days)"="#ff7f00",
                               "COVID-19 positive test (90 days)"="orange"))+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12)) , 
  tooltip = "text")%>%
  config(displaylogo = FALSE)%>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                    "pan2d", "zoom2d",
                                    "select2d",
                                    "lasso2d",
                                    "drawclosedpath",
                                    "drawopenpath",
                                    "drawrect",
                                    "drawcircle",
                                    "eraseshape",
                                    "autoScale2d",
                                    "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "hoverClosestGeo",
                                    "hoverClosestGl2d",
                                    "toggleSpikelines")) 
  # %>% 
  #   layout(legend = list(orientation = "h", y=-2, itemsizing='constant'))

    })

get.tbl.IRR<-reactive({
  # browser()
  table.data<-bind_rows(
    COVID_diagnosis_21d_background.pop.IRR,
    COVID_diagnosis_90d_background.pop.IRR ,
    Pf1_background.pop.IRR, 
    Pf1.no.covid_background.pop.IRR, 
    Pf2_background.pop.IRR, 
    Pf2.no.covid_background.pop.IRR,
    AZ1_background.pop.IRR,
    AZ1.no.covid_background.pop.IRR,
    AZ2_background.pop.IRR,
    AZ2.no.covid_background.pop.IRR,
    Jnj_background.pop.IRR,
    Jnj.no.covid_background.pop.IRR,
    Mod1_background.pop.IRR, 
    Mod1.no.covid_background.pop.IRR, 
    Mod2_background.pop.IRR
              ) %>% 
    filter(target.pop %in% input$IRRTargetStudyPopulationTypeSelector) %>% 
    filter(comparator %in% input$IRRComparatorStudyPopulationTypeSelector)
  
   # browser() 
  table.data<-table.data %>% 
  filter(target.time.window%in% input$IRRTimeWindowSelector) %>%
  filter(target.pop.type %in% "All") %>%
  filter(prior.obs.required%in% input$IRRStudyPopulationSelector)%>%
  filter(outcome.name %in%  input$IRROutcomeSelector) 

  
  table.data<-table.data %>% 
    mutate(target.ir=
           ifelse(!is.na(ir_100000_target),
           paste0(nice.num.count((ir_100000_target)),
                    " (",
                    nice.num.count((ir_100000_lower_target)),
                    " to ",
                    nice.num.count((ir_100000_upper_target)), ")"
                    ), NA)) %>% 
    mutate(comparator.ir=
           ifelse(!is.na(ir_100000_comparator),
           paste0(nice.num.count((ir_100000_comparator)),
                    " (",
                    nice.num.count((ir_100000_lower_comparator)),
                    " to ",
                    nice.num.count((ir_100000_upper_comparator)), ")"
                    ), NA)) %>% 
    mutate(rrr=
           ifelse(!is.na(ir_100000_comparator),
           paste0(nice.num2((rrr.est)),
                    " (",
                    nice.num2((rrr.lower)),
                    " to ",
                    nice.num2((rrr.upper)), ")"
                    ), NA)) %>% 
    mutate(n_target=nice.num.count(n_target)) %>% 
    mutate(years_target=nice.num.count(years_target)) %>% 
    mutate(events_target=nice.num.count(events_target)) %>% 
    mutate(n_comparator=nice.num.count(n_comparator)) %>%  
    mutate(years_comparator=nice.num.count(years_comparator)) %>%  
    mutate(events_comparator=nice.num.count(events_comparator))
   
    
    if(input$IRRStrataSelector=="overall" ){
    table.data<- table.data %>% 
        filter(strata=="overall") 
    }
  
      if(input$IRRStrataSelector=="Sex" ){
         table.data<- table.data %>% 
        filter(strata=="gender") 
      }
  
    
      if(input$IRRStrataSelector=="Condition of interest" ){
         table.data<- table.data %>% 
        filter(strata=="cond.comp") 
      }
    
      if(input$IRRStrataSelector=="Medication of interest" ){
         table.data<- table.data %>% 
        filter(strata=="drug.comp") 
      }
      
      if(input$IRRStrataSelector=="Condition or medication of interest" ){
         table.data<- table.data %>% 
        filter(strata=="cond.drug.comp") 
      }
  
      if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr") 
      }
  
        if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr_gender") 
        }
  
              if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr_gender_cond.comp") 
      }
  
                  if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr_gender_drug.comp") 
                }
  
                  if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition or medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr_gender_cond.drug.comp") 
      }

  
        if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65)" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr2") 
        }
  
          if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65) and sex" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr2_gender") 
      }
  
            if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and condition of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr2_gender_cond.comp") 
            }
  
              if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr2_gender_drug.comp") 
              }
  
              if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and condition or medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr2_gender_cond.drug.comp") 
            }
  
              if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr2_gender_drug.comp") 
              }  
  
      if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and condition or medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr2_gender_cond.drug.comp") 
      }
  
      if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)" ){
 # browser()
        table.data<-   table.data %>% 
        filter(strata=="age_gr3") 
      }
  
        if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex" ){
 # browser()
        table.data<-   table.data %>% 
        filter(strata=="age_gr3_gender") 
        }
  
              if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr3_gender_cond.comp") 
              }
  
                if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr3_gender_drug.comp") 
                }
  
                  if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition or medication of interest" ){
   table.data<-   table.data %>% 
        filter(strata=="age_gr3_gender_cond.drug.comp") 
      }
  
table.data
  })

output$IRR.plot.overall<- renderPlotly({
 

 
  plot.data<-get.tbl.IRR() %>% 
  filter(db%in% input$IRRDatabaseSelector) %>% 
    mutate(target.pop=ifelse(target.pop=="COVID19 positive test 21d",
                             "COVID-19 positive test (21 days)",target.pop )) %>% 
    mutate(target.pop=ifelse(target.pop=="COVID19 positive test 90d",
                             "COVID-19 positive test (90 days)", target.pop))
  

 
  if(input$IRRStrataSelector=="Overall (standardised)"){
      # browser()
    
      working.data<-list()
  
      
if(input$IRRComparatorStudyPopulationTypeSelector=="General population (index date: 1st December)"){  
   if(any(input$IRRTargetStudyPopulationTypeSelector=="COVID19 positive test 21d")){
   working.data[["working.data.cov.21"]] <-COVID_diagnosis_21d_background.pop.IRS%>%
   mutate(pop="COVID-19 positive test (21 days)")
   }
   if(any(input$IRRTargetStudyPopulationTypeSelector=="COVID19 positive test 90d")){
   working.data[["working.data.cov.90"]] <-COVID_diagnosis_90d_background.pop.IRS%>%
   mutate(pop="COVID-19 positive test (90 days)")
   }
    if(any(input$IRRTargetStudyPopulationTypeSelector=="ChAdOx1 first-dose")){
   working.data[["working.data.az"]] <-AZ1_background.pop.IRS %>%
   mutate(pop="ChAdOx1 first-dose")
    }
  
      if(any(input$IRRTargetStudyPopulationTypeSelector=="ChAdOx1 first-dose (no prior covid)")){
   working.data[["working.data.az.no.covid"]] <-AZ1.no.covid_background.pop.IRS %>%
   mutate(pop="ChAdOx1 first-dose (no prior covid)")
      }
  
      if(any(input$IRRTargetStudyPopulationTypeSelector=="ChAdOx1 second-dose")){
   working.data[["working.data.az2"]] <-AZ2_background.pop.IRS %>%
   mutate(pop="ChAdOx1 second-dose")
    }
  
      if(any(input$IRRTargetStudyPopulationTypeSelector=="ChAdOx1 second-dose (no prior covid)")){
   working.data[["working.data.az2.no.covid"]] <-AZ2.no.covid_background.pop.IRS %>%
   mutate(pop="ChAdOx1 second-dose (no prior covid)")
    }
    
         if(any(input$IRRTargetStudyPopulationTypeSelector=="BNT162b2 first-dose")){
   working.data[["working.data.pf1"]] <-Pf1_background.pop.IRS %>% 
   mutate(pop="BNT162b2 first-dose")
         }
  
           if(any(input$IRRTargetStudyPopulationTypeSelector=="BNT162b2 first-dose (no prior covid)")){
   working.data[["working.data.pf1.no.covid"]] <-Pf1.no.covid_background.pop.IRS %>% 
   mutate(pop="BNT162b2 first-dose (no prior covid)")
         }
      
               if(any(input$IRRTargetStudyPopulationTypeSelector=="BNT162b2 second-dose")){
   working.data[["working.data.pf2"]] <-Pf2_background.pop.IRS %>% 
   mutate(pop="BNT162b2 second-dose")
               }
  
            if(any(input$IRRTargetStudyPopulationTypeSelector=="BNT162b2 second-dose (no prior covid)")){
   working.data[["working.data.pf2.no.covid"]] <-Pf2.no.covid_background.pop.IRS %>% 
   mutate(pop="BNT162b2 second-dose (no prior covid)")
               }
      
               if(any(input$IRRTargetStudyPopulationTypeSelector=="mRNA-1273 first-dose")){
   working.data[["working.data.Mod1"]] <-Mod1_background.pop.IRS %>% 
   mutate(pop="mRNA-1273 first-dose")
               }
                 if(any(input$IRRTargetStudyPopulationTypeSelector=="mRNA-1273 first-dose (no prior covid)")){
   working.data[["working.data.Mod1.no.covid"]] <-Mod1_background.pop.IRS %>% 
   mutate(pop="mRNA-1273 first-dose (no prior covid)")
         }
      
               if(any(input$IRRTargetStudyPopulationTypeSelector=="mRNA-1273 second-dose")){
   working.data[["working.data.Mod2"]] <-Mod2_background.pop.IRS %>% 
   mutate(pop="mRNA-1273 second-dose")
               }
      
                     if(any(input$IRRTargetStudyPopulationTypeSelector=="Ad26.COV2.S first-dose")){
   working.data[["working.data.jnj"]] <-Jnj_background.pop.IRS %>% 
   mutate(pop="Ad26.COV2.S first-dose")
         }
                       if(any(input$IRRTargetStudyPopulationTypeSelector=="Ad26.COV2.S first-dose (no prior covid)")){
   working.data[["working.data.jnj.no.covid"]] <-Jnj_background.pop.IRS %>% 
   mutate(pop="Ad26.COV2.S first-dose (no prior covid)")
         }   
      
    }   
 
 plot.data<-bind_rows(working.data)%>% 
  filter(db%in% input$IRRDatabaseSelector) %>% 
  filter(target.time.window%in% input$IRRTimeWindowSelector) %>%
  filter(target.pop.type %in% "All") %>%
  filter(prior.obs.required%in% input$IRRStudyPopulationSelector)%>%
  filter(outcome.name %in%  input$IRROutcomeSelector) 
   
 # browser()
     plot <-  plot.data %>% 
  ggplot(aes(group=paste0(pop,db), 
                 colour=pop,shape=db,
             text =paste0(outcome.name,
                          "\n", pop,
                          "\nSIR: ", nice.num2(isr.sir),
                          " (", nice.num2(isr.sir_lower),
                          " to ",nice.num2(isr.sir_upper), ")"
                          )))+
  geom_point(aes(x=outcome.name,y=isr.sir ),
             position = position_dodge(0.5))+
   geom_linerange(aes(x=outcome.name,ymin= isr.sir_lower, ymax=isr.sir_upper),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() +
    ylab("Standardised incidence ratio")
    
    
  }
 

  

    validate(need(nrow(plot.data)>=1, 
                "No results for selected inputs"))
    
    
  if(input$IRRStrataSelector=="overall" ){
 plot <-  plot.data%>% 
  ggplot(aes(group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  # facet_grid(age_gr~ gender)+
  geom_point(aes(x=outcome.name,y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(x=outcome.name,ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip()+
    ylab("Incidence rate ratio")}
  
    if(input$IRRStrataSelector=="Sex" ){
         order<-data.frame(gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$gender)) %>% 
      arrange(gender) %>% 
      select(position) %>% 
      distinct()
         
 plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",gender),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  # facet_grid(age_gr~ gender)+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
 }

     if(input$IRRStrataSelector=="Condition of interest" ){
      # browser()
     
  plot.data<-plot.data %>% 
         mutate(cond.comp=ifelse(cond.comp=="0",
                           "Without conidition\nof interest",
                           "With conidition\nof interest" ))
       
       order<-data.frame(cond.comp=plot.data$cond.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$cond.comp)) %>% 
      arrange(cond.comp) %>% 
      select(position) %>% 
      distinct()
         
 plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",cond.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  # facet_grid(age_gr~ gender)+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
 }

   if(input$IRRStrataSelector=="Medication of interest" ){
      # browser()
     
  plot.data<-plot.data %>% 
         mutate(drug.comp=ifelse(drug.comp=="0",
                           "Without medication\nof interest",
                           "With medication\nof interest" ))
       
       order<-data.frame(drug.comp=plot.data$drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$drug.comp)) %>% 
      arrange(drug.comp) %>% 
      select(position) %>% 
      distinct()
         
 plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  # facet_grid(age_gr~ gender)+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
 }

 if(input$IRRStrataSelector=="Condition or medication of interest" ){
      # browser()
     
  plot.data<-plot.data %>% 
         mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "Without conidition\nor medication\nof interest",
                           "With conidition\nor medication\nof interest" ))
       
       order<-data.frame(cond.drug.comp=plot.data$cond.drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$cond.drug.comp)) %>% 
      arrange(cond.drug.comp) %>% 
      select(position) %>% 
      distinct()
         
 plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",cond.drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  # facet_grid(age_gr~ gender)+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
 }
    
  if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)" ){
    order<-data.frame(age=plot.data$age_gr,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr)) %>% 
      arrange(age) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
     }
  
   if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65)" ){
  # browser()
       order<-data.frame(age=plot.data$age_gr2,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr2)) %>% 
      arrange(age) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr2),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
   }

    if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)" ){
       order<-data.frame(age=plot.data$age_gr3,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr3)) %>% 
      arrange(age) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr3),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
     }
 
 if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex" ){
 # browser()
       order<-data.frame(age=plot.data$age_gr,
                         gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr,
                                      "; ",plot.data$gender)) %>% 
      arrange(age, gender) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr,"; ",gender),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
     }

 if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65) and sex" ){
 # browser()
       order<-data.frame(age=plot.data$age_gr2,
                         gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr2,
                                      "; ",plot.data$gender)) %>% 
      arrange(age, gender) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr2,"; ",gender),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }

  if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex" ){
 # browser()
       order<-data.frame(age=plot.data$age_gr3,
                         gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr3,
                                      "; ",plot.data$gender)) %>% 
      arrange(age, gender) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr3,"; ",gender),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
   scale_color_manual(values=c("ChAdOx1 first-dose" = "#e41a1c",
                               "ChAdOx1 first-dose (no prior covid)" = "#e41a1c",
                                 "BNT162b2 first-dose" = "blue",
                                 "BNT162b2 first-dose (no prior covid)" = "blue",
                                 "BNT162b2 second-dose" = "#377eb8",
                                 "BNT162b2 second-dose (no prior covid)" = "#377eb8",
                               "General population (index date: 1st December)" = "#252525",
                               "COVID-19 positive test (90 days)"="#ff7f00"))+
    ylab("Incidence rate ratio")
     
     
     }

        
    if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and condition of interest" ){
    plot.data<-plot.data %>% 
         mutate(cond.comp=ifelse(cond.comp=="0",
                           "Without condition of interest",
                           "With condition of interest" ))
      
      
 order<-data.frame(age=plot.data$age_gr2,
                         gender=plot.data$gender,
                   cond.comp=plot.data$cond.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr2,
                                      "; ",plot.data$gender, "; ",plot.data$cond.comp)) %>% 
      arrange(age, gender,cond.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr2,"; ",gender, "; ",plot.data$cond.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }
        
    if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition of interest" ){
   plot.data<-plot.data %>% 
         mutate(cond.comp=ifelse(cond.comp=="0",
                           "Without condition of interest",
                           "With condition of interest" ))
 order<-data.frame(age=plot.data$age_gr,
                         gender=plot.data$gender,
                   cond.comp=plot.data$cond.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr,
                                      "; ",plot.data$gender, "; ",plot.data$cond.comp)) %>% 
      arrange(age, gender,cond.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr,"; ",gender, "; ",plot.data$cond.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }
        
    if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition of interest" ){
       plot.data<-plot.data %>% 
         mutate(cond.comp=ifelse(cond.comp=="0",
                           "Without condition of interest",
                           "With condition of interest" ))
       
 order<-data.frame(age=plot.data$age_gr3,
                         gender=plot.data$gender,
                   cond.comp=plot.data$cond.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr3,
                                      "; ",plot.data$gender, "; ",plot.data$cond.comp)) %>% 
      arrange(age, gender,cond.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr3,"; ",gender, "; ",plot.data$cond.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }

     if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and medication of interest" ){
        plot.data<-plot.data %>% 
         mutate(drug.comp=ifelse(drug.comp=="0",
                           "Without medication of interest",
                           "With medication of interest" ))
        
 order<-data.frame(age=plot.data$age_gr2,
                         gender=plot.data$gender,
                   drug.comp=plot.data$drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr2,
                                      "; ",plot.data$gender, "; ",plot.data$drug.comp)) %>% 
      arrange(age, gender,drug.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr2,"; ",gender, "; ",plot.data$drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }
        
    if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and medication of interest" ){
        plot.data<-plot.data %>% 
         mutate(drug.comp=ifelse(drug.comp=="0",
                           "Without medication of interest",
                           "With medication of interest" ))
        
 order<-data.frame(age=plot.data$age_gr,
                         gender=plot.data$gender,
                   drug.comp=plot.data$drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr,
                                      "; ",plot.data$gender, "; ",plot.data$drug.comp)) %>% 
      arrange(age, gender,drug.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr,"; ",gender, "; ",plot.data$drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }
        
    if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and medication of interest" ){
     plot.data<-plot.data %>% 
         mutate(drug.comp=ifelse(drug.comp=="0",
                           "Without medication of interest",
                           "With medication of interest" ))
     
 order<-data.frame(age=plot.data$age_gr3,
                         gender=plot.data$gender,
                   drug.comp=plot.data$drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr3,
                                      "; ",plot.data$gender, "; ",plot.data$drug.comp)) %>% 
      arrange(age, gender,drug.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr3,"; ",gender, "; ",plot.data$drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }

    if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65), sex, and condition or medication of interest" ){
     plot.data<-plot.data %>% 
         mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "Without conidition or medication of interest",
                           "With conidition or medication of interest" ))
     
 order<-data.frame(age=plot.data$age_gr2,
                         gender=plot.data$gender,
                   cond.drug.comp=plot.data$cond.drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr2,
                                      "; ",plot.data$gender, "; ",plot.data$cond.drug.comp)) %>% 
      arrange(age, gender,cond.drug.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr2,"; ",gender, "; ",plot.data$cond.drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }
        
    if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85), sex, and condition or medication of interest" ){
   # browser()   
      plot.data<-plot.data %>% 
         mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "Without conidition or medication of interest",
                           "With conidition or medication of interest" ))  
 order<-data.frame(age=plot.data$age_gr,
                         gender=plot.data$gender,
                   cond.drug.comp=plot.data$cond.drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr,
                                      "; ",plot.data$gender, "; ",plot.data$cond.drug.comp)) %>% 
      arrange(age, gender,cond.drug.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr,"; ",gender, "; ",plot.data$cond.drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }
        
    if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80), sex, and condition or medication of interest" ){
     plot.data<-plot.data %>% 
         mutate(cond.drug.comp=ifelse(cond.drug.comp=="0",
                           "Without conidition or medication of interest",
                           "With conidition or medication of interest" ))
     
 order<-data.frame(age=plot.data$age_gr3,
                         gender=plot.data$gender,
                   cond.drug.comp=plot.data$cond.drug.comp,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr3,
                                      "; ",plot.data$gender, "; ",plot.data$cond.drug.comp)) %>% 
      arrange(age, gender,cond.drug.comp) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr3,"; ",gender, "; ",plot.data$cond.drug.comp),
            group=paste0(target.pop,db), shape=db,
                 colour=target.pop,
             text =paste0(outcome.name,
                           "\n", target.pop,
                          "\nIRR: ", round(rrr.est,1),
                          " (", round(rrr.lower,1),
                          " to ",round(rrr.upper,1), ")"
                          )))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))+
    ylab("Incidence rate ratio")
     
     
      }

   
  
#   max.lim<-ceiling(max(plot$data$rrr.upper))
# if(input$IRR_log_scale==TRUE){ 
# plot<-plot+
#   scale_y_continuous(label=label_comma(accuracy= 1), trans='log2',
#                     limits=c(0.1,max.lim) )
# } 
# if(input$IRR_log_scale==FALSE){ 
# plot<-plot+
#   scale_y_continuous(label=label_comma(accuracy= 1), 
#                     limits=c(0,NA) )
# } 

  
  
  
  ggplotly(
    plot+     
  theme_bw()+
   scale_color_manual(values=c("ChAdOx1 first-dose" = "#e41a1c",
                               "ChAdOx1 first-dose (no prior covid)" = "#e41a1c",
                                 "BNT162b2 first-dose" = "blue",
                                 "BNT162b2 first-dose (no prior covid)" = "blue",
                                 "BNT162b2 second-dose" = "#377eb8",
                                 "BNT162b2 second-dose (no prior covid)" = "#377eb8",
                               "Ad26.COV2.S first-dose" = "black",
                               "Ad26.COV2.S first-dose (no prior covid)"   = "black",
                               "mRNA-1273 first-dose" = "grey",
                               "mRNA-1273 first-dose (no prior covid)" = "grey",
                               "mRNA-1273 second-dose" = "grey",
                               "mRNA-1273 second-dose (no prior covid)" = "grey",
                               "General population (index date: 1st December)" = "#252525",
                               "COVID-19 positive test (21 days)"="#ff7f00",
                               "COVID-19 positive test (90 days)"="orange"))+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title.y=element_text(size=12,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12))+
    xlab(" ")  , 
  tooltip = "text")  %>%
  config(displaylogo = FALSE)%>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                    "pan2d", "zoom2d",
                                    "select2d",
                                    "lasso2d",
                                    "drawclosedpath",
                                    "drawopenpath",
                                    "drawrect",
                                    "drawcircle",
                                    "eraseshape",
                                    "autoScale2d",
                                    "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "hoverClosestGeo",
                                    "hoverClosestGl2d",
                                    "toggleSpikelines"))
 
  })

get.tbl.IRS<-reactive({
# browser()
  # table.data<-bind_rows(
  #   COVID_diagnosis_background.pop.IRS,
  #   COVID_diagnosis_background.pop.visit.IRS ,
  #              Pf1_background.pop.IRS, 
  #              Pf_background.pop.visit.IRS,
  #              AZ_background.pop.IRS,
  #              AZ_background.pop.visit.IRS
  #             ) 
  
  # Background.pop
 working.data<-list()

  if(any(input$IRSTargetStudyPopulationTypeSelector=="COVID-19 diagnosis")){
   working.data[["working.data.cov"]] <- COVID_diagnosis_background.pop.IRS %>% 
   mutate(pop="COVID-19 diagnosis") %>% 
  filter(target.time.window%in% input$IRSTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All" |
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRSStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRSOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand")  }
 
if(any(input$IRSTargetStudyPopulationTypeSelector=="ChAdOx1 first-dose")){

   working.data[["working.data.az"]] <- AZ1_background.pop.IRS %>%
   mutate(pop="ChAdOx1 first-dose") %>%
  filter(target.time.window%in% input$IRSTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All" |
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRSStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRSOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand")  
    }

      if(any(input$IRSTargetStudyPopulationTypeSelector=="BNT162b2 first-dose")){
          working.data[["working.data.pf"]] <- Pf1_background.pop.IRS %>%
   mutate(pop="BNT162b2 first-dose") %>%
  filter(target.time.window%in% input$IRSTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All" |
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRSStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRSOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand")  
      }
 
       if(any(input$IRSTargetStudyPopulationTypeSelector=="BNT162b2 second-dose")){
      #   browser()
   working.data[["working.data.pf2"]] <- Pf2_background.pop.IRS %>%
   mutate(pop="BNT162b2 second-dose") %>%
  filter(target.time.window%in% input$IRSTimeWindowSelector|
             is.na(target.time.window)) %>%  
  filter(target.pop.type %in% "All" |
             is.na(target.pop.type)) %>% 
  filter(prior.obs.required%in% input$IRSStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IRSOutcomeSelector) %>% 
  rename("ir_100000"="ir_100000.stand") %>% 
  rename("ir_100000_lower"="ir_100000_lower.stand") %>% 
  rename("ir_100000_upper"="ir_100000_upper.stand")  
      }
 
 working.data<-bind_rows(working.data)
 table.data<-working.data
    
  
  # 
  #   browser() 
  # table.data<-table.data %>% 
  # filter(target.time.window%in% input$IRRTimeWindowSelector) %>%
  # filter(target.pop.type %in% input$IRRPopulationSelector) %>%
  # filter(prior.obs.required%in% input$IRRStudyPopulationSelector)%>%
  # filter(outcome.name %in%  input$IRROutcomeSelector) 

 #  
 #  table.data<-table.data$ %>% 
 #    mutate(target.ir=
 #           ifelse(!is.na(ir_100000_target),
 #           paste0(nice.num.count((ir_100000_target)*100),
 #                    " (",
 #                    nice.num.count((ir_100000_lower_target)),
 #                    " to ",
 #                    nice.num.count((ir_100000_upper_target)), ")"
 #                    ), NA)) %>% 
 #    mutate(comparator.ir=
 #           ifelse(!is.na(ir_100000_comparator),
 #           paste0(nice.num.count((ir_100000_comparator)*100),
 #                    " (",
 #                    nice.num.count((ir_100000_lower_comparator)),
 #                    " to ",
 #                    nice.num.count((ir_100000_upper_comparator)), ")"
 #                    ), NA)) %>% 
 #    mutate(rrr=
 #           ifelse(!is.na(ir_100000_comparator),
 #           paste0(nice.num2((rrr.est)),
 #                    " (",
 #                    nice.num2((rrr.lower)),
 #                    " to ",
 #                    nice.num2((rrr.upper)), ")"
 #                    ), NA)) %>% 
 #    mutate(n_target=nice.num.count(n_target)) %>% 
 #    mutate(years_target=nice.num.count(years_target)) %>% 
 #    mutate(events_target=nice.num.count(events_target)) %>% 
 #    mutate(n_comparator=nice.num.count(n_comparator)) %>%  
 #    mutate(years_comparator=nice.num.count(years_comparator)) %>%  
 #    mutate(events_comparator=nice.num.count(events_comparator))
 #   
 #    
 #    if(input$IRRStrataSelector=="overall" ){
 #    table.data<- table.data %>% 
 #        filter(strata=="overall") 
 #    }
 #  
 #      if(input$IRRStrataSelector=="Sex" ){
 #         table.data<- table.data %>% 
 #        filter(strata=="gender") 
 #    }
 #  
 #      if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)" ){
 #   table.data<-   table.data %>% 
 #        filter(strata=="age_gr") 
 #      }
 #  
 #        if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex" ){
 #   table.data<-   table.data %>% 
 #        filter(strata=="age_gr_gender") 
 #      }
 #  
 #        if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65)" ){
 #   table.data<-   table.data %>% 
 #        filter(strata=="age_gr2") 
 #        }
 #  
 #          if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65) and sex" ){
 #   table.data<-   table.data %>% 
 #        filter(strata=="age_gr2_gender") 
 #      }
 #  
 #      if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)" ){
 # # browser()
 #        table.data<-   table.data %>% 
 #        filter(strata=="age_gr3") 
 #      }
 #  
 #        if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex" ){
 # # browser()
 #        table.data<-   table.data %>% 
 #        filter(strata=="age_gr3_gender") 
 #      }
  
table.data
  })

output$IRS.plot.overall<- renderPlotly({
 

  
  plot.data<-get.tbl.IRS()
  
   # browser()
  validate(need(nrow(plot.data)>=1, 
                "No results for selected inputs"))
 
  if(input$IRRStrataSelector=="overall" ){
 plot <-  plot.data %>% 
  ggplot(aes(group=pop, 
                 colour=pop))+
  # facet_grid(age_gr~ gender)+
  geom_point(aes(x=outcome.name,y=isr.sir ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(x=outcome.name,ymin= isr.sir_upper, ymax=isr.sir_lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip()}
  
    if(input$IRRStrataSelector=="Sex" ){
         order<-data.frame(gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$gender)) %>% 
      arrange(gender) %>% 
      select(position) %>% 
      distinct()
         
 plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",gender),
             group=target.pop, 
                 colour=target.pop))+
  # facet_grid(age_gr~ gender)+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))
 }

 
  if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)" ){
    order<-data.frame(age=plot.data$age_gr,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr)) %>% 
      arrange(age) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr),
             group=target.pop, 
                 colour=target.pop))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))
     
     
     }
  
   if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65)" ){
  # browser()
       order<-data.frame(age=plot.data$age_gr2,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr2)) %>% 
      arrange(age) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr2),
             group=target.pop, 
                 colour=target.pop))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))
     
     
   }

    if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)" ){
       order<-data.frame(age=plot.data$age_gr3,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr3)) %>% 
      arrange(age) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr3),
             group=target.pop, 
                 colour=target.pop))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))
     
     
     }
 
        if(input$IRRStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex" ){
 # browser()
       order<-data.frame(age=plot.data$age_gr,
                         gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr,
                                      "; ",plot.data$gender)) %>% 
      arrange(age, gender) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr,"; ",gender),
             group=target.pop, 
                 colour=target.pop))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))
     
     
     }

      if(input$IRRStrataSelector=="Age group (<=44, 45-64, >=65) and sex" ){
 # browser()
       order<-data.frame(age=plot.data$age_gr2,
                         gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr2,
                                      "; ",plot.data$gender)) %>% 
      arrange(age, gender) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr2,"; ",gender),
             group=target.pop, 
                 colour=target.pop))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))
     
     
     }

       if(input$IRRStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex" ){
 # browser()
       order<-data.frame(age=plot.data$age_gr3,
                         gender=plot.data$gender,
                      position=paste0(plot.data$outcome.name,"; ",plot.data$age_gr3,
                                      "; ",plot.data$gender)) %>% 
      arrange(age, gender) %>% 
      select(position) %>% 
      distinct()

     plot <-  plot.data%>% 
  ggplot(aes(x=paste0(outcome.name,"; ",age_gr3,"; ",gender),
             group=target.pop, 
                 colour=target.pop))+
  geom_point(aes(y=rrr.est ),
             position = position_dodge(0.5))+
  # geom_errorbar(aes(x=outcome.name, ymin= rrr.upper, ymax=rrr.lower), 
  #               width=0.03,size=.01,
  #            position = position_dodge(0.5))+
   geom_linerange(aes(ymin= rrr.upper, ymax=rrr.lower),
             position = position_dodge(0.5))+
  ylim(c(0, NA))+ 
  geom_hline(yintercept = 1
             )+
   coord_flip() + 
       scale_x_discrete(limits = rev(order$position))
     
     
     }

  
#   max.lim<-ceiling(max(plot$data$rrr.upper))
# if(input$IRR_log_scale==TRUE){ 
# plot<-plot+
#   scale_y_continuous(label=label_comma(accuracy= 1), trans='log2',
#                     limits=c(0.1,max.lim) )
# } 
# if(input$IRR_log_scale==FALSE){ 
# plot<-plot+
#   scale_y_continuous(label=label_comma(accuracy= 1), 
#                     limits=c(0,NA) )
# } 

  
  
  
  ggplotly(
    plot+     
  theme_bw()+
   scale_color_manual(values=c("ChAdOx1 first-dose" = "#e41a1c",
                               "ChAdOx1 first-dose (no prior covid)" = "#e41a1c",
                                 "BNT162b2 first-dose" = "blue",
                                 "BNT162b2 first-dose (no prior covid)" = "blue",
                                 "BNT162b2 second-dose" = "#377eb8",
                                 "BNT162b2 second-dose (no prior covid)" = "#377eb8",
                               "Ad26.COV2.S first-dose" = "black",
                               "Ad26.COV2.S first-dose (no prior covid)"   = "black",
                               "mRNA-1273 first-dose" = "grey",
                               "mRNA-1273 first-dose (no prior covid)" = "grey",
                               "mRNA-1273 second-dose" = "grey",
                               "mRNA-1273 second-dose (no prior covid)" = "grey",
                               "General population (index date: 1st December)" = "#252525",
                               "COVID-19 positive test (21 days)"="#ff7f00",
                               "COVID-19 positive test (90 days)"="orange"))+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title.y=element_text(size=12,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12))+
    xlab(" ") +
    ylab("Incidence rate ratio") , 
  tooltip = "text")  %>%
  config(displaylogo = FALSE)%>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                    "pan2d", "zoom2d",
                                    "select2d",
                                    "lasso2d",
                                    "drawclosedpath",
                                    "drawopenpath",
                                    "drawrect",
                                    "drawcircle",
                                    "eraseshape",
                                    "autoScale2d",
                                    "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "hoverClosestGeo",
                                    "hoverClosestGl2d",
                                    "toggleSpikelines"))
 
  })

  output$tbl.IRR<-  renderDataTable({
  # validate(need(length(input$CohortProfileDatabaseSelector)>0, 
  #               "No results for selected inputs"))
  # browser()
    table<-get.tbl.IRR()

   datatable(table
    ,rownames= FALSE,
    colnames = c('Variable' = 1),
     extensions = 'Buttons',
    # options = list(lengthChange = FALSE,
    #                pageLength = 40,
    #                buttons = c('copy', 'csv', 'excel')))
    options = list(lengthChange = FALSE,
                    pageLength = 40,
                                dom = 'tB',
                               buttons = list(list(extend = "excel",
                                             text = "Download table as excel",
                                             filename = "PatientProfiles.csv"))
                            ))
    } )

  
get.tbl.sccs.mddr<-reactive({ 
    
    # browser()
  table<-MDRR %>%
      filter(db %in% input$SCCSDatabaseSelector ) %>% 
      filter(exposureName %in% input$SccsExposureNameSelector) %>% 
      filter(outcomeName %in% input$SCCSOutcomeSelector) %>% 
      filter(analysisName%in%input$SCCSAnalysisSelector)  %>% 
      filter(history.req%in%input$SCCSHistory.req)  %>% 
      select(db, exposureName, outcomeName, 
            timeTotal, propTimeExposed,
            events,mdrr) %>% 
    mutate(timeTotal=nice.num.count(timeTotal)) %>% 
    mutate(events=nice.num.count(events))
  })

output$tbl.sccs.mddr <-  renderDataTable({
  # # validate(need(length(input$CohortProfileDatabaseSelector)>0, 
  # #               "No results for selected inputs"))
  # # browser()
     table<-get.tbl.sccs.mddr()
  # 
   datatable(table
    ,rownames= FALSE,
    colnames = c('Variable' = 1),
     extensions = 'Buttons',
    # options = list(lengthChange = FALSE,
    #                pageLength = 40,
    #                buttons = c('copy', 'csv', 'excel')))
    options = list(lengthChange = FALSE,
                    pageLength = 40,
                                dom = 'tB',
                               buttons = list(list(extend = "excel",
                                             text = "Download table as excel",
                                             filename = "PatientProfiles.csv"))
                            ))
     } )


get.tbl.sccs.results<-reactive({ 
    
     # browser()
  table<-sccsSummary.tidy %>%
      filter(db %in% input$SCCSDatabaseSelector ) %>% 
      filter(exposureName %in% input$SccsExposureNameSelector) %>% 
      filter(outcomeName %in% input$SCCSOutcomeSelector) %>% 
      filter(analysisName%in%input$SCCSAnalysisSelector)  %>% 
      filter(history.req%in%input$SCCSHistory.req)  %>% 
    mutate(rr_est=paste0(nice.num2(rr), 
                         " (", nice.num2(ci95lb), 
                         " to ",nice.num2(ci95ub),  ")")) %>% 
    select(-c(rr,ci95lb, ci95ub))
  # %>% 
  #     select(db, exposureName, outcomeName, 
  #           timeTotal, propTimeExposed,
  #           events,mdrr) %>% 
  #   mutate(timeTotal=nice.num.count(timeTotal)) %>% 
  #   mutate(events=nice.num.count(events))
  })

output$tbl.sccs.results <-  renderDataTable({
  # # validate(need(length(input$CohortProfileDatabaseSelector)>0, 
  # #               "No results for selected inputs"))
  # # browser()
     table<-get.tbl.sccs.results()
  # 
   datatable(table
    ,rownames= FALSE,
    colnames = c('Variable' = 1),
     extensions = 'Buttons',
    # options = list(lengthChange = FALSE,
    #                pageLength = 40,
    #                buttons = c('copy', 'csv', 'excel')))
    options = list(lengthChange = FALSE,
                    pageLength = 40,
                                dom = 'tB',
                               buttons = list(list(extend = "excel",
                                             text = "Download table as excel",
                                             filename = "PatientProfiles.csv"))
                            ))
     } )
 }
  


#### RUN APP ---- 
shinyApp(ui = ui, server = server)


