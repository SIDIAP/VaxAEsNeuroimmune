# New functions for plotting with "study start date" included.

outputFolderName <- outputFolder_Vax_w_history
reference <- readRDS(file.path(outputFolderName, "outcomeModelReference.rds"))

options(scipen = 999)
studyPopFile <- unique(reference$studyPopFile)
studyPop <- readRDS(file.path(outputFolder_Vax_w_history, studyPopFile[1]))


# plotEventToCalendarTime ------------

plotEventToCalendarTime_modify <- function(studyPopulation,
                                    fileName = NULL) {
  dates <- studyPopulation$outcomes %>%
    inner_join(studyPopulation$cases , by = "caseId") %>% filter(outcomeDay >= 0) %>%   #=== add " filter(outcomeDay >= 0) %>% " =========
  transmute(outcomeDate = .data$startDate  + .data$outcomeDay)

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(dates, ggplot2::aes(x = .data$outcomeDate)) +
    ggplot2::geom_histogram(binwidth = 30.5, fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::scale_x_date("Calendar time") +
    ggplot2::scale_y_continuous("Frequency") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}


plotEventToCalendarTime_modify(studyPop)

# plotEventObservationDependence --------------------------------
plotEventObservationDependence_modify <- function(studyPopulation,
                                           fileName = NULL) {


  outcomes <- studyPopulation$outcomes %>%
    group_by(.data$caseId) %>%
    summarise(outcomeDay = min(.data$outcomeDay), .groups = "drop_last") %>%
    inner_join(studyPopulation$cases, by = "caseId") %>% filter(outcomeDay >= 0) %>%   #=== add " filter(outcomeDay >= 0) %>% " =========
  transmute(daysFromEvent = .data$endDay - .data$outcomeDay,
            censoring = case_when(.data$noninformativeEndCensor == 1 ~ "Uncensored",
                                  TRUE ~ "Censored"))

  ageLabels <- 0:ceiling(max(outcomes$daysFromEvent)/365.25)

  ageBreaks <- ageLabels * 365.25

  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(outcomes, ggplot2::aes(x = .data$daysFromEvent)) +
    ggplot2::geom_vline(xintercept = ageBreaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    ggplot2::geom_histogram(binwidth = 30.5, fill = rgb(0, 0, 0.8), alpha = 0.8) +
    ggplot2::scale_x_continuous("Years from event", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Frequency") +
    ggplot2::facet_grid(censoring~., scales = "free_y") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

plotEventObservationDependence_modify(studyPop)
