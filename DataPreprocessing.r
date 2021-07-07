## List of Functions
##-----------------------------------------------
## Import required libraries
##-----------------------------------------------

library(dplyr)
library(plyr)
library(janitor)

#read a CSV file
#readCSV(String file):The readCSV function reads the CSV file into a dataframe that it creates. 
#Arguments: file is the CSV file in the select work directory.
#Returns a dataframe
readCSV <- function(filename) {
  read.csv(file=filename, header=TRUE, sep=",")
}

#write to CSV file
#writeCSV(table DataSet, String file):The writeCSV function writes the dataframe to a CSV file.
#Arguments: DataSet is the name of dataframe to save, file is the name of the CSV output file.
#Returns a CSV file
writeCSV <- function(dataset){
  write.csv(dataset, file=filename)
}

#clean column headers
#cleanHeaders(table DataSet)
#This function cleans the headers of the columns from spaces and other special characters.
#It only keeps lower case letters, numbers, and underscores (_). The spaces are replaced by ‘_’ and the special characters are removed. 
#Returns a dataframe with clean header names
cleanHeaders <- function(dataset){
  dataset %>% clean_names() -> dataset
}

#arrange records/rows

arrangeRows <- function(.dataset, ...){
  arrange(.dataset, ...) -> dataset
}

#select dataset columns for analysis
#selectColumns(table DataSet, string columnName, …): This function selects/keeps the list columns needed for analysis from the dataset. Only the
#list of selected columns/attributes are included in the dataset. 
#Arguments: DataSet is the name of the dataframe, columnName is the name of the column to keep in the dataset. Many can be listed, separated by commas.
#Returns a dataset including only the list of columns/attributes that are selected

selectColumns <- function(dataset, ...){
  dataset %>% select(...) -> dataset
}

#delete columns from the dataset
deleteColumns <- function(dataset, ...) {
  dataset %>% select(-(...)) -> dataset
}

#filter rows
filter(.dataset, ...) -> dataset

#remove rows with low frequency
#num represents the frequency number
#n represents the count produced by tally
removeEventsLowFrequency <- function(.dataset, event, num){
  eventCount <- .dataset %>%
    group_by(event) %>%
    tally
    
  frequentEvents <- eventCount %>%
    filter(n>num) %>%
    select(event)
  
  .dataset <- .dataset %>% 
    filter(event %in% frequentEvents$event)
}

#delete traces with number of events less than a specific number (num)
deleteTracesLengthLessThan <- function(.dataset, groupID, num){
  traceCounts <- .dataset %>%
    group_by(groupID) %>%
    tally
  
  frequencyTraces <- traceCounts %>%
    filter(n >= num) %>% select(groupID)
    
  .dataset <- .dataset %>%
    filter(groupID %in% frequentTraces$groupID)
}

#delete traces that do not start with a specific start event
#enquo quotes the value of startEvent and captures the environment where the deleteTruncatedTracesStart() function is called

deleteTruncatedTracesStart <- function(dataset, groupID, startEvent, value){
  groupID <- enquo(groupID)
  startEvent <- enquo(startEvent)
  
  dataset %>%
    group_by(!!groupID) %>% filter(first(!!startEvent) == value) -> dataset
}

#delete traces that do not finish with a specific end event
deleteTruncatedTracesEnd <- function(dataset, groupID, lastEvent, value){
  groupID <- enquo(groupID)
  lastEvent <- enquo(lastEvent)
  
  dataset %>% 
    group_by(!!groupID) %>% filter(last(!!lastEvent) == value) -> dataset
}

#delete traces with total duration less than t
deleteTracesWithTimeLess(dataset, groupID, time, t){
  groupID <- enquo(groupID)
  time <- enquo(time)
  
  dataset %>%
    group_by(!!groupID) %>% filter((last(time)-first(time)) <t)
}

#concatenate two columns
concatenateColumns <- function(dataset,newCol,col1,col2){
  newCol <- enquo(newCol)
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  dataset %>%
    mutate(!!newCol := paste(!!col1, !!col2)) -> dataset
}

#create isRepeated column
eventIsRepeated <- function(dataset, groupID, event, isRepeated){
  groupID <- enquo(groupID)
  event <- enquo(event)
  isRepeated <- enquo(isRepeated)
  
  dataset %>%
    group_by(!!groupID) %>% mutate(!!!repeated := as.numeric(!!event == lag(!!event, 1)))
}

#keep first event
keepFirstEvent <- function(.dataset, groupID, eventName, value){
  groupID <- enquo(groupID)
  eventName <- enquo(eventName)
  .dataset %>%
      filter(eventName == value & isRepeated != "1") -> .dataset
}

#keep last event
keepLastEvent <- function(.dataset, groupID, eventName, value){
  groupID <- enquo(groupID)
  eventName <- enquo(eventName)
  .dataset %>%
      filter(eventName == value & last(isRepeated)) -> .dataset
}

#delete all events
deleteAllEvents <- function(.dataset, event, eventName){
    filter(.dataset, event != eventName) -> .dataset
}

# merge rows
mergeRows <- function(.dataset, .variables, ...){
    ddply(.dataset, .variables, summarize, ...)
}

#Sample usage script

# import libraries
library(dplyr)
library(plyr)
library(janitor)
library(tidyverse)

#read CSV file
readCSV("dataset.csv") -> EventLogs

#clean column headers
cleanHeaders(EventLogs) -> EventLogs

#select columns for analysis
selectColumns(EventLogs, company_id, event, time, client, template, guide_name,
              guide_type, 
              number_of_graphs,
              number_of_graphs_on_dashboard,
              graph_origin,
              source,
              kpi_count, 
              template_name, account_type,
              graphs_owned, data_format, data_sources_owned,
              dashboard_template_name, connector_backend,
              x_city, weekday) -> EventLogs

# filter rows where company_id is empty
EventLogs %>% filter(company_id != "") -> EventLogs

# filter rows to remove some events
EventLogs %>% filter(client != "Phone App" & event != "View Dashboard") -> EventLogs

# filter rows for events with low frequency
removeEventsLowFrequency(EventLogs, event, 10) -> EventLogs

# delete cases with few events
deleteTraceLengthLessThan(EventLogs, company_id, 5) -> EventLogs

#arrange rows according to variables company_id and time
arrangeRows(EventLogs, company_id, time) -> EventLogs

#delete cases that do not start with "Trial Sign Up Completed" event
deleteTruncatedTracesStart(EventLogs, company_id, event, "Trial Sign Up Completed") -> EventLogs

#delete cases that do not finish with "Purchase Confirm Details"
deleteTruncatedTracesEnd(EventLogs, company_id, event, "Purchase Confirm Details") -> EventLogs

#concatenate tow columns (optional)
concatenateColumns(EventLogs, act, event, source) -> EventLogs

#create isRepeatedEvent column
eventIsRepeated(EventLogs, company_id, event, isRepeated) -> EventLogs

#keep first occurrence of certain event
keepFirstEvent(EventLogs, company_id, event, "Edit Dashboard") -> EventLogs

#keep first occurrence of all events
EventLogs %>% filter(isRepeated != "1") -> EventLogs

#keep last occurrence of centain event
keepLastEvent(EventLogs, company_id, event, "Edit Data Source") -> EventLogs

## delete all events
deleteAllEvents(EventLogs, event, "Slack Share") -> EventLogs


## merge rows
mergeRows(EventLogs, .(company_id, event), summarise, 
          time = first(time),
          kpi_count = median(kpi_count),
          graphs_owned = median(graphs_owned),
          guide_name = paste(unique(guide_name),
          collapse=','),
          guide_type = paste(unique(guide_type),
          collapse=','),
          number_of_graphs = max(number_of_graphs),
          number_of_graphs_on_dashboard = sum(number_of_graphs_on_dashboard),
          connector_backend= paste(unique(connector_backend), collapse = ','),
          x_city = paste(unique(x_city), collapse = ','),
          weekday = paste(unique(weekday), collapse = ',')) -> EventLogs

## write to CSV file
writeCSV(EventLogs, "dataSet.csv")



