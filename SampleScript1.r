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
