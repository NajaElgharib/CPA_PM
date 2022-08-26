

#Sample usage script

# import libraries
library(dplyr)
library(plyr)
library(janitor)
library(tidyverse)

#read CSV file
readCSV("Sample_Dataset.csv") -> EventLogs

#clean column headers
cleanHeaders(EventLogs) -> EventLogs

#select columns for analysis
selectColumns(EventLogs, company_id, event, time, theme, account_users,
				metric_count, graph_origin, number_of_graphs_on_dashboard, graphs_owned) -> EventLogs

# filter rows where company_id is empty
EventLogs %>% filter(company_id != "") -> EventLogs

# filter rows to remove some events
##EventLogs %>% filter(client != "Phone App" & event != "View Dashboard") -> EventLogs

# filter rows for events with low frequency
removeEventsLowFrequency(EventLogs, event, 10) -> EventLogs

# delete cases with few events
deleteTracesLengthLessThan(EventLogs, company_id, 5) -> EventLogs

#arrange rows according to variables company_id and time
arrangeRows(EventLogs, company_id, time) -> EventLogs

#delete cases that do not start with "Trial Sign Up Completed" event
deleteTruncatedTracesStart(EventLogs, company_id, event, "Trial Sign Up Completed") -> EventLogs

#delete cases that do not finish with "Purchase Confirm Details"
deleteTruncatedTracesEnd(EventLogs, company_id, event, "Purchase Confirm Details") -> EventLogs

#concatenate tow columns (optional)
##concatenateColumns(EventLogs, act, event, source) -> EventLogs

#create isRepeatedEvent column
eventIsRepeated(EventLogs, company_id, event, isRepeated) -> EventLogs

#keep first occurrence of certain event
keepFirstEvent(EventLogs, company_id, event, "View Dashboard") -> EventLogs

#keep first occurrence of all events
EventLogs %>% filter(isRepeated != "1") -> EventLogs

#keep last occurrence of certain event
keepLastEvent(EventLogs, company_id, event, "Edit Data Source") -> EventLogs

## delete all events
deleteAllEvents(EventLogs, event, "Slack Share") -> EventLogs

## write to CSV file
writeCSV(EventLogs, "dataSet.csv")
