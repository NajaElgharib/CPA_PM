#import libraries

library(dplyr)
library(plyr)
library(janitor)
library(tidyverse) 

## read CSV file
readCSV("dataset.csv")-> EventLogs

## clean column headers
cleanHeaders(EventLogs) -> EventLogs

## select columns for analysis
selectColumns(EventLogs, company_id ,event, time, account_type, account_status, number_of_clients, client_status) -> EventLogs

## filter rows where company_id is empty
EventLogs %>% filter(company_id != "") -> EventLogs

## filter rows to remove some events
EventLogs %>% filter(client != "Phone App" & event != "View Dashboard") -> EventLogs

## filter rows for events with low frequency
removeEventsLowFrequency(EventLogs, event, 10) -> EventLogs

## delete cases with few events
deleteTraceLengthLessThan(EventLogs, company_id, 2) -> EventLogs

## arrange rows according to variables company_id and time
arrangeRows(EventLogs, company_id, time) -> EventLogs

## delete cases that do not start with “Trial Sign Up Completed” event
deleteTruncatedTracesStart(EventLogs, company_id, event, "Trial Sign Up Completed") -> EventLogs

## create isRepeatedEvent column
eventIsRepeated(EventLogs, company_id, event, isRepeated) -> EventLogs

## keep first occurrence of certain event
keepFirstEvent(EventLogs, company_id, event, "Edit Dashboard") -> EventLogs

## keep first occurrence of all events
EventLogs %>% filter(isRepeated != “1”) -> EventLogs

## keep last occurrence of certain event
keepLastEvent(EventLogs, company_id, event, "Edit Data Source")-> EventLogs 

## delete all events
deleteAllEvents(EventLogs, event, "Slack Share") -> EventLogs

## merge rows
mergeRows(EventLogs, 
        .(company_id, event),
         summarise,
         time = first(time),
         account_type = paste(unique(account_type),
         collapse=','),
         account_status = paste(unique(account_status),
         collapse=','),
         number_of_clients = sum(number_of_clients),
         client_status = paste(unique(client_status),
         collapse=',')) -> EventLogs

## write to CSV file
writeCSV(EventLogs, "dataset.csv.csv") 

## end of script
