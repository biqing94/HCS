options(scipen=999)
# function to load and install, if needed, required packages
# expects a comma-separated quoted list of package names
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs,require,character.only=TRUE))
  need <- libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("tidyverse", "openxlsx", "reshape2", "data.table", "readxl", "DT")

# reads and aggregates data for one animal/one day
# creates a data-frame
read_file <- function(filename, start.value, rename_variable = NULL, breaks=NULL, combine=NULL){
  rename_variable = unlist(rename_variable)
  breaks = unlist(breaks)
  combine = unlist(combine)
  
  data = read_excel(filename)

  # Remove last row of sum in each file
  if(is.character(data$Bin)){
    data$Bin = toupper(data$Bin)
    data = data[-which(data$Bin=="SUM"),]
    data$Bin = as.numeric(data$Bin)
  }
  
  row1 = which(data$SUM >= as.numeric(start.value))[1]
  row2 = row1 + 629
  
  # If there is no 59.99 then skip the following if loop
  if(length(row1) == 0){
    stop(paste(filename,"does not have 59.9"))
  } else{
      # If 630th row does not exist then skip
      if(row2 <= nrow(data)){
        data = as.data.frame(data[row1:row2,])
      } else{
        stop(paste(filename,"does not have 630th row, i.e. row",row2))
      }
  }
  
  # Change bins to 0 - 629th minutes
  data = data[order(data$Bin),]
  data$Bin = 0:629
  
  # Rename variables e.g. rename_variable = c(Turn = "Turning")
  if(!is.null(rename_variable)){
    names(data) = dplyr::recode(names(data), !!!rename_variable)
  }

  # For more breakdowns, specify, for example breaks=c(1,250,500,+Inf); labels=c("Morning","Noon","Evening")
  # Create aggregation variable
  data$Interval = NA
  if(!is.null(breaks)){
    data$Interval = cut(data$Bin,breaks=breaks,include.lowest = TRUE,right=FALSE)
  } else{
    data$Interval = "Day"
  }
  
  data$Interval = as.factor(data$Interval)
  summary_data = data %>% group_by(Interval) %>% summarise_at(vars(`Travel(m)`:SUM), ~sum(., na.rm = TRUE)) 
  fraction_data = as.data.frame(summary_data)
  for(j in 2:ncol(fraction_data)){
    for(i in 1:nrow(fraction_data)){
      fraction_data[i,j] = fraction_data[i,j]/fraction_data$SUM[i]
    }
  }
  
  
  # Combining behaviours/activities e.g. combine=c(ComeDown = "ComeDown + RearUp", RearUp = "ComeDown + RearUp")
  if (!is.null(combine)) {
    combinedat = read_excel(combine)
    for(i in unique(combinedat$`Combined Behavior`)){
      combine_columns = combinedat$Behavior[which(combinedat$`Combined Behavior`==i)]
      summary_data = summary_data %>%
        mutate(V1 = rowSums(summary_data[,combine_columns])) 
      names(summary_data)[names(summary_data)=="V1"] = i
      
      fraction_data = fraction_data %>%
        mutate(V1 = rowSums(fraction_data[,combine_columns])) 
      names(fraction_data)[names(fraction_data)=="V1"] = i
      
      # Remove the activities combined to avoid summing twice in aggregate_by in next function
      summary_data = summary_data[,-which(names(summary_data) %in% combine_columns)]
      fraction_data = fraction_data[,-which(names(fraction_data) %in% combine_columns)]
    }
  }
  
  
  # Transpose data wide to long and order by intervals
  summary_data = summary_data %>% pivot_longer(
                   names(summary_data[-1]),
                   names_to = "Activity",
                   values_to = "Sum")
  
  fraction_data = fraction_data %>% pivot_longer(
                    names(fraction_data[-1]),
                    names_to = "Activity",
                    values_to = "Fraction")
  
  summary_data = summary_data[order(summary_data$Interval),]
  fraction_data = fraction_data[order(fraction_data$Interval),]
  
  # Remove rows Activity == SUM from data
  summary_data = summary_data[-grep("SUM",summary_data$Activity),]
  fraction_data = fraction_data[-grep("SUM",fraction_data$Activity),]

  return(list("summary" = summary_data, "fraction" = fraction_data))
} 



process_metafile <- function(metaData, datapath, aggregate_by=NULL, 
                             args_list = list(start.value=59.99, rename_variable = NULL, breaks=NULL, combine=NULL)){
  # reads in metadata
  #metaData = read_excel(metadatafile)
  metaData$Filename = file.path(datapath, paste0(metaData$Filename, ".xlsx"))
  
  # create one long dataset to store results
  summary_all_data = data.frame()
  fraction_all_data = data.frame()
  
  
  # loops through metadata and calls read_file on each row
  for(i in 1:nrow(metaData)){
    filename = metaData$Filename[i]
    
    x = do.call(read_file, c(filename, args_list))
    summary_data = x[["summary"]]
    fraction_data = x[["fraction"]]
    
    summary_data = summary_data %>%
      mutate(AnimalID = metaData$`Animal #`[i],
             Genotype = metaData$Genotype[i],
             Sex = metaData$Sex[i],
             RecordingDay = paste("Day",metaData$`Recording Day`[i]))
    summary_data = summary_data[,c(4,5,6,7,1,2,3)]
    
    
    fraction_data = fraction_data %>%
      mutate(AnimalID = metaData$`Animal #`[i],
             Genotype = metaData$Genotype[i],
             Sex = metaData$Sex[i],
             RecordingDay = paste("Day",metaData$`Recording Day`[i]))
    fraction_data = fraction_data[,c(4,5,6,7,1,2,3)]
    
    summary_all_data = c(summary_all_data, list(summary_data))
    fraction_all_data = c(fraction_all_data, list(fraction_data)) 
  }
  
  # combine data lists
  summary_all_data = do.call(rbind, summary_all_data)
  fraction_all_data = do.call(rbind, fraction_all_data)
  
  # Transpose from long to wide
  summary_all_data = summary_all_data %>%
                       pivot_wider(names_from = RecordingDay,
                                   values_from = Sum) 
  fraction_all_data = fraction_all_data %>%
                        pivot_wider(names_from = RecordingDay,
                                    values_from = Fraction) 
  
  # aggregate_by = c("Interval") or aggregate_by = c("Interval","Genotype")
  if(!is.null(aggregate_by)){
    # Remove Activity = SUM before aggregate so that it is not included while summing by group
    summary_all_data = summary_all_data[-which(summary_all_data$Activity=="SUM"),]
    fraction_all_data = fraction_all_data[-which(fraction_all_data$Activity=="SUM"),]
    
    summary_all_data = summary_all_data %>% 
                         group_by_at(aggregate_by) %>% 
                         summarise_at(names(summary_all_data)[grep("Day", names(summary_all_data))], sum, na.rm=TRUE)
    fraction_all_data = fraction_all_data %>% 
                          group_by_at(aggregate_by) %>% 
                          summarise_at(names(fraction_all_data)[grep("Day", names(fraction_all_data))], sum, na.rm=TRUE)
  }
  
  # Write to excel
  #write.xlsx(list("Fraction" = fraction_all_data,"Summation" = summary_all_data), summary_file, col.names = TRUE, row.names = FALSE, append = FALSE, showNA = FALSE) 
  
  # Change labelling for Interval (bin breaks) to show that 629th minute is included 
  summary_all_data$Interval = gsub("629]","630)",summary_all_data$Interval)
  fraction_all_data$Interval = gsub("629]","630)",fraction_all_data$Interval)
  
  # Change to factor so that it shows a drop down list in filter
  cols = c("AnimalID", "Genotype", "Sex", "Interval","Activity")
  summary_all_data[,cols] <- lapply(summary_all_data[,cols], factor)
  fraction_all_data[,cols] <- lapply(fraction_all_data[,cols], factor)
  
  
  return(list("summary_all" = summary_all_data, "fraction_all" = fraction_all_data))
}


### !!!The following function strictly follows the example given, not suitable if aggregate_by
### argument is used in process_metafile function
export_for_Prism <- function(data, filter = NULL, outfilename){
  PrismData = data.frame()
  
  # for filterting, write filter = "Interval == 'Morning'" or filter = "Interval == 'Morning' & Genotype == 'KO_Hemi'"
  # Note: if breaks argument is not NULL in args_list, need to use the filter argument to save different time of the day in different files. 
  if(!is.null(filter)){
    data = subset(data,rlang::eval_tidy(rlang::parse_expr(filter)))
  }
  
  for(i in unique(data$Activity)){
    subsetData = subset(data,Activity==i)
    subsetData = subsetData[order(subsetData$Genotype,subsetData$AnimalID),]
    subsetData = subsetData[,c(5,2,1,6,7,8)]
    subsetData$Activity[2:nrow(subsetData)] = NA
    subsetData = as.data.frame(t(subsetData))
    colnames(subsetData)[1:ncol(subsetData)] = paste("V",1:ncol(subsetData),sep="")
    subsetData = as.data.frame(setDT(subsetData, keep.rownames = TRUE)[])
    
    # Add empty row after each activity
    subsetData[nrow(subsetData)+1,] = NA
    
    PrismData = as.data.frame(rbind(PrismData,subsetData))
  }
  
  
  # Tidy up before export
  ind = grep("Activity",PrismData[,1])
  PrismData[ind,1] = as.character(PrismData[ind,2])
  PrismData[ind,2] = NA
  
  # Write to excel
  write.xlsx(PrismData, outfilename, col.names = FALSE, row.names = FALSE, append = TRUE, showNA = FALSE) 
}


