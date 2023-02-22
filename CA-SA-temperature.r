####################################################################################
# Temperature designation script
# Tests different values of temperatures introduced in temperature_thresholds.csv file
# Uses same input and settings file as main script(omits temperature settings)
####################################################################################

#Simulated Annealing library import
#see: https://CRAN.R-project.org/package=GenSA OR https://www.rdocumentation.org/packages/GenSA/versions/1.1.7/topics/GenSA
library(GenSA)

# Loading additional libraries
#library()
#XLSX files library
#see: https://cran.r-project.org/package=openxlsx
library(openxlsx)

######################
# Settings variables #
######################

#########
script.dir <- getSrcDirectory(function(x) {x})
main_directory <- script.dir
#inputs file directory(default is source dir)
input_file_directory <- "data_and_settings"
#########

#########
#USE SETTINGS FROM FILE??
file_settings <- TRUE
#settings file name
settings_filename <- 'settings.xlsx'
temperature_thresholds_file <- 'temperature_thresholds.xlsx'
#########

#set working directory
setwd(main_directory)
setwd(input_file_directory)

############################################################
#if settings file is not used please change variables below#
############################################################
if(!file_settings) {
  #input file name
  input_filename <- 'applicants.xlsx'
  #characteristics file name
  par_filename <- 'categories.xlsx'
  #assembly size
  N <- 50
  #number of draws
  draws_number <- 6
  #allow repetitions across particular draws(unimplemented yet)
  #allow_duplicates = TRUE
  #max time for each draw in [ms]
  SA_script_max_time <- 3600
  #SA temperature parameter
  SA_temperature <- 5000
  #Random seed integer. Negative integer value that can be set to initialize the internal random generator.
  SA_seed <- -1
  #Integer for maximum iterations count
  SA_max_iterations <- 10000
  #The program will stop when there is no any improvement in steps 
  SA_nb_stop_imp <- 100
  #Iteration will stop when the expected evaluation function value is reached
  SA_threshold_stop <- 0
  #Maximum number of call of the objective function
  SA_max_call <- 10000
  #Script run's tries in the row when contains duplicates
  draw_repeats_until <- 100
  #Does applicants list need to be shuffled before each draw
  shuffle_applicants <- FALSE
  #Output evaluation function result value to the console
  live_preview <- TRUE
  #Do create detailed output files for each draw
  draws_files <- TRUE
  #Format(extension) of output files. Need to be csv or xlsx
  output_file_format <- "xlsx"
  #Number of GenSA run's per every temperature
  iterations_per_temp <- 5
  
  ################################################################
  #if settings file is not provided please change variables above#
  ################################################################  
}else{
  
  #load setting file
  if(tolower(sub("^[^.]*", "", settings_filename)) == ".csv"){
    settings <- read.csv(file = settings_filename, row.names = 1, header= TRUE)
  }else if(tolower(sub("^[^.]*", "", settings_filename)) == ".xlsx"){
    settings <- read.xlsx(settings_filename, sheet = 1, rowNames = TRUE)
  }
  print("Loaded settings: ")
  print(settings)
  
  #input filename
  input_filename <- as.character(settings["input_filename","setting_value"])
  #characteristics filename
  par_filename <- as.character(settings["par_filename","setting_value"])
  
  #assembly size
  N <- as.numeric(settings["assembly_size","setting_value"])
  #allow repetitions across particular draws
  #allow_duplicates = as.logical(settings["allow_duplicates","setting_value"])
  
  
  #input filename
  input_filename <- as.character(settings["input_filename","setting_value"])
  #characteristics filename
  par_filename <- as.character(settings["par_filename","setting_value"])
  #assembly size
  N <- as.numeric(settings["assembly_size","setting_value"])
  #number of draws
  draws_number <- as.numeric(settings["draws_number","setting_value"])
  #allow repetitions across particular draws(unimplemented yet)
  #allow_duplicates = as.logical(as.numeric(settings["allow_duplicates","setting_value"]))
  #max time for each draw in [ms]
  SA_script_max_time <- as.numeric(settings["SA_max_time","setting_value"])
  #SA temperature parameter
  SA_temperature <- as.numeric(settings["SA_temperature","setting_value"])
  #Random seed integer. Negative integer value that can be set to initialize the internal random generator.
  SA_seed <- as.numeric(settings["SA_seed","setting_value"])
  #Integer for maximum iterations count
  SA_max_iterations <- as.numeric(settings["SA_max_iterations","setting_value"])
  #Integer. The program will stop when there is no any improvement in nb.stop.improvement steps 
  SA_nb_stop_imp <- as.numeric(settings["SA_nb_stop_imp","setting_value"])
  #Iteration will stop when the expected evaluation function value is reached
  SA_threshold_stop <- as.numeric(settings["SA_threshold_stop","setting_value"])
  #Maximum number of call of the objective function
  SA_max_call <- as.numeric(settings["SA_max_call","setting_value"])
  #Script run's tries in the row when contains duplicates
  draw_repeats_until <- as.numeric(settings["draw_repeats_until","setting_value"])
  #Does applicants list need to be shuffled before each draw
  shuffle_applicants <- as.logical(as.numeric(settings["shuffle_applicants","setting_value"]))
  #Output evaluation function result value to the console
  live_preview <- as.logical(as.numeric(settings["live_preview","setting_value"]))
  #Do create detailed output files for each draw
  draws_files <- as.logical(as.numeric(settings["draws_files","setting_value"]))
  #Format(extension) of output files. Need to be csv or xlsx
  output_file_format <- as.character(settings["output_file_format","setting_value"])
  #Number of GenSA run's per every temperature
  iterations_per_temp <- as.numeric(settings["iterations_per_temp","setting_value"])
  
}
###############

#helper trim function
trimws_str <- function(x) {
  if(is.character(x)){
    return(trimws(x))
  }else{
    return(x)
  }
}

#data loading
if(tolower(sub("^[^.]*", "", input_filename)) == ".csv"){
  applicants <- read.csv(file = input_filename, row.names = 1, header= TRUE)
}else if(tolower(sub("^[^.]*", "", input_filename)) == ".xlsx"){
  applicants <- read.xlsx(input_filename, sheet = 1, rowNames = TRUE)
}
applicants_names <- lapply(names(applicants), trimws_str)
applicants <- data.frame(lapply(applicants, trimws_str))
names(applicants) <- applicants_names
print("Loaded applicants: ")
print(applicants)

if(tolower(sub("^[^.]*", "", par_filename)) == ".csv"){
  characteristics <- read.csv(file = par_filename, header= TRUE)
}else if(tolower(sub("^[^.]*", "", par_filename)) == ".xlsx"){
  characteristics <- read.xlsx(par_filename, sheet = 1)
}
characteristics_names <- lapply(names(characteristics), trimws_str)
characteristics <- data.frame(lapply(characteristics, trimws_str))
names(characteristics) <- characteristics_names
print("Loaded characteristics: ")
print(characteristics)

# input entries size
INPUT_SIZE <- nrow(applicants)
#number of different characteristics values
categories_number <- length(characteristics$category)
#number of different characteristics values
applicants_number <- length(applicants$ID)
#setting up counter column
characteristics$counter <- rep(0,categories_number)
#helper vector
Nv <- 1:N

#####################
#####################

#Temperature searching specific data:
draws <- data.frame()
draws_additional <- data.frame()
#Thresholds loading
if(tolower(sub("^[^.]*", "", temperature_thresholds_file)) == ".csv"){
  temperature_thresholds <- read.csv(file = temperature_thresholds_file, header= TRUE)
}else if(tolower(sub("^[^.]*", "", temperature_thresholds_file)) == ".xlsx"){
  temperature_thresholds <- read.xlsx(temperature_thresholds_file, sheet = 1)
}
print("Loaded temperatures thresholds:")
print(temperature_thresholds)



#evaluation function declaration
#v - vectors
#b - final call indicator
evaluation_function <- function(v,b) {
  
  ret <- 0
  v <- (round(v) %% INPUT_SIZE)+1
  
  #in-draw duplicates check
  if(any(duplicated(applicants$ID[v]))){
    ret <- 99999999
  }else{
    
    #preparing data structure from input files for counters
    for (i in 1:length(v)) {
      for (k in 1:categories_number) {
        #fix for column names
        input_category <- gsub(" ",".",characteristics[k,'category'])
        #checks for data
        if(characteristics[k,'value'] == applicants[v[i],input_category]){
          characteristics[k,'counter'] <- characteristics[k,'counter'] + 1
        }
      }
    }
    
    #calculating the ideal composition - evaluation for checking parameter
    check_par <- 0
    
    #main evaluation function calculation
    for (k in 1:categories_number) {
      if(as.numeric(characteristics[k,'priority'])>1){
        #increase value if current characteristic is more important than others
        check_par <- check_par + as.numeric(characteristics[k,'priority'])*((characteristics[k,'counter']-characteristics[k,'amount']))^2
      }else{
        check_par <- check_par + (characteristics[k,'counter']-characteristics[k,'amount'])^2
      }
    }
    
    #results evaluation
    ret <- ret + check_par
    
  }
  
  #print(ret)
  
  ret
}

#temperature designation output files folder
setwd(main_directory)
setwd("results_temperature")

#main temperatures values designation
for(threshold in 1:length(temperature_thresholds[,'init_temp'])){
  temp_current <- temperature_thresholds[threshold,'init_temp']
  print("Current temperature: ")
  print(temp_current)
  
  #setting initial values
  result_value_avg <- c()
  result_time_avg <- c()
  
  #temp. iterations
  for(iteration in 1:iterations_per_temp){
    print("Current iteration: ")
    print(iteration)
    #time measuring
    start_time <- Sys.time()
    #GenSA launch
    RESULT <- GenSA(par=as.numeric(Nv*INPUT_SIZE/N), fn=evaluation_function, lower=as.numeric(rep(-1000000,N)), upper=as.numeric(rep(1000000,N)), b=FALSE, control=list(max.time=SA_script_max_time,smooth=FALSE,verbose=TRUE,temperature=as.integer(temp_current),threshold.stop=SA_threshold_stop,max.call=SA_max_call,maxit=SA_max_iterations,nb.stop.improvement=SA_nb_stop_imp,seed=SA_seed))
    end_time <- Sys.time()
    #saving trace matrix data
    if(output_file_format=="csv"){
      write.csv(RESULT$trace.mat,paste('result_T_',temp_current,'_',iteration,'_tracemat.csv',sep = ''),row.names = FALSE)
    }else if(output_file_format=="xlsx"){
      write.xlsx(data.frame(RESULT$trace.mat), paste('result_T_',temp_current,'_',iteration,'_tracemat.xlsx',sep = ''), sheetName = paste('result_T_',temp_current,'_',iteration,'_tracemat',sep = ''))
    }
    
    
    current_result <- RESULT$value
    print("Current result(GenSA value): ")
    print(current_result)
    
    #stats
    column_name <- paste('result_', iteration, '_value',sep = '')
    result_value_avg <- append(result_value_avg, as.numeric(current_result))
    draws[threshold,'temp'] <- temp_current
    draws[threshold,'result_value_avg'] <- 0
    draws[threshold,'result_time_avg'] <- 0
    draws[threshold,column_name] <- current_result
    column_name <- paste('result_', iteration, '_time',sep = '')
    time_diff <- difftime(end_time, start_time, units = "secs")
    print("Iteration duration: ")
    print(time_diff)
    result_time_avg <- append(result_time_avg, time_diff)
    draws[threshold,column_name] <- time_diff
    
  }
  #averages
  column_name <- 'result_value_avg'
  draws[threshold,column_name] <- mean(result_value_avg)
  column_name <- 'result_time_avg'
  draws[threshold,column_name] <- mean(result_time_avg)
  
  print("Current temperatures results: ")
  print(draws)
  
}
#save main temperatures stats
draws_save <- draws
draws_save$result_time_avg <- paste(draws_save$result_time_avg, ' secs',sep = '')
if(output_file_format=="csv"){
  write.csv(draws_save,paste('temperatures_stats_main.csv',sep = ''),row.names = FALSE)
}else if(output_file_format=="xlsx"){
  write.xlsx(draws_save, paste('temperatures_stats_main.xlsx',sep = ''), sheetName = paste('temperatures_stats_main',sep = ''))
}


#temperature candidates
draws_values_sorted <- draws[order(draws$result_value_avg),]
print("Sorted stats: ")
print(draws_values_sorted)
best_value <- draws_values_sorted[,'result_value_avg'][1]
print("Best GenSA value: ")
print(best_value)
best_values_keys <- which(draws$result_value_avg == best_value)
print("Best top temperatures keys: ")
print(best_values_keys)

#check if there are more than 1 best temperature based on value and compare times
if(length(best_values_keys)>1){
  print("Time comparision: ")
  #time candidates
  draws_values_sorted <- draws[order(draws$result_time_avg),]
  draws_values_sorted <- draws_values_sorted[order(draws$result_value_avg),]
  print("Sorted stats: ")
  print(draws_values_sorted)
  print("Best GenSA value and time: ")
  best_value <- draws_values_sorted[,'result_value_avg'][1]
  print(best_value)
  print("Best top temperatures and time keys: ")
  best_values_keys <- which(draws$result_value_avg == best_value)
  print(best_values_keys)
}

best_init_temp <- draws[best_values_keys[1],'temp']
print("Best initial temperature: ")
print(best_init_temp)

temperatures_additional <- temperature_thresholds[best_values_keys[1],]
print("Additional temperatures to ckeck: ")
print(temperatures_additional)

#adding best main temperature to additional's status
draws_additional <- draws[0,]
draws_additional[1,] <- draws[best_values_keys[1],]
rownames(draws_additional) <- c(1)

#Additional temperatures values designation and stats
for(threshold in 1:(length(temperatures_additional)-1)){
  
  temp_current <- temperatures_additional[paste('around_',threshold,sep = '')]
  print("Current temperature: ")
  print(temp_current)
  
  #Omit undefined temperatures
  if(!is.na(temp_current)){
    #Setting stats beginning values
    result_value_avg <- c()
    result_time_avg <- c()
    
    
    for(iteration in 1:iterations_per_temp){
      print("Current iteration: ")
      print(iteration)
      #time measurement
      start_time <- Sys.time()
      #GenSA launch
      RESULT <- GenSA(par=as.numeric(Nv*INPUT_SIZE/N), fn=evaluation_function, lower=as.numeric(rep(-1000000,N)), upper=as.numeric(rep(1000000,N)), b=FALSE, control=list(max.time=SA_script_max_time,smooth=FALSE,verbose=TRUE,temperature=as.integer(temp_current),threshold.stop=SA_threshold_stop,max.call=SA_max_call,maxit=SA_max_iterations,nb.stop.improvement=SA_nb_stop_imp,seed=SA_seed))
      end_time <- Sys.time()
      #saving trace matrix for temp.
      
      if(output_file_format=="csv"){
        write.csv(RESULT$trace.mat,paste('result_T_',temp_current,'_',iteration,'_tracemat.csv',sep = ''),row.names = FALSE)
      }else if(output_file_format=="xlsx"){
        write.xlsx(data.frame(RESULT$trace.mat), paste('result_T_',temp_current,'_',iteration,'_tracemat.xlsx',sep = ''), sheetName = paste('result_T_',temp_current,'_',iteration,'_tracemat',sep = ''))
      }
      
      current_result <- RESULT$value
      print("Current GenSA value: ")
      print(current_result)
      #stats
      column_name <- paste('result_', iteration, '_value',sep = '')
      result_value_avg <- append(result_value_avg, as.numeric(current_result))
      draws_additional[threshold+1,'temp'] <- temp_current
      draws_additional[threshold+1,'result_value_avg'] <- 0
      draws_additional[threshold+1,'result_time_avg'] <- 0
      draws_additional[threshold+1,column_name] <- current_result
      column_name <- paste('result_', iteration, '_time',sep = '')
      time_diff <- difftime(end_time, start_time, units = "secs")
      print("Iteration duration: ")
      print(time_diff)
      result_time_avg <- append(result_time_avg, time_diff)
      draws_additional[threshold+1,column_name] <- time_diff
      
    }
    #averages
    column_name <- 'result_value_avg'
    draws_additional[threshold+1,column_name] <- mean(result_value_avg)
    column_name <- 'result_time_avg'
    draws_additional[threshold+1,column_name] <- mean(result_time_avg)
    print("Current additional temperatures results: ")
    print(draws_additional)
  }
  
}
#saving additional temperatures stats and results
draws_save <- draws_additional
draws_save$result_time_avg <- paste(draws_save$result_time_avg, ' secs',sep = '')
if(output_file_format=="csv"){
  write.csv(draws_save,paste('temp_stats_add_T_',best_init_temp,'.csv',sep = ''),row.names = FALSE)
}else if(output_file_format=="xlsx"){
  write.xlsx(draws_save, paste('temp_stats_add_T_',best_init_temp,'.xlsx',sep = ''), sheetName = paste('temp_stats_add_T_',best_init_temp,'',sep = ''))
}


#temperature candidates
draws_values_sorted <- draws_additional[order(draws_additional$result_value_avg),]
print("Sorted stats: ")
print(draws_values_sorted)
best_value <- draws_values_sorted[,'result_value_avg'][1]
print("Best GenSA value: ")
print(best_value)
best_values_keys <- which(draws_additional$result_value_avg == best_value)
print("Best top temperatures keys: ")
print(best_values_keys)

#check if there are more than 1 best temperature based on value and compare times
if(length(best_values_keys)>1){
  print("Time comparision: ")
  #time candidates
  draws_values_sorted <- draws_additional[order(draws_additional$result_time_avg),]
  draws_values_sorted <- draws_additional[order(draws_additional$result_value_avg),]
  print("Sorted stats: ")
  print(draws_values_sorted)
  best_value <- draws_values_sorted[,'result_value_avg'][1]
  print("Best GenSA value and time: ")
  print(best_value)
  best_values_keys <- which(draws_additional$result_value_avg == best_value)
  print("Best top temperatures and time keys: ")
  print(best_values_keys)
}

#Final result
best_final_temp <- draws_additional[best_values_keys[1],'temp']
print("Best final temperature: ")
print(best_final_temp)
