####################################################################################
# Script using Simmulated Annealing for draw participants for Citizen's Assemblies #
# Original idea and first version of the script by PhD Nikodem Mrożek (UoG)        #
# Upgraded by Damian Kozłowski (dkozlowski@teclado.pl)                             #
####################################################################################

#Simulated Annealing library import
#see: https://CRAN.R-project.org/package=GenSA OR https://www.rdocumentation.org/packages/GenSA/versions/1.1.7/topics/GenSA
library(GenSA)

# Loading additional libraries
#library()
#XLSX files library
#see: https://cran.r-project.org/package=openxlsx
library(openxlsx)
#XLSX results file name
xlsx_results_filename <- "Results.xlsx"



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
  
}
###############


#####################
#####################

#helper trim function
trimws_str <- function(x) {
  if(is.character(x)){
    return(trimws(x))
  }else{
    return(x)
  }
}


#"adding randomness" to internal number generator
if(SA_seed<0){
  Sys.sleep(floor(runif(1, min=1, max=3)))
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
characteristics_summary <- characteristics
#helper vector
Nv <- 1:N


#prepare statistics for files
results_summary <- data.frame('applicants' = as.character(applicants$ID), 'drawn_counter' = rep(0,applicants_number), row.names = 'applicants', fix.empty.names = FALSE)

#evaluation function declaration
#v - vectors
#b - final call indicator
evaluation_function <- function(v,b,draw_no,live_preview,draws_files) {
  
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
          characteristics_summary[k,'counter'] <- characteristics_summary[k,'counter'] + 1
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
    
    #current loop evaluation
    if((check_par==0 && ret<99999999) || b){
      #data output presentation
      print("Identificatiors list:")
      print( applicants$ID[v] )
      print("Characteristics amounts:")
      print( characteristics )
      print("Characteristics summary amounts:")
      print(characteristics_summary)

      drawed_df <- data.frame(No = rownames(applicants)[v])
      (drawed_df <- cbind(drawed_df, ID = applicants$ID[v]))
      print(drawed_df)
      
      #create result list file
      if(output_file_format=="csv"){
        write.csv(drawed_df, paste('result_',draw_no,'.csv',sep = ''),row.names = TRUE)
      }else if(output_file_format=="xlsx"){
        write.xlsx(drawed_df, paste('result_',draw_no,'.xlsx',sep = ''), sheetName = paste('result_',draw_no,sep = ''))
      }
      
      if(draws_files) {
        
        #create data characteristics file
        if(output_file_format=="csv"){
          write.csv(characteristics, paste('result_',draw_no,'_characteristics.csv',sep = ''),row.names = FALSE)
        }else if(output_file_format=="xlsx"){
          write.xlsx(characteristics, paste('result_',draw_no,'_characteristics.xlsx',sep = ''), sheetName = paste('result_',draw_no,'_characteristics',sep = ''))
        }
        
        #create ordinal distribution plot file
        png(paste('result_',draw_no,'_ordinal_dist.png',sep = ''),width=1000, height=800)
        plot(v,Nv,main = paste("Ordinal number distribution - result ",draw_no,sep = ''), xlab = "Applicant's ordinal number", ylab = "Participant's ordinal number")
        dev.off()
        #create characteristics distribution plot file
        png(paste('result_',draw_no,'_characteristics.png',sep = ''),width=length(characteristics$value)*50, height=1000)
        barplot(characteristics$counter,names.arg=characteristics$value,main = paste("Characteristics result ",draw_no,sep = ''), xlab = "Characteristic", ylab = "Amount")
        dev.off()
      }
      
    }
    
    if(live_preview){
      print(ret)
    }
    
  }
  
  ret
}


#change dir for results' output files
setwd(main_directory)
setwd("./results")

#run script for every defined draw
for(draw_no in 1:draws_number){
  
  print( paste("Starting draw no: ", draw_no, ' of ', draws_number,sep='' ))
  
  if(shuffle_applicants) {
    applicants = applicants[sample(1:nrow(applicants)), ]
  }
  
  #current iteration checking parameter
  check_par <- 0
  
  #simulated annealing
  tryCatch(
    expr = {
      characteristics$counter <- rep(0,categories_number)
      RESULT <- GenSA(par=as.numeric(Nv*INPUT_SIZE/N), fn=evaluation_function, lower=as.numeric(rep(-1000000,N)), upper=as.numeric(rep(1000000,N)), b=FALSE, draw_no=draw_no, live_preview=live_preview, draws_files=draws_files, control=list(max.time=SA_script_max_time,smooth=FALSE,verbose=TRUE,temperature=SA_temperature,threshold.stop=SA_threshold_stop,max.call=SA_max_call,maxit=SA_max_iterations,nb.stop.improvement=SA_nb_stop_imp,seed=SA_seed))
      print(characteristics)
      print("Results numbers set: ")
      print(RESULT$par)
      
      if(any(duplicated(applicants$ID[(round(RESULT$par) %% INPUT_SIZE)+1]))){
        print("Duplicated results. Repeat draw")
        draw_repeats_counter <- 1
        repeat {
          characteristics$counter <- rep(0,categories_number)
          RESULT <- GenSA(par=as.numeric(Nv*INPUT_SIZE/N), fn=evaluation_function, lower=as.numeric(rep(-1000000,N)), upper=as.numeric(rep(1000000,N)), b=FALSE, draw_no=draw_no, live_preview=live_preview, draws_files=draws_files, control=list(max.time=SA_script_max_time,smooth=FALSE,verbose=TRUE,temperature=SA_temperature,threshold.stop=SA_threshold_stop,max.call=SA_max_call,maxit=SA_max_iterations,nb.stop.improvement=SA_nb_stop_imp,seed=SA_seed))
          if(!any(duplicated(applicants$ID[(round(RESULT$par) %% INPUT_SIZE)+1]))){
            print("Found applicants without duplicates.")
            break
          }else if(draw_repeats_counter >= draw_repeats_until){
            stop("Too many draws in the row contains duplicates.")
          }else{
            print("Duplicated results. Repeat draw")
            draw_repeats_counter <- draw_repeats_counter + 1
          }
        }
      }
    },
    error = function(e){ 
      stop(e)
    },
    finally = print("Draw processed")
  )
  
  #main function
  characteristics$counter <- rep(0,categories_number)
  evaluation_function(RESULT$par, TRUE, draw_no, live_preview, draws_files)
  if(draws_files) {
    if(output_file_format=="csv"){
      write.csv(RESULT$trace.mat,paste('result_',draw_no,'_tracemat.csv',sep = ''),row.names = FALSE)
    }else if(output_file_format=="xlsx"){
      write.xlsx(data.frame(RESULT$trace.mat), paste('result_',draw_no,'_tracemat.xlsx',sep = ''), sheetName = paste('result_',draw_no,'_tracemat',sep = ''))
    }
  }
  
  
  characteristics_summary[paste('draw_', draw_no,sep='')] <- rep(0,categories_number)
  for (i in 1:length((round(RESULT$par) %% INPUT_SIZE)+1)) {
    for (k in 1:categories_number) {
      #fix for column names
      input_category <- gsub(" ",".",characteristics_summary[k,'category'])
      #checks for data
      if(characteristics_summary[k,'value'] == applicants[(round(RESULT$par[i]) %% INPUT_SIZE)+1,input_category]){
        characteristics_summary[k,paste('draw_', draw_no,sep='')] <- characteristics_summary[k,paste('draw_', draw_no,sep='')] + 1
        characteristics_summary[k,'counter'] <- characteristics_summary[k,'counter'] + 1
      }
    }
  }
  
  #results summary data
  results_summary[paste('draw_', draw_no,sep='')] <- rep(0,applicants_number)
  for(ID in applicants$ID[(round(RESULT$par) %% INPUT_SIZE)+1]){
    results_summary[as.character(ID), paste('draw_', draw_no,sep='')] <- results_summary[as.character(ID), paste('draw_', draw_no,sep='')]+1
    results_summary[as.character(ID), 'drawn_counter'] <- results_summary[as.character(ID), 'drawn_counter']+1
  }
}

#prepare xlsx for multisheet
if(output_file_format=="xlsx"){
  wb <- createWorkbook()
}

#save results summary data
if(output_file_format=="csv"){
  write.csv(results_summary,paste('results_summary.csv',sep = ''),row.names = TRUE)
}else if(output_file_format=="xlsx"){
  addWorksheet(wb,sheetName = paste('results_summary',sep = ''))
  writeDataTable(wb, sheet = 1, results_summary, rowNames = TRUE)
}


#Summary statistics data
summary_stats_data <- data.frame(draws_number)
summary_stats_data <- append(summary_stats_data, N)
summary_stats_data <- append(summary_stats_data, INPUT_SIZE)
summary_stats_data <- append(summary_stats_data, mean(results_summary[,'drawn_counter']))
summary_stats_data <- append(summary_stats_data, as.numeric(names(sort(table(results_summary[,'drawn_counter']),decreasing = TRUE))[1]))
summary_stats_data <- append(summary_stats_data, median(results_summary[,'drawn_counter']))
summary_stats_data <- append(summary_stats_data, var(results_summary[,'drawn_counter']))
summary_stats_data <- append(summary_stats_data, sd(results_summary[,'drawn_counter']))
summary_stats_data <- append(summary_stats_data, sd(results_summary[,'drawn_counter']) / mean(results_summary[,'drawn_counter']) * 100)
summary_stats_data <- append(summary_stats_data, (sum(results_summary[,'drawn_counter']) / INPUT_SIZE) / sd(results_summary[,'drawn_counter'])^3)
summary_stats <- data.frame(summary_stats_data)
names(summary_stats) <- c('Draws', 'Participants amount', 'Applicants amount', 'Arithmetic mean', 'Dominant', 'Median', 'Variance', 'Standard deviation', 'Coefficient of variation', 'Asymmetry factor')


if(output_file_format=="csv"){
  write.csv(summary_stats,paste('summary_stats.csv',sep = ''),row.names = FALSE)
}else if(output_file_format=="xlsx"){
  addWorksheet(wb,sheetName='summary_stats')
  writeDataTable(wb, sheet = 2, summary_stats)
}


#create ordinal distribution plot file
png('result_summary.png',width=length(row.names(results_summary))*50, height=1000)
plot(factor(row.names(results_summary)),results_summary$drawn_counter)
dev.off()
if(output_file_format=="csv"){
  write.csv(characteristics_summary,paste('characteristics_summary.csv',sep = ''))
}else if(output_file_format=="xlsx"){
  addWorksheet(wb,sheetName='characteristics_summary')
  writeDataTable(wb, sheet = 3, characteristics_summary)
  saveWorkbook(wb, xlsx_results_filename, overwrite = TRUE)
}

print("Draw(s) completed")

