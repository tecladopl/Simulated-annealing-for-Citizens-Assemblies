####################################################################################
# Script using Simmulated Annealing for draw participants for Citizen's Assemblies #
# Original idea and first version of the script by PhD Nikodem Mrożek (UoG)        #
# Upgraded by Damian Kozłowski (dkozlowski@teclado.pl)                             #
####################################################################################

######################
# Settings variables #
######################

#########
#inputs file directory(default is source dir)
input_file_directory <- "."
#########

#########
#USE SETTINGS FROM CSV FILE??
csv_settings <- TRUE
#settings file name
settings_filename <- 'settings.csv'
#########

#set working directory
script.dir <- getSrcDirectory(function(x) {x})
setwd(script.dir)
setwd(input_file_directory)

############################################################
#if settings file is not used please change variables below#
############################################################
if(!csv_settings) {
  #input file name
  input_filename <- 'applicants.csv'
  #characteristics file name
  par_filename <- 'categories.csv'
  
  #assembly size
  N <- 50
  #number of draws
  draws_number <- 6
  #allow repetitions across particular draws
  #allow_duplicates = TRUE
  
  
  #max time for each draw in [ms]
  SA_script_max_time <- 3000
  #SA temperature parameter
  SA_temperature <- 50
  #Random seed integer. Negative integer value that can be set to initialize the internal random generator.
  SA_seed <- -1
  #Integer for maximum iterations count
  SA_max_iterations <- 10000
  #The program will stop when there is no any improvement in steps 
  SA_nb_stop_imp <- 1000
  #Iteration will stop when the expected evaluation function value is reached
  SA_threshold_stop <- 0
  #Maximum number of call of the objective function
  SA_max_call <- 1e7
  
  ################################################################
  #if settings file is not provided please change variables above#
  ################################################################  
}else{
  
  #load setting file
  settings <- read.csv(file = settings_filename, row.names = 1, header= TRUE)
  
  #input filename
  input_filename <- as.character(settings["input_filename","setting_value"])
  #characteristics filename
  par_filename <- as.character(settings["par_filename","setting_value"])
  
  #assembly size
  N <- as.numeric(settings["assembly_size","setting_value"])
  #number of draws
  draws_number <- as.numeric(settings["draws_number","setting_value"])
  #allow repetitions across particular draws
  #allow_duplicates = as.logical(settings["allow_duplicates","setting_value"])
  
  
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
  
}
###############


#####################
#####################


#Simulated Annealing library import
#see: https://CRAN.R-project.org/package=GenSA OR https://www.rdocumentation.org/packages/GenSA/versions/1.1.7/topics/GenSA
library(GenSA)

# Loading additional libraries
#library()



# data loading
applicants <- read.csv(file = input_filename, row.names = 1, header= TRUE)
print("Loaded applicants: ")
print(applicants)


characteristics <- read.csv(file = par_filename, row.names = 1, header= TRUE)
print("Loaded characteristics: ")
print(characteristics)

# input entries size
INPUT_SIZE <- nrow(applicants)
#number of different characteristics values
categories_number <- length(characteristics$category)
#setting up counter column
characteristics$counter <- rep(0,categories_number)
#helper vector
Nv <- 1:N


#evaluation function declaration
#v - vectors
#b - final call indicator
evaluation_function <- function(v,b,draw_no) {

  ret <- 0
  
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
      if(characteristics[k,'is_important']){
        #increase value if current characteristic is more important than others
        check_par <- check_par + 2*(characteristics[k,'counter']-characteristics[k,'amount'])^2
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
      
      #create result list file
      write.csv(cbind(applicants$LP[v],applicants$ID[v]),paste('result_',draw_no,'.csv',sep = ''),row.names = FALSE)
      #create data characteristics file
      write.csv(characteristics,paste('result_',draw_no,'_characteristics.csv',sep = ''),row.names = FALSE)
      #create ordinal distribution plot file
      png(paste('result_',draw_no,'_ordinal_dist.png',sep = ''),width=600, height=400)
      plot(v,Nv,main = paste("Ordinal number distribution - result ",draw_no,sep = ''), xlab = "Applicant's ordinal number", ylab = "Participant's ordinal number")
      dev.off()
      #create characteristics distribution plot file
      png(paste('result_',draw_no,'_characteristics.png',sep = ''),width=1000, height=400)
      barplot(characteristics$counter,names.arg=characteristics$value,main = paste("Characteristics result ",draw_no,sep = ''), xlab = "Characteristic", ylab = "Amount")
      dev.off()
      
    }
    
  }

  ret
}

#change dir for results' output files
setwd("results")

#run script for every defined draw
for(draw_no in 1:draws_number){
  #current iteration checking parameter
  check_par <- 0
  
  #simulated annealing
  RESULT <- GenSA(par=as.numeric(sample(Nv)*INPUT_SIZE/N), fn=evaluation_function, lower=as.numeric(seq(1,N)), upper=as.numeric(seq(INPUT_SIZE-N+1,INPUT_SIZE)), FALSE, draw_no, control=list(max.time=SA_script_max_time,smooth=FALSE,verbose=TRUE,temperature=SA_temperature,threshold.stop=SA_threshold_stop,max.call=SA_max_call,maxit=SA_max_iterations,nb.stop.improvement=SA_nb_stop_imp,seed=SA_seed))

  #main function
  evaluation_function(RESULT$par, TRUE, draw_no)
}


