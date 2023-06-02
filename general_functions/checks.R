#path checks----
check_path<-function(path,origin=""){
  if(!is.character(path)){
    print(paste(origin ," received a path that is not a character: ",path))
    stop()
  }
  return(TRUE)
}

check_file<- function(path,origin=""){
  check_path(path,origin)
  
  if(!file.exists(path)){
    print(paste(origin, ": received a path to an inexistent file: ",path))
    stop()
  }
  return(TRUE)
}

check_directory<- function(path,origin=""){
  check_path(path,origin)
  
  if(!dir.exists(path)){
    print(paste(origin, ": received a path to an inexistent directory: ",path))
    stop()
  }
  return(TRUE)
}

#Census checks-----

check_clean_data <- function(clean_data, origin=""){
  check_list(clean_data, origin = origin)
  check_HEI_clean(clean_data$HEI, origin = origin)
  check_faculty_clean(clean_data$faculty, origin = origin)
  check_program_clean(clean_data$program, origin = origin)
  check_identity_HEI_clean(clean_data$identity_HEI, origin = origin)
  
  return(TRUE)
}

check_identity_HEI_clean <- function(DT, origin=""){
  check_data_table(DT,origin = origin)
  if(!"confessional"%in%names(DT)){
    print(paste(origin,": Headers of DT do not match clean identity data"))
    stop()
  }
  return(TRUE)
}

check_faculty_clean <- function(DT, origin=""){
  check_data_table(DT,origin = origin)
  if(!"faculty_code"%in%names(DT)){
    print(paste(origin,": Headers of DT do not match clean faculty data"))
    stop()
  }
  return(TRUE)
}

check_program_clean <- function(DT, origin=""){
  check_data_table(DT,origin = origin)
  if(!"program_code"%in%names(DT)){
    print(paste(origin,": Headers of DT do not match clean program data"))
    stop()
  }
  return(TRUE)
}

check_HEI_clean <- function(DT, origin=""){
  check_data_table(DT,origin = origin)
  if(!"maintainer_code"%in%names(DT)){
    print(paste(origin,": Headers of DT do not match clean HEI data"))
    stop()
  }
  return(TRUE)
}

#data type checks-----
check_list <- function(x, origin=""){
  if(!is.list(x)) {
    print(paste(origin,": Data received was not a list"))
    stop()
  }
  return(TRUE)
}

check_data_table<-function(DT,origin=""){
  if(!is.data.table(DT)){
    print(paste(origin,": Data received was not a Data.Table"))
    stop()
  }
  return(TRUE)
}

check_factor<- function(X,origin=""){
  if(!is.factor(X)){
    print(paste(origin,": Data received was not a factor"))
    stop()
  }
  return(TRUE)
}

check_vector<- function(X,origin=""){
  if(!is.vector(X)){
    print(paste(origin,": Data received was not a vector"))
    stop()
  }
  return(TRUE)
}
check_NA<- function(X,origin=""){
  if(!anyNA(X)){
    print(paste(origin,": Data received has NA values"))
    stop()
  }
  return(TRUE)
}

check_numeric <- function(x,origin){
  if(!is.numeric(x)){
    print(paste(origin,": Data received was not numeric"))
    stop()
  }
  return(TRUE)
}

check_string <-function(STRING,origin=""){
  if(!is.character(STRING)){
    print(paste(origin ," received a value that is not a character: ",STRING))
    stop()
  }
  return(TRUE)
}

