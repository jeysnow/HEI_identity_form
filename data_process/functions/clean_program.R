program_cleaning <- function(raw_program){
  #check if inputs are correct_____
  if(!"CO_CURSO" %in% names(raw_program)){
    print("program_cleaning received raw program data that is not a 
          Census imported program data.table")
    stop()
  }
  
  if(raw_program[1,NU_ANO_CENSO]!="2019"){
    print("program_cleaning received Raw faculty data that is not 
          from 2019 Census")
    stop()
  }
  
  
  # Rename the tables for clarity--------
  
  names_raw <- c(
    # Identification info
    "CO_CURSO", "CO_IES", "TP_SITUACAO",
    "NO_CURSO", "CO_LOCAL_OFERTA",
    "CO_CINE_ROTULO",
    
    # academic info
    "TP_GRAU_ACADEMICO", "TP_MODALIDADE_ENSINO",
    "NU_CARGA_HORARIA", "NU_PERC_CARGA_SEMI_PRES",
    "QT_MATRICULA_TOTAL", "TP_ATRIBUTO_INGRESSO"
  )
  
  names_clean <-  c(
    #identification infos
    "program_code", "HEI_code", "active",
    "program_name", "campus_code", 
    "program_cine_code",
    
    # academic info
    "degree_type", "program_type",
    "course_load", "percent_dist",
    "students_enrolled", "entry_mode"
  )
  
  clean_program <- raw_program[,..names_raw]
  setnames(clean_program,old= names_raw,new=names_clean)
  setkey(clean_program,"program_code")
  
  # Factor columns for later use-------
  
  program_type_factors <- c("classroom", "distance")
  
  degree_type_factors <- c("bachelor", "license", 
                           "technology", "bachelor & license")
  
  #Checking if conversion is needed
  if(is.numeric(clean_program$program_type)){
    clean_program[, 
                  program_type := factor(as.factor(program_type),
                  levels =1:2, 
                  labels = program_type_factors )]
  } else{
    clean_program[,
                  program_type := factor(as.factor(program_type),
                  levels = program_type_factors)]
  }
  
  #Checking if conversion is needed
  if(is.numeric(clean_program$degree_type)){
    clean_program[, 
                  degree_type := factor(as.factor(degree_type),
                                          levels =1:4, 
                                          labels = degree_type_factors )]
  } else{
    clean_program[,
                  degree_type := factor(as.factor(degree_type),
                                          levels = degree_type_factors)]
  }
  
  
  # Convert entry mode (1) to base Cycle and anything
  # else is certification, as factors 3 and 4 are already 
  # accounted in degree_type.
  
  clean_program$entry_mode <- 1==clean_program$entry_mode
  
  clean_program$entry_mode <- factor(
    clean_program$entry_mode,
    labels = c("certification", "base cycle"))
  clean_program[,entry_mode := as.character(entry_mode)]
  
  # add entry mode to missing degree types and exclude entry mode
  
  clean_program[is.na(degree_type), degree_type := entry_mode] 
 
  
  clean_program[, entry_mode := NULL]
  
  
  
  # Convert active (1) and anything else is on leave 
  clean_program$active <- 1==clean_program$active
  
  clean_program$active <- factor(
    clean_program$active,
    labels = c("extinction", "active"))
  
 
  
  # check for NAs
  
  clean_program$percent_dist <- as.numeric(
    clean_program$percent_dist)
  
  
  clean_program$percent_dist[is.na(
    clean_program$percent_dist)] <- 0
  
  # Return-----
  return(clean_program)
}
