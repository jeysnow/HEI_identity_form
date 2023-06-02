faculty_cleaning <- function(raw_faculty){
  #check if inputs are correct_____
  if(!"CO_DOCENTE_IES" %in% names(raw_faculty)){
    print("faculty_cleaning received Raw faculty data that is not a 
          Census imported faculty data.table")
    stop()
  }
  
  if(raw_faculty[1,NU_ANO_CENSO]!="2019"){
    print("faculty_cleaning received Raw faculty data that is not 
          from 2019 Census")
    stop()
  }
  
  # Rename the tables for clarity--------
  names_raw <- c(
    # Identification info
    "CO_DOCENTE_IES", "CO_IES", "TP_SITUACAO",
    "TP_ESCOLARIDADE", "TP_REGIME_TRABALHO",
    
    # Activities info
    "IN_ATUACAO_EAD","IN_ATUACAO_GRAD_PRESENCIAL",
    "IN_ATUACAO_POS_EAD", "IN_ATUACAO_POS_PRESENCIAL",
    "IN_ATUACAO_EXTENSAO", "IN_ATUACAO_GESTAO",
    "IN_ATUACAO_PESQUISA", "IN_BOLSA_PESQUISA"
    )
  
  names_clean <-  c(
    #identification infos
    "faculty_code", "HEI_code", "active",
    "qualification", "employment_type", 
    
    # activities info
    "teaching_bac_dist", "teaching_bac_pres",
    "teaching_post_dist", "teaching_post_pres",
    "service_external", "service_management",
    "research", "research_w_funding"
  )
  
  clean_faculty <- raw_faculty[,..names_raw]
  setnames(clean_faculty,old= names_raw,new=names_clean)
  setkey(clean_faculty,"faculty_code")
  
  # Factor columns for later use-------
  employment_type_factors <- c(
    "full time exclusive", "full time non exclusive",
    "part time", "hourly contract"
  )
  
  qualification_factors <- c(
    "none", "bachelor", "specialist",
    "master", "doctor"
  )
  
  #Checking if conversion is needed
  if(is.numeric(clean_faculty$employment_type)){
    clean_faculty[, 
      employment_type := factor(as.factor(employment_type),
      levels =1:4, 
      labels = employment_type_factors )]
  } else{
    clean_faculty[,
      employment_type := factor(as.factor(employment_type),
      levels = employment_type_factors)]
  }
  
  #Checking if conversion is needed
  if(is.numeric(clean_faculty$qualification)){
    clean_faculty[, 
      qualification := factor(as.factor(qualification),
      levels =1:5, 
      labels = qualification_factors )]
  } else{
    clean_faculty[,
      qualification := factor(as.factor(qualification),
      levels = qualification_factors)]
  }
  

  # Convert active (1) and anything else is on leave 
  clean_faculty$active <- 1 == clean_faculty$active
  
  clean_faculty$active <- factor(
    clean_faculty$active,
    labels = c("on leave", "active"))
  
  # Convert Research_w_funding (1) and everything else is 0.
  clean_faculty$research_w_funding <- nafill(
    clean_faculty$research_w_funding, fill = 0)
  
  
  #return----
  return(clean_faculty)
}


