HEI_cleaning <- function(raw_HEI){
  
  #check if inputs are correct-----
  if(!"CO_MANTENEDORA"%in%names(raw_HEI)){
    print("HEI_cleaning received Raw HEI data that is not a Census imported HEI data.table")
    stop()
  }
  
  if(raw_HEI[1,NU_ANO_CENSO]!="2019"){
    print("HEI_cleaning received Raw HEI data that is not from 2019 Census")
    stop()
  }
  
  # Rename the tables for clarity------
  names_raw <-c(
    #identification infos
    "NU_ANO_CENSO", "CO_IES", "NO_IES","TP_CATEGORIA_ADMINISTRATIVA",
    "TP_ORGANIZACAO_ACADEMICA", "CO_MANTENEDORA", "CO_REGIAO",
    "CO_UF", "CO_MUNICIPIO", "TP_REFERENTE",
    
    #financial information
    "VL_RECEITA_PROPRIA", "VL_RECEITA_TRANSFERENCIA",
    "VL_RECEITA_OUTRA", "VL_DESPESA_PESSOAL_DOCENTE", "VL_DESPESA_PESSOAL_TECNICO",
    "VL_DESPESA_PESSOAL_ENCARGO", "VL_DESPESA_CUSTEIO", "VL_DESPESA_INVESTIMENTO",
    "VL_DESPESA_PESQUISA", "VL_DESPESA_OUTRA"
  )
  
  names_clean <-  c(
    #identification infos
    "census_year", "HEI_code", "HEI_name", "administrative_structure",
    "academic_status", "maintainer_code", "headquarters_region", 
    "headquarters_state", "headquarters_city", "institution_type",
    
    #financial information
    "own_revenue", "transferred_revenue", "other_revenue",
    "faculty_expenses", "staff_expenses", "social_contribution_expenses",
    "cost_expenses", "investment_expenses", "research_expenses", "other_expenses"
    
    
  )
  
  
  
  clean_HEI <- raw_HEI[,..names_raw]
  setnames(clean_HEI,old= names_raw,new=names_clean)
  setkey(clean_HEI,"HEI_code")
  
  # Factor columns for later use--------
  
  adm_factors <- c(
    "federally_owned","state_owned","municipally_owned",
    "private_for_profit","private_nonprofit","private_strictly",
    "special","comunity_owned","confessional")
  
  academic_factors <- c(
    "university","university_center","faculty",
    "federal_institute","federal_center")
  
  #Checking if conversion is needed
  if(is.numeric(clean_HEI$administrative_structure)){
    
    clean_HEI[, 
      administrative_structure := factor(
      as.factor(administrative_structure),
      levels =1:9, 
      labels = adm_factors )]
  } else{
    clean_HEI[,
      administrative_structure := factor(
      as.factor(administrative_structure),
      levels = adm_factors)]
  }
  
  #Checking if conversion is needed
  if(is.numeric(clean_HEI$academic_status)){
    clean_HEI[,
      academic_status := factor(
      as.factor(academic_status),
      levels =1:5, 
      labels = academic_factors 
    )]
  } else{
    clean_HEI[,academic_status := factor(
      as.factor(academic_status),
      levels = academic_factors
    )]
  }
  
  #return----
  
  return(clean_HEI)
  
}
