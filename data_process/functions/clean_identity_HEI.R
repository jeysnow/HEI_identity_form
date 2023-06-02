identity_HEI_cleaning <- function(raw_identity_HEI){
  #check if inputs are correct-----
  if(!"CODIGO_DA_IES"%in%names(raw_identity_HEI)){
    print("identity_HEI_cleaning received Raw identity data that is not a the PDA HEI list")
    stop()
  }
  
  # Rename the variables for clarity------
  names_raw <-c(
    # Identification infos
    "CODIGO_DA_IES", "SITUACAO_IES",
    
    # Identity information
    "COMUNITARIA",	"CONFESSIONAL",	"FILANTROPICA"
  )
  
  names_clean <-  c(
    # Identification infos
    "HEI_code", "active",
    
    # Identity information
    "communitary", "confessional", "filantropic"
  )
  
  
  clean_identity_HEI <- raw_identity_HEI[,..names_raw]
  setnames(clean_identity_HEI, old = names_raw,new = names_clean)
  
  setkey(clean_identity_HEI, "HEI_code")
  
  # Convert contents-----
  # Convert active from Ativa to TRUE
  
  clean_identity_HEI$active <- "Ativa" == clean_identity_HEI$active
  
  # Convert identities from S to TRUE
  
  clean_identity_HEI$communitary <- "S" == clean_identity_HEI$communitary
  clean_identity_HEI$confessional <- "S" == clean_identity_HEI$confessional
  clean_identity_HEI$filantropic <- "S" == clean_identity_HEI$filantropic
  
  # Return
  return(clean_identity_HEI)
}
