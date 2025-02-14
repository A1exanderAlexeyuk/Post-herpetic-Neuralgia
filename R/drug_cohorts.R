library(tidyverse)
library(Capr)
cdm <- AlexSettings::vocabularyCdm()
TCA <- 21604687L
Botulotoxin <- 729855L
Patches_with_lidocaine <- 21602020L
GABA_agonists = c('gabapentin', 'pregabalin')
anticonvulsants <- c('carbamazepine', 'oxcarbazepine', 'lamotrigine')
capsaicin <- 'capsaicin'
SNRIs  <- 'duloxetine'
Opioids <- 21604254L

objects_list <- ls()[-4]

for(obj in objects_list) {
  el <- eval(str2lang(obj))
  if (is.character(el)) {
    conceptIds <- cdm$concept |> 
      filter(
        concept_class_id == 'Ingredient' &
          tolower(concept_name) %in% el & 
          standard_concept == 'S'
          
      ) |> pull(concept_id) |> unique()
  } else {
    conceptIds <- el
  }
  coh <- cohort(
    entry = entry(
      drugExposure(cs(descendants(conceptIds), name = obj) |> 
                     AlexSettings::getCaprCsDetails(cdm)
                   ),
      primaryCriteriaLimit = 'All',
      qualifiedLimit = 'All'
    ),
    attrition = attrition(
      expressionLimit = 'All'
    ),
    exit = exit(
      endStrategy = Capr::drugExit(
        conceptSet = cs(descendants(conceptIds), name = obj),
        persistenceWindow = 30L
      )
    ))
  
  writeCohort(coh,
      fs::path(
        'cohorts', paste0(obj, '.json')
      )
    )
}
