library(tidyverse)
library(Capr)
cdm <- AlexSettings::vocabularyCdm()
# A specific diagnosis of PHN 
# (SNOMED code 2177002, 76462000); or 
# 
# A diagnosis of herpes zoster (HZ) 
# (SNOMED code 4740000) + neuropathic pain 
# (SNOMED code 247398009) developed within 6 
# months after the herpetic rash resolution in 
# the dermatome affected by HZ. 
PHN <- AlexSettings::conceptIdsFromSources(
  cdm, c(2177002, 76462000),  'SNOMED') |> 
  Capr::cs(name = 'PHN') |> 
  AlexSettings::getCaprCsDetails(cdm)
HZ <- AlexSettings::conceptIdsFromSources(
  cdm, c(4740000),'SNOMED') |> 
  Capr::cs(name = 'HZ') |> 
  AlexSettings::getCaprCsDetails(cdm)
neuropathic_pain <- AlexSettings::conceptIdsFromSources(
  cdm, c(247398009),'SNOMED') |> 
  Capr::cs(name = 'neuropathic_pain') |> 
  AlexSettings::getCaprCsDetails(cdm)

HIV <- AlexSettings::conceptIdsFromSources(
  cdm, c(86406008),'SNOMED') |> 
  Capr::cs(name = 'HIV') |> 
  AlexSettings::getCaprCsDetails(cdm)
DM <- Capr::cs(201820, name = 'DM') |> 
  AlexSettings::getCaprCsDetails(cdm)

cancer <- Capr::cs(201820,### need to change it
                   name = 'cancer') |> 
  AlexSettings::getCaprCsDetails(cdm)


PHNCohort <- cohort(
  entry = entry(
    conditionOccurrence(PHN),
    conditionOccurrence(HZ, nestedWithAny(
      atLeast(
        1,
      conditionOccurrence(neuropathic_pain),
      duringInterval(eventStarts(0, 180)))
    ))),
  attrition = attrition(
    'Exclude patients with DM, HIV, cancer' = 
      withAll(
        exactly(
          0,
          conditionOccurrence(DM),
          duringInterval(eventStarts(-Inf, 0))
          ),
        exactly(
          0,
          conditionOccurrence(HIV),
          duringInterval(eventStarts(-Inf, 0))
        ),
        exactly(
          0,
          conditionOccurrence(cancer),
          duringInterval(eventStarts(-Inf, 0))
        ),
        exactly(
          0,
          drugExposure(cancer),
          duringInterval(eventStarts(-Inf, 0))
        )
      )
    ) 
  )
PHNCohort |> writeCohort('PHN_init.json')
targetIngredients <- list(
  Anticonvulsants1 = c('gabapentin'),
  Anticonvulsants2 = c('pregabalin'),
  TCA = 'amitriptyline',
  Topical = 'capsaicin',
  patches  = 'lidocaine',
  SNRIs = 'duloxetine'
)

conditionDuloxetine <- list(
  'Depression' = 35489007, 
  'Anxiety' = 197480006, 
  'Fibromyalgia' = 203082005,
  'Diabetic polyneuropathy' = 230572002
) |> list_c() %>% 
  AlexSettings::conceptIdsFromSources(
    cdm, .,'SNOMED')
conditionDuloxetine <- 
  Capr::cs(descendants(conditionDuloxetine),
    name = 'conditions To Exclude Duloxetine') |> 
  AlexSettings::getCaprCsDetails(cdm)
writeConceptSet(conditionDuloxetine, fs::path(
  'concept_sets', 'conditionsToExcludeDuloxetine.json'
))
conditionAmi <- list(
  'Depression' = 35489007, 
  'Anxiety' = 197480006, 
  'Migraine' = 37796009, 
  'Fibromyalgia' = 203082005, 
  'Radiculopathy' = 72274001, 
  'Diabetic polyneuropathy' = 230572002,
  'Irritable Bowel Syndrome' = 10743008
) |> list_c() %>% 
  AlexSettings::conceptIdsFromSources(
    cdm, .,'SNOMED')
conditionAmi <- Capr::cs(descendants(conditionAmi),
    name = 'conditions To Exclude Amitriptyline') |> 
  AlexSettings::getCaprCsDetails(cdm)
writeConceptSet(conditionAmi, fs::path(
  'concept_sets', 'conditionsToExcludeAmi.json'
))

