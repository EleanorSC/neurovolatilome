# Neuropsychiatric volatilome paper screening script


#install.packages(
#  c('rentrez', 'xml2', 'dplyr', 'stringr', 'purrr', 'tibble', 'openxlsx','tidyr', 'networkD3', 'htmlwidgets')
#)

# Set the working directory
# setwd("~/repos/neuro_VOCs")


STAGE <- 1.5 # 1 = search; 1.5 = prescreen existing retrieval; 2 = extraction; 3 = validate/export
library(rentrez)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(openxlsx)
stopifnot(STAGE %in% c(1, 1.5, 2, 3))
project_dir <- path.expand('~/Documents/VOCs_project')
dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
setwd(project_dir)
dir.create('data', recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists('data'), file.access('data', 2) == 0)

path <- function(x) file.path('data', x)
write_csv <- function(x, name) write.csv(x, path(name), row.names = FALSE, na = '')
check_new <- function(f) if (file.exists(f)) stop('File already exists; archive it before regenerating: ', f)

# Search dictionaries:

disorders <- c(
  
  # Neurodegenerative diseases -----------------------------------------------
  
  # Alzheimer's disease 
  "Alzheimer's disease" = 'alzheimer',
  
  # Parkinsonian disorders and synucleinopathies
  "Parkinson's disease" = 'parkinson',
  "Dementia with Lewy bodies" = 'lewy bod(y|ies)|\\bDLB\\b',
  "Multiple system atrophy" = 'multiple system atrophy|shy.drager',
  "Progressive supranuclear palsy" = 'progressive supranuclear palsy',
  "Corticobasal degeneration" = 'corticobasal degeneration|corticobasal syndrome',
  
  # Motor neuron diseases
  "Motor neuron disease" = 'motor neurone? disease',
  "Amyotrophic lateral sclerosis" = 'amyotrophic lateral sclerosis',
  "Primary lateral sclerosis" = 'primary lateral sclerosis',
  "Progressive muscular atrophy" = 'progressive muscular atrophy',
  "Spinal muscular atrophy" = 'spinal muscular atrophy',
  
  # Huntington's disease and hereditary neurodegeneration
  "Huntington's disease" = 'huntington',
  "Spinocerebellar ataxia" = 'spinocerebellar ataxia',
  "Friedreich's ataxia" = 'friedreich',
  "Hereditary spastic paraplegia" = 'hereditary spastic paraplegia',
  
  # other major dementia subtypes
  "Vascular dementia" = 'vascular dementia|multi.infarct dementia',
  "Frontotemporal dementia" = 'frontotemporal|\\bFTD\\b|\\bFTLD\\b|pick.s disease',
  "Mixed dementia" = 'mixed dementia|mixed alzheimer',
  "Unspecified dementia" = '\\bdementia\\b',
  
  # Prion diseases
  "Creutzfeldt-Jakob disease" = 'creutzfeldt.jakob|\\bCJD\\b|\\bvCJD\\b',
  "Other prion diseases" = 'prion disease',
  
  # Central nervous system tumours + other neurological disorders
  'Brain tumours' = 'brain tumou?r|brain neoplasm|brain cancer|intracranial tumou?r|CNS tumou?r|glioma|glioblastoma|astrocytoma|oligodendroglioma|ependymoma|meningioma|medulloblastoma|ganglioglioma|craniopharyngioma|pituitary adenoma|pituitary tumou?r|schwannoma|acoustic neuroma|neurocytoma|pineoblastoma|pineocytoma|choroid plexus tumou?r|choroid plexus papilloma|haemangioblastoma|hemangioblastoma|CNS lymphoma|central nervous system lymphoma|brain metastas|cerebral metastas|intracranial metastas',
  'Epilepsy' = 'epilep|seizure|ictal',
  'Stroke' = '\\bstroke\\b|cerebrovascular|cerebral infarct|intracerebral h[ae]+morrhage|transient isch[ae]+mic attack',
  'Migraine' = 'migraine',
  'Traumatic brain injury' = 'traumatic brain injury|\\bTBI\\b|concussion',
  'Multiple sclerosis' = 'multiple sclerosis',
  'Encephalitis' = 'encephalitis',
  'Meningitis' = 'meningitis',
  
  # Prodromal states (and) cognitive impairment
  "Mild cognitive impairment" = 'mild cognitive impairment|\\bMCI\\b|\\baMCI\\b',
  "cognitive decline" = 'cognitive decline|subjective memory complaint|memory disorder',
  "Cognitive impairment" = 'cognitive impairment|cognitive dysfunction',
  
  # General neurodegenerative terminology
  "Unspecified neurodegeneration" = 'neurodegenerat|neuro.degenerat',
  
  # Psychotic disorders ----------------------------------------------------
  
  'Schizophrenia' = 'schizophren',
  'Schizoaffective disorder' = 'schizoaffective',
  'Schizotypal disorder' = 'schizotypal',
  'Psychosis' = 'psychosis|psychotic',
  
  # Depressive and mood disorders ------------------------------------------
  
  'Depression' = 'depress|\\bMDD\\b|dysthymi',
  'Bipolar disorder' = 'bipolar|manic depression|manic.depressive',
  'Mania' = '\\bmania\\b|\\bmanic\\b|hypomania',
  'Cyclothymic disorder' = 'cyclothymi',
  'Postpartum depression' = 'postpartum depression|postnatal depression',
  'Seasonal affective disorder' = 'seasonal affective disorder',
  'Premenstrual dysphoric disorder' = 'premenstrual dysphoric disorder|PMDD',
  
  # Anxiety and compulsive disorders ------------------------------------------------------
  
  'Anxiety disorders' = 'anxiety disorder',
  'Generalised anxiety disorder' = 'generali[sz]ed anxiety disorder',
  'Panic disorder' = 'panic disorder|panic attack',
  'Specific phobia' = 'specific phobia|specific phobic disorder',
  'Agoraphobia' = 'agoraphobi',
  'Obsessive-compulsive disorder' = 'obsessive.compulsive|\\bOCD\\b',
  'Body dysmorphic disorder' = 'body dysmorphic disorder|dysmorphophobia',
  'Hoarding disorder' = 'hoarding disorder',
  'Trichotillomania' = 'trichotillomania',
  
  # Trauma and stress-related disorders ------------------------------------
  
  'Post-traumatic stress disorder' = 'post.traumatic stress|\\bPTSD\\b',
  'Complex PTSD' = 'complex PTSD|complex post.traumatic stress',
  
  # Neurodevelopmental disorders -------------------------------------------
  
  'Autism spectrum disorder' = 'autis|asperger',
  'Attention deficit hyperactivity disorder' = 'attention.deficit|\\bADHD\\b',
  'Intellectual disability' = 'intellectual disabilit',
  'Global developmental delay' = 'global developmental delay',
  'Developmental language disorder' = 'developmental language disorder',
  'Developmental coordination disorder' = 'developmental coordination disorder|dyspraxia',
  'Specific neurodevelopmental learning disorders' = 'neurodevelopmental learning disorder|dyslexia|dyscalculia',
  'Tourette syndrome and tic disorders' = 'tourette|tic disorder|motor tic|vocal tic',
  'Fragile X syndrome' = 'fragile X',
  'Rett syndrome' = 'rett syndrome',
  'Down syndrome' = 'down syndrome|down.s syndrome|trisomy 21',
  'Angelman syndrome' = 'angelman',
  'Prader-Willi syndrome' = 'prader.willi',
  'Williams syndrome' = 'williams syndrome|williams.beuren',
  'Foetal alcohol spectrum disorders' = 'f[oe]+tal alcohol|\\bFASD\\b',
  
  # Personality disorders --------------------------------------------------
  
  # Cluster A
  'Paranoid personality disorder' = 'paranoid personality',
  'Schizoid personality disorder' = 'schizoid personality',
  'Schizotypal personality disorder' = 'schizotypal personality',
  
  # Cluster B
  'Borderline personality disorder' = 'borderline personality|emotionally unstable personality|\\bEUPD\\b',
  'Narcissistic personality disorder' = 'narcissistic personality',
  'Histrionic personality disorder' = 'histrionic personality',
  'Antisocial personality disorder' = 'antisocial personality|dissocial personality|\\bASPD\\b',
  
  # Cluster C
  'Avoidant personality disorder' = 'avoidant personality|anxious personality|\\bAvPD\\b',
  'Dependent personality disorder' = 'dependent personality',
  'Obsessive-compulsive personality disorder' = 'obsessive.compulsive personality|anankastic personality|\\bOCPD\\b',
  
  # Dissociative, somatic and functional disorders --------------------------
  'Dissociative identity disorder' = 'dissociative identity disorder|multiple personality disorder',
  'Functional neurological disorder' = 'functional neurological disorder|conversion disorder',
  'Fibromyalgia' = 'fibromyalgia',
  'Chronic fatigue syndrome' = 'chronic fatigue syndrome|myalgic encephalomyelitis|ME/CFS',
  
  # Neuroimmunological and inflammatory disorders ---------------------------
  
  'Autoimmune encephalitis' = 'autoimmune encephalitis',
  'Neuropsychiatric lupus' = 'neuropsychiatric lupus|neuropsychiatric systemic lupus',
  'Neurosarcoidosis' = 'neurosarcoidosis',
  
  # Neurometabolic disorders -----------------------------------------------
  
  # Inborn errors of amino acid metabolism
  'Phenylketonuria' = 'phenylketonuria|\\bPKU\\b',
  'Maple syrup urine disease' = 'maple syrup urine disease|\\bMSUD\\b|branched.chain ketoaciduria',
  'Tyrosinaemia' = 'tyrosin[ae]+mi',
  'Homocystinuria' = 'homocystinuria',
  'Non-ketotic hyperglycinaemia' = 'non.ketotic hyperglycin[ae]+mi|glycine encephalopathy',
  
  # Acquired metabolic encephalopathies -------------------------------------
  
  'Hepatic encephalopathy' = 'hepatic encephalopathy|fetor hepaticus',
  'Wernicke encephalopathy' = 'wernicke',
  'Korsakoff syndrome' = 'korsakoff',
  'Diabetic encephalopathy' = 'diabetic encephalopathy',
  
  # Organic acidaemias -----------------------------------------------------
  
  'Organic acidaemias' = 'organic acid[ae]+mi|organic aciduria',
  'Isovaleric acidaemia' = 'isovaleric acid[ae]+mi|isovaleric aciduria',
  'Propionic acidaemia' = 'propionic acid[ae]+mi',
  'Methylmalonic acidaemia' = 'methylmalonic acid[ae]+mi|methylmalonic aciduria',
  'Glutaric acidaemia' = 'glutaric acid[ae]+mi|glutaric aciduria',
  'Multiple acyl-CoA dehydrogenase deficiency' = 'multiple acyl.CoA dehydrogenase deficiency|glutaric aciduria type II',
  '3-methylcrotonyl-CoA carboxylase deficiency' = 'methylcrotonyl.CoA carboxylase deficiency',
  
  # Urea cycle disorders ---------------------------------------------------
  
  'Urea cycle disorders' = 'urea cycle disorder|urea cycle defect',
  'Ornithine transcarbamylase deficiency' = 'ornithine transcarbamylase deficiency',
  'Citrullinaemia' = 'citrullin[ae]+mi',
  'Argininosuccinic aciduria' = 'argininosuccinic aciduria',
  'Argininaemia' = 'arginin[ae]+mi',
  
  # Lysosomal storage disorders ---------------------------------------------
  
  'Gaucher disease' = 'gaucher',
  'Niemann-Pick disease' = 'niemann.pick',
  'Fabry disease' = 'fabry disease',
  'Tay-Sachs disease' = 'tay.sachs',
  'Sandhoff disease' = 'sandhoff',
  'Krabbe disease' = 'krabbe',
  'Metachromatic leukodystrophy' = 'metachromatic leukodystrophy',
  'Mucopolysaccharidoses' = 'mucopolysaccharidos',
  
  # Mitochondrial and peroxisomal disorders ---------------------------------
  
  'MELAS syndrome' = '\\bMELAS\\b|mitochondrial encephalomyopathy.*lactic acidosis',
  'Leigh syndrome' = 'leigh syndrome|leigh.s disease',
  'MERRF syndrome' = '\\bMERRF\\b|myoclonic epilepsy with ragged.red fibres',
  'Adrenoleukodystrophy' = 'adrenoleukodystrophy',
  'Zellweger spectrum disorder' = 'zellweger',
  'Refsum disease' = 'refsum',
  
  # Other neurometabolic disorders ------------------------------------------
  
  'Wilson disease' = 'wilson.s disease|wilson disease|hepatolenticular degeneration',
  'Acute intermittent porphyria' = 'acute intermittent porphyria',
  'Biotinidase deficiency' = 'biotinidase deficiency',
  'Cerebral creatine deficiency' = 'cerebral creatine deficiency|creatine transporter deficiency',
  
  # General phenotype terminology ------------------------------------------
  
  'Unspecified psychiatric disorder' = 'psychiatric disorder|mental disorder|mental illness',
  'Unspecified neurodevelopmental disorder' = 'neurodevelopmental disorder',
  'Unspecified neurometabolic disorder' = 'neurometabolic disorder|inborn error.*metabolism',
  'Cancer-related cognitive impairment' = 'cancer.related cognitive impairment|chemobrain|chemo.brain',
  'Catatonia' = 'catatoni',
  'Chronic pain' = 'chronic pain|persistent pain',
  'Delirium' = 'delirium|acute confusional state',
  'Encephalopathy' = 'encephalopath',
  'Functional seizures' = 'psychogenic non.epileptic seizure|functional seizure',
  'Illness anxiety disorder' = 'illness anxiety disorder|hypochondria',
  'Neuropathic pain' = 'neuropathic pain|neuralgia',
  'Post-COVID cognitive impairment' = 'post.covid cognitive',
  'Postoperative cognitive dysfunction' = 'postoperative cognitive dysfunction|perioperative neurocognitive',
  'Somatic symptom disorder' = 'somatic symptom disorder|somatoform'
)

specimens <- c(
  Breath = 'breath|exhaled air',
  Sweat = 'sweat|perspiration|axillary',
  Sebum = 'sebum|sebaceous',
  'Skin emissions' = 'skin volatile|skin emission|skin odou?r',
  Urine = 'urin',
  Stool = 'stool|faec|fec',
  Saliva = 'saliva',
  Blood = 'blood|plasma|serum',
  Earwax = 'earwax|cerumen',
  Semen = 'semen|seminal fluid',
  Other = '(?!)'
)

platforms <- c(
  'GC-MS' = 'GC[- ]?MS|gas chromatography.mass spectrom',
  'GC-IMS' = 'GC[- ]?IMS|gas chromatography.ion mobility',
  'GC-FID' = 'GC[- ]?FID|flame ioni[sz]ation',
  'PTR-MS' = 'PTR[- ]?(MS|TOF)|proton transfer reaction',
  'SIFT-MS' = 'SIFT[- ]?MS|selected ion flow tube',
  'Electronic nose' = 'electronic nose|e-nose|sensor array|nanosensor',
  FAIMS = 'FAIMS|field asymmetric ion mobility',
  IMS = 'ion mobility spectrom|\\bIMS\\b',
  DMS = 'differential mobility spectrom|\\bDMS\\b',
  Other = '(?!)'
)


# Phenotype classification -------------------------------------------------

groups <- c(
  'Neurodegenerative disorders',
  'Other neurological disorders',
  'Neuropsychiatric disorders',
  'Neurodevelopmental disorders',
  'Prodromal neurocognitive phenotypes',
  'Neurometabolic disorders'
)

phenotype_groups <- list(
  
  'Neurodegenerative disorders' = c(
    "Alzheimer's disease",
    "Parkinson's disease",
    "Dementia with Lewy bodies",
    "Multiple system atrophy",
    "Progressive supranuclear palsy",
    "Corticobasal degeneration",
    "Motor neuron disease",
    "Amyotrophic lateral sclerosis",
    "Primary lateral sclerosis",
    "Progressive muscular atrophy",
    "Spinal muscular atrophy",
    "Huntington's disease",
    "Spinocerebellar ataxia",
    "Friedreich's ataxia",
    "Hereditary spastic paraplegia",
    "Frontotemporal dementia",
    "Mixed dementia",
    "Unspecified dementia",
    "Creutzfeldt-Jakob disease",
    "Other prion diseases",
    "Unspecified neurodegeneration"
  ),
  
  'Other neurological disorders' = c(
    'Brain tumours',
    'Epilepsy',
    'Stroke',
    'Vascular dementia',
    'Migraine',
    'Traumatic brain injury',
    'Multiple sclerosis',
    'Encephalitis',
    'Meningitis',
    'Functional neurological disorder',
    'Functional seizures',
    'Delirium',
    'Encephalopathy',
    'Postoperative cognitive dysfunction',
    'Cancer-related cognitive impairment',
    'Post-COVID cognitive impairment',
    'Chronic pain',
    'Neuropathic pain',
    'Fibromyalgia',
    'Chronic fatigue syndrome',
    'Autoimmune encephalitis',
    'Neuropsychiatric lupus',
    'Neurosarcoidosis'
  ),
  
  'Neuropsychiatric disorders' = c(
    # Psychotic disorders
    'Schizophrenia',
    'Schizoaffective disorder',
    'Schizotypal disorder',
    'Psychosis',
    
    # Mood disorders
    'Depression',
    'Bipolar disorder',
    'Mania',
    'Cyclothymic disorder',
    'Postpartum depression',
    'Seasonal affective disorder',
    'Premenstrual dysphoric disorder',
    
    # Anxiety and compulsive disorders
    'Anxiety disorders',
    'Generalised anxiety disorder',
    'Panic disorder',
    'Specific phobia',
    'Agoraphobia',
    'Obsessive-compulsive disorder',
    'Body dysmorphic disorder',
    'Hoarding disorder',
    'Trichotillomania',
    
    # Trauma and stress-related disorders
    'Post-traumatic stress disorder',
    'Complex PTSD',
    
    # Personality disorders
    'Paranoid personality disorder',
    'Schizoid personality disorder',
    'Schizotypal personality disorder',
    'Borderline personality disorder',
    'Narcissistic personality disorder',
    'Histrionic personality disorder',
    'Antisocial personality disorder',
    'Avoidant personality disorder',
    'Dependent personality disorder',
    'Obsessive-compulsive personality disorder',
    
    # Dissociative and somatic disorders
    'Dissociative identity disorder',
    'Somatic symptom disorder',
    'Illness anxiety disorder',
    'Catatonia',
    'Unspecified psychiatric disorder'
  ),
  
  'Neurodevelopmental disorders' = c(
    'Autism spectrum disorder',
    'Attention deficit hyperactivity disorder',
    'Intellectual disability',
    'Global developmental delay',
    'Developmental language disorder',
    'Developmental coordination disorder',
    'Specific neurodevelopmental learning disorders',
    'Tourette syndrome and tic disorders',
    'Fragile X syndrome',
    'Rett syndrome',
    'Down syndrome',
    'Angelman syndrome',
    'Prader-Willi syndrome',
    'Williams syndrome',
    'Foetal alcohol spectrum disorders',
    'Unspecified neurodevelopmental disorder'
  ),
  
  'Prodromal neurocognitive phenotypes' = c(
    'Mild cognitive impairment',
    'cognitive decline',
    'Cognitive impairment'
  ),
  
  'Neurometabolic disorders' = c(
    # Amino acid metabolism
    'Phenylketonuria',
    'Maple syrup urine disease',
    'Tyrosinaemia',
    'Homocystinuria',
    'Non-ketotic hyperglycinaemia',
    
    # Acquired metabolic encephalopathies
    'Hepatic encephalopathy',
    'Wernicke encephalopathy',
    'Korsakoff syndrome',
    'Diabetic encephalopathy',
    
    # Organic acidaemias
    'Organic acidaemias',
    'Isovaleric acidaemia',
    'Propionic acidaemia',
    'Methylmalonic acidaemia',
    'Glutaric acidaemia',
    'Multiple acyl-CoA dehydrogenase deficiency',
    '3-methylcrotonyl-CoA carboxylase deficiency',
    
    # Urea cycle disorders
    'Urea cycle disorders',
    'Ornithine transcarbamylase deficiency',
    'Citrullinaemia',
    'Argininosuccinic aciduria',
    'Argininaemia',
    
    # Lysosomal storage disorders
    'Gaucher disease',
    'Niemann-Pick disease',
    'Fabry disease',
    'Tay-Sachs disease',
    'Sandhoff disease',
    'Krabbe disease',
    'Metachromatic leukodystrophy',
    'Mucopolysaccharidoses',
    
    # Mitochondrial and peroxisomal disorders
    'MELAS syndrome',
    'Leigh syndrome',
    'MERRF syndrome',
    'Adrenoleukodystrophy',
    'Zellweger spectrum disorder',
    'Refsum disease',
    
    # Other neurometabolic disorders
    'Wilson disease',
    'Acute intermittent porphyria',
    'Biotinidase deficiency',
    'Cerebral creatine deficiency',
    'Unspecified neurometabolic disorder'
  )
)


# Phenotype-to-group lookup -------------------------------------------------

phenotype_lookup <- tibble(
  Disorder = unlist(phenotype_groups, use.names = FALSE),
  Phenotype_group = rep(
    names(phenotype_groups),
    lengths(phenotype_groups)
  )
)

# Check for duplicate dictionary entries
duplicates <- unique(names(disorders)[duplicated(names(disorders))])

if (length(duplicates)) {
  stop(
    'Duplicate disorder entries: ',
    paste(duplicates, collapse = '; ')
  )
}

# Check for disorders assigned to multiple groups
stopifnot(!anyDuplicated(phenotype_lookup$Disorder))

# Check that all dictionary phenotypes have been assigned a group
unclassified <- setdiff(
  names(disorders),
  phenotype_lookup$Disorder
)

if (length(unclassified)) {
  stop(
    'Unclassified disorders: ',
    paste(unclassified, collapse = '; ')
  )
}

# Check for lookup entries absent from the search dictionary
undefined <- setdiff(
  phenotype_lookup$Disorder,
  names(disorders)
)

if (length(undefined)) {
  stop(
    'Undefined disorders: ',
    paste(undefined, collapse = '; ')
  )
}

# Summarise dictionary coverage
phenotype_lookup %>%
  count(Phenotype_group, name = 'N_phenotypes') %>%
  print()


# Classification functions -------------------------------------------------

classify <- function(x, dict) {
  
  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }
  
  z <- names(dict)[
    str_detect(
      x,
      regex(unname(dict), ignore_case = TRUE)
    )
  ]
  
  if (length(z)) {
    paste(unique(z), collapse = '; ')
  } else {
    NA_character_
  }
}


classify_group <- function(x, lookup = phenotype_lookup) {
  
  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }
  
  matched_disorders <- trimws(
    strsplit(x, ';', fixed = TRUE)[[1]]
  )
  
  matched_groups <- unique(
    lookup$Phenotype_group[
      lookup$Disorder %in% matched_disorders
    ]
  )
  
  # Preserve predefined group ordering
  matched_groups <- groups[groups %in% matched_groups]
  
  if (length(matched_groups)) {
    paste(matched_groups, collapse = '; ')
  } else {
    NA_character_
  }
}


# Offline dictionary smoke tests (candidate matches, not eligibility decisions).
invisible(lapply(disorders, function(p) str_detect('', regex(p, ignore_case = TRUE))))
stopifnot(
  is.na(classify('GC-MS was used.', disorders)),
  grepl('Other neurological disorders', classify_group(classify('Epilepsy', disorders)), fixed = TRUE),
  grepl('Neurometabolic disorders', classify_group(classify('Maple syrup urine disease', disorders)), fixed = TRUE)
)

# PubMed search block constructor ------------------------------------------

block <- function(terms, mesh = character()) {
  
  paste0(
    '(',
    paste(
      c(
        if (length(terms)) paste0('"', terms, '"[Title/Abstract]') else character(),
        if (length(mesh)) if (length(mesh)) paste0('"', mesh, '"[MeSH Terms]') else character() else character()
      ),
      collapse = ' OR '
    ),
    ')'
  )
}


voc <- c(
  'volatile organic compound*',
  'volatile compound*',
  'volatile metabolite*',
  'volatile biomarker*',
  'VOC',
  'VOCs',
  'volatilome',
  'volatilomics',
  'breathomics',
  'breath metabolom*',
  'breath analys*',
  'breath profil*',
  'exhaled breath',
  'breath fingerprint*',
  'breathprint*',
  'electronic nose',
  'electronic noses',
  'e-nose',
  'chemical fingerprint*',
  'volatile fingerprint*',
  'body odor*',
  'body odour*',
  'human odor*',
  'human odour*',
  'skin volatile*',
  'sebum volatile*',
  'urinary volatile*',
  'fecal volatile*',
  'faecal volatile*',
  'axillary odor*',
  'axillary odour*',
  'chemosignal*',
  'fetor hepaticus',
  'sotolon',
  'phenylacetate',
  'musty odor',
  'musty odour',
  'maple syrup odor',
  'maple syrup odour'
)
brain_general <- c(
  'neuropsychiatr*',
  'neurologic*',
  'psychiatric',
  'neurodegenerat*',
  'brain health',
  'mental health',
  'schizophrenia',
  'psychosis',
  'psychotic',
  'depress*',
  'bipolar',
  'anxiety',
  'post-traumatic stress',
  'PTSD',
  'obsessive compulsive',
  'autis*',
  'ADHD',
  'attention deficit',
  'Parkinson*',
  'Alzheimer*',
  'dementia',
  'mild cognitive impairment',
  'cognitive decline',
  'cognitive impairment',
  'multiple sclerosis',
  'motor neuron disease',
  'motor neurone disease',
  'amyotrophic lateral sclerosis',
  'Huntington*',
  'epilep*',
  'migraine*',
  'stroke',
  'traumatic brain injury',
  'hepatic encephalopathy',
  'phenylketonuria',
  'maple syrup urine disease',
  'inborn errors of metabolism'
)

# Literal PubMed terms, not R regular expressions. Include all named phenotypes.
brain_labels <- setdiff(names(disorders), c(
  'Unspecified dementia', 'Unspecified neurodegeneration',
  'Unspecified psychiatric disorder', 'Unspecified neurodevelopmental disorder',
  'Unspecified neurometabolic disorder', 'Tourette syndrome and tic disorders',
  'Specific neurodevelopmental learning disorders', 'Other prion diseases'
))
brain_synonyms <- c(
  'neurodevelopmental disorder*', 'neurometabolic disorder*', 'prion disease*',
  'frontotemporal lobar degeneration', 'Pick disease', 'corticobasal syndrome',
  'Lewy bod*', 'multi-infarct dementia', 'seizure*', 'ictal', 'concussion',
  'cerebrovascular accident', 'cerebral infarct*', 'transient ischemic attack',
  'transient ischaemic attack', 'brain tumor*', 'brain tumour*', 'brain neoplasm*',
  'brain cancer', 'glioma*', 'glioblastoma*', 'astrocytoma*', 'oligodendroglioma*',
  'ependymoma*', 'meningioma*', 'medulloblastoma*', 'ganglioglioma*',
  'craniopharyngioma*', 'pituitary adenoma*', 'pituitary tumor*', 'schwannoma*',
  'acoustic neuroma*', 'neurocytoma*', 'pineoblastoma*', 'pineocytoma*',
  'choroid plexus tumor*', 'choroid plexus tumour*', 'choroid plexus papilloma*',
  'hemangioblastoma*', 'haemangioblastoma*', 'central nervous system lymphoma',
  'brain metastas*', 'cerebral metastas*', 'intracranial metastas*',
  'conversion disorder', 'psychogenic non-epileptic seizure*',
  'myalgic encephalomyelitis', 'dysthymi*', 'manic depression', 'hypomania',
  'emotionally unstable personality', 'dissocial personality', 'anankastic personality',
  'multiple personality disorder', 'somatoform disorder*', 'Asperger*',
  'Tourette syndrome', 'tic disorder*', 'dyspraxia', 'dyslexia', 'dyscalculia',
  'trisomy 21', 'fetal alcohol', 'foetal alcohol', 'subjective cognitive decline',
  'subjective memory complaint*', 'memory impairment', 'cognitive dysfunction',
  'branched-chain ketoaciduria', 'organic acidemia*', 'organic aciduria*',
  'isovaleric acidemia', 'propionic acidemia', 'methylmalonic acidemia',
  'glutaric aciduria', 'tyrosinemia', 'hyperglycinemia', 'citrullinemia',
  'argininemia', 'hepatolenticular degeneration', 'lysosomal storage disorder*',
  'mitochondrial disorder*', 'peroxisomal disorder*', 'MELAS', 'MERRF', 'MSUD', 'PKU'
)
brain <- unique(c(brain_general, brain_labels, brain_synonyms))
brain_mesh <- c('Mental Disorders', 'Nervous System Diseases', 'Cognition Disorders',
                'Neurodegenerative Diseases', 'Neurodevelopmental Disorders',
                'Brain Neoplasms', 'Metabolism, Inborn Errors', 'Hepatic Encephalopathy')
query <- paste(block(voc, 'Volatile Organic Compounds'), 'AND', block(brain, brain_mesh))

# Workbook helper: filters, frozen header, dropdowns, readable widths.
book <- function(tables, file, dropdowns = list()) {
  check_new(file)
  wb <- createWorkbook()
  for (s in names(tables)) {
    x <- tables[[s]]
    addWorksheet(wb, s, gridLines = FALSE)
    writeData(wb, s, x, withFilter = nrow(x) > 0, headerStyle = createStyle(fgFill = '#D9EAF7', textDecoration = 'bold'))
    freezePane(wb, s, firstActiveRow = 2, firstActiveCol = 2)
    setColWidths(wb, s, cols = seq_along(x), widths = 20)
    long <- which(names(x) %in% c('Title', 'Abstract', 'Notes', 'Source_location', 'Final_reason', 'Instructions'))
    if (length(long)) setColWidths(wb, s, cols = long, widths = 65)
    for (field in names(dropdowns[[s]])) {
      j <- match(field, names(x))
      if (!is.na(j)) dataValidation(
        wb,
        s,
        cols = j,
        rows = 2:max(1000, nrow(x) + 1),
        type = 'list',
        value = paste0('"', paste(dropdowns[[s]][[field]], collapse = ','), '"')
      )
    }
  }
  saveWorkbook(wb, file, overwrite = FALSE)
}
read_sheet <- function(file, sheet) {
  x <- read.xlsx(file, sheet = sheet, skipEmptyRows = TRUE, check.names = FALSE)
  if (is.null(x)) return(tibble())
  x
}
blank <- function(x) is.na(x) | trimws(as.character(x)) == ''
require_fields <- function(x, fields, label) {
  if (!all(fields %in% names(x))) stop(label, ': missing columns')
  for (f in fields) if (any(blank(x[[f]]))) stop(label, ': missing values in ', f)
}

# Screening attrition Sankey ------------------------------------------------
# Each PMID contributes exactly one unit of flow. Gate order is operational,
# not a reconstruction of the chronological title/abstract and full-text steps.
# Stage 2: screening decisions; Stage 3: validated extraction and group coverage.

screening_sankey <- function(s, assays = NULL) {
  require_fields(s, c('PMID', 'Final_verdict'), 'Screening')
  stopifnot(!anyDuplicated(s$PMID),
            all(s$Final_verdict %in% c('Include', 'Exclude', 'Not retrieved')))
  excluded <- s[s$Final_verdict == 'Exclude', , drop = FALSE]
  require_fields(excluded, c('Final_gate', 'Final_reason'), 'Exclusions')
  stopifnot(all(excluded$Final_gate %in% paste('Gate', 1:4)))
  retrieved <- nrow(s)
  included <- sum(s$Final_verdict == 'Include')
  unavailable <- sum(s$Final_verdict == 'Not retrieved')
  losses <- as.integer(table(factor(excluded$Final_gate, levels = paste('Gate', 1:4))))
  stopifnot(retrieved == included + unavailable + sum(losses))
  
  gate_labels <- c('Study design', 'Relevant phenotype', 'VOC measurement', 'Relevant analysis')
  links <- tibble(Source = character(), Target = character(), N = integer(), Flow = character())
  add <- function(source, target, n, flow) {
    if (n > 0) links <<- bind_rows(links, tibble(Source = source, Target = target, N = as.integer(n), Flow = flow))
  }
  label <- function(x, n) paste0(x, ' (n = ', n, ')')
  start <- label('PubMed publications retrieved', retrieved)
  current <- label('Screening decisions completed', retrieved)
  add(start, current, retrieved, 'Retained')
  remaining <- retrieved
  for (i in seq_along(losses)) {
    add(current, label(paste0('Excluded: Gate ', i, ' - ', gate_labels[i]), losses[i]), losses[i], 'Excluded')
    remaining <- remaining - losses[i]
    next_node <- label(paste0('Remaining after Gate ', i), remaining)
    add(current, next_node, remaining, 'Retained')
    current <- next_node
  }
  add(current, label('Full text not retrieved', unavailable), unavailable, 'Not retrieved')
  final <- label('Included in review', included)
  add(current, final, included, 'Included')
  
  if (!is.null(assays)) {
    # A publication can cover several groups. Use mutually exclusive branches
    # to conserve publication counts; export overlapping group totals separately.
    membership <- assays %>% distinct(Study_ID, Phenotype_group) %>%
      group_by(Study_ID) %>% summarise(
        Branch = if (n() == 1L) first(Phenotype_group) else 'Multiple phenotype groups',
        .groups = 'drop')
    stopifnot(setequal(membership$Study_ID, paste0('VOC_', s$PMID[s$Final_verdict == 'Include'])))
    branches <- count(membership, Branch, name = 'N')
    stopifnot(sum(branches$N) == included)
    for (i in seq_len(nrow(branches))) {
      add(final, label(branches$Branch[i], branches$N[i]), branches$N[i], 'Included')
    }
    group_totals <- tibble(Phenotype_group = groups) %>%
      left_join(assays %>% distinct(Study_ID, Phenotype_group) %>% count(Phenotype_group, name = 'N_publications'), by = 'Phenotype_group') %>%
      mutate(N_publications = coalesce(N_publications, 0L))
    write_csv(group_totals, 'Sankey_phenotype_group_counts.csv')
    write_csv(branches, 'Sankey_exclusive_group_counts.csv')
  }
  nodes <- tibble(Name = unique(c(links$Source, links$Target)))
  edges <- links %>% mutate(source = match(Source, nodes$Name) - 1L,
                            target = match(Target, nodes$Name) - 1L)
  colours <- 'd3.scaleOrdinal().domain(["Retained","Excluded","Not retrieved","Included"]).range(["#5079A5","#BA7373","#A0A0A0","#438B88"])'
  diagram <- networkD3::sankeyNetwork(
    Links = as.data.frame(edges), Nodes = as.data.frame(nodes),
    Source = 'source', Target = 'target', Value = 'N', NodeID = 'Name',
    LinkGroup = 'Flow', colourScale = htmlwidgets::JS(colours),
    fontSize = 12, nodeWidth = 20, nodePadding = 18,
    sinksRight = FALSE, width = 1400, height = 650, margin = list(right = 280))
  suffix <- if (is.null(assays)) 'screening' else 'final'
  # Non-self-contained HTML avoids requiring Pandoc; keep its dependency folder.
  htmlwidgets::saveWidget(diagram, path(paste0('Sankey_', suffix, '.html')), selfcontained = FALSE)
  write_csv(links, paste0('Sankey_', suffix, '_flows.csv'))
  write_csv(tibble(Stage = c('Retrieved', paste('Excluded at Gate', 1:4), 'Not retrieved', 'Included'),
                   N = c(retrieved, losses, unavailable, included)), 'Sankey_attrition_counts.csv')
  message('Screening: ', retrieved, ' retrieved; ', included, ' included; ', sum(losses), ' excluded; ', unavailable, ' not retrieved.')
  invisible(diagram)
}

if (STAGE == 1L) {
  out <- path('PubMed_manual_screening.xlsx')
  check_new(out)
  writeLines(query, path('PubMed_search_strategy.txt'))
  # Partition the phenotype OR block; the union preserves the original query.
  chunks <- split(brain, ceiling(seq_along(brain) / 12))
  queries <- c(vapply(chunks, function(x) paste(block(voc, 'Volatile Organic Compounds'),
                                                'AND', block(x)), character(1)),
               paste(block(voc, 'Volatile Organic Compounds'), 'AND', block(character(), brain_mesh)))
  search_log <- list()
  search_parts <- list()
  search_part <- function(q, id, lo = NULL, hi = NULL) {
    term <- if (is.null(lo)) q else paste0('(', q, ') AND (', lo, ':', hi, '[dp])')
    Sys.sleep(.4)
    result <- entrez_search(db = 'pubmed', term = term, retmax = 0, use_history = TRUE)
    n <- as.integer(result$count)
    message(id, ': ', n, ' records')
    if (n > 9999L) {
      if (is.null(lo)) { lo <- 1000L; hi <- 3000L }
      if (lo >= hi) stop('More than 9999 results in one year: ', id)
      mid <- floor((lo + hi)/2)
      before <- names(search_log)
      search_part(q, paste0(id, 'a'), lo, mid)
      search_part(q, paste0(id, 'b'), mid + 1L, hi)
      added <- setdiff(names(search_log), before)
      if (sum(vapply(search_log[added], function(x) x$Count, integer(1))) != n)
        stop('Date partition counts do not reconcile: ', id)
      return(invisible(NULL))
    }
    search_log[[id]] <<- tibble(Database = 'PubMed', Search_date = Sys.Date(),
                                Partition = id, Count = n, Query = term,
                                Translation = if (is.null(result$QueryTranslation)) NA_character_ else result$QueryTranslation)
    search_parts[[id]] <<- result
  }
  for (i in seq_along(queries)) search_part(queries[[i]], sprintf('Q%03d', i))
  write_csv(bind_rows(search_log), 'Search_log.csv')
  get <- function(node, xp) {
    a <- xml_find_first(node, xp)
    if (inherits(a, 'xml_missing')) NA_character_ else xml_text(a, trim = TRUE)
  }
  many <- function(node, xp) paste(xml_text(xml_find_all(node, xp), trim = TRUE), collapse = '; ')
  parse <- function(a) {
    yr <- coalesce(
      get(a, './/JournalIssue/PubDate/Year'),
      get(a, './/ArticleDate/Year'),
      get(a, './/Book/PubDate/Year'),
      str_extract(get(a, './/Book/PubDate/MedlineDate'), '[0-9]{4}'),
      str_extract(get(a, './/JournalIssue/PubDate/MedlineDate'), '[0-9]{4}')
    )
    tibble(
      PMID = coalesce(get(a, './MedlineCitation/PMID'), get(a, './BookDocument/PMID')),
      DOI = get(a, './/ArticleId[@IdType="doi"]'),
      First_author = get(a, './/AuthorList/Author[1]/LastName'),
      Year = yr,
      Journal = coalesce(get(a, './/Article/Journal/Title'), get(a, './/Book/BookTitle')),
      Title = coalesce(get(a, './/ArticleTitle'), get(a, './/Book/BookTitle')),
      Record_type = xml_name(a),
      Abstract = many(a, './/Abstract/AbstractText'),
      Publication_types = many(a, './/PublicationType'),
      MeSH_terms = many(a, './/MeshHeading/DescriptorName')
    )
  }
  dir.create(path('xml'), showWarnings = FALSE)
  retrieved_parts <- map(names(search_parts), function(id) {
    search <- search_parts[[id]]
    n <- as.integer(search$count)
    if (n == 0L) return(NULL)
    batches <- map(
      seq(0, n-1, by = 200),
      function(offset) {
        message('Retrieving ', offset + 1, '–', min(offset + 200, n))
        result <- NULL
        for (attempt in 1:3) {
          result <- tryCatch(
            entrez_fetch(
              db = 'pubmed',
              web_history = search$web_history,
              rettype = 'xml',
              retmode = 'xml',
              retstart = offset,
              retmax = min(200, n-offset)
            ),
            error = function(e) {
              message(conditionMessage(e))
              NULL
            }
          )
          if (!is.null(result)) break
          Sys.sleep(attempt * 2)
        }
        if (is.null(result)) stop('Batch failed at ', offset)
        writeLines(result, path(sprintf('xml/%s_batch_%06d.xml', id, offset)))
        Sys.sleep(.4)
        map_dfr(xml_find_all(read_xml(result), './/PubmedArticle | .//PubmedBookArticle'), parse)
      }
    )
    part <- bind_rows(batches)
    message(id, ": expected ", n, "; parsed ", nrow(part), "; missing PMIDs ", sum(blank(part$PMID)), "; duplicate PMIDs ", sum(duplicated(part$PMID)))
    if (nrow(part) != n || any(blank(part$PMID)) || anyDuplicated(part$PMID))
      stop('Retrieval integrity failure in ', id)
    part$Search_partition <- id
    part
  })
  all_records <- bind_rows(retrieved_parts)
  if (!nrow(all_records)) stop('No PubMed publications retrieved.')
  membership <- all_records %>% distinct(PMID, Search_partition)
  write_csv(membership, 'PubMed_partition_membership.csv')
  records <- all_records %>% distinct(PMID, .keep_all = TRUE) %>% select(-Search_partition)
  write_csv(tibble(Raw_hits = nrow(all_records), Unique_PMIDs = nrow(records),
                   Duplicate_hits = nrow(all_records) - nrow(records)), 'PubMed_retrieval_summary.csv')
  message('Unique PubMed publications: ', nrow(records))
  screening <- records %>%
    mutate(
      search_text = paste(coalesce(Title, ''), coalesce(Abstract, ''), coalesce(MeSH_terms, '')),
      Candidate_disorder = map_chr(search_text, classify, dict = disorders),
      Candidate_phenotype_group = map_chr(Candidate_disorder, classify_group),
      Candidate_biospecimen = map_chr(search_text, classify, dict = specimens),
      Candidate_platform = map_chr(search_text, classify, dict = platforms),
      flag_review = str_detect(Publication_types, regex('review|meta.analysis|editorial|comment', ignore_case = TRUE)),
      flag_animal = str_detect(search_text, regex('\\bmice\\b|\\bmouse\\b|\\brats?\\b|murine|rodent', ignore_case = TRUE)),
      flag_exposure = str_detect(
        search_text,
        regex('ambient air|air pollution|occupational exposure|solvent exposure', ignore_case = TRUE)
      ),
      flag_olfaction = str_detect(
        search_text,
        regex(
          'olfactory dysfunction|olfactory threshold|odou?r identification|hyposmia|anosmia',
          ignore_case = TRUE
        )
      ),
      Suggested_gate = case_when(flag_review | flag_animal ~ 'Gate 1', flag_exposure | flag_olfaction ~ 'Gate 3', TRUE ~ ''),
      Suggested_reason = case_when(
        flag_review ~ 'Possible non-primary publication',
        flag_animal ~ 'Possible animal study',
        flag_exposure ~ 'Possible exposure study',
        flag_olfaction ~ 'Possible olfactory function study',
        TRUE ~ ''
      ),
      Priority_score = as.integer(!is.na(Candidate_disorder)) + as.integer(!is.na(Candidate_biospecimen)) + as.integer(!is.na(Candidate_platform)),
      Reference = paste(First_author, 'et al.', Year)
    ) %>%
    arrange(Suggested_gate != '', desc(Priority_score), desc(Year))
  write_csv(screening, 'PubMed_screening_records.csv')
  manual <- screening %>%
    select(-search_text) %>%
    mutate(
      Reviewer_1 = '',
      Reviewer_2 = '',
      Title_abstract_verdict = '',
      Full_text_verdict = '',
      Final_verdict = '',
      Final_gate = '',
      Final_reason = '',
      Notes = ''
    )
  gates <- tibble(
    Gate = paste('Gate', 1:4),
    Instructions = c(
      'Original empirical human research; secondary analyses allowed.',
      'Investigates a phenotype within at least one of the six predefined phenotype groups; standalone emotional states are outside scope.',
      'Human-derived VOCs or instrument-measured volatile signatures; not exposure-only or olfactory function-only.',
      'Relevant phenotype–VOC analysis or descriptive empirical evidence; null results eligible.'
    )
  )
  decisions <- c('Include', 'Exclude', 'Unsure')
  book(
    list(Screening = manual, Eligibility = gates),
    out,
    list(
      Screening = list(
        Reviewer_1 = decisions,
        Reviewer_2 = decisions,
        Title_abstract_verdict = decisions,
        Full_text_verdict = c(decisions, 'Not retrieved', 'Not required'),
        Final_verdict = c('Include', 'Exclude', 'Not retrieved'),
        Final_gate = paste('Gate', 1:4)
      )
    )
  )
  # Optional positive-control reference set with PMID column
  if (file.exists(path('Known_eligible_studies.csv'))) {
    sentinel <- read.csv(path('Known_eligible_studies.csv'), colClasses = 'character')
    sentinel$Retrieved <- sentinel$PMID %in% records$PMID
    write_csv(sentinel, 'Search_validation.csv')
  }
}


if (STAGE == 1.5) {
  records <- read.csv(path('PubMed_screening_records.csv'), colClasses='character', check.names=FALSE)
  require_fields(records, 'PMID', 'Retrieved records')
  stopifnot(!anyDuplicated(records$PMID), nrow(records)>0)
  N <- nrow(records)
  # Exact semicolon-separated PubMed metadata labels, not substring matches.
  has_label <- function(x, labels) {
    vapply(strsplit(coalesce(x, ''), ';', fixed=TRUE), function(z)
      any(tolower(trimws(z)) %in% tolower(labels)), logical(1))
  }
  match_text <- function(x, pattern) str_detect(x, regex(pattern, ignore_case=TRUE))
  present <- function(x) !is.na(x) & nzchar(trimws(x))
  nonprimary <- c('Review','Systematic Review','Meta-Analysis','Editorial','Comment','News','Published Erratum','Retraction of Publication','Study Protocol')
  primary <- c('Clinical Trial','Randomized Controlled Trial','Controlled Clinical Trial','Observational Study','Comparative Study','Case Reports','Validation Study','Evaluation Study','Clinical Study','Clinical Trial, Phase I','Clinical Trial, Phase II','Clinical Trial, Phase III','Clinical Trial, Phase IV','Pragmatic Clinical Trial')
  human_pattern <- '\\b(patient[s]?|participant[s]?|volunteer[s]?|healthy controls?|human (subjects?|samples?|breath|sweat|sebum|urine)|clinical cohorts?|case.control|cross.sectional|clinical trial)\\b'
  empirical_pattern <- '\\b(recruited|enrolled|collected|sampled|measured|quantified|analysed|analyzed|detected|compared|concentrations?|abundance|chromatography|mass spectrometry|sensor arrays?|sensitivity|specificity|classification accuracy)\\b'
  
  prescreen <- records %>% mutate(
    text_for_screening = paste(coalesce(Title,''), coalesce(Abstract,'')),
    Has_abstract = present(Abstract),
    Non_primary_type = has_label(Publication_types, nonprimary),
    Primary_type = has_label(Publication_types, primary),
    Human_MeSH = has_label(MeSH_terms, 'Humans'),
    Animal_MeSH = has_label(MeSH_terms, c('Animals','Mice','Rats','Dogs','Rabbits','Swine','Zebrafish','Primates','Cattle','Sheep')),
    Human_text = match_text(text_for_screening, human_pattern),
    Empirical_text = match_text(text_for_screening, empirical_pattern),
    Specimen_detected = present(Candidate_biospecimen),
    Platform_detected = present(Candidate_platform),
    Prescreen_status = case_when(
      Non_primary_type & !Primary_type ~ 'Likely non-primary',
      Animal_MeSH & !Human_MeSH & !Human_text ~ 'Likely animal-only',
      (Human_MeSH | Human_text) & (Empirical_text | Primary_type) ~ 'Likely primary human research',
      TRUE ~ 'Uncertain'),
    High_priority = Prescreen_status == 'Likely primary human research' & Specimen_detected & Platform_detected,
    Retain_S1 = Prescreen_status != 'Likely non-primary',
    Retain_S2 = Retain_S1 & Prescreen_status != 'Likely animal-only',
    Retain_S3 = Prescreen_status == 'Likely primary human research'
  ) %>% select(-text_for_screening)
  
  measurement_terms <- c('chromatograph','spectrometr','spectroscop','sensor','electronic nose','headspace','chemical analys','chemical profil','volatile profil','volatile compound','volatile organic','volatile metabolite','volatile emission','volatile signature','compound identification','compound concentration','breathprint','breathomics','volatilom','gas analys','solid.phase microextraction','thermal desorption','\\bSPME\\b','\\bGC\\b','\\bGC.MS\\b','\\bPTR.MS\\b','\\bSIFT.MS\\b')
  prescreen <- prescreen %>% mutate(
    Measurement_evidence = match_text(paste(Title, Abstract), paste(measurement_terms, collapse='|')),
    Screening_tier = case_when(
      Retain_S3 & Specimen_detected & Platform_detected ~ 1L,
      Retain_S3 & Specimen_detected & Measurement_evidence ~ 2L,
      Retain_S3 & Specimen_detected ~ 3L,
      Retain_S3 ~ 4L,
      Prescreen_status == 'Uncertain' ~ 5L,
      TRUE ~ 6L))
  
  counts <- prescreen %>% count(Screening_tier, name='N')
  print(counts)
  summary <- tibble(Scenario=c('Original','Remove likely non-primary','Also remove likely animal-only','Positive human empirical evidence','Priority tiers 1 and 2','Named platform and specimen'), Retained=c(N,sum(prescreen$Retain_S1),sum(prescreen$Retain_S2),sum(prescreen$Retain_S3),sum(prescreen$Screening_tier<=2),sum(prescreen$Screening_tier==1))) %>% mutate(Removed=N-Retained, Reduction_percent=round(100*Removed/N,1))
  print(summary)
  original <- path('PubMed_manual_screening.xlsx')
  manual <- read_sheet(original,'Screening')
  manual$PMID <- as.character(manual$PMID)
  stopifnot(!anyDuplicated(manual$PMID),setequal(manual$PMID,prescreen$PMID))
  flags <- prescreen %>% select(PMID, Prescreen_status, Human_MeSH, Animal_MeSH, Human_text, Empirical_text, Specimen_detected, Platform_detected, Measurement_evidence, Screening_tier)
  manual <- manual %>% select(-any_of(setdiff(names(flags),'PMID'))) %>% left_join(flags,by='PMID') %>% arrange(Screening_tier)
  set.seed(2026)
  audit <- manual %>% filter(Screening_tier>2) %>% group_by(Screening_tier) %>% group_modify(~slice_sample(.x,n=min(30L,nrow(.x)))) %>% ungroup() %>% select(PMID,Screening_tier,Title,Abstract) %>% mutate(Audit_decision='',Audit_reason='')
  tables <- list(Screening=manual, Eligibility=read_sheet(original,'Eligibility'), Prescreen_summary=summary, Tiers=counts, Audit_sample=audit)
  if(file.exists(path('Known_eligible_studies.csv'))) {
    known <- read.csv(path('Known_eligible_studies.csv'),colClasses='character')
    tables$Known_eligible <- distinct(known,PMID) %>% left_join(flags,by='PMID') %>% mutate(Retrieved=!is.na(Screening_tier), Priority=Retrieved & Screening_tier<=2)
  }
  book(tables,path('PubMed_prioritised_screening.xlsx'),list(Screening=list(Reviewer_1=c('Include','Exclude','Unsure'),Reviewer_2=c('Include','Exclude','Unsure'),Title_abstract_verdict=c('Include','Exclude','Unsure'),Full_text_verdict=c('Include','Exclude','Unsure','Not retrieved','Not required'),Final_verdict=c('Include','Exclude','Not retrieved'),Final_gate=paste('Gate',1:4))))
  write_csv(prescreen,'PubMed_prescreened_records.csv')
  write_csv(summary,'Prescreening_scenarios.csv')
  write_csv(counts,'Prescreening_tiers.csv')
  # Flow counts: deferred records are not exclusions.
  r <- read.csv(path('PubMed_retrieval_summary.csv'))
  stopifnot(nrow(r)==1,r$Unique_PMIDs==N,r$Raw_hits==N+r$Duplicate_hits)
  a <- sum(!prescreen$Retain_S1); b <- sum(prescreen$Retain_S1 & !prescreen$Retain_S2)
  c <- sum(prescreen$Retain_S2 & !prescreen$Retain_S3); d <- sum(prescreen$Retain_S3 & prescreen$Screening_tier>2)
  e <- sum(prescreen$Screening_tier==1); f <- sum(prescreen$Screening_tier==2)
  stopifnot(a+b+c+d+e+f==N)
  lab <- function(x,n) paste0(x,' (',n,')')
  nodes <- c(lab('Search hits',r$Raw_hits),lab('Duplicate hits',r$Duplicate_hits),lab('Unique records',N),lab('Likely non-primary',a),lab('After publication type',N-a),lab('Likely animal-only',b),lab('After animal flag',N-a-b),lab('Uncertain human research',c),lab('Likely human empirical',N-a-b-c),lab('Lower measurement priority',d),lab('Priority screening',e+f),lab('Named platform',e),lab('Broader measurement',f))
  edges <- data.frame(source=c(0,0,2,2,4,4,6,6,8,8,10,10),target=c(1,2,3,4,5,6,7,8,9,10,11,12),N=c(r$Duplicate_hits,N,a,N-a,b,N-a-b,c,N-a-b-c,d,e+f,e,f),Flow=rep(c('Deferred','Retained'),6))
  edges$Flow[1] <- 'Duplicate'; edges$Flow[11:12] <- 'Priority'
  edges <- edges[edges$N>0,]
  diagram <- networkD3::sankeyNetwork(Links=edges,Nodes=data.frame(Name=nodes),Source='source',Target='target',Value='N',NodeID='Name',LinkGroup='Flow',colourScale=htmlwidgets::JS('d3.scaleOrdinal().domain(["Duplicate","Deferred","Retained","Priority"]).range(["#999999","#B8BEC7","#5079A5","#39836A"])'),fontSize=13,nodeWidth=18,nodePadding=20,sinksRight=FALSE,width=1500,height=700,margin=list(right=250))
  htmlwidgets::saveWidget(diagram,path('Sankey_prescreening.html'),selfcontained=FALSE)
  write_csv(transform(edges,Source=nodes[source+1],Target=nodes[target+1]),'Sankey_prescreening_flows.csv')
  message('Screen tiers 1 and 2 first; all other records remain pending until adjudicated.')
}

if (STAGE == 2L) {
  out <- path('VOC_data_extraction.xlsx')
  check_new(out)
  s <- read_sheet(path('PubMed_prioritised_screening.xlsx'), 'Screening')
  expected <- read.csv(path('PubMed_screening_records.csv'), colClasses='character')
  s$PMID <- as.character(s$PMID)
  stopifnot(setequal(s$PMID, expected$PMID), !anyDuplicated(s$PMID))
  require_fields(s, c('PMID', 'Final_verdict'), 'Screening')
  stopifnot(!anyDuplicated(s$PMID), all(s$Final_verdict %in% c('Include', 'Exclude', 'Not retrieved')))
  excluded <- filter(s, Final_verdict == 'Exclude')
  require_fields(excluded, c('Final_gate', 'Final_reason'), 'Exclusions')
  stopifnot(all(excluded$Final_gate %in% paste('Gate', 1:4)))
  included <- filter(s, Final_verdict == 'Include') %>%
    mutate(Study_ID = paste0('VOC_', PMID))
  screening_sankey(s)
  if (!nrow(included)) stop('No included publications')
  metadata <- included %>%
    select(Study_ID, PMID, DOI, Reference, Year, Journal, Title) %>%
    mutate(
      Country = '',
      Cohort = '',
      Study_design = '',
      Recruitment_setting = '',
      N_total = NA_real_,
      Age_mean = NA_real_,
      Age_SD = NA_real_,
      Female_percent = NA_real_,
      Funding = '',
      Notes = ''
    )
  assays <- metadata %>%
    select(Study_ID) %>%
    mutate(
      Assay_ID = paste0(Study_ID, '_A01'),
      Phenotype_group = '',
      Disorder = '',
      Diagnostic_criteria = '',
      Comparator = '',
      Biospecimen = '',
      Platform = '',
      Sampling_method = '',
      N_analysed = NA_real_,
      N_cases = NA_real_,
      N_controls = NA_real_,
      N_VOCs_measured = NA_real_,
      Targeted_untargeted = '',
      Compound_identification = '',
      Medication_adjusted = '',
      Smoking_adjusted = '',
      Diet_controlled = '',
      Ambient_air_control = '',
      External_validation = '',
      Source_location = '',
      Notes = ''
    )
  compounds <- tibble(
    Assay_ID = character(),
    Compound_original = character(),
    Compound_standardised = character(),
    CAS_number = character(),
    PubChem_CID = character(),
    InChIKey = character(),
    Chemical_class = character(),
    Direction = character(),
    Effect_size = numeric(),
    Effect_type = character(),
    P_value = numeric(),
    Adjusted_P = numeric(),
    Identification_method = character(),
    Contrast = character(),
    Source_location = character()
  )
  models <- tibble(
    Assay_ID = character(),
    Model_ID = character(),
    Model_type = character(),
    Diagnostic_target = character(),
    Comparator = character(),
    N_training = numeric(),
    N_testing = numeric(),
    AUC = numeric(),
    AUC_lower = numeric(),
    AUC_upper = numeric(),
    Sensitivity = numeric(),
    Specificity = numeric(),
    Validation_type = character(),
    Source_location = character()
  )
  instructions <- tibble(
    Field = c(
      'Study_metadata',
      'Study_assays',
      'Compound_associations',
      'Diagnostic_performance',
      'Missing data',
      'Source_location',
      'Counting unit'
    ),
    Instructions = c(
      'One row per publication; record shared cohorts in Cohort.',
      'One row per observed phenotype–specimen–platform combination or distinct analysis; add rows and unique Assay_IDs. Do not infer combinations from keywords.',
      'One row per reported compound and contrast; retain null results and original names.',
      'One row per model evaluation; use fractions 0–1 for AUC, sensitivity and specificity. For multimodal models link to a representative assay and document all inputs in Model_type.',
      'Leave numeric missing values blank; use Not reported for unknown categorical information.',
      'Record page, table or supplement for verification.',
      'Publication counts are not independent cohort counts. Do not sum overlapping participant counts.'
    )
  )
  # Long disorder list exceeds Excel inline validation limit: use manual controlled labels.
  dictionary <- bind_rows(
    tibble(Field = 'Disorder', Value = names(disorders)),
    tibble(Field = 'Biospecimen', Value = names(specimens)),
    tibble(Field = 'Platform', Value = names(platforms)),
    tibble(Field = 'Phenotype_group', Value = groups)
  )
  book(
    list(
      Study_metadata = metadata,
      Study_assays = assays,
      Compound_associations = compounds,
      Diagnostic_performance = models,
      Instructions = instructions,
      Dictionary = dictionary,
      Phenotype_dictionary = phenotype_lookup
    ),
    out,
    list(
      Study_assays = list(
        Phenotype_group = groups,
        Biospecimen = names(specimens),
        Platform = names(platforms),
        External_validation = c('Yes', 'No', 'Not reported', 'Not applicable')
      ),
      Compound_associations = list(Direction = c('Increased', 'Decreased', 'Null', 'Mixed', 'Not reported')),
      Diagnostic_performance = list(
        Validation_type = c('Apparent', 'Cross-validation', 'Held-out', 'External', 'Prospective', 'Not reported')
      )
    )
  )
  write_csv(count(s, Final_verdict), 'Screening_summary.csv')
  write_csv(count(excluded, Final_gate, Final_reason), 'Screening_exclusions.csv')
}

if (STAGE == 3L) {
  f <- path('VOC_data_extraction.xlsx')
  studies <- read_sheet(f, 'Study_metadata')
  assays <- read_sheet(f, 'Study_assays')
  compounds <- read_sheet(f, 'Compound_associations')
  models <- read_sheet(f, 'Diagnostic_performance')
  require_fields(studies, c('Study_ID', 'PMID'), 'Studies')
  require_fields(
    assays,
    c('Study_ID', 'Assay_ID', 'Phenotype_group', 'Disorder', 'Biospecimen', 'Platform', 'Source_location'),
    'Assays'
  )
  stopifnot(
    !anyDuplicated(studies$Study_ID),
    !anyDuplicated(studies$PMID),
    !anyDuplicated(assays$Assay_ID),
    setequal(studies$Study_ID, assays$Study_ID),
    all(assays$Phenotype_group %in% groups),
    all(assays$Biospecimen %in% names(specimens)),
    all(assays$Platform %in% names(platforms))
  )
  s <- read_sheet(path('PubMed_prioritised_screening.xlsx'), 'Screening')
  expected <- read.csv(path('PubMed_screening_records.csv'), colClasses='character')
  s$PMID <- as.character(s$PMID)
  stopifnot(setequal(s$PMID, expected$PMID), !anyDuplicated(s$PMID))
  stopifnot(setequal(as.character(studies$PMID), as.character(s$PMID[s$Final_verdict == 'Include'])))
  if (nrow(compounds)) {
    require_fields(compounds, c('Assay_ID', 'Compound_original', 'Source_location'), 'Compounds')
    stopifnot(all(compounds$Assay_ID %in% assays$Assay_ID))
  }
  if (nrow(models)) {
    require_fields(models, c('Assay_ID', 'Model_ID', 'Validation_type', 'Source_location'), 'Models')
    stopifnot(all(models$Assay_ID %in% assays$Assay_ID))
    for (k in c('AUC', 'AUC_lower', 'AUC_upper', 'Sensitivity', 'Specificity')) {
      v <- models[[k]]
      z <- suppressWarnings(as.numeric(v))
      if (any(!blank(v) & (is.na(z) | z < 0 | z > 1))) stop('Invalid fraction: ', k)
    }
  }
  invalid <- anti_join(distinct(assays, Disorder, Phenotype_group), phenotype_lookup,
                       by = c('Disorder', 'Phenotype_group'))
  if (nrow(invalid)) {
    print(invalid)
    stop('Invalid phenotype-group combinations')
  }
  evidence <- left_join(assays, studies, by = 'Study_ID')
  observed_counts <- evidence %>%
    distinct(Study_ID, Phenotype_group, Disorder, Biospecimen) %>%
    count(Phenotype_group, Disorder, Biospecimen, name = 'n_publications')
  map <- tidyr::crossing(phenotype_lookup, Biospecimen = names(specimens)) %>%
    left_join(observed_counts, by = c('Phenotype_group', 'Disorder', 'Biospecimen')) %>%
    mutate(n_publications = coalesce(n_publications, 0L)) %>%
    arrange(match(Phenotype_group, groups), Disorder, Biospecimen)
  group_coverage <- phenotype_lookup %>%
    count(Phenotype_group, name = 'n_prespecified_phenotypes') %>%
    left_join(evidence %>% group_by(Phenotype_group) %>%
                summarise(n_publications = n_distinct(Study_ID),
                          n_phenotypes_identified = n_distinct(Disorder), .groups = 'drop'),
              by = 'Phenotype_group') %>%
    mutate(across(starts_with('n_'), ~ coalesce(.x, 0L)),
           percentage_coverage = 100 * n_phenotypes_identified / n_prespecified_phenotypes)
  write_csv(group_coverage, 'Phenotype_group_coverage.csv')
  write_csv(phenotype_lookup, 'Phenotype_classification.csv')
  trends <- evidence %>%
    distinct(Study_ID, Year, Phenotype_group) %>%
    count(Year, Phenotype_group, name = 'n_publications')
  tech <- evidence %>%
    distinct(Study_ID, Platform, Phenotype_group) %>%
    count(Platform, Phenotype_group, name = 'n_publications')
  write_csv(studies, 'VOC_study_metadata.csv')
  write_csv(assays, 'VOC_study_assays.csv')
  write_csv(compounds, 'VOC_compound_associations.csv')
  write_csv(models, 'VOC_diagnostic_performance.csv')
  write_csv(evidence, 'VOC_evidence_joined.csv')
  write_csv(map, 'Figure2_Evidence_map.csv')
  write_csv(trends, 'Figure2_Publication_trends.csv')
  write_csv(tech, 'Figure2_Platform_summary.csv')
  screening_sankey(s, assays)
  message('Validated ', nrow(studies), ' publications and ', nrow(assays), ' assay rows.')
}


## PLOT of data attrition
system2(
  "open",
  shQuote(path("Sankey_prescreening.html"))
)

##
system2(
  "open",
  shQuote(path("PubMed_prioritised_screening.xlsx"))
)


# N.b after manual inspections, there are some ineligible publications; so a second round of screening is needed:

library(dplyr)
library(stringr)
library(tidyr)

root <- path.expand("~/Documents/VOCs_project/data")

papers <- read.xlsx(
  file.path(root, "PubMed_prioritised_screening.xlsx"),
  sheet = "Screening"
) %>%
  mutate(PMID = as.character(PMID)) %>%
  distinct(PMID, .keep_all = TRUE)

priority <- papers %>%
  filter(Screening_tier %in% c(1, 2))

# OR:
           
priority <- read.csv(
  file.path(root, "PubMed_priority_dataset.csv"),
  stringsAsFactors = FALSE
)

priority_screened <- priority %>%
  mutate(
    Title_text = str_to_lower(coalesce(Title, "")),
    Abstract_text = str_to_lower(coalesce(Abstract, "")),
    
    # 1. Sleep and respiratory disorders
    Flag_Sleep_respiratory = str_detect(
      Title_text,
      "sleep apn|sleep apno|sleep quality|sleep duration|insomnia|sleep deprivation|copd|obstructive pulmonary"
    ),
    
    # 2. Animal and cell-based research
    Flag_Animal_or_cell = str_detect(
      Title_text,
      "\\bmice\\b|\\bmouse\\b|\\brats?\\b|murine|rodent|zebrafish|animal model|5xfad|in vitro|cell lines?|cultured cells|drosophila"
    ),
    
    # 3. Environmental and occupational exposure
    Flag_Exposure = str_detect(
      Title_text,
      "exposure|air pollution|occupational|neurotoxicity|environmental pollutants|indoor air|ambient air|nhanes"
    ),
    
    # 4. Olfactory function and chemosensory communication
    Flag_Olfaction_chemosignals = str_detect(
      Title_text,
      "olfactory function|olfactory dysfunction|olfactory perception|olfactory threshold|chemosignal|chemosensory|odor perception|odour perception|odorant receptor|olfactory receptor"
    ),
    
    # 5. Drug detection and toxicology
    Flag_Drug_toxicology = str_detect(
      Title_text,
      "intoxication|poisoning|forensic|drug testing|drugs of abuse|pharmacokinetic|drug detection|therapeutic drug monitoring"
    ),
    
    # 6. Non-neurological primary phenotypes
    Flag_Other_phenotype = str_detect(
      Title_text,
      "sarcopenia|halitosis|gastroesophageal reflux|periodontitis|lactose intolerance|pneumonia|oral malodour|oral malodor|lung cancer|breast cancer|colorectal cancer"
    ),
    
    # 7. Food, plant and industrial chemistry
    Flag_Food_plant_industrial = str_detect(
      Title_text,
      "essential oils?|aromatherapy|brewed coffee|food quality|food flavour|food flavor|forest therapy|botanic garden|plant volatiles|industrial emissions"
    ),
    
    # 8. Reviews and protocols
    Flag_Nonprimary = str_detect(
      str_to_lower(coalesce(Publication_types, "")),
      "\\breview\\b|meta-analysis|editorial|commentary"
    ) |
      str_detect(
        Title_text,
        "systematic review|scoping review|narrative review|study protocol|review of the literature"
      ),
    
    # 9. Incorrect VOC acronym
    Flag_Acronym = str_detect(
      Abstract_text,
      "validity of cognition|voice of customer|vocabulary of"
    ),
    
    # 10. Analytical development without clear clinical application
    Flag_Analytical = str_detect(
      Title_text,
      "method development|analytical validation|sensor development|sampling optimisation|sampling optimization|identifying robust and reliable"
    )
  )


flag_columns <- grep(
  "^Flag_",
  names(priority_screened),
  value = TRUE
)

priority_screened <- priority_screened %>%
  mutate(
    Flag_count = rowSums(
      across(all_of(flag_columns)),
      na.rm = TRUE
    )
  ) %>%
  rowwise() %>%
  mutate(
    Screening_flags = paste(
      str_remove(
        flag_columns[
          which(c_across(all_of(flag_columns)))
        ],
        "^Flag_"
      ),
      collapse = "; "
    )
  ) %>%
  ungroup() %>%
  mutate(
    Manual_eligibility = "",
    Manual_exclusion_reason = "",
    Screening_notes = ""
  ) %>%
  select(-Title_text, -Abstract_text)

write.csv(
  priority_screened,
  file.path(root, "PubMed_priority_exclusion_screen.csv"),
  row.names = FALSE,
  na = ""
)


priority_screened %>%
  summarise(
    across(
      all_of(flag_columns),
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Exclusion_category",
    values_to = "N"
  ) %>%
  arrange(desc(N))

priority_refined <- priority_screened %>%
  filter(Flag_count == 0)

cat("Original publications:", nrow(priority), "\n")
cat("Flagged publications:", nrow(flagged_papers), "\n")
cat("Remaining publications:", nrow(priority_refined), "\n")

View(priority_refined)

View(
  priority_screened %>%
    filter(Flag_Sleep_respiratory) %>%
    select(PMID, Title, Abstract)
)

priority_refined <- priority_screened %>%
  filter(Flag_count == 0)

write.csv(
  priority_refined,
  file.path(root, "PubMed_priority_refined.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
           
# additional screening
           exclude_pmids <- c(
  40777204, 31990659, 38892525, 41998538,
  19362154, 38997651, 35619259, 40059298,
  39038625, 38531449, 38408965, 37661612,
  39730697, 31108353, 39835431, 38413886,
  30871731, 42035679, 40812234, 36833470,
  37447245, 36893999, 30231844, 23414594,
  36521651, 33422593, 9580447, 7942515,
  41855864, 31349234, 24452069, 21439147,
  20638346, 26222873, 1799580, 7096519,
  35012788, 38085692, 41494207, 28778200,
  39615812, 28408159, 32650940, 32197792,
  35577098, 8254857, 1619927, 8675549
)

priority_refined <- priority_refined %>%
  mutate(PMID = as.character(PMID))

# Preserve excluded publications for the screening audit
additional_exclusions <- priority_refined %>%
  filter(PMID %in% as.character(exclude_pmids))

# Retain remaining candidate publications
priority_cleaned <- priority_refined %>%
  filter(!PMID %in% as.character(exclude_pmids))

cat("Before screening:", nrow(priority_refined), "\n")
cat("Additional exclusions:", nrow(additional_exclusions), "\n")
cat("Remaining publications:", nrow(priority_cleaned), "\n")

write.csv(
  priority_cleaned,
  file.path(root, "PubMed_priority_cleaned.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  additional_exclusions,
  file.path(root, "PubMed_additional_exclusions.csv"),
  row.names = FALSE,
  na = ""
)

           
#More manual screening: studies that fail Gate 2

library(dplyr)
library(tibble)
library(tidyr)

# Further manual screening: grouped exclusion reasons

manual_exclusions <- list(

  "Environmental or occupational VOC exposure" = c(
    33231851, 23803111, 43011044, 25283072,
    23024572, 11057602, 7601073, 8345533,
    40127300, 18485684, 8206027, 32033278,
    25639183, 40305737, 28709981, 26943595,
    26892792, 25199969, 20823639, 10912362,
    10418167, 9311533, 32470670, 39820093,
    40670509,38523395
  ),

  "Chemical toxicity and substance exposure" = c(
    41381046, 28173825, 38277722,
    20010286, 26881973, 11204316, 7173912
  ),

  "Pharmaceutical detection and therapeutic drug monitoring" = c(
    40081310, 37678210, 28888854, 20694517,
    11595694, 3016011, 27206645, 1443816,
    37449384
  ),

  "Botanical, cellular or experimental studies" = c(
    35026505, 27537375, 42263918, 38398590,
    37367833, 27466346, 7727503
  ),

  "Olfactory perception and chemosignalling" = c(
    39137693, 36170254, 33360399,
    34111768, 28835640, 36412058
  ),

  "Other clinical phenotype, no brain phenotype" = c(
    36864845, 36312001, 29720305, 7380907,
    1247169, 15102667, 31022456, 27405054,
    21727771, 27833693,30373291
  ),

  "Hypoglycaemia or ketosis detection without brain health phenotype" = c(
  40414890,28569238),

  "Normal physiological variation" = c(
    26582820, 25214354, 25545545, 29553604,
    12214757, 24497852, 33143327),

    "No relevant VOC profiling" = c(
      37654668,10199339,24548198,2809457,
    33052545,21383452)
  
)

# Convert exclusion list into a lookup table

exclusion_lookup <- enframe(
  manual_exclusions,
  name = "Exclusion_reason",
  value = "PMID"
) %>%
  unnest_longer(PMID) %>%
  mutate(PMID = as.character(PMID))

# Check that each PMID has only one exclusion reason

stopifnot(!anyDuplicated(exclusion_lookup$PMID))

# Assign eligibility decisions to publications

priority_screened_final <- priority_cleaned %>%
  mutate(PMID = as.character(PMID)) %>%
  left_join(exclusion_lookup, by = "PMID") %>%
  mutate(
    Manual_verdict = if_else(
      is.na(Exclusion_reason),
      "Retained",
      "Exclude"
    )
  )

# Retain excluded publications and their reasons

manual_excluded <- priority_screened_final %>%
  filter(Manual_verdict == "Exclude")

# Create final screened dataset

priority_cleaned_final <- priority_screened_final %>%
  filter(Manual_verdict == "Retained")

# Generate exclusion counts for Sankey diagram

exclusion_summary <- manual_excluded %>%
  count(Exclusion_reason, name = "N", sort = TRUE)

View(exclusion_summary)

# Check screening totals

nrow(priority_cleaned)
nrow(manual_excluded)
nrow(priority_cleaned_final)

write.csv(
  priority_cleaned_final,
  file.path(root, "PubMed_priority_cleaned_final.csv"),
  row.names = FALSE
)



           



           
