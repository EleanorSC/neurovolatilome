# Neurovolatilome literature screening script
# =============================================================================
#
# PURPOSE
# This script runs a reproducible literature search and screening pipeline
# to identify human empirical studies investigating volatile organic compounds
# (VOCs) in neurological, neuropsychiatric and related brain health phenotypes.
#
# SEARCH SCOPE
# The search encompasses six predefined phenotype groups:
#   1. Neurodegenerative disorders
#   2. Neuropsychiatric disorders
#   3. Neurodevelopmental disorders
#   4. Neurometabolic disorders
#   5. Other neurological disorders
#   6. Prodromal neurocognitive phenotypes
#
# Studies are retrieved irrespective of biospecimen or analytical platform,
# including breath, sebum, skin emissions, sweat, urine, stool, saliva,
# blood, cerebrospinal fluid and cerumen.
#
# SEARCH STRATEGY
# Four complementary PubMed retrieval routes are used:
#   1. VOC and volatilomics terminology
#   2. Targeted volatile compounds and relevant sampling contexts
#   3. Analytical platforms and relevant sampling contexts
#   4. Volatile fatty acids, including short-chain fatty acids (SCFAs)
#
# Free-text search terms and MeSH headings are combined using Boolean
# operators. Queries are partitioned to accommodate PubMed API constraints.
# Known candidate publications are used to assess retrieval sensitivity.
#
# SCREENING PIPELINE
# Stage 1:   PubMed retrieval, metadata extraction, PMID deduplication
#            and preliminary classification of publications.
#
# Stage 1.5: Automated prescreening based on publication type, evidence
#            of human empirical research, biospecimen, analytical platform
#            and evidence of VOC measurement. Publications are assigned
#            to six screening priority tiers.
#
# Stage 2:   Candidate prioritisation, application of screening flags
#            and historical exclusion lists, generation of an editable
#            screening workbook and descriptive visualisations.
#
# Stage 3:   Validation of manual eligibility decisions and export of
#            the final included publications and screening attrition.
#
# INSTRUCTIONS
#   1. Install the required R packages listed below.
#   2. Specify the desired output directory.
#   3. Source the complete script in RStudio to execute Stages 1–2.
#   4. Review the generated screening workbook and record eligibility
#      decisions manually.
#   5. Execute Stage 3 ONLY AFTER manual screening is complete.
#
# OUTPUTS
#   - PubMed search strategy, partition queries and retrieval logs
#   - Deduplicated publication metadata and abstracts
#   - Positive-control retrieval audit
#   - Prescreening classifications and priority tiers
#   - Excel workbooks for manual eligibility assessment
#   - CSV files containing retained candidate publications
#   - Publication counts by phenotype group and individual disorder
#   - Screening attrition plots and interactive Sankey diagrams
#   - Execution timing logs
#
# REPRODUCIBILITY AND QUALITY CONTROL
# Each execution performs a fresh PubMed search and saves outputs to a
# separate timestamped directory, preserving previous screening runs.
# Retrieval safeguards are applied to identify excessively broad queries.
#
# IMPORTANT
# Automated screening classifications, priority tiers and exclusion flags
# are intended to support, rather than replace, manual eligibility assessment.
# Retained candidate publications should not be interpreted as the final
# evidence base until eligibility has been confirmed through manual review.
#
# =============================================================================


#install.packages(
#  c('rentrez', 'xml2', 'dplyr', 'stringr', 'purrr', 'tibble', 'openxlsx','tidyr', 'networkD3', 'htmlwidgets', 'ggplot2', 'wesanderson')
#)

# Set the working directory
# setwd("~/repos/neuro_VOCs")


voc_execute_stage <- function(STAGE, output_dir) {
  library(rentrez)
  library(xml2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(openxlsx)
  library(tidyr)
  stopifnot(STAGE %in% c(1, 1.5, 2, 3))
  project_dir <- path.expand('~/Documents/VOCs_project')
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  # Separate output directory: existing data files are never overwritten.
  legacy_dir <- file.path(project_dir, 'data')
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  stopifnot(dir.exists(output_dir), file.access(output_dir, 2) == 0)
  path <- function(x) file.path(output_dir, x)
  # For stage 1.5, explicitly opt in to importing the previous retrieval.
  IMPORT_EXISTING_RETRIEVAL <- FALSE
  if (STAGE == 1.5 && IMPORT_EXISTING_RETRIEVAL) {
    inputs <- c('PubMed_screening_records.csv', 'PubMed_manual_screening.xlsx',
                'PubMed_retrieval_summary.csv')
    if (!any(file.exists(path(inputs)))) {
      stopifnot(all(file.exists(file.path(legacy_dir, inputs))))
      stopifnot(all(file.copy(file.path(legacy_dir, inputs), output_dir, overwrite = FALSE)))
      message('Imported previous retrieval into ', output_dir)
    }
    stopifnot(all(file.exists(path(inputs))))
  }
  if (!file.exists(path('Known_eligible_studies.csv')) &&
      file.exists(file.path(legacy_dir, 'Known_eligible_studies.csv'))) {
    file.copy(file.path(legacy_dir, 'Known_eligible_studies.csv'), output_dir)
  }
  # Wall-clock timing; ETA is based on average completed work units.
  run_start <- Sys.time()
  step_start <- run_start
  run_id <- format(run_start, '%Y%m%d_%H%M%OS6')
  timing_file <- path(paste0('Timing_stage_', STAGE, '_', run_id, '.csv'))
  run_status <- 'Interrupted or failed'
  seconds <- function(start) as.numeric(difftime(Sys.time(), start, units = 'secs'))
  fmt <- function(x) sprintf('%02d:%02d:%02d', floor(x/3600), floor(x/60) %% 60, floor(x) %% 60)
  log_step <- function(label, elapsed) {
    row <- data.frame(Stage = STAGE, Step = label, Timestamp = as.character(Sys.time()),
                      Elapsed_seconds = round(elapsed, 2))
    exists <- file.exists(timing_file)
    write.table(row, timing_file, sep = ',', row.names = FALSE, col.names = !exists, append = exists)
    message(label, ': ', fmt(elapsed))
  }
  checkpoint <- function(label) {
    log_step(label, seconds(step_start))
    step_start <<- Sys.time()
  }
  progress <- function(label, done, total, start) {
    elapsed <- seconds(start)
    eta <- if (done > 0) fmt(elapsed / done * (total - done)) else 'estimating'
    message(sprintf('%s: %d/%d (%.1f%%) | elapsed %s | ETA %s',
                    label, done, total, 100 * done/total, fmt(elapsed), eta))
  }
  on.exit(log_step(paste('Stage total:', run_status), seconds(run_start)), add = TRUE)
  message('Starting stage ', STAGE, ' | ', output_dir)
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
    "Motor neuron disease" = 'motor neurone? disease|\\bMND\\b',
    "Amyotrophic lateral sclerosis" = 'amyotrophic lateral sclerosis|\\bALS\\b',
    "Primary lateral sclerosis" = 'primary lateral sclerosis',
    "Progressive muscular atrophy" = 'progressive muscular atrophy',
    "Spinal muscular atrophy" = 'spinal muscular atrophy',
    
    # Huntington's disease and hereditary neurodegeneration
    "Huntington's disease" = 'huntington',
    "Spinocerebellar ataxia" = 'spinocerebellar ataxia',
    "Friedreich's ataxia" = 'friedreich',
    
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
    
    
    # Anxiety and compulsive disorders ------------------------------------------------------
    
    'Anxiety disorders' = 'anxiety disorder',
    'Obsessive-compulsive disorder' = 'obsessive.compulsive|\\bOCD\\b',
    
    
    # Trauma and stress-related disorders ------------------------------------
    
    'Post-traumatic stress disorder' = 'post.traumatic stress|\\bPTSD\\b',
    
    # Neurodevelopmental disorders -------------------------------------------
    
    'Autism spectrum disorder' = 'autis|asperger|\\bASD\\b',
    'Attention deficit hyperactivity disorder' = 'attention.deficit|\\bADHD\\b',
    'Intellectual disability' = 'intellectual disabilit',
    'Global developmental delay' = 'global developmental delay',
    'Developmental language disorder' = 'developmental language disorder',
    'Developmental coordination disorder' = 'developmental coordination disorder|dyspraxia',
    'Specific neurodevelopmental learning disorders' = 'neurodevelopmental learning disorder|dyslexia|dyscalculia',
    'Tourette syndrome and tic disorders' = 'tourette|tic disorder|motor tic|vocal tic',
    
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
    
    
    # Mitochondrial and peroxisomal disorders ---------------------------------
    
    'MELAS syndrome' = '\\bMELAS\\b|mitochondrial encephalomyopathy.*lactic acidosis',
    'Leigh syndrome' = 'leigh syndrome|leigh.s disease',
    'MERRF syndrome' = '\\bMERRF\\b|myoclonic epilepsy with ragged.red fibres',
    
    
    # Other neurometabolic disorders ------------------------------------------
    
    'Wilson disease' = 'wilson.s disease|wilson disease|hepatolenticular degeneration',
    
    
    # General phenotype terminology ------------------------------------------
    
    'Unspecified psychiatric disorder' = 'psychiatric disorder|mental disorder|mental illness',
    'Unspecified neurodevelopmental disorder' = 'neurodevelopmental disorder',
    'Unspecified neurometabolic disorder' = 'neurometabolic disorder|inborn error.*metabolism',
    'Delirium' = 'delirium|acute confusional state',
    'Encephalopathy' = 'encephalopath',
    'Functional seizures' = 'psychogenic non.epileptic seizure|functional seizure',
    'Neuropathic pain' = 'neuropathic pain|neuralgia'
    
    
    
  )
  
  specimens <- c(
    Breath = 'breath|exhaled air',
    Sweat = 'sweat|perspiration|axillary',
    Sebum = 'sebum|sebaceous',
    Skin = 'skin volatile|skin emission|skin odou?r',
    Urine = '\\b(urine|urinary)\\b',
    Stool = 'stool|faecal|fecal|faeces|feces',
    CSF = 'cerebrospinal fluid|\\bCSF\\b',
    Saliva = 'saliva',
    Blood = 'blood|plasma|serum',
    Earwax = 'earwax|cerumen',
    Semen = 'semen|seminal fluid'
  )
  
  platforms <- c(
    
    # Gas chromatography
    "GC-MS" = "GC[-– ]?MS|gas chromatograph\\w*.{0,10}mass spectrom",
    "GC-IMS" = "GC[-– ]?IMS|gas chromatograph\\w*.{0,10}ion mobility",
    "GC-FID" = "GC[-– ]?FID|flame ioni[sz]ation",
    "GC-TOF-MS" = "GC[-– ]?TOF[-– ]?MS|gas chromatograph.*time.of.flight",
    "GC-QTOF-MS" = "GC[-– ]?Q[-– ]?TOF",
    "GC×GC-MS" = "GC[x×]GC|comprehensive two.dimensional gas chromatograph",
    "TD-GC-MS" = "TD[-– ]?GC[-– ]?MS|thermal desorption.*gas chromatograph",
    "HS-GC-MS" = "HS[-– ]?GC[-– ]?MS|headspace.*gas chromatograph",
    
    # Direct gas-phase mass spectrometry
    "PTR-MS" = "PTR[-– ]?(MS|TOF)|proton.transfer.reaction",
    "SIFT-MS" = "SIFT[-– ]?MS|selected.ion.flow.tube",
    "SESI-HRMS" = "SESI[-– ]?(HR)?MS|secondary electrospray",
    
    # Liquid chromatography and other mass spectrometry
    "LC-MS" = "LC[-– ]?MS|liquid chromatograph\\w*.{0,10}mass spectrom",
    "DART-MS" = "DART[-– ]?MS|direct analysis in real time",
    
    # Electronic noses and artificial olfaction
    "Electronic nose" = paste(c(
      "electronic noses?",
      "\\be[-– ]?noses?\\b",
      "artificial noses?",
      "artificial olfact",
      "artificial.intelligen\\w* olfactory",
      "artificial olfactory systems?",
      "bioelectronic noses?",
      "biomimetic noses?",
      "olfactory.like systems?",
      "chemical sensor arrays?",
      "gas sensor arrays?",
      "sensor arrays?",
      "nanosensor arrays?",
      "nanomaterial.based sensors?",
      "nanomaterial.based arrays?",
      "nanoarrays?",
      "nanotechnology.based sensors?",
      "chemiresistive sensors?",
      "chemoresistive sensors?",
      "cross.reactive sensors?",
      "cross.reactive arrays?",
      "odou?r sensors?",
      "gas sensors?",
      "gas sensing",
      "volatile sensors?",
      "volatile sensing",
      "\\bNA[-– ]?NOSE\\b",
      "\\bCyranose\\b",
      "\\bAeonose\\b",
      "\\bSpiroNose\\b",
      "\\bPEN[-– ]?[23]\\b"
    ), collapse = "|"),
    
    # Specific sensor technologies
    "SAW" = "surface acoustic wave|\\bSAW\\b",
    "MOS" = "metal oxide semiconductor|metal oxide sensors?",
    "QCM" = "quartz crystal microbalance|\\bQCM\\b",
    "Conducting polymer sensors" = "conducting polymer sensors?|conductive polymer sensors?",
    "Colorimetric sensors" = "colou?rimetric sensor|colou?rimetric array",
    
    # Ion mobility spectrometry
    "FAIMS" = "\\bFAIMS\\b|field asymmetric ion mobility",
    "IMS" = "ion mobility spectrom|\\bIMS\\b",
    "DMS" = "differential mobility spectrom|\\bDMS\\b",
    
    # Spectroscopy
    "Infrared spectroscopy" = "infrared spectroscop|\\bFTIR\\b",
    "Photoacoustic spectroscopy" = "photoacoustic spectroscop",
    "CRDS" = "cavity ring.down spectroscop|\\bCRDS\\b",
    "TDLAS" = "tunable diode laser absorption|\\bTDLAS\\b",
    
    # Other gas analysis technologies
    "Gas chromatography" = "gas chromatograph|\\bGC\\b",
    "Mass spectrometry" = "mass spectrometr",
    "Gas spectroscopy" = "gas.phase spectroscop|gas absorption spectroscop"
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
      
      'Functional seizures',
      'Delirium',
      'Encephalopathy',
      
      'Neuropathic pain',
      
      
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
      
      
      # Anxiety and compulsive disorders
      'Anxiety disorders',
      
      'Obsessive-compulsive disorder',
      
      # Trauma and stress-related disorders
      'Post-traumatic stress disorder',
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
      
      
      # Urea cycle disorders
      
      
      
      
      
      # Lysosomal storage disorders
      
      
      
      
      
      
      
      
      # Mitochondrial and peroxisomal disorders
      'MELAS syndrome',
      'Leigh syndrome',
      'MERRF syndrome',
      
      
      
      # Other neurometabolic disorders
      'Wilson disease',
      
      
      
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
          if (length(mesh)) paste0('"', mesh, '"[MeSH Terms]') else character()
        ),
        collapse = ' OR '
      ),
      ')'
    )
  }
  
  
  voc <- unique(c(
    
    # General VOC terminology
    "volatile organic compound*",
    "volatile compound*",
    "volatile metabolite*",
    "volatile biomarker*",
    "volatile profil*",
    "volatile signature*",
    "volatile chemical*",
    "volatile molecule*",
    "volatile analyte*",
    "volatile emission*",
    "volatile component*",
    "volatile substance*",
    "volatile metabolom*",
    "VOC",
    "VOCs",
    "volatilome",
    "volatilomics",
    "volatomics",
    "chemical fingerprint*",
    "volatile fingerprint*",
    "gas fingerprint*",
    
    # Breath sampling and profiling
    "breathomics",
    "breath metabolom*",
    "breath analys*",
    "breath profil*",
    "breath fingerprint*",
    "breathprint*",
    "breath gas*",
    "exhaled breath",
    "exhaled molecule*",
    "exhaled metabolite*",
    "exhaled air",
    "expired air",
    "alveolar air",
    "respiratory air",
    
    # Hydrocarbons and targeted breath compounds
    "exhaled hydrocarbon*",
    "breath hydrocarbon*",
    "breath alkane*",
    "exhaled alkane*",
    "breath ethane",
    "exhaled ethane",
    "breath pentane",
    "exhaled pentane",
    "breath isoprene",
    "exhaled isoprene",
    
    # Electronic noses and artificial olfaction
    "electronic nose",
    "electronic noses",
    "e-nose",
    "eNose",
    "artificial olfact*",
    "artificial intelligent olfact*",
    
    # Body odour and skin
    "body odor*",
    "body odour*",
    "human odor*",
    "human odour*",
    "skin volatile*",
    "skin emission*",
    "axillary odor*",
    "axillary odour*",
    "odor fingerprint*",
    "odour fingerprint*",
    "chemosignal*",
    
    # Sebum
    "sebum volatile*",
    "sebum profil*",
    "sebum metabolom*",
    "sebum odor*",
    "sebum odour*",
    "sebum headspace",
    
    # Urine and stool
    "urinary volatile*",
    "fecal volatile*",
    "faecal volatile*",
    "stool volatile*",
    "urinary headspace",
    "fecal headspace",
    "faecal headspace",
    
    # Volatile fatty acids
    "volatile fatty acid*",
    "short-chain fatty acid*",
    "short chain fatty acid*",
    "branched-chain fatty acid*",
    "SCFA",
    "SCFAs",
    "BCFA",
    "BCFAs",
    
    # Volatile sulphur compounds
    "volatile sulfur compound*",
    "volatile sulphur compound*",
    
    # Neurometabolic disease terminology
    "fetor hepaticus",
    "sotolon",
    "phenylacetate",
    "musty odor",
    "musty odour",
    "maple syrup odor",
    "maple syrup odour")
    
  )
  
  voc_targeted <- c(
    
    # Hydrocarbons and lipid peroxidation
    "ethane",
    "pentane",
    "butane",
    "isoprene",
    "breath alkane*",
    "breath hydrocarbon*",
    "exhaled alkane*",
    "carbon disulfide",
    "carbon disulphide",
    
    # Microbial volatile metabolites
    "short-chain fatty acid*",
    "short chain fatty acid*",
    "branched-chain fatty acid*",
    "volatile fatty acid*",
    "SCFA",
    "SCFAs",
    "BCFA",
    "BCFAs",
    
    # Specific analytical approaches
    "headspace gas chromatograph*",
    "breath gas analys*",
    "alveolar gas*",
    "breath hydrocarbon analys*"
  )
  
  
  # Combine VOC search terms
  voc <- unique(c(voc, voc_targeted))
  voc_extra <- c(
    "breath ethane",
    "breath pentane",
    "breath hydrocarbon*",
    "alkane exhalation",
    "exhaled gas*",
    "breath constituent*",
    "breath composition",
    "breath chemical*",
    "alveolar gas*",
    "respiratory gas analys*",
    "volatile marker*",
    "volatile pattern*",
    "volatile fraction*",
    "volatile organic acid*",
    "gaseous metabolite*",
    "microbial volatile*",
    "bacterial volatile*",
    "headspace analys*",
    "headspace chromatograph*",
    "headspace mass spectrometr*",
    "thermal desorption",
    "solid phase microextraction",
    "HS-SPME",
    "TD-GC-MS",
    "HS-GC-MS",
    "PTR-MS",
    "SIFT-MS",
    "GC-IMS",
    "SESI-HRMS",
    "artificial nose*",
    "olfactory-like system*",
    "gas sensor array*",
    "cross-selective gas sensor*",
    "cross-reactive gas sensor*",
    "volatile sensing",
    "chemical odour profil*",
    "chemical odor profil*",
    "urine metabolom*",
    "faecal metabolom*",
    "fecal metabolom*",
    "stool metabolom*",
    "cerumen metabolom*",
    "cerumen volatile*",
    "earwax volatile*",
    "sweat volatile*",
    "salivary volatile*",
    "saliva volatile*",
    "skin gas*",
    "skin secretion*",
    "sebaceous secretion*"
  )
  voc <- unique(c(voc, voc_extra))
  
  
  
  
  
  # Core VOC MeSH headings
  voc_mesh <- c(
    "Volatile Organic Compounds",
    "Fatty Acids, Volatile",
    "Electronic Nose"
  )
  
  # Compound-specific MeSH headings
  voc_compound_mesh <- c(
    
    # Hydrocarbons
    "Hydrocarbons",
    "Alkanes",
    "Ethane",
    "Pentanes",
    "Butanes",
    "Isoprene",
    
    # Oxygen-containing compounds
    "Aldehydes",
    "Ketones",
    "Acetone",
    "Alcohols",
    "Esters",
    
    # Volatile fatty acids
    "Acetates",
    "Propionates",
    "Butyrates",
    
    # Sulphur-containing compounds
    "Carbon Disulfide",
    "Organosulfur Compounds",
    "Sulfur Compounds",
    
    # Other chemical classes
    "Terpenes",
    "Amines"
  )
  
  voc_specimen <- c(
    "breath",
    "exhal*",
    "alveolar",
    "expired air",
    "respiratory air",
    "headspace",
    "sweat",
    "perspiration",
    "axillary",
    "sebum",
    "sebaceous",
    "skin",
    "body odor*",
    "body odour*",
    "cerumen",
    "earwax",
    "ear wax",
    "urine",
    "urinary",
    "stool",
    "fecal",
    "faecal",
    "feces",
    "faeces",
    "saliva",
    "salivary",
    "blood",
    "plasma",
    "serum",
    "cerebrospinal fluid"
  )
  
  # Biospecimen MeSH headings
  voc_specimen_mesh <- c(
    "Breath Tests",
    "Exhalation",
    "Sebum",
    "Sweat",
    "Saliva",
    "Urine",
    "Feces",
    "Skin",
    "Blood",
    "Plasma",
    "Serum"
  )
  
  # Analytical platform MeSH headings
  voc_platform_mesh <- c(
    "Chromatography, Gas",
    "Gas Chromatography-Mass Spectrometry",
    "Mass Spectrometry",
    "Spectrometry, Mass, Electrospray Ionization",
    "Biosensing Techniques",
    "Electronic Nose",
    "Metabolomics",
    "Spectroscopy, Infrared"
  )
  
  voc_platform <- c(
    "gas chromatograph*",
    "gas liquid chromatograph*",
    "GC-MS",
    "GC-FID",
    "GC-IMS",
    "GC-TOF-MS",
    "GC-QTOF-MS",
    "GC-MS/MS",
    "PTR-MS",
    "SIFT-MS",
    "SESI-HRMS",
    "ion mobility spectrometr*",
    "photoacoustic spectroscop*",
    "electronic nose*",
    "artificial nose*",
    "gas sensor*",
    "chemical sensor array*",
    "nanomaterial-based sensor*",
    "chemiresistive sensor*",
    "surface acoustic wave",
    "headspace analys*",
    "thermal desorption",
    "solid phase microextraction"
  )

  brain <- c(
    "neuropsychiatr*",
    "neurologic*",
    "psychiatric",
    "neurodegenerat*",
    "brain health",
    "mental health",
    "schizophrenia",
    "psychosis",
    "psychotic",
    "depress*",
    "bipolar",
    "anxiety",
    "post-traumatic stress",
    "PTSD",
    "obsessive compulsive",
    "autis*",
    "ADHD",
    "attention deficit",
    "Parkinson*",
    "Alzheimer*",
    "dementia",
    "mild cognitive impairment",
    "cognitive decline",
    "cognitive impairment",
    "multiple sclerosis",
    "motor neuron disease",
    "motor neurone disease",
    "amyotrophic lateral sclerosis",
    "Huntington*",
    "epilep*",
    "migraine*",
    "stroke",
    "traumatic brain injury",
    "hepatic encephalopathy",
    "phenylketonuria",
    "maple syrup urine disease",
    "inborn errors of metabolism",
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
    "Vascular dementia",
    "Frontotemporal dementia",
    "Mixed dementia",
    "Creutzfeldt-Jakob disease",
    "Brain tumours",
    "Epilepsy",
    "Stroke",
    "Migraine",
    "Traumatic brain injury",
    "Multiple sclerosis",
    "Encephalitis",
    "Meningitis",
    "Mild cognitive impairment",
    "Cognitive impairment",
    "Schizophrenia",
    "Schizoaffective disorder",
    "Schizotypal disorder",
    "Psychosis",
    "Depression",
    "Bipolar disorder",
    "Mania",
    "Cyclothymic disorder",
    "Postpartum depression",
    "Anxiety disorders",
    "Obsessive-compulsive disorder",
    "Post-traumatic stress disorder",
    "Autism spectrum disorder",
    "Attention deficit hyperactivity disorder",
    "Intellectual disability",
    "Global developmental delay",
    "Developmental language disorder",
    "Developmental coordination disorder",
    "Autoimmune encephalitis",
    "Neuropsychiatric lupus",
    "Neurosarcoidosis",
    "Phenylketonuria",
    "Maple syrup urine disease",
    "Tyrosinaemia",
    "Homocystinuria",
    "Non-ketotic hyperglycinaemia",
    "Hepatic encephalopathy",
    "Wernicke encephalopathy",
    "Korsakoff syndrome",
    "Diabetic encephalopathy",
    "MELAS syndrome",
    "Leigh syndrome",
    "MERRF syndrome",
    "Wilson disease",
    "Delirium",
    "Encephalopathy",
    "Functional seizures",
    "Neuropathic pain",
    "neurodevelopmental disorder*",
    "neurometabolic disorder*",
    "prion disease*",
    "frontotemporal lobar degeneration",
    "Pick disease",
    "corticobasal syndrome",
    "Lewy bod*",
    "multi-infarct dementia",
    "seizure*",
    "ictal",
    "concussion",
    "cerebrovascular accident",
    "cerebral infarct*",
    "transient ischemic attack",
    "transient ischaemic attack",
    "brain tumor*",
    "brain tumour*",
    "brain neoplasm*",
    "brain cancer",
    "glioma*",
    "glioblastoma*",
    "astrocytoma*",
    "oligodendroglioma*",
    "ependymoma*",
    "meningioma*",
    "medulloblastoma*",
    "ganglioglioma*",
    "craniopharyngioma*",
    "pituitary adenoma*",
    "pituitary tumor*",
    "schwannoma*",
    "acoustic neuroma*",
    "neurocytoma*",
    "pineoblastoma*",
    "pineocytoma*",
    "choroid plexus tumor*",
    "choroid plexus tumour*",
    "choroid plexus papilloma*",
    "hemangioblastoma*",
    "haemangioblastoma*",
    "central nervous system lymphoma",
    "brain metastas*",
    "cerebral metastas*",
    "intracranial metastas*",
    "psychogenic non-epileptic seizure*",
    "dysthymi*",
    "manic depression",
    "hypomania",
    "Asperger*",
    "Tourette syndrome",
    "tic disorder*",
    "dyspraxia",
    "dyslexia",
    "dyscalculia",
    "subjective cognitive decline",
    "subjective memory complaint*",
    "memory impairment",
    "cognitive dysfunction",
    "branched-chain ketoaciduria",
    "tyrosinemia",
    "hyperglycinemia",
    "hepatolenticular degeneration",
    "mitochondrial disorder*",
    "peroxisomal disorder*",
    "MELAS",
    "MERRF",
    "MSUD",
    "PKU"
  )
  brain_extra <- c(
    "schizophren*",
    "schizoaffective*",
    "schizotyp*",
    "mental disorder*",
    "mental illness*",
    "psychiatric disorder*",
    "affective disorder*",
    "parkinsonism",
    "cognitive disorder*",
    "memory disorder*"
  )
  brain <- unique(c(brain, brain_extra))
  brain_mesh <- c(
    "Neurodegenerative Diseases",
    "Parkinson Disease",
    "Parkinsonian Disorders",
    "Alzheimer Disease",
    "Dementia",
    "Lewy Body Disease",
    "Frontotemporal Dementia",
    "Huntington Disease",
    "Multiple System Atrophy",
    "Amyotrophic Lateral Sclerosis",
    "Motor Neuron Disease",
    "Prion Diseases",
    "Spinocerebellar Ataxias",
    "Schizophrenia",
    "Schizophrenia Spectrum and Other Psychotic Disorders",
    "Psychotic Disorders",
    "Mood Disorders",
    "Depressive Disorder",
    "Depressive Disorder, Major",
    "Bipolar Disorder",
    "Anxiety Disorders",
    "Obsessive-Compulsive Disorder",
    "Stress Disorders, Post-Traumatic",
    "Neurodevelopmental Disorders",
    "Autism Spectrum Disorder",
    "Attention Deficit Disorder with Hyperactivity",
    "Intellectual Disability",
    "Tourette Syndrome",
    "Learning Disabilities",
    "Cognition Disorders",
    "Cognitive Dysfunction",
    "Mild Cognitive Impairment",
    "Brain Neoplasms",
    "Epilepsy",
    "Seizures",
    "Multiple Sclerosis",
    "Demyelinating Diseases",
    "Stroke",
    "Cerebrovascular Disorders",
    "Migraine Disorders",
    "Brain Injuries, Traumatic",
    "Encephalitis",
    "Meningitis",
    "Delirium",
    "Neuralgia",
    "Hepatic Encephalopathy",
    "Phenylketonurias",
    "Maple Syrup Urine Disease",
    "Hepatolenticular Degeneration",
    "Wernicke Encephalopathy",
    "Korsakoff Syndrome",
    "Homocystinuria",
    "Mitochondrial Diseases",
    "Amino Acid Metabolism, Inborn Errors"
  )

  # Four distinct retrieval routes; avoid generic chemistry/phenotype combinations.
  brain <- unique(c(setdiff(brain, c(
    "depress*", "neurologic*", "neuropsychiatr*", "psychiatric",
    "neurodegenerat*", "brain health", "mental health", "anxiety",
    "mental disorder*", "mental illness*", "psychiatric disorder*",
    "affective disorder*", "memory disorder*", "mitochondrial disorder*",
    "peroxisomal disorder*")),
    "depression", "depressive disorder*", "major depression",
    "anxiety disorder*", "schizophren*"))
  brain_mesh <- setdiff(brain_mesh, c("Mood Disorders", "Demyelinating Diseases",
    "Cerebrovascular Disorders", "Mitochondrial Diseases",
    "Amino Acid Metabolism, Inborn Errors", "Neurodegenerative Diseases",
    "Neurodevelopmental Disorders", "Cognition Disorders"))
  # Explicit volatile terminology; no generic analytical methods in this route.
  voc <- c("volatile organic compound*", "volatile compound*", "volatile metabolite*",
    "volatile biomarker*", "volatile profil*", "volatile signature*",
    "volatile molecule*", "volatile emission*", "volatile fingerprint*",
    "volatilome", "volatilomics", "volatomics", "breathomics", "breathprint*",
    "breath metabolom*", "breath fingerprint*", "exhaled molecule*",
    "exhaled metabolite*", "electronic nose*", "e-nose", "eNose",
    "artificial olfact*", "artificial nose*", "body odor*", "body odour*",
    "skin volatile*", "skin emission*", "sebum volatile*", "sebum profil*",
    "sebum metabolom*", "urinary volatile*", "urine volatile*", "fecal volatile*",
    "faecal volatile*", "stool volatile*", "sweat volatile*", "salivary volatile*",
    "cerumen volatile*", "earwax volatile*", "volatile sulfur compound*",
    "volatile sulphur compound*", "fetor hepaticus", "maple syrup odor",
    "maple syrup odour")
  voc_mesh <- character()
  voc_targeted <- c("ethane", "pentane", "butane", "isoprene", "ethylene",
    "acetone", "carbon disulfide", "carbon disulphide", "alkane*", "hydrocarbon*",
    "3-methyl-2-hexenoic acid", "trimethylamine")
  voc_compound_mesh <- character()
  voc_sampling <- c("breath", "exhaled air", "exhalation", "alveolar air",
    "expired air", "respiratory air", "headspace", "sebum", "sweat",
    "axillary secretion*", "cerumen", "earwax", "skin emission*",
    "thermal desorption", "solid phase microextraction", "HS-SPME")
  voc_sampling_mesh <- character()
  voc_platform <- c("gas chromatograph*", "GC-MS", "GC-IMS", "GC-FID",
    "PTR-MS", "SIFT-MS", "SESI-HRMS", "photoacoustic spectroscop*",
    "gas sensor array*", "electronic nose*", "artificial nose*")
  platform_mesh_search <- character()
  fatty <- c("short-chain fatty acid*", "short chain fatty acid*",
    "branched-chain fatty acid*", "volatile fatty acid*")
  # SCFA route: require fatty-acid terminology in title and a specimen in text.
  # This is a focused supplementary search, not a comprehensive SCFA review.
  tagged <- function(terms, mesh = character(), field = "Title/Abstract") unique(c(
    if (length(terms)) paste0('"', terms, '"[', field, ']') else character(),
    if (length(mesh)) paste0('"', mesh, '"[MeSH Terms:noexp]') else character()))
  or_block <- function(x) paste0("(", paste(x, collapse = " OR "), ")")
  phenotype <- tagged(brain, brain_mesh)
  routes <- list(
    Direct = list(tagged(voc), phenotype),
    Compounds = list(tagged(voc_targeted), tagged(voc_sampling), phenotype),
    Platforms = list(tagged(voc_platform), tagged(voc_sampling), phenotype),
    Fatty_acids = list(tagged(fatty, field = "Title"),
      tagged(voc_specimen), tagged(brain, field = "Title")))
  query <- paste(vapply(routes, function(r) paste(vapply(r, or_block,
    character(1)), collapse = " AND "), character(1)), collapse = " OR ")

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
  checkpoint("Dictionary setup")
  if (STAGE == 1L) {
    out <- path('PubMed_manual_screening.xlsx')
    check_new(out)
    writeLines(query, path('PubMed_search_strategy.txt'))
    # Partition each OR block independently; union preserves the full query.
    chunks <- function(x, size = 12L) split(x, ceiling(seq_along(x) / size))
    # Split oversized queries recursively, preserving their Boolean meaning.
    partition <- function(blocks) {
      q <- paste(vapply(blocks, or_block, character(1)), collapse = " AND ")
      if (nchar(utils::URLencode(q, reserved = TRUE), type = "bytes") <= 3000L) return(q)
      i <- which.max(vapply(blocks, function(x) sum(nchar(x)), numeric(1)))
      if (length(blocks[[i]]) < 2L) stop("Cannot split oversized query")
      halves <- chunks(blocks[[i]], ceiling(length(blocks[[i]]) / 2))
      unlist(lapply(halves, function(h) { z <- blocks; z[[i]] <- h; partition(z) }), use.names = FALSE)
    }
    route_queries <- lapply(routes, function(r) {
      last <- length(r)
      unlist(lapply(chunks(r[[last]]), function(b) {
        z <- r; z[[last]] <- b; partition(z)
      }), use.names = FALSE)
    })
    queries <- unlist(route_queries, use.names = FALSE)
    route_labels <- rep(names(route_queries), lengths(route_queries))
    write_csv(data.frame(Partition = sprintf("Q%03d", seq_along(queries)), Route = route_labels, Query = queries),
              "PubMed_query_partitions.csv")
    stopifnot(length(queries) > 0L)
    message('Context-restricted search: ', length(voc), ' VOC terms; ', length(brain),
            ' phenotype terms; ', length(queries), ' partitions.')
    search_log <- list()
    search_parts <- list()
    search_part <- function(q, id, lo = NULL, hi = NULL) {
      term <- if (is.null(lo)) q else paste0('(', q, ') AND (', lo, ':', hi, '[dp])')
      Sys.sleep(.4)
      result <- entrez_search(db = 'pubmed', term = term, retmax = 0, use_history = TRUE)
      n <- as.integer(result$count)
      message(id, ': ', n, ' records')
      if (n > 2500L) {
        write_csv(bind_rows(search_log), "Search_log_partial.csv")
        writeLines(term, path("Excessively_broad_query.txt"))
        message("EXCESSIVELY BROAD QUERY: ", term)
        stop("Partition ", id, " exceeds 2500 records. Refine search terms.")
      }
      search_log[[id]] <<- tibble(Database = 'PubMed', Search_date = Sys.Date(),
                                  Partition = id, Count = n, Query = term,
                                  Translation = if (is.null(result$QueryTranslation)) NA_character_ else result$QueryTranslation)
      search_parts[[id]] <<- result
    }
    search_start <- Sys.time()
    for (i in seq_along(queries)) {
      search_part(queries[[i]], sprintf('Q%03d', i))
      progress('Search queries', i, length(queries), search_start)
    }
    # Count unique PMIDs before downloading any citation XML.
    preflight_ids <- unlist(lapply(names(search_parts), function(id) {
      entry <- search_log[[id]]
      if (entry$Count == 0L) return(character())
      Sys.sleep(.4)
      ids <- entrez_search(db = 'pubmed', term = entry$Query,
                           retmax = entry$Count)$ids
      if (length(ids) != entry$Count) stop('Preflight count changed for ', id)
      ids
    }), use.names = FALSE)
    preflight_n <- length(unique(preflight_ids))
    schizophrenia_controls <- c("18262322", "18433512", "21115087", "8227439",
      "7629295", "16049378", "26021720", "35171077", "37045476", "37298866")
    control_audit <- data.frame(PMID = schizophrenia_controls,
      Retrieved = schizophrenia_controls %in% preflight_ids)
    print(control_audit)
    write_csv(control_audit, "Schizophrenia_retrieval_audit.csv")
    message('PREFLIGHT UNIQUE PUBLICATIONS: ', preflight_n,
            ' (historical reference: 1796)')
    write_csv(bind_rows(search_log), 'Search_log.csv')
    if (preflight_n > 2500L) stop('Search exceeds 2500 unique publications; stopped before XML retrieval. Inspect Search_log.csv.')
    checkpoint('PubMed search queries')
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
    batch_total <- sum(vapply(search_parts, function(x) ceiling(as.numeric(x$count)/200), numeric(1)))
    batch_done <- 0L
    fetch_start <- Sys.time()
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
          parsed_batch <- map_dfr(xml_find_all(read_xml(result), './/PubmedArticle | .//PubmedBookArticle'), parse)
          batch_done <<- batch_done + 1L
          progress('XML retrieval and parsing', batch_done, batch_total, fetch_start)
          parsed_batch
        }
      )
      part <- bind_rows(batches)
      message(id, ": expected ", n, "; parsed ", nrow(part), "; missing PMIDs ", sum(blank(part$PMID)), "; duplicate PMIDs ", sum(duplicated(part$PMID)))
      if (nrow(part) != n || any(blank(part$PMID)) || anyDuplicated(part$PMID))
        stop('Retrieval integrity failure in ', id)
      part$Search_partition <- id
      part
    })
    checkpoint('XML retrieval and parsing')
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
    checkpoint('Deduplication and candidate classification')
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
        'Investigates a phenotype within at least one of the six predefined phenotype groups; standalone emotional states, substance use disorders, eating disorders and removed phenotype categories (including personality disorders) are outside scope.',
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
    records <- records %>% mutate(search_text=paste(coalesce(Title, ""),coalesce(Abstract, ""),coalesce(MeSH_terms, "")),
                                  Candidate_disorder=map_chr(search_text,classify,dict=disorders),
                                  Candidate_phenotype_group=map_chr(Candidate_disorder,classify_group),
                                  Candidate_biospecimen=map_chr(search_text,classify,dict=specimens),
                                  Candidate_platform=map_chr(search_text,classify,dict=platforms))
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
    
    animal_pattern <- paste(c(
      '\\bmice\\b',
      '\\bmouse\\b',
      '\\brats?\\b',
      '\\bmurine\\b',
      '\\brodents?\\b',
      '\\bdrosophila\\b',
      '\\bfruit fl(y|ies)\\b',
      '\\bzebrafish\\b',
      '\\bdanio rerio\\b',
      '\\bcaenorhabditis\\b',
      '\\bC\\. elegans\\b',
      '\\bnematodes?\\b',
      '\\bguinea pigs?\\b',
      '\\bhamsters?\\b',
      '\\bgerbils?\\b',
      '\\brabbits?\\b',
      '\\bdogs?\\b',
      '\\bcanine\\b',
      '\\bcats?\\b',
      '\\bfeline\\b',
      '\\bpigs?\\b',
      '\\bporcine\\b',
      '\\bswine\\b',
      '\\bbovine\\b',
      '\\bsheep\\b',
      '\\bovine\\b',
      '\\bgoats?\\b',
      '\\bferrets?\\b',
      '\\bmonkeys?\\b',
      '\\bmacaques?\\b',
      '\\bmarmosets?\\b',
      '\\bchimpanzees?\\b',
      'non.human primates?',
      '\\bxenopus\\b',
      '\\bfrogs?\\b',
      '\\bchickens?\\b',
      'animal models?',
      'animal experiments?',
      'laboratory animals?',
      'experimental animals?',
      'C57BL',
      'BALB/c',
      'Sprague.Dawley',
      '\\bWistar\\b'
    ), collapse = "|")        
    
    empirical_pattern <- paste0(
      "\\b(",
      paste(c(
        
        # Study recruitment and sample collection
        "recruited", "enrolled", "collected", "sampled",
        "obtained", "participants?", "patients?", "cohorts?",
        
        # Empirical measurement
        "measured", "quantified", "analysed", "analyzed",
        "detected", "compared", "concentrations?", "abundance",
        "identified", "investigated", "validated",
        "examined", "assessed", "characterised", "characterized",
        "determined", "estimated", "profiled",
        
        # Gas chromatography
        "chromatograph\\w*",
        "GC[-– ]?MS",
        "GC[-– ]?IMS",
        "GC[-– ]?FID",
        "GC[-– ]?TOF[-– ]?MS",
        "GC[-– ]?QTOF",
        "GC[-– ]?MS/MS",
        "GC[x×]GC",
        "TD[-– ]?GC[-– ]?MS",
        "HS[-– ]?GC[-– ]?MS",
        
        # Mass spectrometry
        "mass spectrometr\\w*",
        "PTR[-– ]?MS",
        "PTR[-– ]?TOF[-– ]?MS",
        "SIFT[-– ]?MS",
        "SESI[-– ]?HRMS",
        "SESI[-– ]?MS",
        "LC[-– ]?MS",
        
        # Ion mobility and spectroscopy
        "ion mobility spectrometr\\w*",
        "FAIMS", "IMS", "DMS",
        "spectroscop\\w*",
        "photoacoustic spectroscop\\w*",
        
        # Electronic noses and chemical sensors
        "electronic noses?",
        "e[-– ]?nose",
        "sensor arrays?",
        "gas sensors?",
        "nanosensors?",
        "surface acoustic wave",
        "chemiresistive sensors?",
        
        # Sample preparation and volatile extraction
        "headspace",
        "thermal desorption",
        "solid.phase microextraction",
        "SPME",
        "HS[-– ]?SPME",
        "SBSE",
        "purge.and.trap",
        
        # Statistical and diagnostic evaluation
        "discriminated", "distinguished", "classified",
        "sensitivity", "specificity",
        "classification accuracy",
        "diagnostic accuracy",
        "receiver operating characteristic",
        "AUC", "AUROC",
        "cross.validation",
        "findings", "results"
        
      ), collapse = "|"),
      ")\\b"
    )
    
    prescreen <- records %>% mutate(
      text_for_screening = paste(coalesce(Title,''), coalesce(Abstract,'')),
      Has_abstract = present(Abstract),
      Non_primary_type = has_label(Publication_types, nonprimary),
      Primary_type = has_label(Publication_types, primary),
      Human_MeSH = has_label(MeSH_terms, 'Humans'),
      Animal_MeSH = has_label(MeSH_terms, c('Animals', 'Mice', 'Rats','Dogs','Rabbits','Swine','Zebrafish','Primates','Cattle','Sheep')),
      Human_text = match_text(text_for_screening, human_pattern),
      Animal_text = match_text(text_for_screening, animal_pattern),
      Empirical_text = match_text(text_for_screening, empirical_pattern),
      Specimen_detected = present(Candidate_biospecimen),
      Platform_detected = present(Candidate_platform),
      Prescreen_status = case_when(
        Non_primary_type & !Primary_type ~ 'Likely non-primary',
        (Animal_MeSH | Animal_text) &
          !Human_MeSH &
          !Human_text ~ 'Likely animal-only',
        (Human_MeSH | Human_text) & (Empirical_text | Primary_type) ~ 'Likely primary human research',
        TRUE ~ 'Uncertain'),
      High_priority = Prescreen_status == 'Likely primary human research' & Specimen_detected & Platform_detected,
      Retain_S1 = Prescreen_status != 'Likely non-primary',
      Retain_S2 = Retain_S1 & Prescreen_status != 'Likely animal-only',
      Retain_S3 = Prescreen_status == 'Likely primary human research'
    ) %>% select(-text_for_screening)
    
    # measurement_terms <- c('chromatograph','spectrometr','spectroscop','sensor','electronic nose','headspace','chemical analys','chemical profil','volatile profil','volatile compound','volatile organic','volatile metabolite','volatile emission','volatile signature','compound identification','compound concentration','breathprint','breathomics','volatilom','gas analys','solid.phase microextraction','thermal desorption','\\bSPME\\b','\\bGC\\b','\\bGC.MS\\b','\\bPTR.MS\\b','\\bSIFT.MS\\b')
    
    measurement_terms <- c(
      
      # volatile compound terminology
      '\\bvolatile organic',
      '\\bvolatile compound',
      '\\bvolatile metabolite',
      '\\bvolatile molecule',
      '\\bvolatile chemical',
      '\\bvolatile biomarker',
      '\\bvolatile marker',
      '\\bvolatile analyte',
      '\\bvolatile emission',
      '\\bvolatile species',
      '\\bvolatile substance',
      '\\bvolatile signature',
      '\\bvolatile fingerprint',
      '\\bvolatile pattern',
      '\\bvolatile profil',
      '\\bvolatile fraction',
      '\\bvolatile component',
      '\\bvolatile feature',
      '\\bvolatile metabolom',
      '\\bvolatile lipid',
      '\\bvolatilom',
      
      # Gas chromatography
      'gas chromatograph',
      '\\bGC[- ]?MS\\b',
      '\\bGC[- ]?IMS\\b',
      '\\bGC[- ]?FID\\b',
      '\\bGC[- ]?TOF[- ]?MS\\b',
      '\\bGC[- ]?QTOF',
      '\\bGC[- ]?MS[- ]?MS\\b',
      '\\bGC[- ]?MS/MS\\b',
      '\\bGC[x×]GC\\b',
      'comprehensive two.dimensional gas chromatograph',
      'flame ioni[sz]ation detect',
      
      # Direct gas-phase mass spectrometry
      'proton.transfer.reaction',
      'selected.ion.flow.tube',
      'secondary electrospray ioni[sz]ation',
      '\\bPTR[- ]?MS\\b',
      '\\bPTR[- ]?TOF[- ]?MS\\b',
      '\\bPTR[- ]?ToF\\b',
      '\\bSIFT[- ]?MS\\b',
      '\\bSESI[- ]?HRMS\\b',
      '\\bSESI[- ]?MS\\b',
      
      # Gas-phase ion mobility
      'gas chromatography.ion mobility',
      'field asymmetric ion mobility',
      'differential mobility spectrometr',
      '\\bFAIMS\\b',
      '\\bGC[- ]?IMS\\b',
      
      # Gas-phase spectroscopy
      'gas.phase infrared',
      'gas.phase spectroscop',
      'gas absorption spectroscop',
      'photoacoustic gas',
      'photoacoustic spectroscop',
      'cavity ring.down spectroscop',
      'tunable diode laser absorption',
      '\\bTDLAS\\b',
      '\\bCRDS\\b',
      
      # Electronic noses and volatile sensing
      'electronic nose',
      'e[- ]?nose',
      'artificial nose',
      'artificial intelligen\\w* olfactory',
      'artificial olfactory',
      'olfactory.like system',
      'gas sensor',
      'gas sensing',
      'volatile sensor',
      'volatile sensing',
      'gas sensor array',
      'chemical gas sensor',
      'chemiresistive gas sensor',
      'chemoresistive gas sensor',
      'odou?r sensor',
      'odou?r sensing',
      'surface acoustic wave gas sensor',
      'virtual surface acoustic wave',
      
      # Breathomics
      'breathomics',
      'breathprint',
      'breath metabolom',
      'breath fingerprint',
      'breath volatil',
      'breath chemical profil',
      'breath chemical composition',
      'breath VOC',
      'exhaled metabolom',
      'exhaled volatil',
      'exhaled VOC',
      'exhaled organic compound',
      'alveolar volatil',
      
      # Sebum and skin volatilomics
      'sebum volatil',
      'volatile.*sebum',
      'skin volatil',
      'volatile.*skin emission',
      'skin gas emission',
      'skin VOC',
      'sebum VOC',
      
      # Other biospecimen-specific VOC terminology
      'urinary volatil',
      'urine volatil',
      'urinary VOC',
      'urine VOC',
      'faecal volatil',
      'fecal volatil',
      'faecal VOC',
      'fecal VOC',
      'stool volatil',
      'stool VOC',
      'blood volatil',
      'blood VOC',
      'plasma volatil',
      'plasma VOC',
      'serum volatil',
      'serum VOC',
      'salivary volatil',
      'saliva volatil',
      'salivary VOC',
      'sweat volatil',
      'sweat VOC',
      'cerumen volatil',
      'earwax volatil',
      'cerebrospinal fluid volatil',
      'CSF volatil',
      
      # Volatile sampling and extraction
      'headspace',
      'head.space',
      'thermal desorption',
      'solid.phase microextraction',
      'stir.bar sorptive extraction',
      'purge.and.trap',
      'sorbent tube',
      'adsorbent tube',
      'volatile extraction',
      'volatile collection',
      '\\bSPME\\b',
      '\\bHS[- ]?SPME\\b',
      '\\bTD[- ]?GC\\b',
      '\\bSBSE\\b',
      '\\bTenax\\b',
      '\\bTedlar\\b',
      '\\bCarboxen\\b',
      
      # Volatile metabolic products
      'short.chain fatty acid',
      'branched.chain fatty acid',
      'volatile fatty acid',
      'volatile sul[fph]+ur compound',
      'volatile organic acid',
      'gaseous metabolite',
      'microbial volatile',
      'bacterial volatile',
      'fungal volatile',
      '\\bSCFAs?\\b',
      '\\bBCFAs?\\b'
    )
    
    measurement_terms <- unique(measurement_terms)
    
    
    prescreen <- prescreen %>% mutate(
      Measurement_evidence = match_text(paste(Title, Abstract), paste(measurement_terms, collapse='|')),
      Screening_tier = case_when(
        Retain_S3 & Specimen_detected & Platform_detected ~ 1L,
        Retain_S3 & Specimen_detected & Measurement_evidence ~ 2L,
        Retain_S3 & Specimen_detected ~ 3L,
        Retain_S3 ~ 4L,
        Prescreen_status == 'Uncertain' ~ 5L,
        TRUE ~ 6L))
    
    checkpoint('Automated prescreening and tier assignment')
    counts <- prescreen %>% count(Screening_tier, name='N')
    print(counts)
    summary <- tibble(Scenario=c('Original','Remove likely non-primary','Also remove likely animal-only','Positive human empirical evidence','Priority tiers 1 and 2','Named platform and specimen'), Retained=c(N,sum(prescreen$Retain_S1),sum(prescreen$Retain_S2),sum(prescreen$Retain_S3),sum(prescreen$Screening_tier<=2),sum(prescreen$Screening_tier==1))) %>% mutate(Removed=N-Retained, Reduction_percent=round(100*Removed/N,1))
    print(summary)
    original <- path('PubMed_manual_screening.xlsx')
    manual <- read_sheet(original,'Screening')
    manual$PMID <- as.character(manual$PMID)
    stopifnot(!anyDuplicated(manual$PMID),setequal(manual$PMID,prescreen$PMID))
    flags <- prescreen %>% select(PMID, Candidate_disorder, Candidate_phenotype_group, Candidate_biospecimen, Candidate_platform, Prescreen_status, Human_MeSH, Animal_MeSH, Animal_text, Human_text, Empirical_text, Specimen_detected, Platform_detected, Measurement_evidence, Screening_tier)
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
    
  }
  
  # Historical manual exclusion lists (proposals; confirm in the ledger).
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
    
    "Olfactory perception and chemosignalling, e.g. canine VOC" = c(
      39137693, 36170254, 33360399,
      34111768, 28835640, 36412058,
      39352420
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
  
  exclusion_lookup <- enframe(
    manual_exclusions,
    name = "Exclusion_reason",
    value = "PMID"
  ) %>%
    unnest_longer(PMID) %>%
    mutate(PMID = as.character(PMID))
  
  # Check that each PMID has only one exclusion reason
  
  stopifnot(!anyDuplicated(exclusion_lookup$PMID))
  
  
  # Stage 2: one editable ledger, all retrieved PMIDs retained.
  if (STAGE == 2L) {
    checkpoint("Historical exclusion lookup setup")
    papers <- read_sheet(path('PubMed_prioritised_screening.xlsx'), 'Screening')
    papers$PMID <- as.character(papers$PMID)
    expected <- read.csv(path('PubMed_screening_records.csv'), colClasses='character')
    stopifnot(!anyDuplicated(papers$PMID), setequal(papers$PMID, expected$PMID))
    priority <- papers
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
      ungroup()
    
    ledger <- priority_screened %>%
      select(-Title_text, -Abstract_text) %>%
      left_join(exclusion_lookup, by='PMID') %>%
      mutate(Historical_exclusion = PMID %in% as.character(exclude_pmids) | !blank(Exclusion_reason),
             Historical_reason = case_when(!blank(Exclusion_reason) ~ Exclusion_reason,
                                           Historical_exclusion ~ 'Previously excluded; reason requires confirmation', TRUE ~ ''),
             Screening_route = if_else(Screening_tier <= 2, 'Priority tiers 1-2', 'Lower tiers 3-6')) %>%
      select(-Exclusion_reason) %>%
      mutate(Screening_queue = case_when(
        Screening_tier > 2 ~ 'Lower tier',
        Flag_count > 0 ~ 'Title flag',
        PMID %in% as.character(exclude_pmids) ~ 'First manual exclusion',
        Historical_exclusion ~ 'Further manual exclusion',
        TRUE ~ 'Retained candidate')) %>%
      arrange(match(Screening_queue, c('Retained candidate','Further manual exclusion','First manual exclusion','Title flag','Lower tier')), Screening_tier)
    priority <- ledger %>% filter(Screening_tier <= 2)
    priority_refined <- priority %>% filter(Flag_count == 0)
    priority_cleaned <- priority_refined %>% filter(!PMID %in% as.character(exclude_pmids))
    priority_cleaned_final <- priority_cleaned %>% filter(!Historical_exclusion)
    screening_counts <- tibble(Step=c('Retrieved','Priority tiers 1-2','After title flags','After first manual list','After further manual list'), N=c(nrow(ledger),nrow(priority),nrow(priority_refined),nrow(priority_cleaned),nrow(priority_cleaned_final)))
    print(screening_counts)
    write_csv(screening_counts,'Priority_attrition_counts.csv')
    write_csv(priority_cleaned_final,'PubMed_priority_cleaned_final.csv')
    book(list(Screening=ledger,
              Retained_candidates=priority_cleaned_final,
              Priority_counts=screening_counts,
              Eligibility=read_sheet(path('PubMed_prioritised_screening.xlsx'),'Eligibility'),
              Instructions=tibble(Instructions=c(
                'Screen all records; tiers and flags are not exclusions.',
                'Confirm historical exclusions and record gate and reason.',
                'Title_abstract_verdict: Include, Exclude or Unsure.',
                'Full_text_verdict: Include, Exclude, Not retrieved or Not required.',
                'Use Not required only for title/abstract exclusions.',
                'Final_verdict must be Include, Exclude or Not retrieved. Edit Screening only; Retained_candidates is a reference sheet.',
                'Final inclusion requires full-text inclusion.'))),
         path('PubMed_final_screening.xlsx'),
         list(Screening=list(Title_abstract_verdict=c('Include','Exclude','Unsure'),
                             Full_text_verdict=c('Include','Exclude','Not retrieved','Not required'),
                             Final_verdict=c('Include','Exclude','Not retrieved'), Final_gate=paste('Gate',1:4))))
  }
  
  # Stage 2 figures: generated only after the candidate ledger has been exported.
  if (STAGE == 2L) {
    library(ggplot2)
    library(wesanderson)
    figure_dir <- path('figures')
    dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
    palette <- wesanderson::wes_palette('Zissou1', 100, type = 'continuous')
    save_plot <- function(p, filename, height = 5) {
      ggplot2::ggsave(file.path(figure_dir, paste0(filename, '.png')),
                      plot = p, width = 10, height = height, dpi = 300)
      ggplot2::ggsave(file.path(figure_dir, paste0(filename, '.pdf')),
                      plot = p, width = 10, height = height)
    }
    voc_final <- priority_cleaned_final
    voc_attrition <- screening_counts %>%
      mutate(Step = factor(Step, levels = rev(Step)),
             Removed = lag(N) - N,
             Retained_percent = round(100 * N / first(N), 1))
    voc_attrition_plot <- ggplot(voc_attrition, aes(Step, N, fill = N)) +
      geom_col(width = .7) +
      geom_text(aes(label = paste0(N, ' (', Retained_percent, '%)')), hjust = -.15) +
      coord_flip() + scale_fill_gradientn(colours = palette) +
      scale_y_continuous(expand = expansion(mult = c(0, .25))) +
      labs(title = 'Neurovolatilome literature screening',
           subtitle = 'Priority-screening attrition (not final eligibility)',
           x = NULL, y = 'Number of publications') +
      theme_classic(base_size = 13) + theme(legend.position = 'none')
    save_plot(voc_attrition_plot, 'Priority_attrition')
    print(voc_attrition_plot)
    
    # Each PMID is counted once per label; labels may overlap across papers.
    label_counts <- function(field) {
      voc_final %>% select(PMID, all_of(field)) %>%
        rename(Label = all_of(field)) %>%
        separate_rows(Label, sep = ';\\s*') %>%
        mutate(Label = trimws(coalesce(Label, '')),
               Label = if_else(Label == '', 'Unclassified', Label)) %>%
        distinct(PMID, Label) %>% count(Label, sort = TRUE)
    }
    voc_phenotype_counts <- label_counts('Candidate_phenotype_group') %>%
      rename(Candidate_phenotype_group = Label)
    voc_disorder_counts <- label_counts('Candidate_disorder') %>%
      rename(Candidate_disorder = Label)
    write_csv(voc_phenotype_counts, 'Priority_phenotype_counts.csv')
    write_csv(voc_disorder_counts, 'Priority_disorder_counts.csv')
    label_plot <- function(data, field, heading) {
      ggplot(data, aes(x = reorder(.data[[field]], n), y = n, fill = n)) +
        geom_col(width = .75) + geom_text(aes(label = n), hjust = -.2, size = 3.5) +
        coord_flip() + scale_fill_gradientn(colours = palette) +
        scale_y_continuous(expand = expansion(mult = c(0, .15))) +
        labs(title = heading,
             subtitle = paste(nrow(voc_final), 'retained candidates; non-exclusive labels'),
             x = NULL, y = 'Number of publications') +
        theme_classic(base_size = 12) + theme(legend.position = 'none')
    }
    if (nrow(voc_final) > 0) {
      voc_phenotype_plot <- label_plot(voc_phenotype_counts,
                                       'Candidate_phenotype_group', 'The neurovolatilome: phenotype groups')
      voc_disorder_plot <- label_plot(voc_disorder_counts,
                                      'Candidate_disorder', 'Phenotypic coverage of VOC research')
      save_plot(voc_phenotype_plot, 'Priority_phenotype_groups',
                max(4, .45 * nrow(voc_phenotype_counts) + 2))
      save_plot(voc_disorder_plot, 'Priority_individual_disorders',
                max(5, .30 * nrow(voc_disorder_counts) + 2))
      print(voc_phenotype_plot)
      print(voc_disorder_plot)
    } else message('No retained candidates; phenotype plots skipped.')
    
    # Sankey includes retrieval duplicates and mutually exclusive screening routes.
    r <- read.csv(path('PubMed_retrieval_summary.csv'))
    stopifnot(nrow(r) == 1L, r$Unique_PMIDs == screening_counts$N[1],
              r$Raw_hits == r$Unique_PMIDs + r$Duplicate_hits,
              all(diff(screening_counts$N) <= 0))
    labs_n <- function(label, n) paste0(label, ' (n = ', n, ')')
    retained_nodes <- labs_n(screening_counts$Step, screening_counts$N)
    removed_n <- -diff(screening_counts$N)
    removed_nodes <- labs_n(c('Lower tiers 3-6', 'Title flags',
                              'First historical exclusion list',
                              'Further historical exclusions'), removed_n)
    raw_node <- labs_n('Search partition hits', r$Raw_hits)
    voc_sankey_flows <- bind_rows(
      tibble(Source = raw_node, Target = retained_nodes[1], N = r$Unique_PMIDs, Flow = 'Retained'),
      tibble(Source = raw_node, Target = labs_n('Duplicate hits', r$Duplicate_hits), N = r$Duplicate_hits, Flow = 'Duplicate'),
      tibble(Source = head(retained_nodes, -1), Target = tail(retained_nodes, -1), N = tail(screening_counts$N, -1), Flow = 'Retained'),
      tibble(Source = head(retained_nodes, -1), Target = removed_nodes, N = removed_n, Flow = 'Deprioritised')
    ) %>% filter(N > 0)
    voc_nodes <- tibble(Name = unique(c(voc_sankey_flows$Source, voc_sankey_flows$Target)))
    voc_edges <- voc_sankey_flows %>% mutate(source = match(Source, voc_nodes$Name) - 1L,
                                             target = match(Target, voc_nodes$Name) - 1L)
    for (node in intersect(voc_edges$Source, voc_edges$Target)) {
      stopifnot(sum(voc_edges$N[voc_edges$Source == node]) ==
                  sum(voc_edges$N[voc_edges$Target == node]))
    }
    voc_sankey <- networkD3::sankeyNetwork(
      Links = as.data.frame(voc_edges), Nodes = as.data.frame(voc_nodes),
      Source = 'source', Target = 'target', Value = 'N', NodeID = 'Name',
      LinkGroup = 'Flow', fontSize = 13, nodeWidth = 22, nodePadding = 20,
      sinksRight = FALSE, width = 1500, height = 650, margin = list(right = 280),
      colourScale = htmlwidgets::JS('d3.scaleOrdinal().domain(["Retained","Duplicate","Deprioritised"]).range(["#438B88","#999999","#BA7373"])'))
    write_csv(voc_sankey_flows, 'Priority_sankey_flows.csv')
    htmlwidgets::saveWidget(voc_sankey, file.path(figure_dir, 'Priority_sankey.html'), selfcontained = FALSE)
    print(voc_sankey)
    message('Priority figures saved to: ', figure_dir)
    checkpoint('Priority screening figures and Sankey')
  }
  
  # Stage 3: validate decisions, export eligible papers, then draw the Sankey.
  if (STAGE == 3L) {
    s <- read_sheet(path('PubMed_final_screening.xlsx'),'Screening')
    s$PMID <- as.character(s$PMID)
    expected <- read.csv(path('PubMed_screening_records.csv'),colClasses='character')
    stopifnot(!anyDuplicated(s$PMID), setequal(s$PMID,expected$PMID))
    require_fields(s,c('PMID','Title_abstract_verdict','Full_text_verdict','Final_verdict','Screening_tier'),'Final screening')
    stopifnot(all(s$Screening_tier %in% 1:6), all(s$Title_abstract_verdict %in% c('Include','Exclude','Unsure')),
              all(s$Final_verdict %in% c('Include','Exclude','Not retrieved')))
    ta <- s$Title_abstract_verdict == 'Exclude'
    stopifnot(all(s$Full_text_verdict[ta]=='Not required'),all(s$Final_verdict[ta]=='Exclude'),
              all(s$Full_text_verdict[!ta] %in% c('Include','Exclude','Not retrieved')),
              all(s$Full_text_verdict[!ta]==s$Final_verdict[!ta]))
    excluded <- filter(s,Final_verdict=='Exclude')
    require_fields(excluded,c('Final_gate','Final_reason'),'Excluded publications')
    stopifnot(all(excluded$Final_gate %in% paste('Gate',1:4)))
    included <- filter(s,Final_verdict=='Include')
    checkpoint('Final decision validation')
    r <- read.csv(path('PubMed_retrieval_summary.csv'))
    stopifnot(nrow(r)==1,r$Unique_PMIDs==nrow(s),r$Raw_hits==nrow(s)+r$Duplicate_hits,r$Duplicate_hits>=0)
    links <- tibble(Source=character(),Target=character(),N=integer(),Flow=character())
    lab <- function(x,n) paste0(x,' (n = ',n,')')
    add <- function(a,b,n,flow='Retained') {
      if(n>0) links <<- bind_rows(links,tibble(Source=a,Target=b,N=as.integer(n),Flow=flow))
    }
    raw <- lab('Search partition hits',r$Raw_hits)
    unique_node <- lab('Unique publications',nrow(s))
    add(raw,lab('Duplicate hits',r$Duplicate_hits),r$Duplicate_hits,'Duplicate')
    add(raw,unique_node,nrow(s))
    screened <- lab('Title/abstract screened',nrow(s))
    for (tier in list(1:2,3:6)) {
      n <- sum(s$Screening_tier %in% tier)
      node <- lab(if(length(tier)==2) 'Priority tiers 1-2' else 'Lower tiers 3-6',n)
      add(unique_node,node,n); add(node,screened,n)
    }
    full <- lab('Full text sought',sum(!ta))
    add(screened,full,sum(!ta))
    for(g in paste('Gate',1:4)) {
      n <- sum(ta & s$Final_verdict=='Exclude' & s$Final_gate==g,na.rm=TRUE)
      add(screened,lab(paste('Title/abstract exclusion:',g),n),n,'Excluded')
    }
    missing <- sum(s$Final_verdict=='Not retrieved')
    assessed <- lab('Full text assessed',sum(!ta)-missing)
    add(full,lab('Full text not retrieved',missing),missing,'Not retrieved')
    add(full,assessed,sum(!ta)-missing)
    for(g in paste('Gate',1:4)) {
      n <- sum(!ta & s$Final_verdict=='Exclude' & s$Final_gate==g,na.rm=TRUE)
      add(assessed,lab(paste('Full-text exclusion:',g),n),n,'Excluded')
    }
    add(assessed,lab('Eligible publications',nrow(included)),nrow(included),'Included')
    stopifnot(nrow(s)==nrow(included)+nrow(excluded)+missing)
    nodes <- tibble(Name=unique(c(links$Source,links$Target)))
    edges <- mutate(links,source=match(Source,nodes$Name)-1L,target=match(Target,nodes$Name)-1L)
    for(node in intersect(links$Source,links$Target)) {
      stopifnot(sum(links$N[links$Source==node])==sum(links$N[links$Target==node]))
    }
    summary <- count(s,Final_verdict,name='N')
    reasons <- excluded %>% count(Title_abstract_verdict,Final_gate,Final_reason,name='N')
    book(list(Eligible_papers=included,All_decisions=s,Exclusions=excluded,
              Summary=summary,Exclusion_reasons=reasons,Attrition_flows=links),
         path('PubMed_eligible_papers.xlsx'))
    write_csv(included,'PubMed_final_included.csv')
    write_csv(reasons,'Screening_exclusions.csv')
    write_csv(links,'Sankey_final_flows.csv')
    checkpoint('Final workbook and CSV export')
    diagram <- networkD3::sankeyNetwork(Links=as.data.frame(edges),Nodes=as.data.frame(nodes),
                                        Source='source',Target='target',Value='N',NodeID='Name',LinkGroup='Flow',
                                        colourScale=htmlwidgets::JS('d3.scaleOrdinal().domain(["Retained","Duplicate","Excluded","Not retrieved","Included"]).range(["#5079A5","#999999","#BA7373","#A0A0A0","#438B88"])'),
                                        fontSize=12,nodeWidth=20,nodePadding=18,sinksRight=FALSE,width=1500,height=750,margin=list(right=300))
    htmlwidgets::saveWidget(diagram,path('Sankey_final.html'),selfcontained=FALSE)
    checkpoint('Final Sankey rendering')
  }
  
  if (STAGE == 1) message('RETRIEVED UNIQUE PUBLICATIONS: ', nrow(records), '; run stage 1.5 next.')
  if (STAGE == 1.5) message('PRIORITY PUBLICATIONS (TIERS 1-2): ', sum(prescreen$Screening_tier <= 2), '; run stage 2 next.')
  if (STAGE == 2) message('FINAL PRIORITY SCREENED CANDIDATES: ', nrow(priority_cleaned_final), '; eligibility confirmation pending.')
  if (STAGE == 3) message('FINAL ELIGIBLE PUBLICATIONS: ', nrow(included))
  checkpoint("Remaining stage operations")
  run_status <- "Completed"
  message("Timing log: ", timing_file)
  invisible(environment())
}

# One-click pipeline: always perform a fresh PubMed search.
voc_screening_run <- (function() {
  base_dir <- path.expand('~/Documents/VOCs_project/data_screening_four_routes')
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- file.path(base_dir, 'runs', format(Sys.time(), '%Y%m%d_%H%M%OS6'))
  stopifnot(dir.create(run_dir, recursive = TRUE))
  voc_execute_stage(1, run_dir)
  voc_execute_stage(1.5, run_dir)
  result <- voc_execute_stage(2, run_dir)
  message('COMPLETE: ', nrow(result$priority_cleaned_final), ' retained candidates.')
  message('Workbook: ', file.path(run_dir, 'PubMed_final_screening.xlsx'))
  message('Figures: ', file.path(run_dir, 'figures'))
  if (exists('voc_disorder_plot', envir = result, inherits = FALSE)) print(result$voc_disorder_plot)
  result
})()

# Optional: after completing the Screening sheet manually, run this in the console.
# voc_final_eligibility <- voc_execute_stage(3, voc_screening_run$output_dir)

voc_final <- voc_screening_run$priority_cleaned_final

View(voc_final)


# Screening attrition bar plot
#print(voc_screening_run$voc_attrition_plot)


# Sankey diagram
#voc_screening_run$voc_sankey

# Publications by phenotype group
#print(voc_screening_run$voc_phenotype_plot)

# Publications by individual disorder
if (exists("voc_disorder_plot", envir = voc_screening_run, inherits = FALSE)) print(voc_screening_run$voc_disorder_plot)

