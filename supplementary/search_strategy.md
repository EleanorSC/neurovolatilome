# Literature search strategy

## Objective

To identify publications investigating human-derived volatile organic compounds (VOCs) or instrument-measured volatile signatures in neurological, neuropsychiatric, neurodevelopmental and neurometabolic disorders and prodromal neurocognitive phenotypes.

## Database and search implementation

-   Database: PubMed (National Library of Medicine).
-   Interface: NCBI Entrez E-utilities, accessed in R using `rentrez`.
-   Search fields: Title/Abstract and Medical Subject Headings (MeSH).
-   Coverage: database inception to the search date; no explicit language, human-only or publication-type filter in the retrieval query.
-   Search date: see `Search_log.csv` for the executed search date.

## Search logic

Two concept blocks were combined with `AND`. Terms within each block were combined with `OR`. Asterisks denote PubMed truncation. Phenotype search terms are literal PubMed terms, not the regular expressions used for subsequent candidate classification.

### Block 1: Volatile organic compounds

``` text
(
  "volatile organic compound*"[Title/Abstract]
  OR "volatile compound*"[Title/Abstract]
  OR "volatile metabolite*"[Title/Abstract]
  OR "volatile biomarker*"[Title/Abstract]
  OR "VOC"[Title/Abstract]
  OR "VOCs"[Title/Abstract]
  OR "volatilome"[Title/Abstract]
  OR "volatilomics"[Title/Abstract]
  OR "breathomics"[Title/Abstract]
  OR "breath metabolom*"[Title/Abstract]
  OR "breath analys*"[Title/Abstract]
  OR "breath profil*"[Title/Abstract]
  OR "exhaled breath"[Title/Abstract]
  OR "breath fingerprint*"[Title/Abstract]
  OR "breathprint*"[Title/Abstract]
  OR "electronic nose"[Title/Abstract]
  OR "electronic noses"[Title/Abstract]
  OR "e-nose"[Title/Abstract]
  OR "chemical fingerprint*"[Title/Abstract]
  OR "volatile fingerprint*"[Title/Abstract]
  OR "body odor*"[Title/Abstract]
  OR "body odour*"[Title/Abstract]
  OR "human odor*"[Title/Abstract]
  OR "human odour*"[Title/Abstract]
  OR "skin volatile*"[Title/Abstract]
  OR "sebum volatile*"[Title/Abstract]
  OR "urinary volatile*"[Title/Abstract]
  OR "fecal volatile*"[Title/Abstract]
  OR "faecal volatile*"[Title/Abstract]
  OR "axillary odor*"[Title/Abstract]
  OR "axillary odour*"[Title/Abstract]
  OR "chemosignal*"[Title/Abstract]
  OR "fetor hepaticus"[Title/Abstract]
  OR "sotolon"[Title/Abstract]
  OR "phenylacetate"[Title/Abstract]
  OR "musty odor"[Title/Abstract]
  OR "musty odour"[Title/Abstract]
  OR "maple syrup odor"[Title/Abstract]
  OR "maple syrup odour"[Title/Abstract]
  OR "Volatile Organic Compounds"[MeSH Terms]
)
```

### Block 2: Clinical phenotypes

The classification dictionary covers 140 phenotype labels across six groups:

  Phenotype group                         Labels
  ------------------------------------- --------
  Neurodegenerative disorders                 21
  Other neurological disorders                23
  Neuropsychiatric disorders                  37
  Neurodevelopmental disorders                16
  Prodromal neurocognitive phenotypes          3
  Neurometabolic disorders                    40
  Total                                      140

The search block combines general clinical terms, named phenotype labels (excluding eight nonspecific or composite labels), additional synonyms and MeSH headings. The reconstructed Boolean block from `VOC_comprehensive_screening.R` is:

``` text
(
  "neuropsychiatr*"[Title/Abstract]
  OR "neurologic*"[Title/Abstract]
  OR "psychiatric"[Title/Abstract]
  OR "neurodegenerat*"[Title/Abstract]
  OR "brain health"[Title/Abstract]
  OR "mental health"[Title/Abstract]
  OR "schizophrenia"[Title/Abstract]
  OR "psychosis"[Title/Abstract]
  OR "psychotic"[Title/Abstract]
  OR "depress*"[Title/Abstract]
  OR "bipolar"[Title/Abstract]
  OR "anxiety"[Title/Abstract]
  OR "post-traumatic stress"[Title/Abstract]
  OR "PTSD"[Title/Abstract]
  OR "obsessive compulsive"[Title/Abstract]
  OR "autis*"[Title/Abstract]
  OR "ADHD"[Title/Abstract]
  OR "attention deficit"[Title/Abstract]
  OR "Parkinson*"[Title/Abstract]
  OR "Alzheimer*"[Title/Abstract]
  OR "dementia"[Title/Abstract]
  OR "mild cognitive impairment"[Title/Abstract]
  OR "cognitive decline"[Title/Abstract]
  OR "cognitive impairment"[Title/Abstract]
  OR "multiple sclerosis"[Title/Abstract]
  OR "motor neuron disease"[Title/Abstract]
  OR "motor neurone disease"[Title/Abstract]
  OR "amyotrophic lateral sclerosis"[Title/Abstract]
  OR "Huntington*"[Title/Abstract]
  OR "epilep*"[Title/Abstract]
  OR "migraine*"[Title/Abstract]
  OR "stroke"[Title/Abstract]
  OR "traumatic brain injury"[Title/Abstract]
  OR "hepatic encephalopathy"[Title/Abstract]
  OR "phenylketonuria"[Title/Abstract]
  OR "maple syrup urine disease"[Title/Abstract]
  OR "inborn errors of metabolism"[Title/Abstract]
  OR "Alzheimer's disease"[Title/Abstract]
  OR "Parkinson's disease"[Title/Abstract]
  OR "Dementia with Lewy bodies"[Title/Abstract]
  OR "Multiple system atrophy"[Title/Abstract]
  OR "Progressive supranuclear palsy"[Title/Abstract]
  OR "Corticobasal degeneration"[Title/Abstract]
  OR "Motor neuron disease"[Title/Abstract]
  OR "Amyotrophic lateral sclerosis"[Title/Abstract]
  OR "Primary lateral sclerosis"[Title/Abstract]
  OR "Progressive muscular atrophy"[Title/Abstract]
  OR "Spinal muscular atrophy"[Title/Abstract]
  OR "Huntington's disease"[Title/Abstract]
  OR "Spinocerebellar ataxia"[Title/Abstract]
  OR "Friedreich's ataxia"[Title/Abstract]
  OR "Hereditary spastic paraplegia"[Title/Abstract]
  OR "Vascular dementia"[Title/Abstract]
  OR "Frontotemporal dementia"[Title/Abstract]
  OR "Mixed dementia"[Title/Abstract]
  OR "Creutzfeldt-Jakob disease"[Title/Abstract]
  OR "Brain tumours"[Title/Abstract]
  OR "Epilepsy"[Title/Abstract]
  OR "Stroke"[Title/Abstract]
  OR "Migraine"[Title/Abstract]
  OR "Traumatic brain injury"[Title/Abstract]
  OR "Multiple sclerosis"[Title/Abstract]
  OR "Encephalitis"[Title/Abstract]
  OR "Meningitis"[Title/Abstract]
  OR "Mild cognitive impairment"[Title/Abstract]
  OR "Cognitive impairment"[Title/Abstract]
  OR "Schizophrenia"[Title/Abstract]
  OR "Schizoaffective disorder"[Title/Abstract]
  OR "Schizotypal disorder"[Title/Abstract]
  OR "Psychosis"[Title/Abstract]
  OR "Depression"[Title/Abstract]
  OR "Bipolar disorder"[Title/Abstract]
  OR "Mania"[Title/Abstract]
  OR "Cyclothymic disorder"[Title/Abstract]
  OR "Postpartum depression"[Title/Abstract]
  OR "Seasonal affective disorder"[Title/Abstract]
  OR "Premenstrual dysphoric disorder"[Title/Abstract]
  OR "Anxiety disorders"[Title/Abstract]
  OR "Generalised anxiety disorder"[Title/Abstract]
  OR "Panic disorder"[Title/Abstract]
  OR "Specific phobia"[Title/Abstract]
  OR "Agoraphobia"[Title/Abstract]
  OR "Obsessive-compulsive disorder"[Title/Abstract]
  OR "Body dysmorphic disorder"[Title/Abstract]
  OR "Hoarding disorder"[Title/Abstract]
  OR "Trichotillomania"[Title/Abstract]
  OR "Post-traumatic stress disorder"[Title/Abstract]
  OR "Complex PTSD"[Title/Abstract]
  OR "Autism spectrum disorder"[Title/Abstract]
  OR "Attention deficit hyperactivity disorder"[Title/Abstract]
  OR "Intellectual disability"[Title/Abstract]
  OR "Global developmental delay"[Title/Abstract]
  OR "Developmental language disorder"[Title/Abstract]
  OR "Developmental coordination disorder"[Title/Abstract]
  OR "Fragile X syndrome"[Title/Abstract]
  OR "Rett syndrome"[Title/Abstract]
  OR "Down syndrome"[Title/Abstract]
  OR "Angelman syndrome"[Title/Abstract]
  OR "Prader-Willi syndrome"[Title/Abstract]
  OR "Williams syndrome"[Title/Abstract]
  OR "Foetal alcohol spectrum disorders"[Title/Abstract]
  OR "Paranoid personality disorder"[Title/Abstract]
  OR "Schizoid personality disorder"[Title/Abstract]
  OR "Schizotypal personality disorder"[Title/Abstract]
  OR "Borderline personality disorder"[Title/Abstract]
  OR "Narcissistic personality disorder"[Title/Abstract]
  OR "Histrionic personality disorder"[Title/Abstract]
  OR "Antisocial personality disorder"[Title/Abstract]
  OR "Avoidant personality disorder"[Title/Abstract]
  OR "Dependent personality disorder"[Title/Abstract]
  OR "Obsessive-compulsive personality disorder"[Title/Abstract]
  OR "Dissociative identity disorder"[Title/Abstract]
  OR "Functional neurological disorder"[Title/Abstract]
  OR "Fibromyalgia"[Title/Abstract]
  OR "Chronic fatigue syndrome"[Title/Abstract]
  OR "Autoimmune encephalitis"[Title/Abstract]
  OR "Neuropsychiatric lupus"[Title/Abstract]
  OR "Neurosarcoidosis"[Title/Abstract]
  OR "Phenylketonuria"[Title/Abstract]
  OR "Maple syrup urine disease"[Title/Abstract]
  OR "Tyrosinaemia"[Title/Abstract]
  OR "Homocystinuria"[Title/Abstract]
  OR "Non-ketotic hyperglycinaemia"[Title/Abstract]
  OR "Hepatic encephalopathy"[Title/Abstract]
  OR "Wernicke encephalopathy"[Title/Abstract]
  OR "Korsakoff syndrome"[Title/Abstract]
  OR "Diabetic encephalopathy"[Title/Abstract]
  OR "Organic acidaemias"[Title/Abstract]
  OR "Isovaleric acidaemia"[Title/Abstract]
  OR "Propionic acidaemia"[Title/Abstract]
  OR "Methylmalonic acidaemia"[Title/Abstract]
  OR "Glutaric acidaemia"[Title/Abstract]
  OR "Multiple acyl-CoA dehydrogenase deficiency"[Title/Abstract]
  OR "3-methylcrotonyl-CoA carboxylase deficiency"[Title/Abstract]
  OR "Urea cycle disorders"[Title/Abstract]
  OR "Ornithine transcarbamylase deficiency"[Title/Abstract]
  OR "Citrullinaemia"[Title/Abstract]
  OR "Argininosuccinic aciduria"[Title/Abstract]
  OR "Argininaemia"[Title/Abstract]
  OR "Gaucher disease"[Title/Abstract]
  OR "Niemann-Pick disease"[Title/Abstract]
  OR "Fabry disease"[Title/Abstract]
  OR "Tay-Sachs disease"[Title/Abstract]
  OR "Sandhoff disease"[Title/Abstract]
  OR "Krabbe disease"[Title/Abstract]
  OR "Metachromatic leukodystrophy"[Title/Abstract]
  OR "Mucopolysaccharidoses"[Title/Abstract]
  OR "MELAS syndrome"[Title/Abstract]
  OR "Leigh syndrome"[Title/Abstract]
  OR "MERRF syndrome"[Title/Abstract]
  OR "Adrenoleukodystrophy"[Title/Abstract]
  OR "Zellweger spectrum disorder"[Title/Abstract]
  OR "Refsum disease"[Title/Abstract]
  OR "Wilson disease"[Title/Abstract]
  OR "Acute intermittent porphyria"[Title/Abstract]
  OR "Biotinidase deficiency"[Title/Abstract]
  OR "Cerebral creatine deficiency"[Title/Abstract]
  OR "Cancer-related cognitive impairment"[Title/Abstract]
  OR "Catatonia"[Title/Abstract]
  OR "Chronic pain"[Title/Abstract]
  OR "Delirium"[Title/Abstract]
  OR "Encephalopathy"[Title/Abstract]
  OR "Functional seizures"[Title/Abstract]
  OR "Illness anxiety disorder"[Title/Abstract]
  OR "Neuropathic pain"[Title/Abstract]
  OR "Post-COVID cognitive impairment"[Title/Abstract]
  OR "Postoperative cognitive dysfunction"[Title/Abstract]
  OR "Somatic symptom disorder"[Title/Abstract]
  OR "neurodevelopmental disorder*"[Title/Abstract]
  OR "neurometabolic disorder*"[Title/Abstract]
  OR "prion disease*"[Title/Abstract]
  OR "frontotemporal lobar degeneration"[Title/Abstract]
  OR "Pick disease"[Title/Abstract]
  OR "corticobasal syndrome"[Title/Abstract]
  OR "Lewy bod*"[Title/Abstract]
  OR "multi-infarct dementia"[Title/Abstract]
  OR "seizure*"[Title/Abstract]
  OR "ictal"[Title/Abstract]
  OR "concussion"[Title/Abstract]
  OR "cerebrovascular accident"[Title/Abstract]
  OR "cerebral infarct*"[Title/Abstract]
  OR "transient ischemic attack"[Title/Abstract]
  OR "transient ischaemic attack"[Title/Abstract]
  OR "brain tumor*"[Title/Abstract]
  OR "brain tumour*"[Title/Abstract]
  OR "brain neoplasm*"[Title/Abstract]
  OR "brain cancer"[Title/Abstract]
  OR "glioma*"[Title/Abstract]
  OR "glioblastoma*"[Title/Abstract]
  OR "astrocytoma*"[Title/Abstract]
  OR "oligodendroglioma*"[Title/Abstract]
  OR "ependymoma*"[Title/Abstract]
  OR "meningioma*"[Title/Abstract]
  OR "medulloblastoma*"[Title/Abstract]
  OR "ganglioglioma*"[Title/Abstract]
  OR "craniopharyngioma*"[Title/Abstract]
  OR "pituitary adenoma*"[Title/Abstract]
  OR "pituitary tumor*"[Title/Abstract]
  OR "schwannoma*"[Title/Abstract]
  OR "acoustic neuroma*"[Title/Abstract]
  OR "neurocytoma*"[Title/Abstract]
  OR "pineoblastoma*"[Title/Abstract]
  OR "pineocytoma*"[Title/Abstract]
  OR "choroid plexus tumor*"[Title/Abstract]
  OR "choroid plexus tumour*"[Title/Abstract]
  OR "choroid plexus papilloma*"[Title/Abstract]
  OR "hemangioblastoma*"[Title/Abstract]
  OR "haemangioblastoma*"[Title/Abstract]
  OR "central nervous system lymphoma"[Title/Abstract]
  OR "brain metastas*"[Title/Abstract]
  OR "cerebral metastas*"[Title/Abstract]
  OR "intracranial metastas*"[Title/Abstract]
  OR "conversion disorder"[Title/Abstract]
  OR "psychogenic non-epileptic seizure*"[Title/Abstract]
  OR "myalgic encephalomyelitis"[Title/Abstract]
  OR "dysthymi*"[Title/Abstract]
  OR "manic depression"[Title/Abstract]
  OR "hypomania"[Title/Abstract]
  OR "emotionally unstable personality"[Title/Abstract]
  OR "dissocial personality"[Title/Abstract]
  OR "anankastic personality"[Title/Abstract]
  OR "multiple personality disorder"[Title/Abstract]
  OR "somatoform disorder*"[Title/Abstract]
  OR "Asperger*"[Title/Abstract]
  OR "Tourette syndrome"[Title/Abstract]
  OR "tic disorder*"[Title/Abstract]
  OR "dyspraxia"[Title/Abstract]
  OR "dyslexia"[Title/Abstract]
  OR "dyscalculia"[Title/Abstract]
  OR "trisomy 21"[Title/Abstract]
  OR "fetal alcohol"[Title/Abstract]
  OR "foetal alcohol"[Title/Abstract]
  OR "subjective cognitive decline"[Title/Abstract]
  OR "subjective memory complaint*"[Title/Abstract]
  OR "memory impairment"[Title/Abstract]
  OR "cognitive dysfunction"[Title/Abstract]
  OR "branched-chain ketoaciduria"[Title/Abstract]
  OR "organic acidemia*"[Title/Abstract]
  OR "organic aciduria*"[Title/Abstract]
  OR "isovaleric acidemia"[Title/Abstract]
  OR "propionic acidemia"[Title/Abstract]
  OR "methylmalonic acidemia"[Title/Abstract]
  OR "glutaric aciduria"[Title/Abstract]
  OR "tyrosinemia"[Title/Abstract]
  OR "hyperglycinemia"[Title/Abstract]
  OR "citrullinemia"[Title/Abstract]
  OR "argininemia"[Title/Abstract]
  OR "hepatolenticular degeneration"[Title/Abstract]
  OR "lysosomal storage disorder*"[Title/Abstract]
  OR "mitochondrial disorder*"[Title/Abstract]
  OR "peroxisomal disorder*"[Title/Abstract]
  OR "MELAS"[Title/Abstract]
  OR "MERRF"[Title/Abstract]
  OR "MSUD"[Title/Abstract]
  OR "PKU"[Title/Abstract]
  OR "Mental Disorders"[MeSH Terms]
  OR "Nervous System Diseases"[MeSH Terms]
  OR "Cognition Disorders"[MeSH Terms]
  OR "Neurodegenerative Diseases"[MeSH Terms]
  OR "Neurodevelopmental Disorders"[MeSH Terms]
  OR "Brain Neoplasms"[MeSH Terms]
  OR "Metabolism, Inborn Errors"[MeSH Terms]
  OR "Hepatic Encephalopathy"[MeSH Terms]
)
```

## Query partitioning and retrieval

To avoid excessive query URL length, the phenotype Title/Abstract terms were partitioned into batches of 12. Each batch was combined with the complete VOC block. A separate query combined the VOC block with the phenotype MeSH headings. The union of these searches represents the intended full Boolean search. The script additionally supports publication-year partitioning if a query exceeds 9,999 records.

Records were retrieved as XML in batches of up to 200 using Entrez search history. Both journal and book records were parsed. For each partition, the parsed record count was checked against the reported search count, and missing or duplicate PMIDs triggered an error. Records were then deduplicated across partitions by PMID.

## Initial retrieval results

  Measure                        Count
  ---------------------------- -------
  Search partitions                 23
  Unique PubMed publications     1,796

These are retrieval counts, not counts of eligible studies. Automated phenotype, biospecimen and analytical platform classification and preliminary screening flags support manual eligibility assessment; they do not establish eligibility.

## Search audit trail

  ----------------------------------------------------------------------------------------------------------------------
  Output                              Contents
  ----------------------------------- ----------------------------------------------------------------------------------
  `PubMed_search_strategy.txt`        Full unpartitioned Boolean query

  `Search_log.csv`                    Executed partition queries, dates, counts and query translations where available

  `PubMed_partition_membership.csv`   PMID-to-partition mapping

  `PubMed_retrieval_summary.csv`      Raw hits, unique PMIDs and duplicate hits

  `PubMed_screening_records.csv`      Retrieved citation metadata and candidate classifications
  ----------------------------------------------------------------------------------------------------------------------

The executed search log is the authoritative record of the search. The Boolean blocks above document the available script version; compare them against the archived search log before publication.

## Supplementary diagnostic evidence retrieval

Europe PMC is used downstream to retrieve accessible full-text XML for PubMed records and identify passages reporting diagnostic performance metrics. This is an evidence extraction step, not an independent bibliographic database search.

## Limitations

PubMed-only retrieval may omit relevant publications indexed elsewhere. Broad terminology can retrieve exposure studies, olfactory-function studies and non-primary research. Incomplete abstracts and indexing may also affect retrieval and automated classification. Final eligibility and extracted findings require manual verification.
