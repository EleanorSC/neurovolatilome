# Extraction of candidate papers from literature screening
# install.packages(c('openxlsx','dplyr','stringr','purrr','tibble','ggplot2','xml2','httr2'))
library(openxlsx); library(dplyr); library(stringr); library(purrr)
library(tibble); library(ggplot2); library(xml2); library(httr2)

STAGE <- 1L
MODE <- 'all' # all, priority, included (n.b select 'priority')
FETCH_OA <- TRUE
root <- path.expand('~/Documents/VOCs_project/data')
out <- file.path(root,'diagnostic_extraction'); dir.create(out,recursive=TRUE,showWarnings=FALSE)
cache <- file.path(out,'xml'); dir.create(cache,showWarnings=FALSE)
wb <- file.path(out,paste0('Diagnostic_review_',MODE,'.xlsx'))
blank <- function(x) is.na(x) | trimws(as.character(x))==''
metric <- regex('\\b(?:AUC(?:ROC)?|AUROC|area under (?:the )?(?:ROC|receiver operating characteristic|(?:ROC )?curve)|sensitivity|specificity|accuracy|positive predictive value|negative predictive value|PPV|NPV|balanced accuracy|F1[- ]score)\\b',ignore_case=TRUE)
number <- regex('(?<![[:alnum:]])(?:[0-9]+(?:[.,][0-9]+)?|\\.[0-9]+)\\s*%?')
if(STAGE==1L){
  if(file.exists(wb)) stop('Workbook exists; rename it before rebuilding to preserve manual edits.')
  s <- read.xlsx(file.path(root,'PubMed_prioritised_screening.xlsx'),sheet='Screening')
  stopifnot(all(c('PMID','Title','Abstract','Screening_tier','Final_verdict') %in% names(s)))
  s <- s %>% mutate(PMID=as.character(PMID)) %>% distinct(PMID,.keep_all=TRUE)
  stopifnot(all(!blank(s$PMID)))
  if(MODE=='priority') s <- filter(s,Screening_tier %in% 1:2)
  if(MODE=='included') s <- filter(s,Final_verdict=='Include')
  stopifnot(nrow(s)>0, MODE %in% c('all','priority','included'))
  fetch <- function(id){
    f <- file.path(cache,paste0(id,'.xml'))
    if(file.exists(f)) return(list(doc=read_xml(f),status='Cached full text'))
    if(!FETCH_OA) return(list(doc=NULL,status='Not requested'))
    tryCatch({
      r <- request('https://www.ebi.ac.uk/europepmc/webservices/rest/search') %>% req_url_query(query=paste0('EXT_ID:',id,' AND SRC:MED'),format='xml') %>% req_timeout(30) %>% req_retry(max_tries=3) %>% req_perform()
      d <- read_xml(resp_body_raw(r)); pmc <- xml_text(xml_find_first(d,'.//result/pmcid'))
      if(blank(pmc)) return(list(doc=NULL,status='No PMCID found'))
      Sys.sleep(.25)
      r <- request(paste0('https://www.ebi.ac.uk/europepmc/webservices/rest/',pmc,'/fullTextXML')) %>% req_timeout(45) %>% req_retry(max_tries=3) %>% req_perform()
      d <- read_xml(resp_body_raw(r)); if(length(xml_find_all(d,'.//body'))==0) stop('No article body')
      write_xml(d,f); list(doc=d,status='Full text retrieved')
    },error=function(e) list(doc=NULL,status=paste('Unavailable:',conditionMessage(e))))
  }
  scan <- function(text,loc,id){
    if(blank(text)) return(tibble())
    # Keep entire paragraph/table row for context; no automatic metric pairing.
    if(!str_detect(text,metric)) return(tibble())
    tibble(PMID=id,Source_location=loc,Metric_terms=paste(unique(str_extract_all(text,metric)[[1]]),collapse='; '),Numeric_tokens=paste(str_extract_all(text,number)[[1]],collapse='; '),Evidence_text=text)
  }
  results <- map(seq_len(nrow(s)),function(i){
    id <- s$PMID[i]; message(i,'/',nrow(s),' PMID ',id)
    a <- scan(s$Abstract[i],'Abstract',id); f <- fetch(id); Sys.sleep(.25)
    if(!is.null(f$doc)){
      nodes <- xml_find_all(f$doc,'.//body//p[not(ancestor::table-wrap)] | .//table-wrap//tr | .//fig/caption | .//table-wrap/caption')
      b <- map_dfr(seq_along(nodes),function(j){
        node <- nodes[[j]]; sec <- xml_text(xml_find_first(node,'ancestor::sec[1]/title')); if(blank(sec)) sec <- 'Full text'
        tab <- xml_find_first(node,'ancestor::table-wrap[1]'); fig <- xml_find_first(node,'ancestor::fig[1]')
        label <- if(!inherits(tab,'xml_missing')) xml_text(xml_find_first(tab,'./label')) else if(!inherits(fig,'xml_missing')) xml_text(xml_find_first(fig,'./label')) else ''
        txt <- if(xml_name(node)=='tr') paste(xml_text(xml_children(node)),collapse=' | ') else xml_text(node)
        scan(txt,paste(sec,label,xml_path(node),sep=' / '),id)
      }); a <- bind_rows(a,b)
    }
    list(hits=a,log=tibble(PMID=id,Full_text_status=f$status))
  })
  hits <- bind_rows(tibble(PMID=character(),Source_location=character(),Metric_terms=character(),Numeric_tokens=character(),Evidence_text=character()),map_dfr(results,'hits')) %>% distinct() %>% mutate(Evidence_ID=sprintf('E%06d',row_number()),.before=1)
  # Excel cell limit; retain complete evidence separately in CSV.
  write.csv(hits,file.path(out,paste0('Evidence_',MODE,'.csv')),row.names=FALSE,na='')
  log <- map_dfr(results,'log')
  papers <- s %>% select(any_of(c('PMID','DOI','Title','Year','Screening_tier','Final_verdict','Candidate_disorder','Candidate_biospecimen','Candidate_platform'))) %>% left_join(log,by='PMID') %>% left_join(count(hits,PMID,name='Candidate_hits'),by='PMID') %>% mutate(Candidate_hits=coalesce(Candidate_hits,0L),Review_status='',Notes='')
  models <- tibble(PMID='',Model_ID='',Evidence_ID='',Disorder='',Biospecimen='',Platform='',Diagnostic_target='',Comparator='',Model_type='',Validation_type='',N_cases=NA_real_,N_controls=NA_real_,N_training=NA_real_,N_testing=NA_real_,AUC=NA_real_,AUC_lower=NA_real_,AUC_upper=NA_real_,CI_level=NA_real_,Sensitivity=NA_real_,Specificity=NA_real_,Accuracy=NA_real_,PPV=NA_real_,NPV=NA_real_,Source_location='',Verified='',Include_in_plot='',Notes='')
  instructions <- tibble(Instruction=c('Candidate hits are not verified estimates; inspect original paper including tables, supplements and figures.','Review every eligible paper even if no hits or full text unavailable.','One row per distinct model, target, comparator and evaluation dataset. Use unique Model_ID within PMID.','Enter proportions 0–1; CI_level as percentage (e.g. 95). Leave unreported values blank. Never infer CI from AUC alone.','Record source location and Evidence_ID if available; manual full-text extraction is permitted.','Set Verified and Include_in_plot to Yes only after checking eligibility, values and evaluation design.','Use a prespecified model selection rule; avoid choosing the highest AUC.','Plot is descriptive, not a pooled meta-analysis.'))
  write.xlsx(list(Instructions=instructions,Papers=papers,Evidence=mutate(hits,Evidence_text=str_sub(Evidence_text,1,32000),Numeric_tokens=str_sub(Numeric_tokens,1,32000)),Models=models),wb,overwrite=FALSE)
  message('Papers: ',nrow(papers),' | Candidate papers: ',n_distinct(hits$PMID),' | Evidence passages: ',nrow(hits),' | Workbook: ',wb)
}
if(STAGE==2L){
  m <- read.xlsx(wb,sheet='Models'); p <- read.xlsx(wb,sheet='Papers')
  m <- m %>% filter(tolower(trimws(Verified))=='yes',tolower(trimws(Include_in_plot))=='yes') %>% mutate(PMID=as.character(PMID))
  stopifnot(nrow(m)>0,all(m$PMID %in% as.character(p$PMID)),!anyDuplicated(m[c('PMID','Model_ID')]))
  for(nm in c('Model_ID','Disorder','Diagnostic_target','Comparator','Validation_type','Source_location')) if(any(blank(m[[nm]]))) stop('Missing ',nm)
  for(nm in c('AUC','AUC_lower','AUC_upper','CI_level')){old <- m[[nm]]; m[[nm]] <- suppressWarnings(as.numeric(old)); if(any(!blank(old)&is.na(m[[nm]]))) stop('Non-numeric ',nm)}
  stopifnot(all(is.finite(m$AUC)&m$AUC>=0&m$AUC<=1))
  ci <- !is.na(m$AUC_lower)&!is.na(m$AUC_upper)
  stopifnot(all(is.na(m$AUC_lower)==is.na(m$AUC_upper)),all(m$AUC_lower[ci]>=0&m$AUC_lower[ci]<=m$AUC[ci]&m$AUC_upper[ci]>=m$AUC[ci]&m$AUC_upper[ci]<=1),all(!is.na(m$CI_level[ci])&m$CI_level[ci]==95))
  m <- m %>% mutate(Label=paste0(PMID,' / ',Model_ID,' | ',Diagnostic_target,' vs ',Comparator),Label=reorder(Label,AUC))
  plot <- ggplot(m,aes(AUC,Label))+geom_vline(xintercept=.5,linetype=2,colour='grey60')+geom_segment(data=filter(m,!is.na(AUC_lower)),aes(x=AUC_lower,xend=AUC_upper,yend=Label),linewidth=.6)+geom_point(aes(colour=Validation_type),size=2.8)+facet_grid(Disorder~.,scales='free_y',space='free_y')+scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.2))+labs(x='Area under the ROC curve (95% CI where reported)',y=NULL,colour='Validation',caption='Descriptive model-level estimates; no pooled effect.')+theme_minimal(base_size=11)+theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),legend.position='bottom')
  print(plot); ggsave(file.path(out,'Diagnostic_AUC_forest.pdf'),plot,width=12,height=max(5,nrow(m)*.32+2),limitsize=FALSE)
  write.csv(m,file.path(out,'Verified_diagnostic_models.csv'),row.names=FALSE)
}
