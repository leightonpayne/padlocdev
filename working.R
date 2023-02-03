hmm_meta <- read_hmm_meta("/Volumes/SSD-01/02_working/padloc_update_230119/hmm_meta.txt")
sys_meta <- read_sys_meta("/Volumes/SSD-01/02_working/padloc_update_230119/sys_meta.txt")
sys_groups <- read_sys_groups("/Volumes/SSD-01/02_working/padloc_update_230119/sys_groups.txt")



filter_models(padloc_models, sys_groups, "new")

gene_types <- c("core_genes", "optional_genes", "prohibited_genes")

# Expand all gene groups in padloc models
# TODO: Not sure how to vectorise this? But it works fine as is
for (i in 1:length(padloc_models)) {
  for (gene_type in gene_types) {
    padloc_models[[i]][[gene_type]] <- expand_gene_groups(padloc_models[[i]], gene_type, hmm_meta)
  }
}



filter <- which(names(padloc_models) %in% c("Lit", "gabija"))
padloc_models_filtered <- padloc_models[filter]

find_relevant_proteins<-function(system.type){
  #system.type<-"retron_I-A"

  yaml_path <- paste0("padlocdb-",new.release,"/sys/", system.type, ".yaml")

  # check that the YAML file exists
  if( ! file.exists(yaml_path) ) {
    warning_msg(paste0("YAML does not exist for system: ", system.type))
    return(NULL)
  }

  # read in the yaml file
  system_param <- yaml::read_yaml(yaml_path)
  genes.core       <- system_param$core_genes
  genes.optional   <- system_param$optional_genes
  genes.prohibited <- system_param$prohibited_genes


  genes.all<-c(genes.core,genes.optional,genes.prohibited)



  genes.all<-expand_secondary_gene_assignments(genes.all)




  return(genes.all)


}


#find_relevant_proteins("retron_I-A")

proteins <- lapply(system.list$yaml.name,find_relevant_proteins)

# gene_groups <- function(padloc_model) {
#   gene_types <- c("core_genes", "optional_genes", "prohibited_genes")
#   gene_groups <- sapply(
#     gene_types,
#     function(gene_type) {
#       index <- which(sapply(slot(padloc_model, gene_type), names) == "gene_groups")
#       if (length(index) != 0) {
#         gene_groups <- unlist(slot(padloc_model, gene_type)[index], use.names = FALSE)
#       } else {
#         gene_groups <- NA
#       }
#       gene_groups
#     }
#   )
#   as.list(gene_groups)
# }
#
# tmp <- lapply(padloc_models, gene_groups)
#
# with_groups <- which(!is.na(gene_groups))
# for (i in with_groups) {
#   a <- gene_groups[i]
#   a
# }

# expand_gene_groups <- function(model, gene_type, hmm_meta) {
#   gene_lookup <- dplyr::select(hmm_meta, protein.name, secondary.name)
#   gene_lookup_distinct <- dplyr::distinct(gene_lookup, protein.name, secondary.name)
#   gene_groups_index <- which(sapply(slot(model, gene_type), names) == "gene_groups")
#   if (length(gene_groups_index) != 0) {
#     gene_groups <- unlist(slot(model, gene_type)[gene_groups_index])
#     genes_to_add <- dplyr::filter(gene_lookup_distinct, secondary.name %in% gene_groups)[["protein.name"]]
#     expanded <- c(slot(model, gene_type)[-gene_groups_index], genes_to_add)
#     expanded <- unlist(expanded)
#   } else {
#     expanded <- slot(model, gene_type)
#   }
#   expanded
# }
#
model <- read_padloc_model("/Volumes/SSD-01/02_working/padloc_update_230119/sys/cas_type_I-E.yaml")
expand_gene_groups(object, "optional_genes", hmm_meta)
#
# expand_gene_groups(tmp, "core_genes", hmm_meta)
#
# for (padloc_model in padloc_models) {
#   gene_groups <- c()
#   model <- read_padloc_model(padloc_model)
#   gene_groups_index <- which(sapply(slot(model, gene_type), names) == "gene_groups")
#   if (length(gene_groups_index) != 0) {
#     gene_groups <- c(gene_groups, )
#   }
# }

# setClass("hmm_meta", contains = "data.frame")
# valid_hmm_meta <- function(object) {
#   errors <- ""
#   correct_cols <- all(names(object) == c("protein.name", "description"))
#   if (!correct_cols) {
#     errors <- "Wrong headers"
#   }
#   if (errors != "") return(errors)
# }
# setValidity("hmm_meta", valid_hmm_meta)
#
# hmm_meta <- function(df) {
#   new(
#     "hmm_meta",
#     df
#   )
# }
#
# df <- data.frame(protein.name = c("Prot1", "Prot2", "Prot3"), description = c("asdasdasdsasd", "vbnbvnbvbnvbn", "qweqweqewqewq"), extra.col = c("asdad", "dfsfdgfs", "fgswrew"))
#
# df
#
# hmm_meta(df)



library(tidyverse)

#setwd("~/Bioinformatics/padloc/webserver_updates")
setwd("/mnt/c/Users/Simon/Documents/Bioinformatics/padloc/webserver_updates")

release_date <- "13-05-22"
padlocdb_path <- paste0("padlocdb-", release_date, "/")

sys_meta_path <- paste0(padlocdb_path, "sys_meta.txt")
sys_meta <- read_sys_meta(sys_meta_path) %>% filter(search == "T")

hmm_meta_path <- paste0(padlocdb_path, "hmm_meta.txt")
hmm_meta <- read_hmm_meta(hmm_meta_path)

system_groups <- c(
  "SG1",
  "DG1",
  "MG1",
  "DMS",
  "DRT",
  "SigPro",
  "CRISPR",
  "new"
)

extract_hmms <- function(system_group, sys_meta, hmm_meta) {

  system_list <- sys_meta %>% filter(group == system_group)

  all_genes <- c()

  for (system in system_list) {
    yaml_path <- paste0(padlocdb_path, "sys/", system, ".yaml")
    model <- read_padloc_model(yaml_path)
    genes <- sapply(
      c("core_genes", "optional_genes", "prohibited_genes"),
      function(x) expand_gene_groups(model, x, hmm_meta)
    )
    all_genes <- c(all_genes, unlist(genes, use.names = FALSE))
  }

  tibble::tibble(protein.name = all_genes)

  hmm_meta_relevant <- dplyr::filter(hmm_meta, protein.name %in% all_genes)



  # join list of proteins we want to the hmm table, then output a list of the relevant hmm names

  proteins.to.use<-proteins.to.use %>% distinct() %>% filter(protein.name!= "NA") %>%
    mutate(to.extract="T")

  extract.hmms<-meta.hmm %>% full_join(proteins.to.use)

  # cehck for proteins with missing hmms

  missing.hmm<-extract.hmms%>%filter(is.na(hmm.name))

  if (nrow(missing.hmm) > 0){
    warning_msg(paste0("Proteins listed without hmms for ",nrow(missing.hmm)," proteins."))
    print(missing.hmm$protein.name)

  }


  extract.hmm.table<-extract.hmms%>%filter(!is.na(hmm.name) & to.extract=="T") %>% select(-to.extract)


  extract.hmm.list<-extract.hmm.table %>% select(hmm.name) %>% distinct()



  extract.hmm.list %>% write_delim(paste0("padlocdb-",new.release,"/subsets/",system.extract,"/hmms.list"),
                                   col_names=F,delim="\t")


  # doesn't work as the secondary assingments are gone and dual | proteins are gone
  # extract.hmm.table %>% write_delim(paste0("padlocdb-",new.release,"/subsets/",system.extract,"/hmm_meta.txt"),
  #                                   col_names=T,delim="\t")
  #


  system.list %>% write_delim(paste0("padlocdb-",new.release,"/subsets/",system.extract,"/sys_meta.txt"),
                              col_names=T,delim="\t")

}


