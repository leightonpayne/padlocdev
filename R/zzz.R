.onLoad <- function(libname, pkgname) {

  utils::globalVariables("hmm.acc") # collapse_hmm_meta()
  utils::globalVariables("secondary.name") # collapse_hmm_meta()
  utils::globalVariables("protein.name") # collapse_hmm_meta()
  utils::globalVariables("hmms") # compare_hmm_files_hmm_meta()
  utils::globalVariables("protein.name") # compare_hmm_files_hmm_meta()
  utils::globalVariables("sys_list") # divide_database()
  utils::globalVariables("secondary.name") # expand_gene_groups()
  utils::globalVariables("protein.name") # expand_gene_groups()
  utils::globalVariables("protein.name") # filter_hmm_meta()
  utils::globalVariables("group") # filter_models()
  utils::globalVariables("yaml.name") # filter_sys_meta()
  utils::globalVariables("file_name") # verify_hmm_names()
  utils::globalVariables("accession_field") # verify_hmm_names()
  utils::globalVariables("ind") # which_uses()
  utils::globalVariables("name") # build_generic_padloc_models()

  invisible()

}
