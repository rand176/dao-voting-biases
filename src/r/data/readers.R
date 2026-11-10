loadChoices <- function(NEW=TRUE, LLM=TRUE, version=-1) {

  ## Init version.
  if (version == -1) {
    if (NEW == TRUE) {
      if (LLM == TRUE) {
        version = "v3"
      } else {
        version = "v2"
      }
    } else {
      version = "v14"
    }
  }

  if (NEW == TRUE) {
    f = file.path(DERIVED_LEGACY_DIR, 'choices', paste0('choices_25_', version, '.csv'))
  } else {
    f = file.path(DERIVED_LEGACY_DIR, 'choices', paste0('choices_augmented_', version, '.csv'))
  }

  df = read.csv(f, colClasses = c(proposal = "character"))

  df$type = factor(df$type, levels = c(
    "single-choice", "basic", "quadratic", "weighted", "ranked-choice", "approval"
  ))
  if (NEW == TRUE) {
    print(paste0("NEW", " ", "Loaded choices: ", nrow(df)))
  } else {
    print(paste0(version, " ", "Loaded choices: ", nrow(df)))
  }


  df$proposal = tolower(df$proposal)


  return(df)
}

loadLLMLabels <- function(llm) {
  f = llmDataPath(paste0('choices_25_', llm, '.csv'))
  df = read.csv(f, colClasses = c(proposal = "character"))
  df$proposal = tolower(df$proposal)
  if (!"prop_chid" %in% colnames(df)) {
    df$prop_chid = paste0(df$proposal, df$choice_idx)
  }
  return(df)
}

loadAllNets <- function(file) {
  name = 'TVL Majority'
  DIR = 'covoting_weights_llama_T=10_Majority'
  df1 <- read.csv(file.path(INPUT_ROOT,"data",DIR,file))
  df1$net <- name

  name = 'TVL'
  DIR = 'covoting_weights_llama_T=10'
  df2 <- read.csv(file.path(INPUT_ROOT,"data",DIR,file))
  df2$net <- name

  name = 'All Majority'
  DIR = 'covoting_weights_all_T=10_Majority'
  df3 <- read.csv(file.path(INPUT_ROOT,"data",DIR,file))
  df3$net <- name

  name = 'All'
  DIR = 'covoting_weights_all_T=10'
  df4 <- read.csv(file.path(INPUT_ROOT,"data",DIR,file))
  df4$X <- NULL
  df4$voter <- NULL
  df4$net <- name

  df <- rbind(df1, df2, df3, df4)

  return(df)
}
