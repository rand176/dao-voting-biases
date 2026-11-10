initialize_analysis <- function(context, envir = parent.frame()) {
  list2env(context, envir=envir)
  for (module in c("data/paths.R","data/readers.R","preparation/exclusions.R","preparation/choice_processing.R","reporting/style.R")) sys.source(file.path(context$PACKAGE_ROOT,"src/r",module), envir=envir)
  evalq({
    library(foreign)
    library(ggplot2)
    library(gridExtra)
    library(psych)
    library(scales)
    library(reshape2)
    library(grid)
    library(plyr)
    library(dplyr)
    library(data.table)
    library(ineq)
    library(texreg)
    library(zoo)
    library(lme4)
    library(lmerTest)
    library(mediation)
    library(gridExtra)
    library(tidyverse)  # load the package containing both ggplot2 and dplyr

    library(Rmisc)
    library(nnet)
    if (requireNamespace("hhi", quietly = TRUE)) {
  library(hhi)
} else {
  warning("Optional package hhi is not installed; hhi-dependent analysis code will fail if called.")
}

    library(car)
    library(Rmisc)
    COLOR_SELF_CONTR = "magenta"

    COLOR_CONTR      = "dodgerblue"

    COLOR_NON_CONTR  = "cyan2"

    COLOR_ALL        = "orange"

    theme_set(theme_bw(base_size = 20))

    myThemeMod <- theme(axis.title.x = element_text(vjust=-1, size=24),
                    axis.title.y = element_text(vjust=-0.1, size=24),
                    plot.margin=unit(c(10,10,10,10),"mm"),
                    plot.title = element_text(vjust=3, size=24,face="bold"),
                    legend.background = element_rect(fill = "white", color="grey"),
                    legend.title = element_blank(),
                    #legend.title = element_text(vjust=3, size=16, face="bold"),
                    #legend.direction = "horizzontal",
                    legend.text = element_text(size=16),
                    legend.key.width = unit(1.5, "cm"),
                    legend.key = element_rect(fill = "white", colour = "white")
)

  }, envir)
  invisible(envir)
}
