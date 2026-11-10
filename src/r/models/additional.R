# Extracted analytical implementation; execution is explicit.
run_additional_models <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    library(modelsummary);library(marginaleffects);library(fixest)
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)


    printAnalysisDataProvenance()


    ## Default regression sample is the full analytical dataset.
    ## Keep the >1-vote subset available for sensitivity analyses.
    nrow(df_choices)
    nrow(df_choices_full)
    nrow(df_choices_gt1vote)


    df_proposals <- read.csv(rawDataPath('proposals_0.csv.gz'), colClasses = c(id = "character"))
    df_proposals$id <- trimws(tolower(df_proposals$id))



    DEFAULT_CLUSTER <- "space"

    ## Run final regression
    do_ate_ms <- function(models, ...) {

      # Capture all user-supplied arguments
      user_args <- list(...)

      # print(user_args)

      # Define the defaults
      default_args <-
        list(se = "cluster", coef_omit=DEFAULT_CLUSTER, oneLiner=TRUE, output="gt")

      # Combine, with user-supplied arguments taking precedence
      final_args <- modifyList(default_args, user_args, keep.null = TRUE)

      # print(final_args)

      ## Previously in here, but moved out to allow passing multiple models.
      ################################################################################
     #  fit.smallest <- glm(smallest ,
     #             data = mydf,
     #             family="binomial")
     #
     #  fit.smallest2 <- glm(smallest2 ,
     #             data = mydf,
     #             family="binomial")
     #
     #  fit.smallest3 <- glm(smallest3 ,
     #             data = mydf,
     #             family="binomial")
     #
     #  fit.smallest4 <- glm(smallest4 ,
     #             data = mydf,
     #             family="binomial")
     #
     # models = list(fit.smallest, fit.smallest2, fit.smallest3, fit.smallest4)

      models_ate <- do_ate(models, se="cluster", cluster=DEFAULT_CLUSTER)

        dms_args <-
        c(list(models=models_ate), final_args)

        # print(dms_args)

      ## Doing clusters by default.
      do.call(do_ms, dms_args)
    }

    do_ate <- function(models, se = "iid", cluster = DEFAULT_CLUSTER, ...) {

      # Handles robust standard errors
      # if (se == "cluster") {
      #   vcov <- switch(cluster,
      #                  "space" = function(x) vcovCL(x, cluster = ~ space, data = model.frame(x)),
      #                  stop(paste0("Unknown cluster option: ", cluster))
      #   )
      # }
      ## With fixest we can simplify.
      if (se == "cluster") {
        vcov <- as.formula(paste0("~", cluster))
      } else if (se == "iid") {
        vcov <- FALSE
      } else {
        stop(paste0("Unknown se option: ", se))
      }


      # If requested, replace models with ATE versions.
      models <- lapply(models, function(MODEL) {
        avg_slopes(MODEL, vcov = vcov)
      })

      return(models)
    }

    do_ms <- function(models, oneLiner = FALSE, output = "gt",
                     coef_map = coef_map_default, ...) {


      # Stile della tabella (una riga o due)
      if (oneLiner) {
        estimate <- "{estimate} ({std.error}){stars}"
        statistic <- NULL
      } else {
        estimate <- "{estimate}{stars}"
        statistic <- "({std.error})"
      }

      # Generazione tabella riassuntiva con modelsummary
      modelsummary(
        models,
        # stars = TRUE,
        stars = c("+" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
        estimate = estimate,
        statistic = statistic,
        output = output,
        # coef_rename = coef_rename_dms,
        coef_map = coef_map,
        ...
      )
    }


    do_ms2 <- function(models, oneLiner = FALSE, output = "gt",
                     coef_map = coef_map_default, ...) {


      # Stile della tabella (una riga o due)
      if (oneLiner) {
        estimate <- "{estimate} ({std.error}){stars}"
        statistic <- NULL
      } else {
        estimate <- "{estimate}{stars}"
        statistic <- "({std.error})"
      }

      # Generazione tabella riassuntiva con modelsummary
      modelsummary(
        models,
        # stars = TRUE,
        stars = c("+" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
        estimate = estimate,
        statistic = statistic,
        output = output,
        # coef_rename = coef_rename_dms,
        # coef_map = coef_map,
        transform = "avg_slopes",
        vcov = as.formula("~space"),
        ...
      )
    }


    clean_choice_share <- function(mydata) {
      mydata = mydata[!is.na(mydata$choice_share_2),]
      mydata$choice_share <- mydata$choice_share_2
      return(mydata)
    }


    # Renames and filters coefficients.
    coef_map_default <- c(
      "choice_idx" = "Choice Idx",
      "N_choices"  = "Num. Choices",
      "choice_text_len_rel" = "Relative Choice Length",
      "n_votes" = "Num. Voters",
      "is_author_choice2" = "Author Choice",
      "is_first_choice" = "First Choice",
      "space" = "DAO",
      "type" = "Type",
      "choice_share" = "Voting Power",
      "created_rel" = "Blocks since DAO Creation",
      "author_pos6" = "Author Voted Early (1-6)"
    )

    coef_omit = c("type")






    do_regr_first_choice_nvotes <- function(mydata) {

      mydata <- clean_choice_share(mydata)


      ## Regr.

      ## FE-GLM
      fit0 <- feglm(choice_share ~  type +
                   is_first_choice + is_author_choice2 + is_infavor  + n_votes
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      fit1 <- feglm(choice_share ~  type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel + n_votes
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      fit2 <- feglm(choice_share ~   type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel + N_choices  + n_votes
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)

      fit3 <- feglm(choice_share ~   type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel + N_choices + author_pos6  + n_votes
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)

      fit4 <- feglm(choice_share ~   type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel + N_choices + author_pos6 + choice_text_len_rel  + n_votes
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      # fit5 <- feglm(choice_share ~   type +
      #              is_first_choice + is_infavor +
      #              is_author_choice2*author_pos6 +
      #              created_rel + N_choices + choice_text_len_rel +
      #              space  + n_votes,
      #            family = quasibinomial('logit'),
      #            data=mydata)
      #

      # summary(fit1)


      ml <- list(fit0, fit1, fit2, fit3, fit4)#, fit5)

      return(ml)
    }


    do_regr_first_choice <- function(mydata) {

      mydata <- clean_choice_share(mydata)


      ## Regr.

      ## FE-GLM
      fit0 <- feglm(choice_share ~  type +
                   is_first_choice + is_author_choice2 + is_infavor
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      fit1 <- feglm(choice_share ~  type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      fit2 <- feglm(choice_share ~   type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel + N_choices
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)

      fit3 <- feglm(choice_share ~   type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel + N_choices + author_pos6
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)

      fit4 <- feglm(choice_share ~   type +
                   is_first_choice + is_author_choice2 + is_infavor +
                   created_rel + N_choices + author_pos6 + choice_text_len_rel
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      # fit5 <- feglm(choice_share ~   type +
      #              is_first_choice + is_infavor +
      #              is_author_choice2*author_pos6 +
      #              created_rel + N_choices + choice_text_len_rel,
      #            family = quasibinomial('logit'),
      #            data=mydata)


      # summary(fit1)


      ml <- list(fit0, fit1, fit2, fit3, fit4)#, fit5)

      return(ml)
    }



    do_regr_choice_idx <- function(mydata) {

      mydata <- clean_choice_share(mydata)


      ## Regr.

      ## FE-GLM
      fit0 <- feglm(choice_share ~  type +
                    choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      fit1 <- feglm(choice_share ~  type +
                    choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                   created_rel
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      fit2 <- feglm(choice_share ~   type +
                    choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                   created_rel + N_choices
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)

      fit3 <- feglm(choice_share ~   type +
                    choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                   created_rel + N_choices + author_pos6
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)

      fit4 <- feglm(choice_share ~   type +
                    choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                   created_rel + N_choices + author_pos6 + choice_text_len_rel
                   | space,
                 family = quasibinomial('logit'),
                 data=mydata)


      fit5 <- feglm(choice_share ~   type +
                    choice_idx + I(choice_idx^2) + is_infavor +
                   is_author_choice2*author_pos6 +
                   created_rel + N_choices + choice_text_len_rel,
                 family = quasibinomial('logit'),
                 data=mydata)


      # summary(fit1)


      ml <- list(fit0, fit1, fit2, fit3, fit4, fit5)

      return(ml)
    }



    md = df_choices
    md = md[md$type %in% c("single-choice", "basic"),]

    fits <- do_regr_first_choice(md)

    # ates <- do_ate(fits)
    # do_ms(ates)
    # do_ms(ates, output = "latex")

    do_ms2(fits)




    md = df_choices
    md = md[md$type %in% c("single-choice", "basic"),]

    fits <- do_regr_first_choice_nvotes(md)

    # ates <- do_ate(fits)
    # do_ms(ates)
    # do_ms(ates, output = "latex")

    do_ms2(fits)




    md = df_choices
    md = md[md$type %in% c("single-choice", "basic"),]

    ## REPLACING WITH CHOICE SHARE MINUS SHARE OF AUTHOR
    md$choice_share <- md$choice_share_no_author_vp

    fits <- do_regr_first_choice(md)

    # ates <- do_ate(fits)
    # do_ms(ates)
    # do_ms(ates, output = "latex")

    do_ms2(fits)




    md = df_choices
    md = md[md$type %in% c("single-choice", "basic"),]

    fits <- do_regr_choice_idx(md)

    # ates <- do_ate(fits)
    # do_ms(ates)
    # do_ms(ates, output = "latex")

    do_ms2(fits)





    mydata = df_choices

    mydata = df_choices[df_choices$type %in% c("single-choice", "basic"),]

    ## Important: use choice_share_2.
    mydata$choice_share <- mydata$choice_share_2

    ## Regr.

    fit0 <- glm(choice_share ~  type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor,
               family = quasibinomial('logit'),
               data=mydata)


    fit1 <- glm(choice_share ~  type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel,
               family = quasibinomial('logit'),
               data=mydata)


    fit2 <- glm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel + N_choices,
               family = quasibinomial('logit'),
               data=mydata)


    fit3 <- glm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel + N_choices + author_pos6,
               family = quasibinomial('logit'),
               data=mydata)

    fit4 <- glm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel + N_choices + author_pos6 + choice_text_len_rel,
               family = quasibinomial('logit'),
               data=mydata)


    fit5 <- glm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_infavor +
                 is_author_choice2*author_pos6 +
                 created_rel + N_choices + choice_text_len_rel,
               family = quasibinomial('logit'),
               data=mydata)



    # summary(fit1)


    ml <- list(fit0, fit1, fit2, fit3, fit4, fit5)


    # doms(ml)
    # doms(ml, output = "tables/3biases_fe-space_sc_fc.tex")

    doms(ml)

    doms(ml, output="latex")





    mydata = df_choices

    # mydata = df_choices[df_choices$type %in% c("single-choice", "basic"),]

    # mydata = mydata[1:10000,]

    ## Regr.

    # start_time <- Sys.time()
    #
    # fit0 <- glm(choice_share ~  space + type +
    #              choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor,
    #            family = quasibinomial('logit'),
    #            data=mydata)
    #
    # # Code to measure execution time
    # end_time <- Sys.time()
    # execution_time <- difftime(end_time, start_time, units = "secs")
    # print(execution_time)


    library(fixest)


    fit0 <- feglm(choice_share ~ type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor | space,
               family = quasibinomial('logit'),
               data=mydata)


    fit1 <- feglm(choice_share ~  type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel | space,
               family = quasibinomial('logit'),
               data=mydata)


    fit2 <- feglm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel + N_choices | space,
               family = quasibinomial('logit'),
               data=mydata)


    fit3 <- feglm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel + N_choices + author_pos6 | space,
               family = quasibinomial('logit'),
               data=mydata)

    fit4 <- feglm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_author_choice2 + is_infavor +
                 created_rel + N_choices + author_pos6 + choice_text_len_rel | space,
               family = quasibinomial('logit'),
               data=mydata)


    fit5 <- feglm(choice_share ~   type +
                 choice_idx + I(choice_idx^2) + is_infavor +
                 is_author_choice2*author_pos6 +
                 created_rel + N_choices + choice_text_len_rel | space,
               family = quasibinomial('logit'),
               data=mydata)



    ml <- list(fit0, fit1, fit2, fit3, fit4, fit5)


    modelsummary(ml)




    modelsummary(ml, output = "latex")





    mydata = df_choices

    # mydata = df_choices[df_choices$type %in% c("single-choice", "basic"),]


    ## Regr.

    fit0 <- glm(choice_share ~  type +
                 is_first_choice + is_author_choice2 + is_infavor,
               family = quasibinomial('logit'),
               data=mydata)


    fit1 <- glm(choice_share ~  type +
                 is_first_choice + is_author_choice2 + is_infavor +
                 created_rel,
               family = quasibinomial('logit'),
               data=mydata)


    fit2 <- glm(choice_share ~   type +
                 is_first_choice + is_author_choice2 + is_infavor +
                 created_rel + N_choices,
               family = quasibinomial('logit'),
               data=mydata)


    fit3 <- glm(choice_share ~   type +
                 is_first_choice + is_author_choice2 + is_infavor +
                 created_rel + N_choices + author_pos6,
               family = quasibinomial('logit'),
               data=mydata)

    fit4 <- glm(choice_share ~   type +
                 is_first_choice + is_author_choice2 + is_infavor +
                 created_rel + N_choices + author_pos6 + choice_text_len_rel,
               family = quasibinomial('logit'),
               data=mydata)


    fit5 <- glm(choice_share ~   type +
                 is_first_choice + is_infavor +
                 is_author_choice2*author_pos6 +
                 created_rel + N_choices + choice_text_len_rel,
               family = quasibinomial('logit'),
               data=mydata)



    # summary(fit1)


    ml <- list(fit0, fit1, fit2, fit3, fit4, fit5)



    modelsummary(ml)



    modelsummary(ml, output = "latex")
  }),envir,report=file.path(context$OUTPUT_ROOT,"reports","additional.txt"))
  invisible(envir)
}
