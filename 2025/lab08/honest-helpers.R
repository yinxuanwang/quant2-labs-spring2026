#' @description
#' This function takes a regression estimated using fixest with the sunab option
#' and extracts the aggregated event-study coefficients and their variance-covariance matrix
#' @param sunab_fixest The result of a fixest call using the sunab option
#' @returns A list containing beta (the event-study coefficients),
#'          sigma (the variance-covariance matrix), and
#'          cohorts (the relative times corresponding to beta, sigma)

sunab_beta_vcv <-
    function(sunab_fixest) {
        ## The following code block extracts the weights on individual coefs used in
        # the fixest aggregation ##
        sunab_agg <- sunab_fixest$model_matrix_info$sunab$agg_period
        sunab_names <- base::names(sunab_fixest$coefficients)
        sunab_sel <- base::grepl(sunab_agg, sunab_names, perl = TRUE)
        sunab_names <- sunab_names[sunab_sel]
        if (!base::is.null(sunab_fixest$weights)) {
            sunab_wgt <- base::colSums(sunab_fixest$weights * base::sign(stats::model.matrix(sunab_fixest)[, sunab_names, drop = FALSE]))
        } else {
            sunab_wgt <- base::colSums(base::sign(stats::model.matrix(sunab_fixest)[, sunab_names, drop = FALSE]))
        }

        # Construct matrix sunab_trans such that sunab_trans %*% non-aggregated coefs = aggregated coefs,
        sunab_cohorts <- base::as.numeric(base::gsub(base::paste0(".*", sunab_agg, ".*"), "\\2", sunab_names, perl = TRUE))
        sunab_mat <- stats::model.matrix(~ 0 + base::factor(sunab_cohorts))
        sunab_trans <- base::solve(base::t(sunab_mat) %*% (sunab_wgt * sunab_mat)) %*% base::t(sunab_wgt * sunab_mat)

        # Get the coefs and vcv
        sunab_coefs <- sunab_trans %*% base::cbind(sunab_fixest$coefficients[sunab_sel])
        sunab_vcov <- sunab_trans %*% sunab_fixest$cov.scaled[sunab_sel, sunab_sel] %*% base::t(sunab_trans)

        base::return(base::list(
            beta = sunab_coefs,
            sigma = sunab_vcov,
            cohorts = base::sort(base::unique(sunab_cohorts))
        ))
    }

#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#'
#' @param ... Parameters to pass to the relevant method.
honest_did <- function(...) UseMethod("honest_did")

#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param es Result from aggte (object of class AGGTEobj).
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @param gridPoints Number of grid points used for the underlying test
#'  inversion. Default equals 100. User may wish to change the number of grid
#'  points for computational reasons.
#' @param ... Parameters to pass to `createSensitivityResults` or
#'  `createSensitivityResults_relativeMagnitudes`.
honest_did.AGGTEobj <- function(es,
                                e = 0,
                                type = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
    type <- match.arg(type)

    # Make sure that user is passing in an event study
    if (es$type != "dynamic") {
        stop("need to pass in an event study")
    }

    # Check if used universal base period and warn otherwise
    if (es$DIDparams$base_period != "universal") {
        stop("Use a universal base period for honest_did")
    }

    # Recover influence function for event study estimates
    es_inf_func <- es$inf.function$dynamic.inf.func.e

    # Recover variance-covariance matrix
    n <- nrow(es_inf_func)
    V <- t(es_inf_func) %*% es_inf_func / n / n

    # Check time vector is consecutive with referencePeriod = -1
    referencePeriod <- -1
    consecutivePre <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
    consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
    if (consecutivePre | consecutivePost) {
        msg <- "honest_did expects a time vector with consecutive time periods;"
        msg <- paste(msg, "please re-code your event study and interpret the results accordingly.", sep = "\n")
        stop(msg)
    }

    # Remove the coefficient normalized to zero
    hasReference <- any(es$egt == referencePeriod)
    if (hasReference) {
        referencePeriodIndex <- which(es$egt == referencePeriod)
        V <- V[-referencePeriodIndex, -referencePeriodIndex]
        beta <- es$att.egt[-referencePeriodIndex]
    } else {
        beta <- es$att.egt
    }

    nperiods <- nrow(V)
    npre <- sum(1 * (es$egt < referencePeriod))
    npost <- nperiods - npre
    if (!hasReference & (min(c(npost, npre)) <= 0)) {
        if (npost <= 0) {
            msg <- "not enough post-periods"
        } else {
            msg <- "not enough pre-periods"
        }
        msg <- paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)")
        stop(msg)
    }

    baseVec1 <- basisVector(index = (e + 1), size = npost)
    orig_ci <- constructOriginalCS(
        betahat = beta,
        sigma = V,
        numPrePeriods = npre,
        numPostPeriods = npost,
        l_vec = baseVec1
    )

    if (type == "relative_magnitude") {
        robust_ci <- createSensitivityResults_relativeMagnitudes(
            betahat = beta,
            sigma = V,
            numPrePeriods = npre,
            numPostPeriods = npost,
            l_vec = baseVec1,
            gridPoints = gridPoints,
            ...
        )
    } else if (type == "smoothness") {
        robust_ci <- createSensitivityResults(
            betahat = beta,
            sigma = V,
            numPrePeriods = npre,
            numPostPeriods = npost,
            l_vec = baseVec1,
            ...
        )
    }

    return(list(robust_ci = robust_ci, orig_ci = orig_ci, type = type))
}
