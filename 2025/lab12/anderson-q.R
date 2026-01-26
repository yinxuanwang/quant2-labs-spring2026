get_anderson_qvalues <- function(pval) {
    # This line assumes you have your p-values in a vector called "pval"
    totalpvals <- length(pval)

    original_sorting_order <- 1:length(pval)
    sorted_indices <- order(pval)
    rank <- rank(pval)

    qval <- 1

    bky06_qval <- rep(1, length(pval))

    while (qval > 0) {
        # First Stage
        qval_adj <- qval / (1 + qval)
        fdr_temp1 <- qval_adj * rank / totalpvals
        reject_temp1 <- ifelse(fdr_temp1 >= pval, 1, 0)
        reject_rank1 <- reject_temp1 * rank
        total_rejected1 <- max(reject_rank1, na.rm = TRUE)

        # Second Stage
        qval_2st <- qval_adj * (totalpvals / (totalpvals - total_rejected1))
        fdr_temp2 <- qval_2st * rank / totalpvals
        reject_temp2 <- ifelse(fdr_temp2 >= pval, 1, 0)
        reject_rank2 <- reject_temp2 * rank
        total_rejected2 <- max(reject_rank2, na.rm = TRUE)

        bky06_qval[rank <= total_rejected2] <- qval

        qval <- qval - 0.001
    }

    # Restoring the original sorting order
    sorted_indices_inverse <- order(original_sorting_order)
    bky06_qval <- bky06_qval[sorted_indices_inverse]

    bky06_qval
}
