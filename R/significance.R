#############################################
#                                           #
#                Significance               #
#                                           #
#############################################

significance <- function(df) {
  data.frame(
    target <- deparse(substitute(df)), # extract name of dataframe
    pVal <- (t.test(df$value ~ df$genotype, paired = FALSE))$p.value,
    pText <- ifelse(pVal >= 0.05, pText <- "",
                    ifelse(pVal < 0.05 & pVal >= 0.01, pText <- "*",
                           ifelse(pVal < 0.01 & pVal >= 0.001, pText <- "**",
                                  ifelse(pVal < 0.001 & pVal >= 0.0001, pText <- "***",
                                         ifelse(pVal < 0.0001, pText <- "****", "na"))))),
    return(pText)
  )
}


# Usage:
#
# pTextHDAC4 <- significance2(HDAC4)
