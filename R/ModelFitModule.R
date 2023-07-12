#' @title Model Fit Functions
#' @description
#' A general function that returns the model fit indices.
#' @param ell_A log likelihood of this model
#' @param ell_B log likelihood of saturated model
#' @param ell_N log likelihood of Null model
#' @param df_A degrees of freedom of this model
#' @param df_B degrees of freedom of saturated model
#' @param nobs number of observations
#' @export

Model_Fit <- function(ell_A, ell_B, ell_N, df_A, df_B, nobs) {
  ### this model
  chi_A <- 2 * (ell_B - ell_A)
  ### Null model
  chi_B <- 2 * (ell_B - ell_N)
  ## Standardized Fit Indeces
  NFI <- 1 - (chi_A / chi_B)
  RFI <- 1 - ((chi_A / df_A) / (chi_B / df_B))
  IFI <- 1 - ((chi_A - df_A) / (chi_B - df_A))
  TLI <- 1 - ((chi_A / df_A) - 1) / ((chi_B / df_B) - 1)
  CFI <- 1 - ((chi_A - df_A) / (chi_B - df_B))
  RMSEA <- sqrt((chi_A - df_A) / (df_A * nobs - 1))
  ## Information Criteria
  AIC <- chi_A - 2 * df_A
  CAIC <- chi_A - df_A * log(nobs - 1)
  BIC <- chi_A - df_A * log(nobs)

  ## Clip Funciton
  vars <- list(NFI, RFI, IFI, TLI, CFI)
  names <- c("NFI", "RFI", "IFI", "TLI", "CFI")

  for (i in 1:length(vars)) {
    assign(names[i], pmin(pmax(vars[[i]], 0), 1))
  }

  RMSEA[is.nan(RMSEA)] <- 0

  ret <- structure(
    list(
      model_log_like = ell_A,
      bench_log_like = ell_B,
      null_log_like = ell_N,
      model_Chi_sq = chi_A,
      null_Chi_sq = chi_B,
      model_df = df_A,
      null_df = df_B,
      NFI = NFI,
      RFI = RFI,
      IFI = IFI,
      TLI = TLI,
      CFI = CFI,
      RMSEA = RMSEA,
      AIC = AIC,
      CAIC = CAIC,
      BIC = BIC
    ),
    class = c("Exametrika", "ModelFit")
  )
  return(ret)
}
