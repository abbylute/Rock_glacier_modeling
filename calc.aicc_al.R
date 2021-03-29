# This code was adapted from ENMeval::calc.aicc to work with SWD instead of raster

calc.aicc_al <- function (nparam, occ, predictive.maps) 
{
  AIC.valid <- nparam < nrow(occ)
  if (nrow(predictive.maps) == 0) {
    res <- data.frame(cbind(AICc = NA, delta.AICc = NA, w.AIC = NA, 
                            parameters = nparam))
    warning("Cannot calculate AICc when rasterPreds = FALSE... returning NA's.")
  }
  else {
    vals <- predictive.maps #extract(predictive.maps, occ)
    probsum <- colSums(predictive.maps) #cellStats(predictive.maps, sum)
    LL <- colSums(log(t(t(vals)/probsum)), na.rm = T)
    AICc <- (2 * nparam - 2 * LL) + (2 * (nparam) * (nparam + 
                                                       1)/(nrow(occ) - nparam - 1))
    AICc[AIC.valid == FALSE] <- NA
    AICc[is.infinite(AICc)] <- NA
    if (sum(is.na(AICc)) == length(AICc)) {
      warning("AICc not valid... returning NA's.")
      res <- data.frame(cbind(AICc, delta.AICc = NA, w.AIC = NA, 
                              parameters = nparam))
    }
    else {
      delta.AICc <- (AICc - min(AICc, na.rm = TRUE))
      w.AIC <- (exp(-0.5 * delta.AICc))/(sum(exp(-0.5 * 
                                                   delta.AICc), na.rm = TRUE))
      res <- data.frame(AICc, delta.AICc, w.AIC, parameters = nparam)
      rownames(res) <- NULL
    }
  }
  rownames(res) <- NULL
  return(res)
}
