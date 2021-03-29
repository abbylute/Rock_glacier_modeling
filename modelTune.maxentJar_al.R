
modelTune.maxentJar_al <- function (pres, bg, env, nk, group.data, args.i, userArgs, rasterPreds, 
          clamp, categoricals,outdir) 
{
  x <- rbind(pres, bg)
  p <- c(rep(1, nrow(pres)), rep(0, nrow(bg)))
  full.mod <- dismo::maxent(x, p, args = c(args.i, userArgs,"writeplotdata","responsecurves"), 
                            factors = categoricals,path =outdir)
  pred.args <- c("outputformat=raw", ifelse(clamp == TRUE, 
                                            "doclamp=true", "doclamp=false"))
  if (rasterPreds == TRUE) {
    #predictive.map <- predict(full.mod, env, args = pred.args)
    predictive.map <- predict(full.mod, bg, args = pred.args)# this doesn't seem to do anything: filename=paste0(outdir,'preds.csv'))
    
  }
  else {
    predictive.map <- stack()
  }
  AUC.TEST <- double()
  AUC.DIFF <- double()
  OR10 <- double()
  ORmin <- double()
  for (k in 1:nk) {
    train.val <- pres[group.data$occ.grp != k, , drop = FALSE]
    test.val <- pres[group.data$occ.grp == k, , drop = FALSE]
    bg.val <- bg[group.data$bg.grp != k, , drop = FALSE]
    x <- rbind(train.val, bg.val)
    p <- c(rep(1, nrow(train.val)), rep(0, nrow(bg.val)))
    mod <- dismo::maxent(x, p, args = c(args.i, userArgs,'writeplotdata',"responsecurves"), 
                         factors = categoricals)
    AUC.TEST[k] <- dismo::evaluate(test.val, bg, mod)@auc
    AUC.DIFF[k] <- max(0, dismo::evaluate(train.val, bg, 
                                          mod)@auc - AUC.TEST[k])
    p.train <- dismo::predict(mod, train.val, args = pred.args)
    p.test <- dismo::predict(mod, test.val, args = pred.args)
    if (nrow(train.val) < 10) {
      n90 <- floor(nrow(train.val) * 0.9)
    }
    else {
      n90 <- ceiling(nrow(train.val) * 0.9)
    }
    train.thr.10 <- rev(sort(p.train))[n90]
    OR10[k] <- mean(p.test < train.thr.10)
    train.thr.min <- min(p.train)
    ORmin[k] <- mean(p.test < train.thr.min)
  }
  stats <- c(AUC.DIFF, AUC.TEST, OR10, ORmin)
  out.i <- list(full.mod, stats, predictive.map)
  return(out.i)
}
