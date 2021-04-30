s1 <- s2 %>% filter(fold==2)
bg.coords1 <- bg.coords[bgfold==2,]
bgfold1 <- bgfold[bgfold==2]
occ1 <- occ[s2$fold==2,]


bg11 <- rbind(occ1,bg.coords1)
p = rep(0,nrow(bg11))
p[1:nrow(occ1)] <- 1

bg111 <- bg11[,3:ncol(bg11)]
sb1 <- dismo::maxent(bg111,p,factors='lith')
saveRDS(sb1, file=paste0(outdir,'ENMeval_object.RData'))

testval <- occ[s2$fold==1,]
myauc <- dismo::evaluate(testval,bg.coords,sb1)@auc