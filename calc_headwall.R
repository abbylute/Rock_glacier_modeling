# headwall metric calculation function

calc_headwall <- function(iter,demsmall2,slope,buf,slope_thres){
  library(raster)
  
  xy <- xyFromCell(demsmall2,iter)
  demex <- raster::extract(demsmall2,xy,buffer=buf)[[1]]
  slopex <- raster::extract(slope,xy,buffer=buf)[[1]]
  
  
  headwall <- length(which(slopex[(demex>demsmall2[iter])]>slope_thres))/length(slopex)
  return(headwall)
}