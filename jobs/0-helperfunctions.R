'%!in%' <- function(x,y)!('%in%'(x,y))

na_codes <- function(x, ...) {
  x[x %in% c(...)] <- NA
  x
}

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
