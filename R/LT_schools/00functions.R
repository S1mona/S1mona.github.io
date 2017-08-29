rinline <- function(code) {
  sprintf('``` `r %s` ```', code)
}

bracket_match<- function(s, i, indl, indr, .stro="(", .strc=")"){
  if(length(s)==1){
    s%<>%strsplit(., "*")%>%unlist
  }
  while ((i <= length(s))&(indl!=indr)){
    if (s[i] == .stro){
      indl <- indl+1
    }
    if (s[i] == .strc){
      indr <- indr +1
    }
    i = i+1
  }
  return(i-1)
}

#' @param str string where to look
#' @param .stro opening bracet to look for 
#' @param .strc closing bracet to look for
remove_double <- function(str, .stro="(", .strc=")"){
  if(length(str)==1){
    x <- str%>%strsplit(., "*")%>%unlist
  }else{
    x <- str
  }
  #x <- str%>%gsub(" ", "",.)%>%strsplit(., "*")%>%unlist
  #start of {
  inds <- which(x==.stro)
  #end of }
  inde <- sapply(inds, function(i)x%>%bracket_match(., i+1, 1, 0, .stro, .strc))
  j <- any((inds[-1]-inds[-length(inds)])==1)&any((inde[-1]-inde[-length(inde)])==-1)
  k <- 1
  while(j&k<length(inds)){
    #if {{..}} appears
    if((inds[k+1]-inds[k])==1&(inde[k+1]-inde[k])==-1){
      x <- x[-c(inds[k], inde[k])] 
      #start of {
      inds <- which(x==.stro)
      #end of }
      inde <- sapply(inds, function(i)x%>%bracket_match(., i+1, 1, 0,  .stro, .strc))
      j <- any((inds[-1]-inds[-length(inds)])==1)&any((inde[-1]-inde[-length(inde)])==-1)
    }
    k <- k + 1
  }
  x
}

remove_outer <- function(str, .stro="(", .strc=")", collapseS = NULL){
  if(length(str)==1){
    str %<>%strsplit(., "*")%>%unlist
  }
  #find first stro
  inds <- which(str==.stro)[1]
  inde <- bracket_match(str, inds+1, 1, 0, .stro, .strc)
  #distance from 1 to nchar is equal, thus outer bracets
  if(((inds-1)==(length(str)-inde))&(all((str[c(1:(inds-1), (inde+1):length(str))]%>%unique)%in%"$"))){
    str <- str[-c(inds+1, inde)]
  }
  str%>%paste0(., collapse=collapseS)
}

replace_over <- function(str, .stro="(", .strc=")", collapseS = NULL){
  if(length(str)==1){
    str %<>%strsplit(., "*")%>%unlist
  }
  x <- str%>%remove_double(., .stro, .strc)%>%remove_outer(., .stro, .strc) 
  #replace \over with frac
  inds <- which(x==.stro)
  #end of }
  inde <- sapply(inds, function(i)x%>%bracket_match(., i+1, 1, 0, .stro, .strc))
  xx <- x%>%paste0(., collapse="")
  st <- gregexpr("\\\\\\over", xx)[[1]]
  x[inds[which(inde%in%(st-1))]] <- "\\frac{"
  x%<>%paste0(., collapse="")%>%gsub("\\over","", ., fixed=TRUE)  
  x%<>%strsplit(., "*")%>%unlist
  x%>%paste0(., collapse = collapseS)
}

replace_pmatrix <- function(str, .stro="(", .strc=")", collapseS = NULL){
  if(length(str)==1){
    str %<>%strsplit(., "*")%>%unlist
  }
  x <- str%>%remove_double(., .stro, .strc)%>%remove_outer(., .stro, .strc)%>%replace_over(., .stro, .strc, collapseS = "")
  if(grepl("pmatrix",x)){
    x%<>%gsub("\\$\\$\\\\pmatrix\\{|\\}\\$\\$$","",.)%>%paste0("$$\\begin{bmatrix}", ., "\\end{bmatrix}$$")
  }
  x%<>%strsplit(., "*")%>%unlist
  x%>%paste0(., collapse = collapseS)
}

mod_string <- function(str, .stro="(", .strc=")", collapseS = NULL){
  x <- str%>%remove_double(., .stro, .strc)%>%remove_outer(., .stro, .strc)%>%
    replace_over(., .stro, .strc)%>%replace_pmatrix(., .stro, .strc, collapseS = "")
  x
}


# df <- expand.grid(x = 0:2, y = 0:2)
# df$z <- runif(nrow(df), min=0, max=20)
# # default is compatible with geom_tile()○
# p <- ggplot(df, aes(x, y, fill = z)) + geom_raster()+ 
#   scale_fill_gradient2(low="#ffffb2", mid="#41b6c4", high="#045a8d")+theme(legend.title=element_blank(), legend.position = "none", 
#                                                         legend.background =element_blank(),
#                                                         axis.title.x=element_blank(), 
#                                                         axis.ticks.x=element_blank(),
#                                                         axis.ticks.y=element_blank(),
#                                                         axis.title.y=element_blank(),
#                                                         panel.border = element_blank(),
#                                                         axis.text.x=element_blank(),
#                                                         axis.text.y=element_blank(),
#                                                         panel.grid.major.x =  element_blank(),
#                                                         panel.grid.minor = element_blank(), 
#                                                         panel.grid.major = element_blank(),
#                                                         plot.background = element_rect(fill = "transparent",colour = NA),
#                                                         panel.background = element_rect(fill = "transparent",colour = NA))
# #ggsave("logo1.png", width = 30, height=30)
# #p
# png('logo1.png',width=300,height=300,units="px",bg = "transparent")
# print(p)
# dev.off()
