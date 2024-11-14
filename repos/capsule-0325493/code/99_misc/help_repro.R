# getOs <- function(x){
#   l <- list()
#   for (i in 1:length(x)){
#     l[[i]] <- get(x[i])
#   }
#   return(l)
# }
help_repro <- function(f,res,env=NULL){
  fname <- f[[1]]
  fCon <- eval(fname)
  #get all the helper functions that are loaded into the function via source
  functions <- list()
  #filter out functions form the environment of the main function
  if (!is.null(env)){
    envList <- as.list(env)
    for(i in 1:length(envList)){
      if (is.function(envList[[i]])){
        functions[[names(envList)[i]]] <- envList[[i]]  
      }
    }  
  }
  #if folder does not exist yet create it
  fnameStr <- deparse(fname)
  suppressWarnings(dir.create(fnameStr))
  #create list that contains all information about the call
  repres <- list(res=res,func=fCon,call=f,helpers=functions)
  #save to folder
  curRes<- list.files(path=fnameStr,pattern='[[:digit:]]+.RData')
  numbers <- as.numeric(sub('.RData','',curRes))
  mNum <- max(max(numbers),0)
  save(list='repres',file=paste0(fnameStr,'/',mNum+1,".RData"))
  return(res)
}