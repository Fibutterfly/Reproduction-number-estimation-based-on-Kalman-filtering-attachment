library(methods)

#' @name cleanFolder
#' @title Kiüríti a mappát
#' @description
#' Megadott mappát
#' @param in_path A tisztítandó mappa
#' @return Siker vagy sem
#' @export
cleanFolder <- function(in_path){
  if(dir.exists(in_path)){
    unlink(in_path,recursive = TRUE)
  }
  tryCatch({
    dir.create(in_path, recursive = TRUE)
  }, error = function(e){
    print(e)
    return(FALSE)
  })
  return(TRUE)
}
