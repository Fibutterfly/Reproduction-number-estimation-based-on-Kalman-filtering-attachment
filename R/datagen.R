library(methods)
#' DataGen: Az osztály célja, hogy adatokat letöltsön
#'
#' Egy bizonyos URL-ről letölti az adatokat és elnevezi őket tetszőleges módon,
#' Amennyiben zip fájlt talál az abban található CSV-ket is a megadott helyre
#' teszi.
#' @slot url character Az url, ahonnan az adatot le tölti
#' @slot export_folder character A mappa ahová menteni kell.
#' @slot file_name character A fájl kívánt lokális neve.
#' @export
setClass("DataGen",
         slots = c(
           url = "character",
           export_folder = "character",
           file_name = "character"
         ),
         prototype = list(
           url = NA_character_,
           export_folder = "./data/original",
           file_name = NA_character_
         )
         
)

#' @name loadAndWriteData
#' @title Betölt és kiír adatot
#' @description
#' A short description...
#' 
#' Megadott URL-ről letölti az adatot és megadott helyre kiírja.
#' @param x A DataGen object.
#' @return A lementett fájl elérése
#' @export
#' @examples
#' dataGenObj <- new("DataGen", url = "http://example.com/data.csv",
#'                   export_folder = "./data", file_name = "downloaded_data.csv")
#' loadAndWriteData(dataGenObj)
setValidity("DataGen",function(object) {
  if(!is.character(object@url)) {
    return("@url not character")
  }
  else if(!is.character(object@export_folder)){
    return("@export_folder not character")
  }
  else if(!is.character(object@file_name)){
    return("@file_name not character")
  }
  else{
    return(TRUE)
  }
})

setGeneric("loadAndWriteData", function(x) standardGeneric("loadAndWriteData"))
setMethod("loadAndWriteData","DataGen", function(x) {
  if (!dir.exists(x@export_folder)){
    dir.create(x@export_folder, recursive = TRUE, showWarnings = TRUE)
  }
  tempFile <- tempfile()
  downloadStatus <- download.file(x@url , destfile = tempFile, mode = "wb")
  if (downloadStatus != 0){
    stop("Failed to download the file from", x@url)
  }
  
  tempDir <- tempdir()
  extractedFiles <- tryCatch({
    unzip(tempFile, exdir = tempDir)
    list.files(tempDir, pattern = "\\.csv$", full.names = TRUE )
  }, error = function(e) NULL)
  if (is.null(extractedFiles) || length(extractedFiles) == 0){
    finalCsvPath <- file.path(x@export_folder, x@file_name)
    if (!file.copy(tempFile, finalCsvPath)) {
      stop("Failed to move the downloaded CSV file to the destination folder.")      
    }
  }
  else {
    finalCsvPath <- file.path(x@export_folder,x@file_name)
    if (!file.copy(extractedFiles, finalCsvPath)){
      stop("Failed to move the extracted CSV file to the destination folder.")      
    }
  }
  unlink(tempFile)
  return(finalCsvPath)
})
