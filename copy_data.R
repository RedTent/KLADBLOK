
my_data_copy <- function(filename) {
  
  origin <- paste0("J:/data/", filename)
  destination <- paste0("data/", filename)
  
  make_copy <- !file.exists(destination) | (file.info(origin)$mtime > file.info(destination)$mtime)
  
  if (make_copy) {
    file.copy(from = origin, to = destination, overwrite = TRUE)
    message(paste(filename, "gekopieerd naar /data"))
  } else {
    message(paste(filename, "staat reeds in /data")) 
    }
  
  
}


