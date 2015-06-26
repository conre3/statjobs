#' Pega posts do statjobs na web usando pacote Rfacebook
#' 
#' Pegar um access_token em https://developers.facebook.com/tools/explorer
#' 
#' @export
pega_posts <- function(at) {
  gid <- '563644450333921'
  u <- sprintf('https://graph.facebook.com/v2.3/%s/feed?limit=500&access_token=%s', 
               gid, at)
  cat('baixando posts 1 a 500...\n')
  r <- httr::GET(u)
  d_atual <- jsonlite::fromJSON(httr::content(r, 'text'))
  d <- list(d_atual)
  k <- 2
  while(is.character(d_atual$paging[[2]])) {
    cat('baixando posts', 500 * (k - 1) + 1, 'a', 500 * k, '...\n')
    u <- d_atual$paging[[2]]
    r <- httr::GET(u)
    d_atual <- jsonlite::fromJSON(httr::content(r, 'text'))
    d[[k]] <- d_atual
    k <- k + 1
  }
  return(d)
}

#' Arruma posts
#' 
#' @export
arruma_posts <- function(d) {
  l <- lapply(d, function(y) {
    x <- y$data
    data.frame(id = x$id,
               from_id = x$from$id,
               from_name = x$from$name,
               type = x$type, 
               created_time = x$created_time,
               message = x$message,
               link = x$link,
               name = x$name,
               description = x$description,
               stringsAsFactors = FALSE)
  })
  d_final <- dplyr::rbind_all(l)
  d_final
}




