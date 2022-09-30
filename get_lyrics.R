library(httr)

options(stringsAsFactors=FALSE)

song_db <- NULL
runidx <- as.numeric(gsub('.*/','',getwd()))
if(is.na(runidx)) runidx <- 1
nrun <- 0

cat("Running for index",runidx)

modify <- function(s){
  s <- gsub(':','__',fixed=T,s)
  s <- gsub('/','_',fixed=T,s)
  s <- gsub('.','_',fixed=T,s)
  s <- gsub(':','_',fixed=T,s)
  s
}

for(pidx in seq(runidx,3000,10)){
  
  if(file.exists(paste0('page',pidx,'.html'))){
    cc <- readr::read_file(paste0('page',pidx,'.html'))
  } else {
    l <- GET(paste0('https://klyrics.net/page/',pidx,'/?s'), timeout(30))
    cc <- content(l,'text')
  }
  
  ar.pos <- gregexpr('<article',cc)[[1]]
  en.pos <- gregexpr('</article>',cc)[[1]]
  
  for(idx in seq_along(1:length(ar.pos))){
    
    xp <- ar.pos[idx]
    yp <- en.pos[en.pos > xp][1]
    article <- substr(cc,xp,yp)
    
    url <- gsub('.*a href="([^"]+)".*','\\1',article)
    article <- gsub('.*td-excerpt">','',article)
    article <- gsub('</div.*','',article)
    
    if(!grepl('Korean',article)) next
    
    info <- strsplit(article,'\n')[[1]]
    cat('Processing',info[1])
    
    song_page_file <- paste0('../songs/', modify(url))
    if(file.exists(song_page_file)){
      s <- readr::read_file(song_page_file)
    } else {
      song_page <- GET(url, timeout(30))
      s <- content(song_page,'text')
      cat(s,file=song_page_file)
    }
    
    j <- gregexpr('https://klyrics\\.net/wp-json/wp/v2/posts/([0-9]+)',s)[[1]]
    lurl <- substr(s,j[1],j[1]+attr(j,'match.length')-1)
    
    song_json_file <- paste0('../json/',modify(lurl))
    
    if(file.exists(song_json_file)){
      ltxt <- readr::read_file(song_json_file)
    } else {
      ltxt <- content(GET(lurl),'text')
      cat(ltxt,file=song_json_file)
    }
    
    lyric <- ''
    lid <- 0
    
    try({
      
      ll <- jsonlite::fromJSON(ltxt)
      lid <- ll$id
      lp <- ll$content$rendered
      kl <- strsplit(strsplit(lp,"Hangul</h2>")[[1]][2],"<h2")[[1]][1]
      lyric <- gsub('</[^>]+>',' ',gsub('<[^>]+>',' ',kl))
    })
    
    song_list <- data.frame(
      page = pidx,
      idx = idx,
      url = as.character(url),
      id = lid,
      title = as.character(info[1]),
      genre = as.character(info[2]),
      release = as.character(info[3]),
      language = as.character(info[4]),
      retrieved = Sys.time(),
      lyric = as.character(lyric)
    )
    song_db <- rbind(song_db, song_list)
    
    
  }
  
  cat(cc,file=paste0('page',pidx,'.html'))
  
  nrun <- nrun + 1
  print(pidx)
  
  if(nrun >= 10 ){
    fname <- sprintf('song_db%04d.rds',pidx)
    saveRDS(song_db, file=fname)
    song_db <- NULL
    nrun <- 0
  }
  
}
