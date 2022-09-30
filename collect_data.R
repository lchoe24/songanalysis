
result <- NULL

for(uf in list.files(path='data',pattern='rds')){
  print(uf)
  
  u <- readRDS(paste0('data/',uf))
  x <- u$title
  x1 <- strsplit(x,' - ')
  x2 <- strsplit( sapply(x1,`[`,1),' (',fixed=T)
  
  x3 <- sapply(x1,'[',2)
  title_eng <- gsub('Lyrics','',gsub('.*\\((.*)\\).*','\\1',x3))
  title_kor <- gsub('Lyrics','',gsub('(^.*) \\(.*','\\1',x3))
  
  artist_kor <- sapply(x2,`[`,1)
  artist_eng <- gsub(')','',sapply(x2,`[`,2),fixed=TRUE)
  
  genre <- strsplit(u$genre,' : ')[[1]][2]
  release <- as.Date(strsplit(u$release,' : ')[[1]][2])
  language <- strsplit(u$language,' : ')[[1]][2]
  lyric <- gsub('\n',' ',u$lyric)
  
  v <- data.frame(
    page = u$page, idx = u$idx, url=u$url, id=u$id,
    title_eng = title_eng, title_kor = title_kor,
    artist_eng = artist_eng, artist_kor = artist_kor,
    genre = genre, release=release, langauge = language,
    lyric=lyric
  )
  
  result <- rbind(result,v)
}

saveRDS(result,file='result-2022.rds')

result$genre <- gsub('[Hh]ip[ -][Hh]op','Hip-Hop',result$genre)
result$genre <- gsub('&amp;','&',result$genre)
result$genre <- gsub(' ','',result$genre)

saveRDS(result,file='result-2022-clean.rds')
