library(dplyr)
library(ggplot2)

l <- readRDS('result-total.rds')
l$idx <- 1:nrow(l)

l %>% group_by(title_kor) %>% arrange(desc(release), by_group = TRUE) %>% mutate(n =
                                                                                   row_number()) %>% filter(n == 1) %>%
  select(idx) %>% pull(idx) -> idx.valid

l <- subset(l, idx %in% idx.valid)

######## BAR GRAPH
# Number of Songs Released by Genre per Year

genre_per_year <- l %>%
  mutate(year = substr(release, 1, 4)) %>%
  filter(
    year >= '2019',
    year <= '2021',
    genre %in% c("OST,Ballad", "Rap/Hip-Hop", "Dance", "R&B/Soul")
  ) %>%
  ggplot() +
  geom_bar(aes(year, fill = genre)) +
  facet_wrap( ~ genre) +
  labs(
    title = 'Number of Songs by Genre per Year (2019-2021)\n',
    x = 'Year',
    y = 'Count',
    caption = 'Src: kpoplyrics.net'
  ) +
  #  guides(fill = guide_legend(title='Genre')) +
  guides(fill = 'none') +
  theme_light()

genre_per_year

###############

library(tidytext)
words <-
  l %>% select(artist_eng, genre, release, lyric_e) %>% unnest_tokens(word, lyric_e) %>%
  subset(words, !(word %in% c('ll','re',"can", "won", "gonna", "ve", "will", "let", "around", "cuz", "yes", "didn", "don",'like',"can't","don't","oh","yeah","let's","just","even","go","ooh","woo", "wanna", "won't", "da", "uh", "la", "end", "bong", "dont", "make", "eh", "one", "give","get","na")))
stopwords <-
  httr::content(
    httr::GET(
      'https://raw.githubusercontent.com/stopwords-iso/stopwords-ko/master/raw/geonetwork-kor.txt'
    )
  )
stopwords <- strsplit(stopwords, '\n')
stopwords <- data.frame(word = unlist(stopwords))

library(wordcloud2)
library(tm)

words %>% filter(release >= '2020-04-01' &
                   release <= '2021-04-01') %>% anti_join(stopwords) %>% count(word, sort = TRUE) -> wordCovid

words %>% filter(release > '2021-04-01') %>% anti_join(stopwords) %>% count(word, sort = TRUE) -> wordPost

words %>% filter(release >= '2019-01-01', release < '2020-04-01') %>% anti_join(stopwords) %>% count(word, sort = TRUE) -> wordPre

clearText <- function(df,thres=300){
  
  df <- subset(df, n>thres) %>% rename(freq=n)
  sw <- stopwords('en')
  sw <- c(sw,'ll','re',"can", "won", "gonna", "ve", "will", "let", "around", "cuz", "yes", "didn", "don",'like',"can't","don't","oh","yeah","let's","just","even","go","ooh","woo", "wanna", "won't", "da", "uh", "la", "end", "bong", "dont", "make", "eh", "one", "give","get","na")
  stopwords_regex = paste(sw, collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  good_words <- unique(stringr::str_replace_all(df$word, stopwords_regex, ''))
  
  df %>% filter(word %in% good_words) %>% filter(nchar(word) > 1) %>% filter(grepl('[a-zA-Z]+',word)) -> ww
  ww
}


# Song Lyrics Word Cloud for Pre-Covid
wordcloud2(clearText(wordPre, 100)) -> wc_pre
# Song Lyrics Word Cloud for Covid
wordcloud2(clearText(wordCovid, 100)) -> wc_cov
# Song Lyrics Word Cloud for Post-Covid
wordcloud2(clearText(wordPost, 100)) -> wc_post

cWordPre <- clearText(wordPre, 100)
cWordCovid <- clearText(wordCovid, 100)
cWordPost <- clearText(wordPost, 100)

wmax <- min(c(100, nrow(cWordPre), nrow(cWordCovid), nrow(cWordPost)))

data.frame(
  word = c(head((cWordPre), wmax)$word,
           head((cWordCovid), wmax)$word,
           head((cWordPost), wmax)$word),
  freq = c(head((cWordPre), wmax)$freq,
           head((cWordCovid), wmax)$freq,
           head((cWordPost), wmax)$freq),
  group = c(rep('pre', wmax), rep('covid', wmax), rep('post', wmax)),
  rnk = rep(1:wmax, 3)
) -> word_freq

word_freq %>%
  tidyr::pivot_wider(
    id_col = rnk,
    names_from = group,
    values_from = word,
    values_fn = function(x)
      x
  ) -> word_freq_table

head(clearText(wordPre), wmax)
head(clearText(wordCovid))
head(clearText(wordPost))
#########################################################


## Number of Song Releases by Year
l %>%
  filter(genre %in% c("OST,Ballad", "Rap/Hip-Hop", "Dance", "R&B/Soul")) %>%
  mutate(YearMon = lubridate::floor_date(release, 'year')) %>%
  filter(release >= '2019-01-01', release <= '2022-02-01') %>%
  group_by(YearMon) %>%
  tally() %>%
  ggplot() +
  geom_point(aes(x = YearMon, y = n)) +
  geom_smooth(aes(YearMon, n), method = loess, se = FALSE) +
  labs(title = 'Number of Song Released by Year', x = 'Release Date', y =
         'Songs') +
  theme_classic() -> songs_per_year

songs_per_year

## Number of Artists who released at least a title
l %>%
  filter(genre %in% c("OST,Ballad", "Rap/Hip-Hop", "Dance", "R&B/Soul")) %>%
  mutate(YearMon = lubridate::floor_date(release, 'year')) %>%
  filter(release >= '2019-01-01', release <= '2022-02-01') %>%
  group_by(YearMon, artist_eng) %>%
  summarize(n = length(artist_eng)) %>%
  group_by(YearMon) %>%
  tally() %>%
  ggplot() +
  geom_point(aes(x = YearMon, y = n)) +
  geom_smooth(aes(YearMon, n), method = loess, se = FALSE) +
  labs(title = 'Number of Artists who released a title by Month', x = 'Release Date', y =
         'Artists') +
  theme_classic() -> artists_per_year

artists_per_year

## Number of Artists by Genre who released at least a title
l %>%
  filter(genre %in% c("OST,Ballad", "Rap/Hip-Hop", "Dance", "R&B/Soul")) %>%
  mutate(YearMon = lubridate::floor_date(release, 'year')) %>%
  filter(release >= '2019-01-01', release <= '2022-02-01') %>%
  group_by(YearMon, genre, artist_eng) %>%
  summarize(n = length(artist_eng)) %>%
  group_by(YearMon, genre) %>%
  tally() %>%
  ggplot() +
  geom_point(aes(
    x = YearMon,
    y = n,
    group = genre,
    color = genre
  )) +
  geom_smooth(aes(YearMon, n, group = genre, color = genre),
              method = loess,
              se = FALSE) +
  labs(title = 'Number of Artists by genre, Year', x = 'Release Date', y =
         'Artists') +
  theme(
    strip.text = element_text(face = 'bold'),
    plot.title = element_text(size = 14, face = 'bold'),
    axis.title.x = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 12, face = 'bold')
  ) -> artists_per_year_by_genre

artists_per_year_by_genre

## Number of Songs per Artists by Genre (Korean)
l %>%
  filter(genre %in% c("OST,Ballad", "Rap/Hip-Hop", "Dance", "R&B/Soul")) %>%
  mutate(YearMon = lubridate::floor_date(release, 'year')) %>%
  filter(release >= '2019-01-01', release < '2022-02-01') %>%
  group_by(genre, YearMon, artist_eng) %>%
  summarize(n = length(artist_eng)) %>%
  group_by(genre, YearMon) %>%
  summarize(nartist = length(n), nsong = sum(n)) %>%
  mutate(n = nsong / nartist) %>%
  ggplot() +
  geom_point(aes(
    x = YearMon,
    y = n,
    group = genre,
    color = genre
  )) +
  geom_smooth(aes(YearMon, n, group = genre, color = genre),
              method = loess,
              se = FALSE) +
  labs(title = 'Number of Songs per Artists who released a title by Year', x =
         'Release Date', y = 'Songs per Artist') +
  theme_classic() -> songs_per_artist_by_genre

songs_per_artist_by_genre

## Number of Songs by Genre (All)
l %>%
  filter(genre %in% c("OST,Ballad", "Rap/Hip-Hop", "Dance", "R&B/Soul")) %>%
  mutate(YearMon = lubridate::floor_date(release, 'year')) %>%
  filter(release >= '2019-01-01', release < '2022-02-01') %>%
  group_by(genre, YearMon, artist_eng) %>%
  summarize(n = length(artist_eng)) %>%
  group_by(genre, YearMon) %>%
  tally() %>%
  ggplot() +
  geom_point(aes(
    x = YearMon,
    y = n,
    group = genre,
    color = genre
  )) +
  geom_smooth(aes(YearMon, n, group = genre, color = genre),
              method = loess,
              se = FALSE) +
  labs(title = 'Number of Artists who released a title by Year', x = 'Release Date', y =
         'Songs') +
  theme_classic() -> songs_per_genre

# English translation takes time
l %>%
  filter(genre %in% c("OST,Ballad", "Rap/Hip-Hop", "Dance", "R&B/Soul")) %>%
  filter(release >= '2019-01-01', release < '2022-02-01') %>%
  mutate(YearMon = lubridate::floor_date(release, 'month')) %>%
  group_by(YearMon) %>%
  summarize(
    total = length(lyric),
    english = sum(!is.na(lyric_e)),
    ratio = english / total
  ) %>%
  ggplot() +
  geom_line(aes(x = YearMon, y = ratio), size = 1.2) +
  geom_point(aes(x = YearMon, y = ratio), size = 2) +
  geom_vline(
    xintercept = as.Date('2021-09-01'),
    linetype = 'dotted',
    size = 2,
    color = 'gray'
  ) +
  ggplot2::annotate(
    'text',
    x = as.Date('2021-09-01'),
    y = 0.1,
    label = 'Translation Ratio Drops Sharply',
    vjust = 0,
    hjust = 1,
    size = 6
  ) +
  labs(title = 'Percent of Songs Translated to English by Month', x = 'Release Date', y =
         'Songs') +
  theme(
    strip.text = element_text(face = 'bold'),
    plot.title = element_text(size = 14, face = 'bold'),
    axis.title.x = element_text(size = 12, face = 'bold'),
    axis.title.y = element_text(size = 12, face = 'bold')
  ) -> p_translation_pct

p_translation_pct


ggx <-
  function(x, fn)
    ggsave(
      x,
      width = 7,
      height = 5,
      units = 'in',
      file = fn
    )

songs_per_year %>% ggx('songs_per_year.png')
genre_per_year %>% ggx('genre_per_year.png')
songs_per_genre %>% ggx('songs_per_genre.png')

artists_per_year %>% ggx('artists_per_year.png')
songs_per_artist_by_genre %>% ggx('songs_per_artist_by_genre.png')

p_translation_pct %>% ggx('p_translation_pct.png')

wc_pre
wc_cov
wc_post

saveRDS(word_freq_table, 'freq_table.rds')
