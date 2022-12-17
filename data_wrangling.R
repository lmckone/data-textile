library(dplyr)
library(magrittr)
library(readxl)
library(stringr)
library(clipr)

castaways <- read_excel("desert_island_discs.xlsx", sheet = "castaways")

discs <- read_excel("desert_island_discs.xlsx", sheet = "discs")

spotify <- read_excel("desert_island_discs.xlsx", sheet = "spotify_data")

# Get the row indexes of missing data and data to process

# Missing data

write_clip(which(is.na(castaways$country_of_citizenship)))
write_clip(which(is.na(castaways$place_of_birth)))
write_clip(which(is.na(castaways$country_of_birth)))
write_clip(which(is.na(castaways$date_of_birth)))
write_clip(which(is.na(castaways$favTrack)))
write_clip(which(castaways$favTrack == 'NA by NA'))
write_clip(which(is.na(castaways$luxury)))
write_clip(which(is.na(castaways$book)))
write_clip(which(is.na(castaways$wiki_link)))
write_clip(which(is.na(castaways$link)))

# data that needs processing

write_clip(which(castaways$country_of_citizenship %in% c('Australians', 
                                                        'Austria-Hungary',
                                                        'British India',
                                                        'Czechoslovakia',
                                                        'English people',
                                                        'German Empire',
                                                        'Kenya Colony',
                                                        'Kingdom of the Netherlands',
                                                        'Russian Empire',
                                                        'Soviet Union',
                                                        'United Kingdom of Great Britain and Ireland')))

write_clip(which(str_detect(castaways$place_of_birth, ".*[≈√»ƒ].*")))

write_clip(which(castaways$country_of_birth %in% c('British Empire',
                                                         'British India',
                                                         'Byzantine Empire',
                                                         'Danish Realm',
                                                         'Duchy of Moscow',
                                                         'Kingdom of England',
                                                         'Kingdom of Italy', 
                                                         'Kingdom of the Netherlands',
                                                         'Mandatory Palestine',
                                                         'Margraviate of Brandenburg',
                                                         'Polish People’s Republic',
                                                         'Republic of China (1912‚Äì1949)',
                                                         'Roman Empire',
                                                         'Russian Empire',
                                                         'Tsardom of Russia',
                                                         'United Kingdom of Great Britain and Ireland')))

write_clip(which(str_detect(castaways$favTrack, ".*[≈√Å¬].*")))

write_clip(which(str_detect(castaways$luxury, ".*[√¬].*")))

write_clip(which(str_detect(castaways$book, ".*[√¬].*")))

# fix - clusters

clusters_favTrack <- read.csv('clusters/castaways-favTrack.csv')
write_clip(which(castaways$favTrack %in% clusters_favTrack$cluster_value))

clusters_luxury <- read.csv('clusters/castaways-luxury.csv')
write_clip(which(castaways$luxury %in% clusters_luxury$cluster_value))

clusters_book <- read.csv('clusters/castaways-book.csv')
write_clip(which(castaways$book %in% clusters_book$cluster_value))

# de-duplicate

lubov <- read.csv('lubov-indexes.csv')

lubov %<>% distinct(.keep_all = TRUE)

write.csv(lubov, 'lubov-indexes.csv')

#### Jess's columns ####

# data to fix

write_clip(which(str_detect(discs$track_name, ".*[≈√Å¬ÄÖ].*")))

write_clip(which(str_detect(discs$std_artist, ".*√.*")))

write_clip(which(str_detect(discs$std_disc, ".*[√¬].*")))

write_clip(which(discs$std_disc %in% c('johann sebastian bach  #  cello suite no 1 in g major bwv1007 1 prelude',
                                                   'johann sebastian bach  #  cello suite number 1 prelude in g major',
                                                   'wolfgang amadeus mozart  #  symphony no 41 in c major',
                                                   'wolfgang amadeus mozart  #  symphony no 41 in c major jupiter',
                                                   'robert schumann  #  piano quintet in e flat major',
                                                   'robert schumann  #  ppiano quintet in e flat major')))


write_clip(which(str_detect(discs$std_name, ".*√.*")))

# missing data

write_clip(which(is.na(discs$std_artist)))

write_clip(which(is.na(discs$track_name)))

# fix - clusters

clusters_std_artist <- read.csv('clusters/discs-std_artist.csv')
write_clip(which(discs$std_artist %in% clusters_std_artist$cluster_value))

clusters_std_disc <- read.csv('clusters/discs-std_disc.csv')
write_clip(which(discs$std_disc %in% clusters_std_disc$cluster_value))

clusters_track_name <- read.csv('clusters/discs-track_name.csv')
write_clip(which(discs$track_name %in% clusters_track_name$cluster_value))

# visual checks

write_clip(which(discs$track_name %in% c('Piano Concerto No. 1 in D minor by Johannes Brahms',
                                       'Piano Concerto No. 1 in D minor, Op. 15 by Johannes Brahms',
                                       'Cello Concerto in E Minor by Edward Elgar',
                                       'Cello Concerto in E minor Op. 85 by Edward Elgar',
                                       'Piano Concerto No. 1 in B flat minor - 3rd movement by Peter Ilyich Tchaikovsky',
                                       'Piano Concerto No. 1 in B flat minor - 3rd movement by Pyotr Ilyich Tchaikovsky',
                                       'Violin Concerto in D major by Peter Ilyich Tchaikovsky',
                                       'Violin Concerto in D major by Pyotr Ilyich Tchaikovsky',
                                       'Piano Concerto No. 3 in D minor by Sergey Rachmaninov',
                                       'Piano Concerto No. 3 in D Minor, Op. 30 by Sergey Rachmaninov',
                                       'Cello Concerto in E Minor - opening by Edward Elgar',
                                       'Cello Concerto in E Minor by Edward Elgar',
                                       'Cello Concerto in E minor Op. 85 by Edward Elgar',
                                       'Cello Concerto in E minor, Op. 85 ‚Äì 1st movement: Adagio Moderato by Edward Elgar',
                                       'Cello Concerto No. 1in E flat major - 1st movement by Dmitry Shostakovich',
                                       'Cello Concerto No. 1in E flat major by Dmitry Shostakovich',
                                       'Clarinet Concerto in A major - 2nd movement by Wolfgang Amadeus Mozart',
                                       'Clarinet Concerto in A major by Wolfgang Amadeus Mozart',
                                       'Concerto  for Oboe and Violin in C minor - 1st movement by Johann Sebastian Bach',
                                       'Concerto  for Oboe and Violin in C minor by Johann Sebastian Bach',
                                       'Concerto for Two Mandolins by Antonio Vivaldi',
                                       'Concerto for Two Mandolins in G major by Antonio Vivaldi',
                                       'Concerto for Two Pianos in D Minor - 2nd movement by Francis Poulenc',
                                       'Concerto for Two Pianos in D Minor by Francis Poulenc',
                                       'Horn Concerto No. 3 in E flat major - 3rd movement by Wolfgang Amadeus Mozart',
                                       'Horn Concerto No. 3 in E flat major by Wolfgang Amadeus Mozart',
                                       'Piano Concerto in B flat major - 3rd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto in B Flat Major by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 1 in B flat minor - 1st movement by Pyotr Ilyich Tchaikovsky',
                                       'Piano Concerto No. 1 in B flat minor - 3rd movement by Peter Ilyich Tchaikovsky',
                                       'Piano Concerto No. 1 in B flat minor - 3rd movement by Pyotr Ilyich Tchaikovsky',
                                       'Piano Concerto No. 1 in B flat minor by Peter Ilyich Tchaikovsky',
                                       'Piano Concerto No. 1 in B Flat Minor by Pyotr Ilyich Tchaikovsky',
                                       'Piano Concerto No. 1 in D minor by Johannes Brahms',
                                       'Piano Concerto No. 1 in D minor, Op. 15 by Johannes Brahms', 
                                       'Piano Concerto No. 11 in F by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 11 in F major - 2nd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 2 in B flat major - 1st movement by Johannes Brahms',
                                       'Piano Concerto No. 2 in B flat major by Johannes Brahms',
                                       'Piano Concerto No. 20 in D minor - 2nd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 20 in D minor by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 20, 2nd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 21 in C - the Andante by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 21 in C major - 2nd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 21 in C major by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 22 in E flat major - 3rd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 22 in E flat major by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 23 in A major - 1st movement (opening) by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 23 in A major - 1st movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 23 in A major - 2nd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 23 in A major - 3rd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 23 in A major by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 24 in C minor - 2nd movement by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 24 in C minor by Wolfgang Amadeus Mozart',
                                       'Piano Concerto No. 3 in D minor by Sergey Rachmaninov',
                                       'Piano Concerto No. 3 in D Minor, Op. 30 by Sergey Rachmaninov',
                                       'Trumpet Concerto in D major - 1st movement by Georg Philipp Telemann',
                                       'Trumpet Concerto in D major by Georg Philipp Telemann',
                                       'Trumpet Concerto in E flat by Joseph Haydn',
                                       'Trumpet Concerto in E flat major - 2nd movement by Joseph Haydn',
                                       'Trumpet Concerto in E flat major - 3rd movement by Johann Nepomuk Hummel',
                                       'Trumpet Concerto in E flat major by Johann Nepomuk Hummel',
                                       'Violin Concerto in D major by Peter Ilyich Tchaikovsky',
                                       'Violin Concerto in D major by Pyotr Ilyich Tchaikovsky',
                                       'Violin Concerto No. 2 in G minor - 2nd movement by Sergei Prokofiev',
                                       'Violin Concerto No. 2 in G Minor by Sergei Prokofiev')))

write_clip(which(discs$std_artist %in% c('peter ilyich tchaikovsky',
                                         'pyotr ilyich tchaikovsky')))

write_clip(which(discs$std_disc %in% c('johann sebastian bach  #  violin concerto',
                                       'johann sebastian bach  #  violin concerto in a minor',
                                       'wolfgang amadeus mozart  #  concerto for orchestra',
                                       'alan rawsthorne  #  piano concerto',
                                       'alan rawsthorne  #  piano concerto no 2',
                                       'antonio vivaldi  #  concerto no 4',
                                       'antonio vivaldi  #  concerto no 4 in f minor',
                                       'Cello Concerto in E minor Op. 85',
                                       'Cello Concerto in E minor, Op. 85 ‚Äì 1st movement: Adagio Moderato',
                                       'francis poulenc  #  concerto for two pianos in d minor',
                                       'francis poulenc  #  concerto in d minor',
                                       'francis poulenc  #  piano concerto',
                                       'frederic chopin  #  concerto no 1 in e minor op 11',
                                       'frederic chopin  #  piano concerto no 1',
                                       'georg philipp telemann  #  trumpet concerto in d',
                                       'georg philipp telemann  #  trumpet concerto in d major',
                                       'johann nepomuk hummel  #  trumpet concerto in e flat',
                                       'johann nepomuk hummel  #  trumpet concerto in e flat major',
                                       'max bruch  #  violin concerto',
                                       'max bruch  #  violin concerto in d minor',
                                       'nicolo paganini  #  violin concerto',
                                       'nicolo paganini  #  violin concerto in b minor',
                                       'pyotr ilyich tchaikovsky  #  violin concerto in d major',
                                       'peter ilyich tchaikovsky  #  violin concerto in d major',
                                       'johann sebastian bach  #  cello suite no 1 in g major bwv1007 1 prelude',
                                       'johann sebastian bach  #  cello suite number 1 prelude in g major',
                                       'wolfgang amadeus mozart  #  symphony no 41 in c major',
                                       'wolfgang amadeus mozart  #  symphony no 41 in c major jupiter',
                                       'robert schumann  #  piano quintet in e flat major',
                                       'robert schumann  #  ppiano quintet in e flat major')))



# de-duplicate

jess <- read.csv('jess-indexes.csv')

jess %<>% distinct(.keep_all = TRUE)

write.csv(jess, 'jess-indexes.csv')


