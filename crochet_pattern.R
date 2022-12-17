library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(rle)

ava <- read.csv("ava-indexes.csv")
lubov <- read.csv("lubov-indexes.csv")

# ensure no duplicates

ava %<>% distinct(.keep_all = TRUE)
lubov %<>% distinct(.keep_all = TRUE)

# standardize datasets to combine
lubov %<>% select(-X)
names(ava) <- tolower(names(ava))

# combine
castaway_indexes <- rbind(lubov, ava)

castaways_matrix <- data.frame(matrix(1, nrow = 3218, ncol = 17))
names(castaways_matrix) <- names(castaways)

# create the 'map' showing where values are missing or need to be fixed

castaways_matrix %<>%
  # create temporary variable for index
  mutate(index = row_number()-1) %>%
  mutate(country_of_citizenship = case_when(index %in% (castaway_indexes %>% filter(column_index == 'country_of_citizenship' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'country_of_citizenship' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ country_of_citizenship),
         place_of_birth = case_when(index %in% (castaway_indexes %>% filter(column_index == 'place_of_birth' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'place_of_birth' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ place_of_birth),
         country_of_birth = case_when(index %in% (castaway_indexes %>% filter(column_index == 'country_of_birth' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'country_of_birth' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ country_of_birth),
         date_of_birth = case_when(index %in% (castaway_indexes %>% filter(column_index == 'date_of_birth' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'date_of_birth' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ date_of_birth),
         favTrack = case_when(index %in% (castaway_indexes %>% filter(column_index == 'favTrack' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'favTrack' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ favTrack),
         luxury = case_when(index %in% (castaway_indexes %>% filter(column_index == 'luxury' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'luxury' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ luxury),
         book = case_when(index %in% (castaway_indexes %>% filter(column_index == 'book' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'book' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ book),
         wiki_link = case_when(index %in% (castaway_indexes %>% filter(column_index == 'wiki_link' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'wiki_link' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ wiki_link),
         name = case_when(index %in% (castaway_indexes %>% filter(column_index == 'name' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'name' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ name),
         std_name = case_when(index %in% (castaway_indexes %>% filter(column_index == 'std_name' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'std_name' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ std_name),
         gender = case_when(index %in% (castaway_indexes %>% filter(column_index == 'gender' & action == 'missing') %>% pull(row_index)) ~ 0,
                                 index %in% (castaway_indexes %>% filter(column_index == 'gender' & action == 'fix') %>% pull(row_index)) ~ 2,
                                 TRUE ~ gender),
         profession = case_when(index %in% (castaway_indexes %>% filter(column_index == 'profession' & action == 'missing') %>% pull(row_index)) ~ 0,
                            index %in% (castaway_indexes %>% filter(column_index == 'profession' & action == 'fix') %>% pull(row_index)) ~ 2,
                            TRUE ~ profession)) %>%
  # remove temporary index variable
  select (-index)

# because patterns are worked left to right and then right to left, every other row in the dataframe needs to be reversed, starting with the first row 
# break the dataframe into odd and even rows, reverse the odd rows, recombine and sort by the index to retain the original row order

castaways_matrix %<>%
  mutate(index = row_number())

row_odd <- seq_len(nrow(castaways_matrix)) %% 2
castaways_matrix_odd_rows <- castaways_matrix[row_odd == 1,]
castaways_matrix_even_rows <- castaways_matrix[row_odd == 0,]

castaways_matrix_odd_rows %<>% rev()

castaways_matrix_even_rows <- castaways_matrix_even_rows[, c(18, 1:17)]
names(castaways_matrix_odd_rows) <- names(castaways_matrix_even_rows)

castaways_matrix_reversed <- rbind(castaways_matrix_odd_rows, castaways_matrix_even_rows) %>%
  arrange(index) %>%
  select(-index)

# turn the dataframe into a string for manipulation into crochet pattern
pattern <- format_delim(castaways_matrix_reversed, ",", col_names = FALSE)

# turn everything into chain or double crochet, etc.
pattern %<>% str_replace_all('1', 'sc')
pattern %<>% str_replace_all('0', 'ch')
pattern %<>% str_replace_all('2', 'bpsc')
pattern %<>% str_replace_all('\n', ',turn,ch,\n,')



# replace multiple consecutive occurences of "1" with that number of single crochets, etc. 
final_pattern <- with(rle(strsplit(pattern, ",")[[1]]), paste(values, lengths, collapse = ", "))
final_pattern %<>% str_replace_all(', \n 1, ', '\n')
final_pattern %<>% str_replace_all('turn 1', 'turn')
writeLines(final_pattern, "pattern.txt")

write.csv(castaways_matrix, "castaways_matrix.csv")


# https://stackoverflow.com/questions/34023617/counting-consecutive-patterns-in-strings-using-r







