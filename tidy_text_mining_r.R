setwd('C:\\Users\\phucn\\Documents\\git\\tidy-text-mining')

# install.packages('tidytext')
# install.packages('tidyverse')
library(tidyverse)
require(tidytext)
# install.packages(c('ggraph', 'widyr'))
# install.packages('ggraph')
require(ggraph)
require(widyr)

# install.packages('gutenbergr')
require(gutenbergr)
library(scales)
require(janeaustenr)
# install.packages('wordcloud')
library(wordcloud)
library(reshape2)

# example tokenization ------
text = c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text
text_df = data_frame(line = 1:4, text = text)
text_df %>% as.tibble
text_df %>% unnest_tokens('word', 'text')
text_df %>% unnest_tokens('word', 'text', token = 'ngrams', n = 2)
text_df %>% unnest_tokens('word', 'text', token = 'skip_ngrams', k = 1)
text_df %>% unnest_tokens('word', 'text', token = 'character_shingles')
text_df %>% unnest_tokens('word', 'text', token = 'sentences')
text_df %>% unnest_tokens('word', 'text', token = 'tweets')
# Built-in options are "words" (default), "characters", "character_shingles", "ngrams", "skip_ngrams", "sentences", "lines", "paragraphs", "regex", "tweets" (tokenization by word that preserves usernames, hashtags, and URLS ), and "ptb" (Penn Treebank). If a function, should take a character vector and return a list of character vectors of the same length.

# same with this, because this is tidy code style lolz
text_df %>% unnest_tokens(word, text)

# tidying text -----

original_books = austen_books() %>% 
    # as.tibble() %>% 
    group_by(book) %>% 
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% 
    ungroup()
# okay \divxlc means arabic number or roman number
# regex "^chapter [\\divxlc]" mean begin by 'chapter ', followed by arabic or roman number, which is clearly signify chapter number

table(original_books$book, original_books$chapter)

tidy_books = original_books %>% 
    unnest_tokens('word', 'text')
tidy_books = tidy_books %>% anti_join(stop_words)
tidy_books %>% count(word, sort = TRUE) %>% 
    filter(n > 500) %>% 
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

hgwells = gutenberg_download(c(35,36,5230,159))

tidy_hgwells = hgwells %>% 
    unnest_tokens('word', 'text') %>% 
    anti_join(stop_words)

tidy_hgwells %>% 
    count(word, sort = TRUE)

gutenberg_metadata %>% 
    filter(gutenberg_id %in% c(1260,768,969,9182,767))

gutenberg_authors %>% 
    filter(str_detect(author, 'Brontë'))

bronte = gutenberg_download(c(1260,768,969,9182,767))
    
tidy_bronte = bronte %>% 
    unnest_tokens('word', 'text') %>% 
    anti_join(stop_words) 

tidy_bronte %>%     
    count(word, sort = TRUE)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>% 
    select(-n) %>% 
    spread(author, proportion) %>% 
    gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)



ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    facet_wrap(~author, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == 'Brontë Sisters',], ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == 'H.G. Wells',], ~ proportion + `Jane Austen`)

# sentiments analysis -----

sentiments
get_sentiments('bing')

get_sentiments('nrc') %>% 
    filter(word == 'joy')

tidy_books %>% 
    inner_join(get_sentiments('nrc')) %>% 
    filter(sentiment == 'joy') %>% 
    filter(book == 'Emma') %>% 
    count(word, sort = T)

jane_austen_sentiment = tidy_books %>% 
    inner_join(get_sentiments('bing')) %>% 
    count(book, index = linenumber %/% 80, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative)

jane_austen_sentiment %>% 
    ggplot(aes(index, sentiment, fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scale = 'free_x')

# analyze tarzan story sentiments ----
tarzan_set_id = (gutenberg_metadata %>% 
    filter(str_detect(title, 'Tarzan')))$gutenberg_id

tarzan_set = gutenberg_download(tarzan_set_id)
table(tarzan_set$gutenberg_id) %>% 
    as_data_frame() %>% 
    mutate(gutenberg_id = as.numeric(Var1)) %>% 
    inner_join(gutenberg_metadata)

tarzan_set_sentiment = tarzan_set %>% 
    inner_join(gutenberg_metadata %>% select(c(gutenberg_id, title))) %>% 
    group_by(title) %>% 
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% 
    ungroup() %>% 
    unnest_tokens('word', 'text') %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments('bing')) %>% 
    count(title, index = linenumber %/% 80, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative)

tarzan_set_sentiment %>% 
    ggplot(aes(index, sentiment, fill = title)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~title, ncol = 2, scale = 'free_x')

# compare sentiment dictionary -----
pride_prejudice = tidy_books %>% 
    filter(book == 'Pride & Prejudice')

afinn = pride_prejudice %>% 
    inner_join(get_sentiments('afinn')) %>% 
    group_by(index = linenumber %/% 80) %>% 
    summarise(sentiment = sum(score)) %>% 
    mutate(method = 'AFINN')
bing_and_nrc = bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments('bing')) %>% 
                            mutate(method = 'Bing'),
                        pride_prejudice %>% 
                            inner_join(get_sentiments('nrc') %>% 
                                           filter(sentiment %in% c('positive', 'negative'))) %>% 
                            mutate(method = 'NRC')) %>% 
    count(method, index = linenumber %/% 80, sentiment) %>% 
    spread(sentiment, n , fill = 0) %>% 
    mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>% 
    ggplot(aes(index, sentiment, fill = method)) +
    geom_col(show.legend = F) +
    facet_wrap(~method, ncol = 1, scales = 'free_y')

bing_word_counts <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

bing_word_counts

bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()

custom_stop_words <- bind_rows(data_frame(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

# word cloud ------


tidy_books %>% 
    anti_join(stop_words) %>% 
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))

tidy_books %>% 
    inner_join(get_sentiments('bing')) %>% 
    count(word, sentiment, sort = TRUE) %>% 
    acast(word ~ sentiment, value.var = 'n', fill = 0) %>%
    as.data.frame() %>% 
    as.tibble() %>% 
    comparison.cloud(colors = c('gray20', 'gray80'),
                     max.words = 100)
# beyond unigram ------
bingnegative <- get_sentiments("bing") %>% 
    filter(sentiment == "negative")

wordcounts <- tidy_books %>%
    group_by(book, chapter) %>%
    summarize(words = n())

tidy_books %>%
    semi_join(bingnegative) %>%
    group_by(book, chapter) %>%
    summarize(negativewords = n()) %>%
    left_join(wordcounts, by = c("book", "chapter")) %>%
    mutate(ratio = negativewords/words) %>%
    filter(chapter != 0) %>%
    top_n(1) %>%
    ungroup()

## TF-IDF -----
book_words = gutenberg_download(tarzan_set_id) %>% 
    inner_join(gutenberg_metadata %>% select(c(gutenberg_id, title)) %>% rename(book = title)) %>% 
    unnest_tokens('word', 'text') %>% 
    count(book, word, sort = T) %>% 
    ungroup()

total_words = book_words %>% 
    group_by(book) %>% 
    summarise(total = sum(n))

book_words = book_words %>% left_join(total_words)

book_words %>% 
    ggplot(aes(n/total, fill = book)) +
    geom_histogram(show.legend = F)+
    xlim(NA, 0.0009)+
    facet_wrap(~book, ncol = 2, scales = 'free_y')

freq_by_rank = book_words %>% 
    group_by(book) %>% 
    mutate(rank = row_number(),
           tf = n/total)

freq_by_rank %>% 
    ggplot(aes(rank, tf, color = book)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
rank_subset <- freq_by_rank %>% 
    filter(rank < 500,
           rank > 10)

ml_fit = lm(log10(tf) ~ log10(rank), data = rank_subset)
fit_intercept = (summary(ml_fit)$coefficients)[1]
fit_slope = (summary(ml_fit)$coefficients)[2]
freq_by_rank %>% 
    ggplot(aes(rank, tf, color = book)) + 
    geom_abline(intercept = fit_intercept, slope = fit_slope, color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()

book_words = book_words %>% 
    bind_tf_idf(word, book, n)

book_words %>%
    select(-total) %>%
    arrange(desc(tf_idf))

book_words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = book)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~book, ncol = 2, scales = "free") +
    coord_flip()

physics <- gutenberg_download(c(37729, 14725, 13476, 5001), 
                              meta_fields = c("author", 'title'))

physics_words <- physics %>%
    unnest_tokens(word, text) %>%
    count(author, word, sort = TRUE) %>%
    ungroup()

mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                   "fig", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>%
    bind_tf_idf(word, author, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan", 
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))
plot_physics %>% 
    group_by(author) %>% 
    top_n(15, tf_idf) %>% 
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    coord_flip()

# words relationship ------

get_books = function(list_gutenberg_id) {
    gutenberg_download(list_gutenberg_id) %>% 
        inner_join(gutenberg_metadata %>% select(c(gutenberg_id, title)) %>% rename(book = title))
}
tarzan_bigrams = get_books(tarzan_set_id) %>% 
    unnest_tokens('bigram', 'text', token = 'ngrams', n = 2)
tarzan_bigrams_sep = tarzan_bigrams %>% 
    separate(bigram, c('w1', 'w2'), sep = ' ') %>% 
    mutate(bigram = str_c(w1, w2, sep = ' '))

tarzan_bigrams_filtered = tarzan_bigrams_sep %>% 
    filter(!w1 %in% stop_words$word & !w2 %in% stop_words$word) 
    
tarzan_bigrams_filtered %>% count(bigram, sort = T)

tarzan_trigrams = get_books(tarzan_set_id) %>% 
    unnest_tokens('trigram', 'text', token = 'ngrams', n = 3)

tarzan_trigrams_sep = tarzan_trigrams %>% 
    separate(trigram, c('w1', 'w2', 'w3'), sep = ' ') %>% 
    mutate(trigram = str_c(w1, w2, w3, sep = ' '))

tarzan_trigrams_filtered = tarzan_trigrams_sep %>% 
    filter(!w1 %in% stop_words$word & !w2 %in% stop_words$word & !w3 %in% stop_words$word)

tarzan_trigrams_filtered %>% 
    count(trigram, sort = T)

tarzan_bigrams_filtered %>% filter(w1 == 'lord')

tarzan_bigrams_filtered %>% 
    count(book, bigram) %>% 
    bind_tf_idf(bigram, book, n) %>% 
    arrange(desc(tf_idf))

# detect negative phrases --------
tarzan_bigrams_sep %>% 
    filter(w1 == 'not') %>% 
    count(w1, w2, sort = TRUE)

afinn = get_sentiments('afinn')

not_words = tarzan_bigrams_sep %>% 
    filter(w1 == 'not') %>% 
    inner_join(afinn, by = c('w2' = 'word')) %>% 
    count(w2, score, sort = T) %>% 
    ungroup()

not_words %>%
    mutate(contribution = n * score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(w2 = reorder(w2, contribution)) %>%
    ggplot(aes(w2, n * score, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by \"not\"") +
    ylab("Sentiment score * number of occurrences") +
    scale_fill_manual(values = c("firebrick", "royalblue")) + 
    coord_flip()

negation_words = c('not', 'no', 'never', 'without')

negated_words <- tarzan_bigrams_sep %>%
    filter(w1 %in% negation_words) %>%
    inner_join(afinn, by = c(w2 = "word")) %>%
    count(w2, score, sort = TRUE) %>%
    ungroup()

negated_words %>%
    mutate(contribution = n * score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(w2 = reorder(w2, contribution)) %>%
    ggplot(aes(w2, n * score, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by \"not\"") +
    ylab("Sentiment score * number of occurrences") +
    scale_fill_manual(values = c("firebrick", "royalblue")) + 
    coord_flip()


# install.packages('igraph')
require(igraph)
require(ggraph)

tarzan_bigrams_filtered %>% 
    count(w1, w2, sort = T)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

bigram_graph = tarzan_bigrams_filtered %>% 
    count(w1, w2, sort = T) %>% 
    filter(n > 20) %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()

count_bigrams <- function(dataset) {
    dataset %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE)
}
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
visualize_bigrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
}

kjv <- gutenberg_download(10)
kjv_bigrams = kjv %>% 
    count_bigrams() %>% 
    filter(n > 40, 
           !str_detect(word1, '\\d'),
           !str_detect(word2, '\\d')) %>% 
    visualize_bigrams()

## counte, correlates --------
austen_section_words = get_books(tarzan_set_id) %>%
    filter(book == "Tarzan of the Apes") %>%
    mutate(section = row_number() %/% 10) %>%
    filter(section > 0) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)

word_pairs = austen_section_words %>% 
    pairwise_count(word, section, sort = T)

word_cors = austen_section_words %>% 
    group_by(word) %>% 
    filter(n() >= 20) %>% 
    pairwise_cor(word, section, sort = T)

word_cors %>%
    # filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
    filter(item1 %in% c("greystoke", "porter", "tarzan", "alice")) %>%
    group_by(item1) %>%
    top_n(6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()

word_cors %>%
    filter(correlation > .15) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()

## tidy vs matrix conversion -------

# install.packages('tm')
library(tm)
# install.packages('topicmodels')
require(topicmodels)
library(broom)

data("AssociatedPress", package = 'topicmodels')
Terms(AssociatedPress)

ap_tidy = tidy(AssociatedPress)
ap_sentiments = ap_tidy %>% 
    inner_join(get_sentiments('bing'), by = c(term = 'word'))

ap_sentiments %>%
    count(sentiment, term, wt = count) %>%
    ungroup() %>%
    filter(n >= 200) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ylab("Contribution to sentiment") +
    coord_flip()


# install.packages('quanteda')
require(quanteda)

data('data_corpus_inaugural', package = 'quanteda')
data_corpus_inaugural

inaug_dfm = dfm(data_corpus_inaugural, verbose = F)
inaug_tidy = tidy(inaug_dfm) %>% 
    bind_tf_idf(term, document, n = count) %>% 
    arrange(desc(tf_idf))

table(inaug_tidy$document)

inaug_tidy_four = inaug_tidy %>% 
    filter(document %in% c('2009-Obama', '2017-Trump', '1941-Roosevelt', '1961-Kennedy'))

inaug_tidy_four %>% 
    anti_join(stop_words, by = c('term' = 'word')) %>% 
    filter(term != '-') %>% 
    group_by(document) %>%
    arrange(desc(tf_idf)) %>%
    slice(1:10) %>%
    ungroup() %>% 
    mutate(term = reorder(term, tf_idf)) %>%
    ggplot(aes(term, tf_idf, fill = document)) +
    geom_bar(stat = 'identity') +
    ylab("Most notable terms") +
    facet_wrap(~document, scales = 'free')+
    coord_flip()
    
# how to reorder via group


year_term_counts = inaug_tidy %>% 
    extract(document, 'year', '(\\d+)', convert = T) %>% 
    complete(year, term, fill = list(count = 0)) %>% 
    group_by(year) %>% 
    mutate(year_total = sum(count)) %>% 
    ungroup()
year_term_counts %>%
    filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
    ggplot(aes(year, count / year_total)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ term, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    ylab("% frequency of word in inaugural address")

ap_tidy %>% cast_dtm(document = document, term = term, value = count)
ap_tidy %>% cast_dfm(document = document, term = term, value = count)
a = ap_tidy %>% cast_sparse(row = document, column = term)
dim(a)

austen_dtm = austen_books() %>%
    unnest_tokens(output = 'word', input = 'text') %>% 
    mutate(book = as.character(book)) %>% 
    group_by(book, word) %>% 
    count(sort = T) %>% 
    # mutate(n = as.integer(n)) %>% 
    cast_dtm(document = book, term = word, value = n)
    
tm::Corpus
data('acq')
acq[[1]][[1]]
a = acq[[1]]
a$content
a$meta

acq_tidy = tidy(acq)
acq_tidy$text[1]

acq_tokens = acq_tidy %>% 
    unnest_tokens(output = 'word', input = 'text') %>% 
    anti_join(stop_words, by = 'word')
acq_tokens %>% count(word, sort = T)

acq_tokens %>%
    count(id, word) %>%
    bind_tf_idf(word, id, n) %>%
    arrange(desc(tf_idf))

## examine financial articles -------
# install.packages('tm.plugin.webmining')
install.packages('rJava')
library(rJava)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_191')
library(tm.plugin.webmining)
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")
GoogleFinanceSource('A')

download_articles = function(symbol) {
    WebCorpus(YahooFinanceSource(symbol))
}
stock_articles = data_frame(company = company, symbol = symbol) %>% 
    mutate(corpus = map(symbol, download_articles))

stock_articles_tokens = stock_articles %>% 
    unnest(map(corpus, tidy)) %>% 
    unnest_tokens(word, text) %>% 
    select(company, datetimestamp, word, id, heading)

stock_tf_idf = stock_articles_tokens %>%
    count(company, word) %>%
    filter(!str_detect(word, "\\d+")) %>%
    bind_tf_idf(word, company, n) %>%
    arrange(-tf_idf)

x$name = factor(x$name, levels = x$name[order(x$val)])

library(rio)
install.packages('installr')

stock_tf_idf %>% export('stock_tf_idf.xlsx')
stock_tf_idf %>%
    group_by(company) %>% 
    arrange(desc(tf_idf)) %>%
    top_n(8) %>% 
    ungroup() %>%
    # mutate(word = fct_reorder2(word, company, tf_idf, .desc = F)) %>%
    ggplot(aes(reorder(word, tf_idf), tf_idf, fill = company)) +
    geom_col(position = 'identity') +
    facet_wrap(~company, scales = 'free') +
    ylab('')+
    coord_flip()
stock_tf_idf[['word']]
stock_tf_idf %>%
    group_by(company) %>%
    arrange(desc(tf_idf)) %>%
    top_n(8) %>% 
    ungroup() %>%
    # mutate(word = reorder(word, tf_idf, FUN = min)) %>%
    # nest() %>% 
    ggplot(aes(reorder(word, tf_idf, FUN = min), tf_idf, fill = company)) +
    geom_col(position = 'identity') +
    facet_wrap(~company, scales = 'free') +
    ylab('')+
    coord_flip()



stock_articles_tokens %>%
    anti_join(stop_words, by = "word") %>%
    count(word, id, sort = TRUE) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(contribution = sum(n * score)) %>%
    top_n(12, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution)) +
    geom_col() +
    coord_flip() +
    labs(y = "Frequency of word * AFINN score")

df = data_frame(gr = c(rep('foo',3), rep('bar',3), rep('cron',3)),
                term = c('c','b','a', 'd', 'a','e', 'f', 'g', 'h'),
                val = c(7,3,4,1.5,1,3,1,2,4))

df %>%
    group_by(gr) %>%
    arrange(desc(val)) %>%
    ungroup() %>%
    mutate(term = reorder(term, val)) %>%
    ggplot(aes(term, val, fill = gr)) +
    geom_col(position = 'identity') +
    facet_wrap(~gr, scales = 'free') +
    ylab('')+
    coord_flip()

ggplot(data = df, aes(reorder(term, val, FUN = min), val)) +
    geom_col() +
    facet_wrap(~ gr, scales = 'free') +
    coord_flip()

stock_articles_tokens %>%
    count(word) %>%
    inner_join(get_sentiments("loughran"), by = "word") %>%
    group_by(sentiment) %>%
    top_n(5, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ sentiment, scales = "free") +
    ylab("Frequency of this word in the recent financial articles")

stock_sentiment_count = stock_articles_tokens %>% 
    inner_join(get_sentiments('loughran')) %>% 
    count(sentiment, company) %>% 
    spread(sentiment, n, fill = 0)

stock_sentiment_count %>%
    mutate(score = (positive - negative) / (positive + negative)) %>%
    mutate(company = reorder(company, score)) %>%
    ggplot(aes(company, score, fill = score > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = "Company",
         y = "Positivity score among 20 recent news articles")

# topic modeling -----
data("AssociatedPress")
AssociatedPress

library(topicmodels)

ap_lda = LDA(AssociatedPress, k = 2, control = list(seed = 1234))
str(ap_lda)
ap_topics = tidy(ap_lda, matrix = 'beta')

ap_topics %>% filter(topic == 1) %>% arrange(-beta)

ap_top_terms = ap_topics %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)

ap_top_terms %>% 
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = 'free')+
    coord_flip()
    
beta_spread = ap_topics %>% 
    mutate(topic = str_c('topic_', topic)) %>% 
    spread(topic, beta) %>% 
    filter(topic_1 > 0.001 | topic_2 > 0.001) %>% 
    mutate(log_ratio = log(topic_1 / topic_2))

summary(beta_spread$log_ratio)
beta_spread %>% filter(log_ratio > 1) %>% arrange(-log_ratio)
beta_spread %>% filter(log_ratio < -1) %>% arrange(log_ratio)

beta_spread %>% 
    top_n(20, abs(log_ratio)) %>% 
    ggplot(aes(reorder(term, log_ratio), log_ratio)) +
    geom_col(position = 'identity') +
    coord_flip()

ap_documents = tidy(ap_lda, matrix = 'gamma')

ap_documents %>% filter(document == 1)

ap_documents_topics = ap_documents %>% 
    group_by(topic) %>% 
    top_n(10, gamma) %>% 
    arrange(-gamma)

tidy(AssociatedPress) %>% 
    filter(document == 6) %>% 
    arrange(-count)


# library heist -----
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")
books = gutenberg_works(title %in% titles) %>% 
    gutenberg_download(meta_fields = 'title')

books_chapters = books %>% 
    group_by(title) %>% 
    mutate(chapter = cumsum(str_detect(text, regex('^chapter ', ignore_case = TRUE)))) %>% 
    ungroup() %>% 
    filter(chapter > 0) %>% 
    unite(document, title, chapter)

text_bag = books_chapters %>% 
    unnest_tokens(output = 'word', input = 'text')

text_bag_count = text_bag %>% 
    anti_join(stop_words) %>% 
    count(document, word, sort = T) %>% 
    ungroup()

# turn text_bag_count to document term matrix
text_bag_count_matrix = text_bag_count %>% 
    cast_dtm(document = document, term = word, value = n)

# text_bag_count_matrix =text_bag_count %>% 
#     cast_sparse(row = document, column = word, value = n)

# remember LDA() word with dtm
chapter_lda = LDA(text_bag_count_matrix, k = 4, control = list(seed = 1234))
chapter_lda_topic_word = tidy(chapter_lda, matrix = 'beta')
chapter_lda_document_topic = tidy(chapter_lda, matrix = 'gamma')

# target: to detect four topic from text bag, which will tell if these four topic coordinate with four original books
# how we might know?? By check if each topic has high concentration of documents from one books (we have document is booknames + chapter to begin with)

chapter_lda_document_topic %>% filter(document == 'Great Expectations_7')

chapter_lda_topic_detect = chapter_lda_document_topic %>% 
    group_by(topic) %>% 
    arrange(-gamma) %>% 
    top_n(20, gamma) %>% 
    ungroup() %>% 
    arrange(topic, -gamma)

# and yes, topic 1 has almost the same with 'Twenty Thousand Leagues under the Sea, and other topic belong to different books

top_terms = chapter_lda_topic_word %>%
    anti_join(stop_words, by = c('term' = 'word')) %>% 
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

chapters_gamma <- chapter_lda_document_topic %>%
    separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma %>%
    mutate(title = reorder(title, gamma * topic)) %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ title)

chapter_classifications <- chapters_gamma %>%
    group_by(title, chapter) %>%
    top_n(1, gamma) %>%
    ungroup()

book_topics <- chapter_classifications %>%
    count(title, topic) %>%
    group_by(title) %>%
    top_n(1, n) %>%
    ungroup() %>%
    transmute(consensus = title, topic)


assignments = augment(chapter_lda, data = text_bag_count_matrix)

assignments = assignments %>% 
    separate(document, c('title', 'chapter'), sep = '_') %>% 
    inner_join(book_topics, by = c('.topic' = 'topic'))

assignments %>% 
    filter(title != consensus) %>% 
    count(title, consensus, term, wt = count) %>% 
    ungroup() %>% 
    arrange(-n)

assignments %>% 
    count(title, consensus) %>% 
    group_by(title) %>% 
    mutate(correct_assignment = n/sum(n)) %>% 
    ggplot(aes(title, consensus, fill = correct_assignment)) +
    geom_tile() +
    scale_fill_gradient2(high = 'royalblue', label = percent_format()) +
    theme_minimal() + 
    labs(y = 'books assigned',
         x = 'original book')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank())
    

assignments %>% filter(term == 'flopson')

# alternative LDA implementation -------
# install.packages('mallet')
library(mallet)
collapsed <- text_bag %>%
    anti_join(stop_words, by = "word") %>%
    mutate(word = str_replace(word, "'", "")) %>%
    group_by(document) %>%
    summarize(text = paste(word, collapse = " "))
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)
mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")
term_counts <- rename(text_bag_count, term = word)
augment(mallet_model, term_counts)


## twitter -------
# install.packages('rtweet')

# whatever name you assigned to your created app
appname <- "twitter_crawl_phucnguyen"

## api key (example below is not a real key)
key <- "QinPczgiz8BCiryPHwJskgv7E"

## api secret (example below is not a real key)
secret <- "aB9jCvCoCRq13zXFySKJEEwlzPI75qU7HLIbFqt3okHU9IdjMK"
access_token = '270797510-7Ovq8hGnDcGG9oigaW10qjvrp0viomy3PP6niQC6'
access_secret = 'IR4SVZHvVhi80PdvCK1zphqaz7qSTAbemRy0EnfLxUAM8  '
    
library(rtweet)

twitter_token = create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret,
    access_token = access_token,
    access_secret = access_secret)

post_tweet("Look, i'm tweeting from R in my #rstats #earthanalytics class! @EarthLabCU")

tweets_julia <- read_csv("data/tweets_julia.csv")
tweets_dave <- read_csv("data/tweets_dave.csv")
remove_reg <- "&amp;|&lt;|&gt;"
require(lubridate)
tweets = tweets_julia %>% mutate(person = 'julia') %>% 
    union_all(tweets_dave %>% mutate(person = 'dave')) %>% 
    mutate(timestamp = ymd_hms(timestamp))

tweets %>% 
    ggplot(aes(timestamp, fill = person)) +
    geom_histogram(position = 'identity', bins = 20, show.legend = F) +
    facet_wrap(~person, ncol = 1)
table(!is.na(tweets$retweeted_status_id))
tidy_tweets  = tweets %>% 
    unnest_tokens('word', 'text', token = 'tweets') %>% 
    filter(is.na(retweeted_status_id),  str_detect(word, "[a-z]")) %>% 
    anti_join(stop_words) %>% 
    anti_join(stop_words %>% mutate(word = str_replace_all(word, "'", ""))) %>% 
    select(tweet_id, timestamp, person, word)

frequency = tidy_tweets  %>% 
    group_by(person) %>% 
    count(word, sort = TRUE) %>% 
    left_join(tidy_tweets %>% 
                  group_by(person) %>% 
                  summarise(total = n())) %>%
    mutate(freq = n/total)

frequency <- frequency %>% 
    select(person, word, freq) %>% 
    spread(person, freq) %>%
    arrange(julia, dave)

ggplot(frequency, aes(julia, dave)) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    geom_smooth(color = "red")

tidy_tweets <- tidy_tweets %>%
    filter(timestamp >= as.Date("2016-01-01"),
           timestamp < as.Date("2017-01-01"))

word_ratios = tidy_tweets %>% 
    filter(!str_detect(word, '^@') & !str_detect(word, '^#')) %>% 
    count(person, word) %>% 
    arrange(-n) %>% 
    group_by(word) %>% 
    filter(sum(n) >= 10) %>% 
    ungroup() %>% 
    spread(key = 'person', value = 'n') %>% 
    group_by() %>% 
    mutate_at(c('julia' , 'dave'), function(x) ifelse(is.na(x), 0 , x)) %>% 
    mutate_at(c('julia' , 'dave'), function(x) (x+1) / (sum(x)+1)) %>% 
    mutate(logratio = log(dave / julia)) %>% 
    arrange(-logratio)

word_ratios %>% 
    arrange(-abs(logratio))
word_ratios %>% 
    arrange(abs(logratio))
factor(word_ratios$logratio >= 0, labels = c("David", "Julia"))
word_ratios %>%
    group_by(logratio < 0) %>%
    top_n(5, abs(logratio)) %>%
    ungroup() %>%
    mutate(word = reorder(word, logratio)) %>%
    ggplot(aes(word, logratio, fill = logratio < 0)) +
    geom_col(show.legend = TRUE) +
    coord_flip() +
    ylab("log odds ratio (David/Julia)") +
    scale_fill_discrete(name = "people", labels = c("David", "Julia"))

words_by_time <- tidy_tweets %>%
    filter(!str_detect(word, "^@")) %>%
    mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
    count(time_floor, person, word) %>%
    group_by(person, time_floor) %>%
    mutate(time_total = sum(n)) %>%
    group_by(person, word) %>%
    mutate(word_total = sum(n)) %>%
    ungroup() %>%
    rename(count = n) %>%
    filter(word_total > 30)

nested_data = words_by_time %>% 
    nest(-person, -word)

library(purrr)

nested_model = nested_data %>% 
    mutate(models = map(data, ~glm(cbind(count, time_total) ~ time_floor, data = ., family = 'binomial')))
cbind(words_by_time$count, words_by_time$time_total)
# this is very creative, because fitting probability of word over all time count, versus timestamp, 
# will return model to check relationship of timestamp and word appearance probability

library(broom)

slopes = nested_model %>% 
    unnest(map(models, tidy)) %>% 
    filter(term == 'time_floor') %>% 
    mutate(adjusted.p.value = p.adjust(p.value))
top_slopes <- slopes %>% 
    filter(adjusted.p.value < 0.05) # significant statistically
#somehow this will get most important slope

words_by_time %>%
    inner_join(top_slopes, by = c("word", "person")) %>%
    filter(person == "dave") %>% 
    ggplot(aes(time_floor, count/time_total, color = word)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")

words_by_time %>%
    inner_join(top_slopes, by = c("word", "person")) %>%
    filter(person == "julia") %>%
    ggplot(aes(time_floor, count/time_total, color = word)) +
    geom_line(size = 1.3) +
    labs(x = NULL, y = "Word frequency")

tweets_julia <- read_csv("data/juliasilge_tweets.csv")
tweets_dave <- read_csv("data/drob_tweets.csv")
tweets <- bind_rows(tweets_julia %>% 
                        mutate(person = "Julia"),
                    tweets_dave %>% 
                        mutate(person = "David")) %>%
    mutate(created_at = ymd_hms(created_at))

tidy_tweets <- tweets %>% 
    filter(!str_detect(text, "^(RT|@)")) %>%
    mutate(text = str_replace_all(text, remove_reg, '')) %>%
    unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_replace_all(stop_words$word, "'", ""))

totals <- tidy_tweets %>% 
    group_by(person, id) %>% 
    summarise(rts = first(retweets)) %>% 
    group_by(person) %>% 
    summarise(total_rts = sum(rts))


word_by_rts <- tidy_tweets %>% 
    group_by(id, word, person) %>% 
    summarise(rts = first(retweets)) %>% 
    group_by(person, word) %>% 
    summarise(retweets = median(rts), uses = n()) %>%
    left_join(totals) %>%
    filter(retweets != 0) %>%
    ungroup()

word_by_rts %>% 
    filter(uses >= 5) %>%
    arrange(desc(retweets))

word_by_rts %>%
    filter(uses >= 5) %>%
    group_by(person) %>%
    top_n(10, retweets) %>%
    arrange(retweets) %>%
    ungroup() %>%
    mutate(word = factor(word, unique(word))) %>%
    ungroup() %>%
    ggplot(aes(word, retweets, fill = person)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ person, scales = "free", ncol = 2) +
    coord_flip() +
    labs(x = NULL, 
         y = "Median # of retweets for tweets containing each word")

totals <- tidy_tweets %>% 
    group_by(person, id) %>% 
    summarise(favs = first(favorites)) %>% 
    group_by(person) %>% 
    summarise(total_favs = sum(favs))

word_by_favs <- tidy_tweets %>% 
    group_by(id, word, person) %>% 
    summarise(favs = first(favorites)) %>% 
    group_by(person, word) %>% 
    summarise(favorites = median(favs), uses = n()) %>%
    left_join(totals) %>%
    filter(favorites != 0) %>%
    ungroup()

word_by_favs %>%
    filter(uses >= 5) %>%
    group_by(person) %>%
    top_n(10, favorites) %>%
    arrange(favorites) %>%
    ungroup() %>%
    mutate(word = factor(word, unique(word))) %>%
    ungroup() %>%
    ggplot(aes(word, favorites, fill = person)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ person, scales = "free", ncol = 2) +
    coord_flip() +
    labs(x = NULL, 
         y = "Median # of favorites for tweets containing each word")

# NASA metadata ------
require(jsonlite)
metadata = fromJSON('https://data.nasa.gov/data.json')
names(metadata$dataset)
metadata$dataset[18] %>% as.tibble()


class(metadata$dataset$title)
head(metadata$dataset)
nasa_title = tibble(identifier = metadata$dataset$identifier, 
                         keyword = metadata$dataset$keyword) %>% 
    filter(!map_lgl(keyword, is.null)) %>%
    unnest(keyword)
nasa_title %>% 
    mutate(keyword = keyword %>% paste(sep = ',')) %>%
    mutate(keyword = keyword %>% str_replace_all('c\\(', ''))

nasa_title %>% mutate(kw = unlist(keyword, recursive = FALSE))
nasa_title$kw = lapply(nasa_title$keyword, FUN = function(x) unlist(un_list(x)))
nasa_title %>% mutate(kw2 = kw[[1]])
lapply(nasa_title$keyword, FUN = function(x) unlist(unlist(un_list(x))))
nasa_title$kw[2]
df <- tibble::tibble(x = as.list(letters[1:3]))
df$x
unnest(df)

df2 = tibble(keyword = metadata$dataset$keyword) %>% 
    filter(!map_lgl(keyword, is.null)) %>% 
    unnest(keyword) 
unnest(df2)
df2$keyword[1:3]

un_list = function(l) {
    unlist(l, recursive = F) %>% paste(collapse = ',')
}
un_list(list(1,2,3))
unlist(list(1,2,3)) 
unnest(nasa_title, keyword)
rlang::last_error()
    # unnest(keyword)

# can not word due to unnest only able to handle list of list from tidyr 0.8.9
## proved wrong, error due to few observation has list of NULL 
unlist(nasa_title$keyword[2])


nasa_keyword = tibble(id = metadata$dataset$identifier, 
                    keyword = metadata$dataset$keyword) %>% 
    filter(!map_lgl(keyword, is.null)) %>%
    unnest(keyword)

nasa_keyword = nasa_keyword %>% 
    unnest_tokens(word, keyword) %>% 
    anti_join(stop_words)

nasa_desc = data_frame(id = metadata$dataset$identifier, 
                        desc = metadata$dataset$description)
my_stopwords <- data_frame(word = c(as.character(1:10), 
                                    "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                    "v003", "v004", "v005", "v006", "v7"))
nasa_desc <- nasa_desc %>% 
    unnest_tokens(word, desc) %>% 
    anti_join(stop_words)
    anti_join(my_stopwords)
    
title_word_pairs <- nasa_title %>% 
    pairwise_count(word, identifier, sort = TRUE, upper = FALSE)

desc_word_pairs <- nasa_desc %>% 
    pairwise_count(word, id, sort = TRUE, upper = FALSE)

library(ggplot2)
library(igraph)
library(ggraph)

title_word_pairs %>%
    filter(n >= 250) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines")) +
    theme_void()

desc_word_pairs %>%
    filter(n >= 1000) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE,
                   point.padding = unit(0.2, "lines")) +
    theme_void()

keyword_pairs <- nasa_keyword %>% 
    pairwise_count(word, identifier, sort = TRUE, upper = FALSE)

keyword_pairs %>%
    filter(n >= 300) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE,
                   point.padding = unit(0.2, "lines")) +
    theme_void()


desc_tf_idf <- nasa_desc %>% 
    count(id, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(word, id, n)

desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword %>% rename(keyword = word), by = "id")


nasa_keyword %>% count(word, sort = T)

desc_tf_idf %>% 
    filter(!near(tf, 1)) %>%
    filter(keyword %in% c("completed", "center", 
                          "space", "research",
                          "flight", "goddard")) %>%
    arrange(desc(tf_idf)) %>%
    group_by(keyword) %>%
    distinct(word, keyword, .keep_all = TRUE) %>%
    top_n(15, tf_idf) %>% 
    ungroup() %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    ggplot(aes(word, tf_idf, fill = keyword)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~keyword, ncol = 3, scales = "free") +
    coord_flip() +
    labs(title = "Highest tf-idf words in NASA metadata description fields",
         caption = "NASA metadata from https://data.nasa.gov/data.json",
         x = NULL, y = "tf-idf")


# usenet text -----
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

training_folder = 'data/20news-bydate/20news-bydate-train'

read_folder <- function(infolder) {
    data_frame(file = dir(infolder, full.names = TRUE)) %>%
        mutate(text = map(file, read_lines)) %>%
        transmute(id = basename(file), text) %>%
        unnest(text)
}

raw_text <- data_frame(folder = dir(training_folder, full.names = TRUE)) %>%
    unnest(map(folder, read_folder)) %>%
    transmute(newsgroup = basename(folder), id, text)

raw_text %>% 
    group_by(newsgroup) %>% 
    summarise(messages = n_distinct(id)) %>% 
    ggplot(aes(newsgroup, messages)) +
    geom_col() +
    coord_flip()

cleaned_text = raw_text %>% 
    group_by(newsgroup, id) %>% 
    filter(cumsum(text == '') > 0,
           cumsum(str_detect(text, '^--') == 0)) %>% 
    ungroup()

cleaned_text <- cleaned_text %>%
    filter(str_detect(text, "^[^>]+[A-Za-z\\d]"),  text != "",
           !str_detect(text, "writes(:|\\.\\.\\.)$"),
           !str_detect(text, "^In article <"),
           !id %in% c(9704, 9985))

usenet_words <- cleaned_text %>%
    unnest_tokens(word, text) %>%
    filter(str_detect(word, "[a-z']$"),
           !word %in% stop_words$word)

words_by_newsgroup <- usenet_words %>%
    count(newsgroup, word, sort = TRUE) %>%
    ungroup()

tf_idf <- words_by_newsgroup %>%
    bind_tf_idf(word, newsgroup, n) %>%
    arrange(desc(tf_idf))

tf_idf %>%
    filter(str_detect(newsgroup, "^sci\\.")) %>%
    group_by(newsgroup) %>%
    top_n(12, tf_idf) %>%
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = newsgroup)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ newsgroup, scales = "free") +
    ylab("tf-idf") +
    coord_flip()

newsgroup_cors <- words_by_newsgroup %>%
    pairwise_cor(newsgroup, word, n, sort = TRUE)

library(ggraph)
library(igraph)
set.seed(2017)

newsgroup_cors %>%
    filter(correlation > .4) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(alpha = correlation, width = correlation)) +
    geom_node_point(size = 6, color = "lightblue") +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()

word_sci_newsgroups = usenet_words %>% 
    filter(str_detect(newsgroup, '^sci')) %>% 
    group_by(word) %>% 
    mutate(word_total = n()) %>% 
    ungroup() %>% 
    filter(word_total > 50)

install.packages('quanteda')
install.packages('tm')
require(quanteda)
require(tm)
sci_dtm = word_sci_newsgroups %>% 
    unite(document, newsgroup, id) %>% 
    count(document, word) %>%
    cast_dtm(document, word, word_total)

install.packages('topicmodels')
require(topicmodels)
sci_lda = LDA(sci_dtm, k = 4, control = list(seed = 2016))

sci_lda %>%
    tidy() %>%
    group_by(topic) %>%
    top_n(8, beta) %>%
    ungroup() %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip()

sci_lda %>% 
    tidy(matrix = 'gamma') %>% 
    separate(document, c('newsgroup', 'id'), sep = '_') %>% 
    mutate(newsgroup = reorder(newsgroup, gamma*topic)) %>% 
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot()+
    facet_wrap(~newsgroup) +
    labs(x = 'topic', y = 'message count higest %')
    
newsgroup_sentiments <- words_by_newsgroup %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(newsgroup) %>%
    summarize(score = sum(score * n) / sum(n))

newsgroup_sentiments %>%
    mutate(newsgroup = reorder(newsgroup, score)) %>%
    ggplot(aes(newsgroup, score, fill = score > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    ylab("Average sentiment score")

contributions <- usenet_words %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))

contributions %>%
    top_n(25, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip()

top_sentiment_words <- words_by_newsgroup %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    mutate(contribution = score * n / sum(n))

top_sentiment_words %>%
    group_by(newsgroup) %>% 
    top_n(10, abs(contribution)) %>%
    ungroup() %>% 
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~newsgroup, scales = 'free')+
    coord_flip()

sentiment_messages <- usenet_words %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(newsgroup, id) %>%
    summarize(sentiment = mean(score),
              words = n()) %>%
    ungroup() %>%
    filter(words >= 5)

sentiment_messages %>%
    arrange(desc(sentiment))

print_message <- function(group, message_id) {
    result <- cleaned_text %>%
        filter(newsgroup == group, id == message_id, text != "")
    
    cat(result$text, sep = "\n")
}

print_message("rec.sport.hockey", 53560)
print_message("rec.sport.hockey", 53907)

usenet_bigrams = cleaned_text %>% 
    unnest_tokens(bigram, text, token = 'ngrams', n = 2)

usenet_bigram_counts <- usenet_bigrams %>%
    count(newsgroup, bigram, sort = TRUE) %>%
    ungroup() %>%
    separate(bigram, c("word1", "word2"), sep = " ")

negate_words <- c("not", "without", "no", "can't", "don't", "won't")

usenet_bigram_counts %>%
    filter(word1 %in% negate_words) %>%
    count(word1, word2, wt = n, sort = TRUE) %>%
    inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
    mutate(contribution = score * nn) %>%
    group_by(word1) %>%
    top_n(10, abs(contribution)) %>%
    ungroup() %>%
    mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
    ggplot(aes(word2, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ word1, scales = "free", nrow = 3) +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    xlab("Words preceded by a negation") +
    ylab("Sentiment score * # of occurrences") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_flip()
