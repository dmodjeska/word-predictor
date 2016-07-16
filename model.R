library(data.table)

doPlots <- FALSE
debug <- TRUE

max_ngram_n <- 4
max_percent <- 80
sample_percent <- 80

# GET DATA --------------------------------------------------------------------

# download and unzip data file that contains 3 language corpora
data_zip_file <- "Coursera-SwiftKey.zip"
if (!file.exists(data_zip_file)) {
  data_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(data_url, data_zip_file)
  unzip(data_zip_file)
}

# set paths to English-language corpora and working files
eng_dir <- "final/en_US"
twit_path <- paste(eng_dir, "en_US.twitter.txt", sep = "/")
news_path <- paste(eng_dir, "en_US.news.txt", sep = "/")
blogs_path <- paste(eng_dir, "en_US.blogs.txt", sep = "/")
plots_dir <- "plots"

# helper function to create files: a corpus sample, dev test, and final test
sample_corpus <- function(genre, full_path = NULL) {
  replacement <- sprintf("\\.%dsample\\.txt", sample_percent)
  sample_path <- gsub("\\.txt", replacement, full_path)

  replacement <- sprintf("\\.dev_test\\.%dsample\\.txt", sample_percent)
  dev_test_path <- gsub(".txt", replacement, sample_path) # FIX

  if (!file.exists(sample_path) & !is.null(full_path)) {
    if (!file.exists(full_path))
      stop(sprintf("File not found: %s", full_path))

    con <- file(full_path, "r")
    read_text <- readLines(con)
    close(con)

    num_lines <- length(read_text)
    # num_words <- stri_stats_latex(read_text)[4]
    # max_line_chars <- max(nchar(read_text))
    # min_line_chars <- min(nchar(read_text))

    # sample % of file
    ratio <- sample_percent / 100
    sample_pattern <- rbinom(num_lines, 1, ratio)
    sample_text <- read_text[sample_pattern == 1]

    # create dev and final test files from unsampled portion of corpus
    unsample_text <- read_text[sample_pattern == 0]
    if (sample_percent == max_percent) {
        dev_test_text <- unsample_text
    } else {
        num_lines <- length(unsample_text)
        ratio <- (sample_percent / ((100 - sample_percent) / 100)) / 100
        sample_pattern <- rbinom(num_lines, 1, ratio)
        dev_test_text <- unsample_text[sample_pattern == 1]
    }

    # write sample file
    con <- file(sample_path, "w")
    writeLines(sample_text, con)
    close(con)

    # write dev test file
    con <- file(dev_test_path, "w")
    writeLines(dev_test_text, con)
    close(con)
  }

  return(sample_path)
}

# CLEAN TEXTS -----------------------------------------------------

# load a NLP file
load_a_file <- function(path = NULL) {
  if (!is.null(path)) {
    if (!file.exists(path))
      stop(sprintf("File not found: %s", path))

    con <- file(path)
    text <- readLines(con)
    close(con)

    return(text)
  }
}

# clean a loaded text
source("clean_words.R")
clean_text <- function(sample_path = NULL) {
  clean_path <- gsub("sample", "clean", sample_path)
  if (!file.exists(clean_path)) {

    # load and clean file
    text <- load_a_file(sample_path)
    text_clean <- do_clean_text(text)

    # write cleaned file
    con <- file(clean_path, "w")
    writeLines(text_clean, con)
    close(con)
  }

  return(clean_path)
}

# CREATE CORPORA -----------------------------------------------------

library(quanteda)

# helper function to make dfms for ngrams
make_word_tokens <- function(clean_path = NULL) {
  tokens_path <- gsub("clean\\.txt", "tokens\\.rds", clean_path)
  if (!file.exists(tokens_path) & !is.null(clean_path)) {
    text <- load_a_file(clean_path)

    # create corpus and tokenize it as sentences
    corpus <- corpus(text)
    if (debug)
      summary <- summary(corpus, n = 5, showmeta = TRUE)

    # tokenize sentences
    sentence_tokens <- tokenize(corpus, what = "sentence", simplify = TRUE)
    sentence_tokens_lc <- toLower(sentence_tokens)
    num_sentences <- length(sentence_tokens_lc)

    # tokenize words
    word_tokens <- tokenize(sentence_tokens_lc, what = "word",
                            removePunct = TRUE, removeSeparators = TRUE,
                            removeTwitter = TRUE, removeURL = FALSE,
                            simplify = FALSE)

    attr(word_tokens, "num_sentences") <- num_sentences
    saveRDS(word_tokens, tokens_path)
  }

  return(tokens_path)
}

make_dfms <- function(tokens_path = NULL, include_dfms = TRUE) {
  dfms_path <- gsub("tokens", "dfms", tokens_path)
  if (!file.exists(dfms_path) & !is.null(tokens_path)) {
    if (!file.exists(tokens_path))
      stop(sprintf("File not found: %s", tokens_path))

    tokens_words <- readRDS(tokens_path)
    ngrams_list <- vector("list", max_ngram_n)

    # 1-grams
    ngrams_list[[1]] <- tokens_words
    attr(ngrams_list[[1]], "num_sentences") <-
      attr(tokens_words, "num_sentences")

    # n-grams 2+
    # 1st insert pair of markers before and after each sentence, cumulatively
    for (i in 2:max_ngram_n) {
      for (j in 1:length(tokens_words))
        tokens_words[[j]] <- c(bos_mark, tokens_words[[j]], eos_mark)
      ngrams_list[[i]] <- ngrams(tokens_words, n = i, concatenator = " ")
    }
    ngrams_path <- gsub("tokens", "ngrams", tokens_path)
    saveRDS(ngrams_list, ngrams_path)

    # make DFM's
    if (include_dfms) {
        dfms <- lapply(ngrams_list, dfm)
        attr(dfms, "num_sentences") <- attr(ngrams_list[[1]], "num_sentences")
        saveRDS(dfms, dfms_path)
    }
  }
  return(dfms_path)
}

# MAKE FREQUENCY TABLES ------------------------------------------------------

# get English-language lexicon to serve as reference vocabulary
data(GradyAugmented)
grady_lower <- toLower(GradyAugmented)

# from https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html
library(RColorBrewer)

# helper function to display exploratory tables and plots for ngrams n
do_a_freq_table <- function(genre, n, dfm) {
  title = sprintf("%s: %d-grams", genre, n)

  # frequency tables
  num_unique_terms <- length(dfm[2])
  term_freq <- topfeatures(dfm, n = num_unique_terms, decreasing = TRUE)
  orig_term_freq <- term_freq[3:num_unique_terms] # skip markers

  # frequency plots
  if (doPlots & n == 1) {
    print(head(orig_term_freq, 100))

    orig_dfm <- removeFeatures(dfm, c("u1d179", "u1d17a"), valuetype = "regex")

    scale <- list(c(8, 0.5), c(4, 0.4), c(3, 0.3), c(3, 0.3))
    plot(orig_dfm, max.words = 100, colors = brewer.pal(6, "Dark2"),
         scale = scale[[n]], main = title)

    plot(orig_term_freq, log = "x", cex = .6, main = title,
         xlab = "Log Term Rank", ylab = "Term Trequency") # skip markers
  }

  # importance of "short head" of term distribution
  if (debug) {
    num_word_instances <- sum(orig_term_freq)
    cum_sum_term_freq <- cumsum(orig_term_freq)
    rank_threshold_50 <- which.max(cum_sum_term_freq / num_word_instances >= 0.5)
    print(sprintf("%dgram: number of unique terms covering 50%% of total word instances: %d out of %d",
                  n, rank_threshold_50, num_unique_terms))
    rank_threshold_90 <- which.max(cum_sum_term_freq / num_word_instances >= 0.9)
    print(sprintf("%dgram: number of unique terms covering 90%% of total word instances: %d out of %d",
                  n, rank_threshold_90, num_unique_terms))
  }

  # cumlative term frequency out of total word instances
  if (doPlots & n == 1) {
    plot(cum_sum_term_freq / num_word_instances, log = "x", cex = .6,
         main = title, xlab = "Log Term Rank",
         ylab = "Cumulative % of Word Instances",
         xlim = c(1, 1000000), ylim = c(0, 1))
    abline(v = rank_threshold_90, col = "red")
  }

  # out-of-vocabulary terms
  if (debug & doPlots & n == 1) {
    oov_terms <- names(term_freq)[!names(term_freq) %in% grady_lower]
    hi_rank_term_freq <- term_freq[1:rank_threshold_90]
    hi_rank_oov_terms <- oov_terms[oov_terms %in% names(hi_rank_term_freq)]
    print(sprintf("%d high-rank OOV terms out of %d total OOV terms",
                  length(hi_rank_oov_terms), length(oov_terms)))
    print(head(hi_rank_oov_terms, 100))
  }

  return(term_freq)
}

# perform Good-Turing smoothing (set attribute of c(N0_r, N0_N) )
smooth_freq_tab <- function(freqs, n) {

    # tabulate the number of ngrams N having each instance count r
    # assume: freqs are already sorted by r = instance count (decreasing)
    GT_table <- table(freqs)
    GT_2 <- data.table(r = as.integer(names(GT_table)), N = unname(GT_table))
    GT_2b <- GT_2[order(-r)]
    GT_2c <- GT_2b[ , N.V1 := NULL]
    names(GT_2c)[2] <- "N"

    # prep vectors for smoothing
    GT_3 <- GT_2c
    nrow_GT_3 <- nrow(GT_3)
    this_r <- GT_3[2:(nrow_GT_3)]$r
    this_N <- GT_3[2:(nrow_GT_3)]$N
    prev_N <- GT_3[1:(nrow_GT_3 - 1)]$N

    # apply Good-Turing smoothing to all ngrams but first and last
    GT_4 <- data.table(r = (this_r + 1) * (prev_N / this_N), N = this_N)

    # add first ngram, not touched by smoothing
    GT_5 <- rbind(GT_3[1], GT_4)

    # take inverse of table() function
    GT_6 <- rep(GT_5$r, GT_5$N)

    # calc N0_r
    num_ngrams <- sum(GT_table)
    last_GT_5_elem <- GT_5[nrow(GT_5)]
    N0_r <- 1 * (last_GT_5_elem$N / num_ngrams) # Good-Turing

    # prep smoothed freqs for returning - sort? where was UNK in freqs
    names(GT_6) <- names(freqs)
    smoothed_freqs_sort <- sort(GT_6, decreasing = TRUE)
    attr(smoothed_freqs_sort, "unseen") <- c(N0_r, num_ngrams)

    return(smoothed_freqs_sort)
}

# helper function to display exploratory tables and plots for a corpus
make_simple_freq_tables <- function(genre, dfms_path = NULL) {
    simple_freqs_path <- gsub("dfms", "simple.freqs", dfms_path)
    if (!file.exists(simple_freqs_path) & !is.null(dfms_path)) {
        if (!file.exists(dfms_path))
            stop(sprintf("File not found: %s", dfms_path))
        dfms <- readRDS(dfms_path)

        # create a simple frequency table
        term_freqs <- vector("list", max_ngram_n)
        for (i in 1:max_ngram_n)
          term_freqs[[i]] <- do_a_freq_table(genre, i, dfms[[i]])

        # save simple frequency tables
        attr(term_freqs, "num_sentences") <- attr(dfms, "num_sentences")
        saveRDS(term_freqs, simple_freqs_path)
    }

    return(simple_freqs_path)
}

# ASSUME: each simple freqs table is already sorted by frequency decreasing
make_freq_tables <- function(simple_freqs_path = NULL) {
    freqs_path <- gsub("simple.freqs", "freqs", simple_freqs_path)
    if (!file.exists(freqs_path) & !is.null(simple_freqs_path)) {
        if (!file.exists(simple_freqs_path))
            stop(sprintf("File not found: %s", simple_freqs_path))
        term_freqs <- readRDS(simple_freqs_path)

        # apply Good-Turing smoothing
        term_freqs_smooth <- vector("list", max_ngram_n)
        for (i in 1:max_ngram_n)
            term_freqs_smooth[[i]] <- smooth_freq_tab(term_freqs[[i]], i)

        # remove singleton ngrams 2+
        single_r_tab <- c(0, 0, 0, 0)
        term_freq_multi <- vector("list", max_ngram_n)
        term_freq_multi[[1]] <- term_freqs_smooth[[1]]
        for (i in 2:max_ngram_n) {
            single_r_tab[[i]] <- min(term_freqs_smooth[[i]])
            term_freq_multi[[i]] <- # cutoff is singleton r at end of smoothed freqs
                term_freqs_smooth[[i]][term_freqs_smooth[[i]] > single_r_tab[[i]]]

            # account for probability of removed singletons
            single_indexes <- which(term_freqs_smooth[[i]] <= single_r_tab[[i]])
            single_r <- single_r_tab[[i]]
            single_N <- length(single_indexes)
            attr(term_freq_multi[[i]], "single_unseen") <- c(single_r, single_N)
        }

        # convert named integer array to 2-column data table and sort it
        term_freq_dt_sort <- vector("list", max_ngram_n)
        for (i in 1:max_ngram_n) {
            term_freq_dt <- data.table(name = names(term_freq_multi[[i]]),
                                       count = term_freq_multi[[i]])
            term_freq_dt_sort[[i]] <- setkey(term_freq_dt, name)

            # adjust unseen info
            attr(term_freq_dt_sort[[i]], "unseen") <-
                attr(term_freqs_smooth[[i]], "unseen")
            if (i > 1) {
                unseen <- attr(term_freqs_smooth[[i]], "unseen")
                single_unseen <- attr(term_freq_multi[[i]], "single_unseen")
                new_unseen_N <- unseen[2] + single_unseen[2]
                new_unseen_mass <- (unseen[1] * unseen[2]) +
                    (single_unseen[1] * single_unseen[2])
                new_unseen_r <- new_unseen_mass / new_unseen_N
                attr(term_freq_dt_sort[[i]], "unseen") <- c(new_unseen_r,
                                                            new_unseen_N)
            }
        }

        attr(term_freq_dt_sort, "num_sentences") <- attr(term_freqs, "num_sentences")
        saveRDS(term_freq_dt_sort, freqs_path)
    }

  return(freqs_path)
}

# BUILD SIMPLE MODELS --------------------------------------------------------

# helper function to create an ngram where n >= 2
do_a_simple_model <- function(n, term_freqs) {
  num_sentences <- attr(term_freqs, "num_sentences")
  tf_n_1 <- term_freqs[[n - 1]]

  tf_n <- term_freqs[[n]]

  # construct marker ngrams for prefix searching in downlevel ngrams
  extra_bos_entry <- data.table(name = bos_mark, count = num_sentences)
  i <- n - 2
  while (i > 0)  {
    extra_bos_entry$name <- paste0(extra_bos_entry$name, " ", bos_mark)
    i <- i - 1
  }
  tf_n_1_search <- rbind(extra_bos_entry, tf_n_1)

  tf_n_prefixes <- gsub(" [^ ]+$", "", tf_n$name, perl = TRUE)
  tf_n_indices <- match(tf_n_prefixes, tf_n_1_search$name)
  model <- data.table(name = tf_n$name,
                      prob = tf_n$count / tf_n_1_search$count[tf_n_indices])

  # print current model level sum of probabilities relative to level n - 1
  # add 1 to level n - 1 to account for BOS marker added to this level
  if (debug) {
      unseen <- attr(tf_n, "unseen")
      unseen_1 <- attr(tf_n_1, "unseen")

      seen_prob <- sum(model$prob)

      prefix_prob_table <- data.table(name = tf_n_prefixes, prob = model$prob)
      group_prefix_probs <- prefix_prob_table[ , list(sum = sum(prob)), by = name]

      childless_prob <- length(tf_n_1$name[!(tf_n_1$name %in% tf_n_prefixes)])
      unseen_prob <- unseen_1[2] + # each unseen unigram leads only to unseen bigrams
          sum(1 - group_prefix_probs$sum) + # per seen prefix, unseen suffix probability
          childless_prob # add 1 for each seen childless parent ngram

      numerator <-  seen_prob + unseen_prob
      denominator <- nrow(tf_n_1) + unseen_1[2]
      print(sprintf("sum of model %d probabilities = %f",
                    n,  (numerator / denominator)))
  }

  return(model)
}

create_simple_models <- function(freqs_path = NULL) {
  models_path <- gsub("freqs", "models", freqs_path)
  if (!file.exists(models_path) & !is.null(freqs_path)) {
    if (!file.exists(freqs_path))
      stop(sprintf("File not found: %s", freqs_path))
    term_freqs <- readRDS(freqs_path)
    models_n <- vector("list", max_ngram_n)

    # 1-gram model
    tf1 <- term_freqs[[1]]
    unseen <- attr(tf1, "unseen")
    num_1gram_instances <- sum(tf1$count) + (unseen[1] * unseen[2])
    models_n[[1]] <- data.table(name = tf1$name,
                                prob = tf1$count / num_1gram_instances)
    unseen_prob <- (unseen[1]*unseen[2]) / num_1gram_instances
    prob_total <- sum(models_n[[1]]$prob) + unseen_prob
    print(sprintf("sum of model 1 probabilities = %f",
                  prob_total))

    # ngram models 2+
    for (i in 2:max_ngram_n) {
      models_n[[i]] <- do_a_simple_model(i, term_freqs)
    }

    # sort models
    models_sort <- vector("list", max_ngram_n)
    for (i in 1:max_ngram_n) {
      models_sort[[i]] <- setkey(models_n[[i]], name)
      attr(models_sort[[i]], "unseen") <- attr(term_freqs[[i]], "unseen")
    }

    attr(models_sort, "num_sentences") <- attr(term_freqs, "num_sentences")
    saveRDS(models_sort, models_path)
  }

  return(models_path)
}

# CREATE TRIE --------------------------------------------------------

# DEBUG: marks sync
create_trie <- function(models_path = NULL) {
  trie_path <- gsub("models", "trie", models_path)
  if (!file.exists(trie_path) & !is.null(models_path)) {

    # load simple models
    if (!file.exists(models_path))
      stop(sprintf("File not found: %s", models_path))
    models0 <- readRDS(models_path)

    # builld prefix counts for ngram levels 2-4, to use on levels 1-3
    # prefixes must be sorted with setkey (not sort), to match the model sort
    prefix_info <- vector("list", max_ngram_n - 1)
    for (i in 1:(max_ngram_n - 1)) {
      prefixes <- gsub(" [^ ]+$", "", models0[[i + 1]]$name)
      prefixes_table <- table(prefixes)
      prefix_counts <- unname(prefixes_table)
      prefix_names <- names(prefixes_table)
      prefix_info[[i]] <- data.table(count = prefix_counts,
                                     name = prefix_names)

      prefix_info[[i]] <- prefix_info[[i]][ , count.V1 := NULL]
      names(prefix_info[[i]])[1] <- "count"
      prefix_info[[i]] <- setkey(prefix_info[[i]], name)
    }


    # insert BOS markers in models for searching on all levels
    models <- vector("list", max_ngram_n)
    for (i in 1:max_ngram_n) {
      bos_elem <- data.table(name = bos_mark, prob = 1)
      j <- i - 1
      while (j > 0) {
        bos_elem$name <- paste0(bos_elem$name, " ", bos_mark)
        j <- j - 1
      }
      models[[i]] <- rbind(bos_elem, models0[[i]])
    }

    # augment prefix counts to match trie nodes with no children
    aug_info_list <- vector("list", max_ngram_n - 1)
    for (i in 1:(max_ngram_n - 1)) {
      model_ngram_count <- nrow(models[[i]])
      aug_prefix_info <- data.table(count = rep(0L, model_ngram_count),
                                    cum = rep(0L, model_ngram_count))

      match_model_indexes <- match(prefix_info[[i]]$name, models[[i]]$name)
      aug_prefix_info[match_model_indexes]$count <- prefix_info[[i]]$count
      aug_prefix_info$cum <- cumsum(aug_prefix_info$count)
      aug_prefix_info[count == 0]$cum = -100L

      aug_info_list[[i]] <- aug_prefix_info
    }

    # add zero'ed child indexes columns for ngram levels 1-3
    trie <- vector("list", max_ngram_n)
    for (i in 1:(max_ngram_n - 1)) {
      index_col <- rep(0L, nrow(models[[i]]))
      trie[[i]] <- cbind(models[[i]], child_start_idx = index_col,
                         child_end_idx = index_col)
    }
    trie[[max_ngram_n]] <- models[[max_ngram_n]]

    # add child node indexes for each term on ngram levels 1-3
    # extra 1 is added to account for inserted BOS markers in model levels
    # positive indexes in trie nodes hold children, negative don't
    for (i in 1:(max_ngram_n - 1)) {
      trie[[i]]$child_start_idx <-
        aug_info_list[[i]]$cum - aug_info_list[[i]]$count + 1L + 1L
      trie[[i]]$child_end_idx <-
        aug_info_list[[i]]$cum + 1L
    }


    # sort 1-grams and set key, and insert EOS mark for keying into
    eos_elem <- data.table(name = eos_mark, prob = 1, child_start_idx = -100L,
                           child_end_idx = -100L)
    trie[[1]] <- rbind(trie[[1]], eos_elem)
    trie[[1]] <- setkey(trie[[1]], name)

    # remove prefixes from names on ngram levels 2-4
    # also replace words in ngrams 2+ with keys into unigrams
    for (i in 2:max_ngram_n) {
        new_trie <- trie[[i]]
        new_trie$name <- gsub("^.+ ", "", trie[[i]]$name)
        new_trie$name_key <- match(new_trie$name, trie[[1]]$name)
        trie[[i]] <- new_trie[, name := NULL]
    }

    # add unseen ngram info and optimize probability storage size
    for (i in 1:max_ngram_n) {
        attr(trie[[i]], "unseen") <- attr(models0[[i]], "unseen")
        trie[[i]]$prob <- as.integer(round(log(trie[[i]]$prob) * 1000000))
    }

    saveRDS(trie, trie_path)
  }

  return(trie_path)
}


# COMBINE GENRES ------------------------------------------------------

# helper function to combine the frequency tables for 3 genres for a given n
# faster than base aggregate() function
do_combo_freqs <- function(twit_freqs, news_freqs, blogs_freqs) {

    # convert each genre vector to a data table
    twit_freqs_dt <- data.table(name = names(twit_freqs), count = twit_freqs)
    news_freqs_dt <- data.table(name = names(news_freqs), count = news_freqs)
    blogs_freqs_dt <- data.table(name = names(blogs_freqs), count = blogs_freqs)

    # concatenate the 3 genre tables
    combo_freqs <- rbind(twit_freqs_dt, news_freqs_dt, blogs_freqs_dt)

    # sort the resulting concatenation using binary sort
    combo_freqs_sort <- setkey(combo_freqs, name)

    # index duplicated/triplicated terms
    dupe_indexes <- which(duplicated(combo_freqs_sort)) # uses 'name' key
    dupe_indexes_lag <- dupe_indexes - 1
    tri_mid_index_flags <- which(dupe_indexes_lag %in% dupe_indexes)
    tri_mid_indexes <- dupe_indexes_lag[tri_mid_index_flags]

    # for middle term of triplcates, add its count to next term and delete it
    combo_freqs_2 <- combo_freqs_sort
    combo_freqs_2[tri_mid_indexes + 1]$count <-
        combo_freqs_2[tri_mid_indexes]$count +
        combo_freqs_2[tri_mid_indexes + 1]$count
    combo_freqs_2[tri_mid_indexes]$count <- 0
    combo_freqs_3 <- combo_freqs_2[count != 0]

    # for first term of duplicates, add its count to next term and delete it
    dupe_indexes <- which(duplicated(combo_freqs_3)) # uses 'name' key
    dupe_indexes_lag <- dupe_indexes - 1
    combo_freqs_3[dupe_indexes]$count <-
        combo_freqs_3[dupe_indexes]$count +
        combo_freqs_3[dupe_indexes_lag]$count
    combo_freqs_3[dupe_indexes_lag]$count <- 0
    combo_freqs_3 <- combo_freqs_3[count != 0]
    combo_sort <- setorder(combo_freqs_3, -count)

    combo_freqs_3_vec <- combo_sort$count
    names(combo_freqs_3_vec) <- combo_sort$name
    return(combo_freqs_3_vec)
}

# for each n, combine frequency tables for all genres
combo_freqs <- function() {
    replacement <- sprintf("combo.%dsimple.freqs.rds", sample_percent)
    simple_freqs_path <- gsub("twitter.txt", replacement, twit_path)

    if (!file.exists(simple_freqs_path)) {
        replacement <- sprintf(".%dsimple.freqs.rds", sample_percent)

        twit_freqs_path <- gsub(".txt", replacement, twit_path)
        if (!file.exists(twit_freqs_path))
            stop(sprintf("File not found: %s", twit_freqs_path))

        news_freqs_path <- gsub(".txt", replacement, news_path)
        if (!file.exists(news_path))
            stop(sprintf("File not found: %s", news_path))

        blogs_freqs_path <- gsub(".txt", replacement, blogs_path)
        if (!file.exists(blogs_path))
            stop(sprintf("File not found: %s", blogs_path))

        twit_freqs <- readRDS(twit_freqs_path)
        news_freqs <- readRDS(news_freqs_path)
        blogs_freqs <- readRDS(blogs_freqs_path)

        num_sentences <- attr(twit_freqs, "num_sentences") +
            attr(news_freqs, "num_sentences") +
            attr(blogs_freqs, "num_sentences")

        combo_freqs <- vector("list", max_ngram_n)
        for (i in 1:max_ngram_n)
            combo_freqs[[i]] <- do_combo_freqs(twit_freqs[[i]],
                                               news_freqs[[i]],
                                               blogs_freqs[[i]])

        attr(combo_freqs, "num_sentences") <- num_sentences
        saveRDS(combo_freqs, simple_freqs_path)
    }

    return(simple_freqs_path)
}

# MANAGE OVERALL MODEL BUILDING ------------------------------------------------

# helper function to handle a data genre end-to-end
handle_genre <- function(genre = NULL, full_path = NULL) {
    sample_path <- sample_corpus(genre, full_path)
    clean_path <- clean_text(sample_path)
    tokens_path <- make_word_tokens(clean_path)
    dfms_path <- make_dfms(tokens_path)
    simple_freqs_path <- make_simple_freq_tables(genre, dfms_path)
    freqs_path <- make_freq_tables(simple_freqs_path)
    models_path <- create_simple_models(freqs_path)
    trie_path <- create_trie(models_path)

    return(trie_path)
}

handle_combo <- function() {
    simple_freqs_path <- combo_freqs()
    freqs_path <- make_freq_tables(simple_freqs_path)
    models_path <- create_simple_models(freqs_path)
    trie_path <- create_trie(models_path)

    return(trie_path)
}

twit_trie_path <- handle_genre("Twitter", full_path = twit_path)
news_trie_path <- handle_genre("news", full_path = news_path)
blogs_trie_path <- handle_genre("blogs", full_path = blogs_path)
combo_trie_path <- handle_combo()
