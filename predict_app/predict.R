trie_path <- "en_US.combo.80trie.rds"

library(data.table)

# INITIALIZE APP -------------------------------------

source("clean_words.R")

# list of language models, one per corpus
num_corpora <- 4
trie_list <- vector("list", num_corpora)

max_ngram_n <- 0

filter_regex <- paste0(bos_mark, "|", eos_mark, "|", url_class, "|",
                       profanity_class, "|", unknown_mark, "|", "5")

# prepare most probable predictable unigrams
unigram_indexes_by_prob_list <- vector("list", num_corpora)

key_to_name <- function(key = 0) { return(trie_list[[cur_trie]][[1]][key]$name) }

# RUN APP ---------------------------

# basic 'find' function
# retrieve list of word indexes (without backoff) with given tokens as history
# indexes are at ngram level one greater than history length
find_word_indexes <- function(tokens, num_words) {
    found_word_indexes <- NULL
    hist_len <- length(tokens)
    child_indexes <- NULL

    # history = 0: find index of most probable predictable unigrams
    if (hist_len == 0) { # search by name
        found_word_indexes <- unigram_indexes_by_prob_list[[cur_trie]][1:num_words]
    }

    # history > 0: find 1-gram (like while loop below, but searching full table)
    else { # search by name
        parent_ngram <- trie_list[[cur_trie]][[1]][name == tokens[[1]]]
        child_indexes <- NULL

        # get indexes of child words at ngram level +1
        if (nrow(parent_ngram) > 0) {
            if (parent_ngram$child_end_idx > 0) # positive values hold children
                child_indexes <- parent_ngram$child_start_idx :
                    parent_ngram$child_end_idx
        }

        # search for ngrams 2+ by key
        i <- 2
        while (length(child_indexes) > 0 & i <= hist_len) {

            # get the target word element
            parent_ngram <-
                trie_list[[cur_trie]][[i]][child_indexes][key_to_name(name_key) == tokens[i]]
            child_indexes <- NULL

            # get indexes of child word elements at ngram level +1
            if (nrow(parent_ngram) > 0) {
                if (parent_ngram$child_end_idx > 0) # positive values hold children
                    child_indexes <- parent_ngram$child_start_idx :
                        parent_ngram$child_end_idx
            }

            i <- i + 1
        }

        if (length(child_indexes > 0))
            found_word_indexes <- child_indexes
    }

    return(found_word_indexes)
}

# helper function to wrap 'find' function with more functionality (no backoff)
do_predict_words <- function(tokens, num_words) {
    the_predicted_words <- NULL
    num_input_tokens <- length(tokens) # includes markers if not backing off
    hist_len <- length(tokens)

    # find matches for history, if any, and pick most probable word in set
    predict_indexes <- find_word_indexes(tokens, num_words)
    if (length(predict_indexes) > 0) {

        # filter out words that shouldn't be predicted & pick best word in set
        if (hist_len == 0) { # search by name
            predict_indexes_filter <-
                predict_indexes[!grepl(filter_regex,
                                       trie_list[[cur_trie
                                                  ]][[hist_len + 1]][predict_indexes]$name)]
        } else { # search by key
        predict_indexes_filter <-
            predict_indexes[!grepl(filter_regex,
                                   key_to_name(trie_list[[cur_trie]][[hist_len + 1
                                                               ]][predict_indexes]$name_key))]
        }

        if (length(predict_indexes_filter > 0)) {

            # sort candidates by max probabilities in this level's ngram
            prob_matches_sort <-
              trie_list[[cur_trie]][[hist_len + 1]][predict_indexes_filter][order(-prob)]

            # retrieve the predicted words themselves
            max_index_to_take <- min(length(predict_indexes_filter), num_words)

            # get the predicted word strings
            if (hist_len == 0) { # search by name
                the_predicted_words <-
                    prob_matches_sort[1:max_index_to_take]$name
            } else { # search by key
                the_predicted_words <-
                    key_to_name(prob_matches_sort[1:max_index_to_take]$name_key)
            }
        }
    }

    return(the_predicted_words)
}

# load required corpus dynamically
load_corpus <- function(type = "all") {
    path <- "en_US.combo.80trie.rds" # fail safe

     if (type == "twit") {
         path <- "en_US.twitter.80trie.rds"
         new_trie_index <- 2
    } else if (type == "news") {
        path <- "en_US.news.80trie.rds"
        new_trie_index <- 3
    } else if (type == "blogs") {
        path <- "en_US.blogs.80trie.rds"
        new_trie_index <- 4
    } else { # type == "all"
        path <- "en_US.combo.80trie.rds"
        new_trie_index <- 1 # default
    }

    if (is.null(trie_list[[new_trie_index]]))
        trie_list[[new_trie_index]] <<- readRDS(path)
    cur_trie <<- new_trie_index

    max_ngram_n <<- length(trie_list[[cur_trie]])

    # prepare list of most probable unigrams with no history
    if (is.null(unigram_indexes_by_prob_list[[cur_trie]])) {
        predictable_unigrams_indexes <- which(!grepl(filter_regex, trie_list[[cur_trie
                                                                           ]][[1]]$name))
        unigram_indexes_by_prob_list[[cur_trie]] <<-
            predictable_unigrams_indexes[order(trie_list[[cur_trie
                                                      ]][[1]][predictable_unigrams_indexes]$prob,
                                               decreasing = TRUE)]
    }
}

# main prediction function, with backoff
predict_main <- function(phrase = NULL, num_suggestions = 3, history_to_use = 3,
                          corpus_type = "all") {

    load_corpus(type = corpus_type)

    # clean input
    if (is.null(phrase))
        stop("Please enter some text for prediction.")
    input_clean <- do_clean_text(phrase)

    # tokenize input
    word_tokens <- tokenize(input_clean, what = "word",
                            removePunct = TRUE, removeSeparators = TRUE,
                            removeTwitter = TRUE, removeURL = TRUE,
                            simplify = TRUE)
    word_tokens_lower <- toLower(word_tokens)

    # prepend BOS marker for parsing
    word_tokens_lower <- c(bos_mark, word_tokens_lower)

    # get predictions, with backoff
    num_words <- num_suggestions
    num_input_tokens <- length(word_tokens_lower)
    predicted_words <- NULL
    hist_len <- min(num_input_tokens, max_ngram_n - 1, history_to_use)
    while (length(predicted_words) < num_words & (hist_len >= 0)) {

        # just get most probable words with no context
        if (hist_len == 0) {
            predicted_words <- do_predict_words(NULL, num_words)
        }

        # seek most probable words in context of history
        else {
            tokens <- word_tokens_lower[(num_input_tokens - hist_len + 1) :
                                            num_input_tokens]
            predicted_words <- unique(c(predicted_words,
                                        do_predict_words(tokens,
                                                         num_words -
                                                             length(predicted_words))))
        }
        hist_len <- hist_len - 1
    }

    return(predicted_words)
}

# wrapper function to handle UI input
predict_ui <- function(phrase = NULL) {
    print(
        predict_main(phrase = phrase, num_suggestions = 3,
          history_to_use = 3, corpus_type = "all")
        )
}