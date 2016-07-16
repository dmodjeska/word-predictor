source("model.R")

measure_model <- NULL

prep_ngrams <- function(genre = NULL, full_path = NULL) {

    replacement <- sprintf("\\.dev_test\\.%dngrams\\.rds", sample_percent)
    ngrams_path <- gsub(".txt", replacement, full_path)

    replacement <- sprintf("\\.dev_test\\.%dsample\\.txt", sample_percent)
    dev_test_path <- gsub(".txt", replacement, full_path)

    if (!file.exists(ngrams_path) & !is.null(full_path)) {
        if (!file.exists(dev_test_path))
            stop(sprintf("File not found: %s", dev_test_path))

        clean_path <- clean_text(dev_test_path)
        tokens_path <- make_word_tokens(clean_path)
        dfms_path <- make_dfms(tokens_path, FALSE)
    }

    return(ngrams_path)
}

handle_genre <- function(genre = NULL, full_path = NULL) {
    replacement <- sprintf("\\.dev_test\\.%dmeasure\\.rds", sample_percent)
    measure_path <- gsub(".txt", replacement, full_path)

    replacement <- sprintf("\\.%dmodels\\.rds", sample_percent)
    model_path <- gsub(".txt", replacement, full_path)

    if (!file.exists(measure_path) & !is.null(full_path)) {
        if (!file.exists(model_path))
            stop(sprintf("File not found: %s", model_path))
        measure_model <<- readRDS(model_path)

        # prepare ngrams for test corpus
        ngrams_path <- prep_ngrams(genre, full_path)
        if (!is.null(ngrams_path))
            ngrams <- readRDS(ngrams_path)
        ngrams_unlist <- vector("list", max_ngram_n)
        for (i in 1:max_ngram_n)
            ngrams_unlist[[i]] <- unlist(ngrams[[i]])

        # get probabilities for ngrams in language models
        ngram_match_indexes <- vector("list", max_ngram_n)
        unfound_ngrams <- vector("list", max_ngram_n)
        unfound_ngrams_tables <- vector("list", max_ngram_n)
        for (i in 1:max_ngram_n) {
            ngram_match_indexes[[i]] <- match(ngrams_unlist[[i]], measure_model[[i]]$name)
            unfound_ngrams[[i]] <- ngrams_unlist[[i]][is.na(ngram_match_indexes[[i]])]
            unfound_ngrams_tables[[i]] <- sort(table(unfound_ngrams[[i]]), decreasing = TRUE)

            print(sprintf("TOP UNFOUND %d-GRAMS", i))
            print(head(unfound_ngrams_tables[[i]], 100))
        }

        saveRDS(unfound_ngrams_tables, measure_path)
    }

    return(measure_path)
}

twit_mesure_path <- handle_genre("Twitter", full_path = twit_path)
# news_measure_path <- handle_genre("news", full_path = news_path)
# blogs_measure_path <- handle_genre("blogs", full_path = blogs_path)



