# clean text for modeling or prediction

# GENERAL CLEANING  -------------------------------------------------------

library(qdap)
library(stringi)
library(quanteda)

# markers (punctuation is OK - these markers are inserted after pre-processing)
bos_mark <- "1_b2o7s1" # mark should sort before all legal words for convenience
eos_mark <- "1_e2o7s1" # mark should sort before all legal words for convenience
marks <- c(bos_mark, eos_mark)
unknown_mark <- "z_u4n7k4"

# equivalence classes
url_class <- "u9r3l7"
profanity_class <- "x8x6x5"
blanks_class <- "8b5b9b"
# number class: all digits are replaced with and reduced to '5'

max_n <- 3

profanity_regex = "(shit|piss|fuck|cunt|cocksucker|motherfucker|tits)(ed|ing)?"

# helper function to expand qdap's built-in contractions dictionary
prep_contractions <- function() {
    data(contractions)
    data(GradyAugmented)

    # add miscellaneous contractions
    misc_contraction <- c("haven't", "y'all", "what's", "here's", "c'mon",
                          "hadn't",
                          "coulda", "woulda", "shoulda",
                          "gotta", "wanna", "gonna",
                          "kinda", "sorta", "cannot")
    misc_expanded <- c("have not", "you all", "what is", "here is", "come on",
                       "had not",
                       "could have", "would have", "should have",
                       "got to", "want to", "going to",
                       "kind of", "sort of", "can not")
    contractions2 <- rbind(contractions,
                          data.frame(contraction = misc_contraction,
                                     expanded = misc_expanded))

    return(contractions2)
}

prep_qdap_abbreviations <- function() {
    data(abbreviations)
    abbreviations2 <- contractions[!(contractions %in% c("www.", ".com"))]

    return(abbreviations2)
}

contractions2 <- prep_contractions()
abbreviations2 <- prep_qdap_abbreviations()

# prep contractions regex for later
fix_contractions <- function(text) {
    # adapt qdap contractions for misspellings list
    misspell <- data.frame(contraction = gsub("'", "", contractions[[1]]),
                           expanded = contractions[[2]])
    misspell2 <- misspell[!(misspell[ , 1] %in% GradyAugmented), ]
    misspell3 <- data.frame(contraction = gsub("^", "\\\\b", misspell2[[1]],
                                               ignore.case = TRUE, perl = TRUE),
                            expanded = misspell2[[2]])
    contractions_misspell <- data.frame(contractions = gsub("$", "\\\\b", misspell3[[1]],
                                                            ignore.case = TRUE,
                                                            perl = TRUE),
                                        expanded = misspell3[[2]])

    # expand misspelled contractions
    fixed_text <- text
    for (i in 1:nrow(contractions_misspell))
        fixed_text <- gsub(contractions_misspell[i, 1],
                           contractions_misspell[i, 2],
                           fixed_text, ignore.case = TRUE, perl = TRUE)

    return(fixed_text)
}

filter_more_profanity <- function(text) {
    profanity_file <- "swearWords.txt"
    if (!file.exists(profanity_file)) {
        profanity_url <- "http://www.bannedwordlist.com/lists/swearWords.txt"
        download.file(profanity_url, profanity_file)
    }
    if (!file.exists(profanity_file))
        stop(sprintf("File not found: %s", profanity_file))

    # read banned words list
    con <- file(profanity_file, "r")
    banned_words_list <- readLines(con, warn = FALSE)
    close(con)

    banned_words_list2 <- gsub("^", "\\\\b", banned_words_list,
                               ignore.case = TRUE, perl = TRUE)
    banned_words_list3 <- gsub("$", "\\\\b", banned_words_list2,
                               ignore.case = TRUE, perl = TRUE)

    fixed_text <- text
    for (i in 1:length(banned_words_list3)) {
        fixed_text <- gsub(banned_words_list3[i], profanity_class, fixed_text,
                           ignore.case = TRUE, perl = TRUE)
    }

    return(fixed_text)
}

do_clean_text <- function(text) {
    t0 <- scrubber(text) # from qdap
    t0b <- stri_trans_general(t0, "latin-ascii") # from stri
    t0c <- iconv(t0b, "latin1", "ASCII", sub="")
    t1 <- clean_words(t0c)
    t2 <- replace_contraction(t1, contraction = contractions2) # from qdap
    t3 <- replace_abbreviation(t2, abbreviation = abbreviations2) # from qdap
    # t4 <- replace_ordinal(t3) # from qdap
    t5 <- replace_symbol(t3, and = TRUE, with = TRUE) # from qdap
    t6 <- fix_contractions(t5)
    t7 <- filter_more_profanity(t6)

    return(t7)
}

# CLEAN SPECIFIC WORDS -------------------------------------------------------

# clean words for contractions, shortenings, casual, punctuation, and UK

clean_words <- function(text) {

    # remove non-printing unbreakable space
    aa1 <- gsub( "\ufeff", "", text, ignore.case = TRUE, perl = TRUE)

    # ---------

    # misspelled contractions
    # t1 <- gsub("\\bim\\b", "I'm", text, ignore.case = TRUE, perl = TRUE)
    # t2 <- gsub("\\bive\\b", "I've", t1, ignore.case = TRUE, perl = TRUE)
    # t3 <- gsub("\\bur\\b", "you're", t2, ignore.case = TRUE, perl = TRUE)
    # t4 <- gsub("\\bdont\\b", "don't", t3, ignore.case = TRUE, perl = TRUE)
    # t5 <- gsub("\\bdidnt\\b", "didn't ", t4, ignore.case = TRUE, perl = TRUE)
    # t6 <- gsub("\\bthats\\b", "that's", t5, ignore.case = TRUE, perl = TRUE)

    t7 <- gsub("\\bwhats\\b", "what's", aa1, ignore.case = TRUE, perl = TRUE)
    t8 <- gsub("\\byall\\b", "y'all", t7, ignore.case = TRUE, perl = TRUE)

    # ---------

    # shortened words
    s1 <- gsub("\\bu\\b|\\byu\\b", "you", t8, ignore.case = TRUE, perl = TRUE)
    s2 <- gsub("\\bk\\b|\\bok\\b", "okay", s1, ignore.case = TRUE, perl = TRUE)
    s3 <- gsub("\\bcuz\\b", "because", s2, ignore.case = TRUE, perl = TRUE)
    s4 <- gsub("\\bthx\\b|\\bthankx\\b", "thanks", s3, ignore.case = TRUE, perl = TRUE)
    s5 <- gsub("\\bpls\\b|\\bplz\\b", "please", s4, ignore.case = TRUE, perl = TRUE)
    s6 <- gsub("\\btho\\b", "though", s5, ignore.case = TRUE, perl = TRUE)
    s7 <- gsub("\\bthru\\b", "through", s6, ignore.case = TRUE, perl = TRUE)
    s8 <- gsub("\\blil\\b", "little", s7, ignore.case = TRUE, perl = TRUE)
    s9 <- gsub("\\binfo\\b", "information", s8, ignore.case = TRUE, perl = TRUE)
    s10 <- gsub("\\bn\\b", "and", s9, ignore.case = TRUE, perl = TRUE)
    s11 <- gsub("\\bpic\\b", "picture", s10, ignore.case = TRUE, perl = TRUE)
    s12 <- gsub("\\bfb\\b", "Facebook", s11, ignore.case = TRUE, perl = TRUE)
    s13 <- gsub("\\bnigga\\b", "nigger", s12, ignore.case = TRUE, perl = TRUE)
    s13b <- gsub("\\bniggas\\b", "niggers", s13, ignore.case = TRUE, perl = TRUE)
    s14 <- gsub("\\brt\\b", "retweet", s13b, ignore.case = TRUE, perl = TRUE)
    s15 <- gsub("\\bapp\\b", "application", s14, ignore.case = TRUE, perl = TRUE)
    s16 <- gsub("\\bbday\\b", "birthday", s15, ignore.case = TRUE, perl = TRUE)
    s17 <- gsub("\\br\\b", "are", s16, ignore.case = TRUE, perl = TRUE)
    s18 <- gsub("\\balot\\b", "a lot", s17, ignore.case = TRUE, perl = TRUE)
    s19 <- gsub("\\bbiz\\b", "business", s18, ignore.case = TRUE, perl = TRUE)
    s20 <- gsub("\\bapps\\b", "applications", s19, ignore.case = TRUE, perl = TRUE)
    s21 <- gsub("\\bvs\\b", "versus", s20, ignore.case = TRUE, perl = TRUE)
    s22 <- gsub("\\bbro\\b", "brother", s21, ignore.case = TRUE, perl = TRUE)
    s23 <- gsub("\\bppl\\b", "people", s22, ignore.case = TRUE, perl = TRUE)
    s24 <- gsub("\\bny\\b", "New York", s23, ignore.case = TRUE, perl = TRUE)
    s25 <- gsub("\\bnyc\\b", "New York City", s24, ignore.case = TRUE, perl = TRUE)
    s26 <- gsub("\\bpics\\b", "pictures", s25, ignore.case = TRUE, perl = TRUE)
    s27 <- gsub("\\btil\\b", "until", s26, ignore.case = TRUE, perl = TRUE)
    s28 <- gsub("\\bgov\\b", "government", s27, ignore.case = TRUE, perl = TRUE)
    s29 <- gsub("\\btv\\b", "television", s28, ignore.case = TRUE, perl = TRUE)
    s30 <- gsub("\\bluv\\b", "love", s29, ignore.case = TRUE, perl = TRUE)
    s31 <- gsub("\\btech\\b", "technology", s30, ignore.case = TRUE, perl = TRUE)
    s32 <- gsub("\\bcongrats\\b", "congratulations", s31, ignore.case = TRUE, perl = TRUE)
    s33 <- gsub("\\bnite\\b", "night", s32, ignore.case = TRUE, perl = TRUE)
    s34 <- gsub("\\btonite\\b", "tonight", s33, ignore.case = TRUE, perl = TRUE)
    s35 <- gsub("\\btix\\b", "tickets", s34, ignore.case = TRUE, perl = TRUE)
    s36 <- gsub("\\bpix\\b", "pictures", s35, ignore.case = TRUE, perl = TRUE)
    s37 <- gsub("\\bf\\b", "fuck", s36, ignore.case = TRUE, perl = TRUE)
    s38 <- gsub("\\bkg\\b", "kilograms", s37, ignore.case = TRUE, perl = TRUE)
    s39 <- gsub("\\bkm\\b", "kilometers", s38, ignore.case = TRUE, perl = TRUE)

    # ---------

    # casual present participles
    g1 <- gsub("\\bdoin\\b", "doing", s39, ignore.case = TRUE, perl = TRUE)
    g2 <- gsub("\\bbein\\b", "being", g1, ignore.case = TRUE, perl = TRUE)
    g3 <- gsub("\\bhavin\\b", "having", g2, ignore.case = TRUE, perl = TRUE)
    g4 <- gsub("\\bgettin\\b", "getting", g3, ignore.case = TRUE, perl = TRUE)
    g5 <- gsub("\\bsayin\\b", "saying", g4, ignore.case = TRUE, perl = TRUE)
    g6 <- gsub("\\bwalkin\\b", "walking", g5, ignore.case = TRUE, perl = TRUE)
    g7 <- gsub("\\btalkin\\b", "talking", g6, ignore.case = TRUE, perl = TRUE)
    g8 <- gsub("\\bgoin\\b", "going", g7, ignore.case = TRUE, perl = TRUE)
    g9 <- gsub("\\baskin\\b", "asking", g8, ignore.case = TRUE, perl = TRUE)
    g10 <- gsub("\\bfuckin\\b", "fucking", g9, ignore.case = TRUE, perl = TRUE)
    g11 <- gsub("\\bshittin\\b", "shitting", g10, ignore.case = TRUE, perl = TRUE)
    g12 <- gsub("\\bpissin\\b", "pissing", g11, ignore.case = TRUE, perl = TRUE)

    # ---------

    # punctuation (e.g., single quotation mark)
    # p1 <- gsub(	"\u2018|\u2019", "'", g12, ignore.case = TRUE, perl = TRUE) #

    # ---------

    # MISCELLANEOUS

    a1 <- gsub("\\be-mail\\b", "email", g12, ignore.case = TRUE, perl = TRUE)

    # replace all digits with 5
    a2 <- gsub("[0-9]", "5", a1, ignore.case = TRUE, perl = TRUE)

    # simplify all digit strings to 5
    a2b <- gsub("5[5.,-/]*5", "5", a2, ignore.case = TRUE, perl = TRUE)

    # shorten long laughter strings
    a3 <- gsub(	"\\bhahaha\\b|\\bha-ha\\b|\\bhahah\\b|\\bhahahaha\\b", "haha",
                a2b, ignore.case = TRUE, perl = TRUE)

    # class obvious URL's
    a4 <- gsub("\\b(http[s]?://)?(www\\.)?[^ ]+\\.(com|edu)(/[^ ]+)?\\b",
               url_class, a3, ignore.case = TRUE, perl = TRUE)

    # shorten long hugs-and-kisses strings
    a5 <- gsub(	"\\bxo(x)?\\b|\\bxoxo((x)?xo(o)?)+(x)?\\b", "xoxo", a4, ignore.case = TRUE, perl = TRUE)


    a6 <- gsub(	"\\b_+\\b", blanks_class, a5, ignore.case = TRUE, perl = TRUE)
    a6b <- gsub("\\b_+|_+\\b", "", a6, ignore.case = TRUE, perl = TRUE) # TO DO: OK?

    # class short URL strings with big 5 consumer tech companies
    a7 <- gsub(	"\\b(amazon|apple|microsoft|google|facebook)\\.[^ ]+\\b",
                url_class, a6b, ignore.case = TRUE, perl = TRUE)

    # replace ordinal symbol with letters
    a8 <- gsub(	"º", "th", a7, fixed = TRUE)

    a9 <- gsub(	"satan claus", "santa claus", a8, fixed = TRUE)

    # shorten long strings of repeated letters 4+
    a10 <- gsub("([a-zA-Z])\\1\\1\\1+", "\\1\\1\\1", a9, fixed = FALSE, perl = TRUE)

    # shorten strings of repeated words 2+
    a11 <- gsub("(\\b[^ ]+\\b)( \\1)+", "\\1",  a10, fixed = FALSE, perl = TRUE)

    # ---------

    # profanity
    z1 <- gsub(profanity_regex, profanity_class, a11, ignore.case = TRUE,
               perl = TRUE)

    # -------------------------------------------------------------------

    # "Short head" general rules for converting British to American spelling

    # "-our" --> "-or"
    b1 = gsub("\\bcolour", "color", z1, ignore.case = TRUE, perl = TRUE)
    b2 = gsub("\\bflavour", "flavor", b1, ignore.case = TRUE, perl = TRUE)
    b3 = gsub("\\bbehaviour", "behavior", b2, ignore.case = TRUE, perl = TRUE)
    b4 = gsub("\\bharbour", "harbor", b3, ignore.case = TRUE, perl = TRUE)
    b5 = gsub("\\bhonour", "honor", b4, ignore.case = TRUE, perl = TRUE)
    b6 = gsub("\\bhumour", "humor", b5, ignore.case = TRUE, perl = TRUE)
    b7 = gsub("\\blabour", "labor", b6, ignore.case = TRUE, perl = TRUE)
    b8 = gsub("\\bneighbour", "neighbor", b7, ignore.case = TRUE, perl = TRUE)
    b9 = gsub("\\brumour", "rumor", b8, ignore.case = TRUE, perl = TRUE)
    b10 = gsub("\\bsplendour", "splendor", b9, ignore.case = TRUE, perl = TRUE)
    b11 = gsub("\\bfavour", "favor", b10, ignore.case = TRUE, perl = TRUE)
    b12 = gsub("favourite", "favorite", b11, ignore.case = TRUE, perl = TRUE)
        # note: "favourite" seems to be used in about 10 hashtags mid-tag

    # "-amme" --> "-am" // "-ogue" --> "-og"
    c1 = gsub("amme\\b", "am", b12, ignore.case = TRUE, perl = TRUE)

    # "-bre" --> "-ber" // "-tre" --> "ter
    d1 = gsub("bre\\b", "ber", c1, ignore.case = TRUE, perl = TRUE)
    d2 = gsub("tre\\b", "ter", d1, ignore.case = TRUE, perl = TRUE)

    # "-isation" --> "-ization"
    e1 = gsub("isation\\b", "ization", d2, ignore.case = TRUE, perl = TRUE)

    # "-ise" --> "-ize"
    f1 = gsub("\\borganise", "organize", e1, ignore.case = TRUE, perl = TRUE)
    f2 = gsub("\\brecognise", "recognize", f1, ignore.case = TRUE, perl = TRUE)
    f3 = gsub("\\brealise", "realize", f2, ignore.case = TRUE, perl = TRUE)

    # double 'l' --> single 'l'
    g1 = gsub("lled\\b", "led", f3, ignore.case = TRUE, perl = TRUE)
    g2 = gsub("lling\\b", "ling", g1, ignore.case = TRUE, perl = TRUE)
    g3 = gsub("ller\\b", "ler", g2, ignore.case = TRUE, perl = TRUE)
    g4 = gsub("llor\\b", "lor", g3, ignore.case = TRUE, perl = TRUE)
    g5 = gsub("llest\\b", "lest", g4, ignore.case = TRUE, perl = TRUE)

    # misc 1
    h1 = gsub("\\baeroplane\\b", "airplane", g5, ignore.case = TRUE, perl = TRUE)
    h2 = gsub("\\baluminium\\b", "aluminum", h1, ignore.case = TRUE, perl = TRUE)
    h3 = gsub("\\barse\\b", "ass", h2, ignore.case = TRUE, perl = TRUE)
    h4 = gsub("\\bbehove\\b", "behoove", h3, ignore.case = TRUE, perl = TRUE)
    h5 = gsub("\\bbogeyman\\b", "boogeyman", h4, ignore.case = TRUE, perl = TRUE)
    h6 = gsub("\\bbrent\\b", "brant", h5, ignore.case = TRUE, perl = TRUE)
    h7 = gsub("\\bcarburetor\\b", "carburator", h6, ignore.case = TRUE, perl = TRUE)
    h8 = gsub("\\beyrie\\b", "aerie", h7, ignore.case = TRUE, perl = TRUE)
    h9 = gsub("\\bfurore\\b", "furor", h8, ignore.case = TRUE, perl = TRUE)
    h10 = gsub("\\bgrotty\\b", "grody", h9, ignore.case = TRUE, perl = TRUE)
    h11 = gsub("\\bhaulier\\b", "hauler", h10, ignore.case = TRUE, perl = TRUE)
    h12 = gsub("\\bjemmy\\b", "jimmy", h11, ignore.case = TRUE, perl = TRUE)
    h13 = gsub("\\bmum\\b", "mon", h12, ignore.case = TRUE, perl = TRUE)
    h14 = gsub("\\bnaivety\\b", "naivete", h13, ignore.case = TRUE, perl = TRUE)
    h15 = gsub("\\boesophagus\\b", "esophagus", h14, ignore.case = TRUE, perl = TRUE)
    h16 = gsub("\\borientated\\b", "oriented", h15, ignore.case = TRUE, perl = TRUE)
    h17 = gsub("\\bpyjamas\\b", "pajamas", h16, ignore.case = TRUE, perl = TRUE)
    h18 = gsub("\\bpernickety\\b", "persnickety", h17, ignore.case = TRUE, perl = TRUE)
    h19 = gsub("\\bquin\\b", "quint", h18, ignore.case = TRUE, perl = TRUE)
    h20 = gsub("\\bscallywag\\b", "scalawag", h19, ignore.case = TRUE, perl = TRUE)
    h21 = gsub("\\bsledge\\b", "sled", h20, ignore.case = TRUE, perl = TRUE)
    h22 = gsub("\\bspeciality\\b", "specialty", h21, ignore.case = TRUE, perl = TRUE)
    h23 = gsub("\\btitbit\\b", "tidbit", h22, ignore.case = TRUE, perl = TRUE)

    # misc 2
    i1 = gsub("\\bannexe\\b", "annex", h23, ignore.case = TRUE, perl = TRUE)
    i2 = gsub("\\baxe\\b", "ax", i1, ignore.case = TRUE, perl = TRUE)
    i3 = gsub("\\bcheque\\b", "check", i2, ignore.case = TRUE, perl = TRUE)
    i4 = gsub("\\bchequer\\b", "checker", i3, ignore.case = TRUE, perl = TRUE)
    i5 = gsub("\\bchilli\\b", "chili", i4, ignore.case = TRUE, perl = TRUE)
    i6 = gsub("\\bcosy\\b", "cozy", i5, ignore.case = TRUE, perl = TRUE)
    i7 = gsub("\\bdoughnut\\b", "donut", i6, ignore.case = TRUE, perl = TRUE)
    i8 = gsub("\\bdraught\\b", "draft", i7, ignore.case = TRUE, perl = TRUE)
    i9 = gsub("\\bgrey\\b", "gray", i8, ignore.case = TRUE, perl = TRUE)
    i10 = gsub("\\bhearken\\b", "harken", i9, ignore.case = TRUE, perl = TRUE)
    i11 = gsub("\\bgaol\\b", "jail", i10, ignore.case = TRUE, perl = TRUE)
    i12 = gsub("\\bkerb\\b", "curb", i11, ignore.case = TRUE, perl = TRUE)
    i13 = gsub("\\bliquorice\\b", "licorice", i12, ignore.case = TRUE, perl = TRUE)
    i14 = gsub("\\bmanoeuvre\\b", "maneuver", i13, ignore.case = TRUE, perl = TRUE)
    i15 = gsub("\\bmollusc\\b", "mollusk", i14, ignore.case = TRUE, perl = TRUE)
    i16 = gsub("\\bmould\\b", "mold", i15, ignore.case = TRUE, perl = TRUE)
    i17 = gsub("\\bmoult\\b", "molt", i16, ignore.case = TRUE, perl = TRUE)
    i18 = gsub("\\bneurone\\b", "neuron", i17, ignore.case = TRUE, perl = TRUE)
    i19 = gsub("\\bplough\\b", "plow", i18, ignore.case = TRUE, perl = TRUE)
    i20 = gsub("\\bprimaeval\\b", "primeval", i19, ignore.case = TRUE, perl = TRUE)
    i21 = gsub("\\bsceptic,\\b", "skeptic", i20, ignore.case = TRUE, perl = TRUE)
    i22 = gsub("\\bslue\\b", "slew", i21, ignore.case = TRUE, perl = TRUE)
    i23 = gsub("\\bsmoulder\\b", "molder", i22, ignore.case = TRUE, perl = TRUE)
    i24 = gsub("\\bstorey\\b", "story", i23, ignore.case = TRUE, perl = TRUE)
    i25 = gsub("\\btyre\\b", "tire", i24, ignore.case = TRUE, perl = TRUE)
    i26 = gsub("\\byoghurt\\b|\\byoghourt\\b", "yogurt", i25, ignore.case = TRUE, perl = TRUE)

    # --------------------------------

    # months
    j1 = gsub("\\bjan\\b", "January", i26, ignore.case = TRUE, perl = TRUE)
    j2 = gsub("\\bfeb\\b", "Febuary", j1, ignore.case = TRUE, perl = TRUE)
    j3 = gsub("\\bmar\\b", "March", j2, ignore.case = TRUE, perl = TRUE)
    j4 = gsub("\\bapr\\b", "April", j3, ignore.case = TRUE, perl = TRUE)
    j5 = gsub("\\bjune\\b", "June", j4, ignore.case = TRUE, perl = TRUE)
    j6 = gsub("\\bjuly\\b", "July", j5, ignore.case = TRUE, perl = TRUE)
    j7 = gsub("\\bjaug\\b", "August", j6, ignore.case = TRUE, perl = TRUE)
    j8 = gsub("\\bsep\\b|\\bsept\\b", "September", j7, ignore.case = TRUE, perl = TRUE)
    j9 = gsub("\\boct\\b", "October", j8, ignore.case = TRUE, perl = TRUE)
    j10 = gsub("\\bnov\\b", "November", j9, ignore.case = TRUE, perl = TRUE)
    j11 = gsub("\\bdec\\b", "December", j10, ignore.case = TRUE, perl = TRUE)

    # ---------

    return(j11)
}