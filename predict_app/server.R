# server.R -- Create back end for word prediction app

source("predict.R")

# define server
shinyServer(function(input, output) {

    # define reactive variable to hold user input
    user_data <- reactive({
        data.frame(num_suggestions = input$num_suggestions,
                   history_length = input$history_length,
                   corpus_type = input$corpus_type,
                   phrase = input$phrase)
    })

    # define reactive variable to hold prediction
    predict_words <- reactive({
        new_data <- user_data()
        predict_main(new_data$phrase,
                   new_data$num_suggestions,
                   new_data$history_length,
                   new_data$corpus_type)
    })

    # create output for prediction as text
    output$predictions_text <- renderUI({
        words <- predict_words()
        string <- NULL
        for (i in 1:length(words)) {
            string <- c(string, words[[i]], sep = "&nbsp;&nbsp;&nbsp;")
        }
        HTML(string)
    })

})