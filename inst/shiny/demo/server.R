library(shiny)
library(koRpus)
library(koRpus.lang.nl)
library(koRpus.lang.pt)

shinyServer(function(input, output){

    tagged.text <- reactive(tokenize(input$text, format="obj", lang=input$lang))
    hyphenated.text <- reactive({
            # set the next line to activate caching, if this application is run on a shiny server
            #set.kRp.env(hyph.cache.file=file.path("/var","shiny-server","cache","koRpus",paste("hyph.cache.",input$lang,".rdata", sep="")))
            hyphen(tagged.text(), quiet=TRUE)
        })

    output$letter.plot <- renderPlot(plot(tagged.text(), what="letters"))
    output$desc <- renderTable({
        basic.desc.data <- as.data.frame(describe(tagged.text())[c("all.chars","normalized.space","chars.no.space", "letters.only","lines",
            "punct","digits","words","sentences","avg.sentc.length","avg.word.length")])
        syll.desc.data <- as.data.frame(describe(hyphenated.text())[c("num.syll", "avg.syll.word")])
        colnames(basic.desc.data) <- c("All characters","Normalized space","Characters (no space)", "Characters (letters only)","Lines",
            "Punctuation","Digits","Words","Sentences","Avg. sentence length","Avg. word length")
        colnames(syll.desc.data) <- c("Syllables", "Avg. syllable per word")
        desc.data <- cbind(basic.desc.data, syll.desc.data)
        rownames(desc.data) <- c("Value")
        t(desc.data)
    }, striped=TRUE, rownames=TRUE)
    output$desc.lttr.disrib <- renderTable({
        t(describe(tagged.text())[["lttr.distrib"]])
    }, striped=TRUE, rownames=TRUE)
    output$syll.disrib <- renderTable({
        t(describe(hyphenated.text())[["syll.distrib"]])
    }, striped=TRUE, rownames=TRUE)

    LD.results <- reactive(lex.div(tagged.text(), segment=input$LD.segment, factor.size=input$LD.factor, min.tokens=input$LD.minTokens, MTLDMA.steps=input$LD.steps,
            rand.sample=input$LD.random, window=input$LD.window, case.sens=input$LD.caseSens, log.base=eval(parse(text=input$LD.logbase)), detailed=FALSE, char=c(), quiet=TRUE))
    output$lexdiv.sum <- renderTable({
        summary(LD.results())
    }, striped=TRUE, rownames=FALSE)
    output$lexdiv.res <- renderPrint({
        LD.results()
    })

    RD.results <- reactive(readability(tagged.text(), hyphen=hyphenated.text(), index=input$RD.indices, quiet=TRUE))
    output$readability.sum <- renderTable({
        summary(RD.results())
    }, striped=TRUE, rownames=FALSE)
    output$readability.res <- renderPrint({
        RD.results()
    })

    langDect.results <- reactive({
        guess.lang(input$text, udhr.path="/var/shiny-server/www/koRpus/udhr_txt", format="obj")
    })
    output$langDect.res <- renderPrint({
        summary(langDect.results())
    })
})
