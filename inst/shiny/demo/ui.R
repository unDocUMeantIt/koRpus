library(shiny)

shinyUI(
  fluidPage(
    titlePanel("koRpus text analysis"),
    sidebarLayout(
      sidebarPanel(
        # limit the maximum amount of text to be analyzed
        includeHTML("./maxlength.html"),
        h4("Text to analyze:"),
        tags$textarea(id="text", rows=4, cols=40, maxlength=5000,
          onblur="if(this.value==\"\") this.value=\"(Paste your text here. Please do not supply sensitive data. Text limit is 5000 characters, but should at least have 100 words.)\";",
          onfocus="if(this.value==\"(Paste your text here. Please do not supply sensitive data. Text limit is 5000 characters, but should at least have 100 words.)\") this.value=\"\";",
          "(Paste your text here. Please do not supply sensitive data. Text limit is 5000 characters, but should at least have 100 words.)"),
        conditionalPanel("input.tab != 'chkLangDect'",
          selectInput("lang", "Language:", choices = c("en", "de", "es", "fr", "it", "nl", "pt", "ru"))
        ),
        conditionalPanel("input.tab == 'chkMain'",
          h4(style="margin-top:60px;", "About"),
          tags$p(HTML("This is a demo of the R package <code>koRpus</code>. For more information, documentation and downloads please visit the",
            "<a href=\"http://reaktanz.de/?c=hacking&amp;s=koRpus\" target=\"_blank\" title=\"koRpus webpage\">project webpage</a>."
          ))
        ),
        conditionalPanel("input.tab == 'chkLexdiv'",
          h4("Lexical diversity options:"),
          numericInput("LD.segment", "MSTTR segment size:", 100),
          sliderInput("LD.factor", "MTLD/MTLD-MA factor size:", min=0, max=1, value=0.72),
          numericInput("LD.minTokens", "MTLD-MA min. tokens/factor:", 9),
          numericInput("LD.steps", "MTLD-MA step size:", 1),
          numericInput("LD.random", "HD-D sample size:", 42),
          numericInput("LD.window", "MATTR moving window:", 100),
          selectInput("LD.logbase", "Base for logarithm:", choices = c(10, "exp(1)", 2)),
          checkboxInput("LD.caseSens", "Case sensitive", FALSE)
        ),
        conditionalPanel("input.tab == 'chkReadability'",
          h4("Readability options:"),
          checkboxGroupInput("RD.indices", label="Measures to calculate",
            choices=c("ARI"="ARI",
              "ARI (NRI)"="ARI.NRI",
              "ARI (simplified)"="ARI.simple",
              "Coleman-Liau"="Coleman.Liau",
              "Danielson-Bryan"="Danielson.Bryan",
              "Dickes-Steiwer"="Dickes.Steiwer",
              "ELF"="ELF",
              "Farr-Jenkins-Paterson"="Farr.Jenkins.Paterson",
              "Farr-Jenkins-Paterson (Powers-Sumner-Kearl)"="Farr.Jenkins.Paterson.PSK",
              "Flesch"="Flesch",
              "Flesch (Powers-Sumner-Kearl)"="Flesch.PSK",
              "Flesch (DE, Amstad)"="Flesch.de",
              "Flesch (ES, Fernandez-Huerta)"="Flesch.es",
              "Flesch (ES, Szigriszt)"="Flesch.Szigriszt",
              "Flesch (FR, Kandel-Moles)"="Flesch.fr",
              "Flesch (NL, Douma)"="Flesch.nl",
              "Flesch (NL, Brouwer)"="Flesch.Brouwer",
              "Flesch-Kincaid"="Flesch.Kincaid",
              "FOG"="FOG",
              "FOG (Powers-Sumner-Kearl)"="FOG.PSK",
              "FOG (NRI)"="FOG.NRI",
              "FORCAST"="FORCAST",
              "FORCAST (reading grade level)"="FORCAST.RGL",
              "Fucks Stilcharakteristik"="Fucks",
              "Linsear-Write"="Linsear.Write",
              "LIX"="LIX",
              "Neue Wiener Sachtextformeln"="nWS",
              "RIX"="RIX",
              "SMOG"="SMOG",
              "SMOG (DE, Bamberger-Vanecek)"="SMOG.de",
              "SMOG (formula C)"="SMOG.C",
              "SMOG (simplified)"="SMOG.simple",
              "Strain"="Strain",
              "TRI"="TRI",
              "Tuldava"="Tuldava",
              "Wheeler-Smith"="Wheeler.Smith",
              "Wheeler-Smith (DE, Bamberger-Vanecek)"="Wheeler.Smith.de"),
            selected=c("ARI",
              "Coleman.Liau",
              "Danielson.Bryan",
              "Dickes.Steiwer",
              "ELF",
              "Farr.Jenkins.Paterson",
              "Flesch",
              "Flesch.Kincaid",
              "FORCAST",
              "Fucks",
              "Linsear.Write",
              "LIX",
              "RIX",
              "SMOG",
              "Strain",
              "Wheeler.Smith"))
        )
    #      submitButton("Update View")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Descriptive statistics",
            tableOutput("desc"),
            h5("Word length (letters)"),
            tableOutput("desc.lttr.disrib"),
            h5("Word length (syllables)"),
            tableOutput("syll.disrib"),
            plotOutput("letter.plot"),
            value="chkMain"
          ),
          tabPanel("Lexical diversity",
            h5("Summary"),
            tableOutput("lexdiv.sum"),
            h5("Details"),
            pre(textOutput("lexdiv.res")),
            value="chkLexdiv"
          ),
          tabPanel("Readability",
            h5("Summary"),
            tableOutput("readability.sum"),
            h5("Details"),
            pre(textOutput("readability.res")),
            value="chkReadability"
          ),
          tabPanel("Language detection",
            pre(textOutput("langDect.res")),
            value="chkLangDect"
          ),
          id="tab"
        )
      )
    )
  )
)
