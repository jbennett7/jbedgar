options(repos=c("https://mirrors.nics.utk.edu/cran/"))
suppressMessages({
    if (!require(shiny)) {
        install.packages("shiny")
        require(shiny)
    }
    if (!require(bslib)) {
        install.packages("bslib")
        require(bslib)
    }
    if (!require(DT)) {
        install.packages("DT")
        require(DT)
    }
    if (!require(jsonlite)) {
        install.packages("jsonlite")
        require(jsonlite)
    }
    if (!require(dplyr)) {
        install.packages("dplyr")
        require(dplyr)
    }
    source("R/getCik.R")
    source("R/getSubmissions.R")
    source("R/constants.R")
    source("R/listFilings.R")
})

default_tik <- "XYZ"

ua <- readLines('.ignore/useragent')
options(HTTPUserAgent = ua)

concept.url <- function(cik, concept){
  paste0("https://data.sec.gov/api/xbrl/companyconcept/CIK",
         cik, "/us-gaap/", concept, ".json")
}
facts.url <- function(cik) {
  paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",
         cik, ".json")
}

ui <- fluidPage(
  titlePanel("Edgar Scraper"),
  tabsetPanel(
      id = "switcher",
      textInput("ticker", label = "Company Ticker", value=default_tik),
      tabPanel("Explore Company Facts",
          sidebarPanel(
              radioButtons("form_type", "Annual or Quarterly",
                           choices = list("Annual" = 1, "Quarterly" = 2, "Debug" = 3)),
              selectizeInput("select_fact", "Facts", choices = NULL)
          ),
          mainPanel(
              textOutput("fact_label"),
              textOutput("fact_description"),
              DTOutput("fact_table")
         )
      ),
      tabPanel("Explore Company Submissions",
          sidebarPanel(
          ),
          mainPanel(
              textOutput("company_name"),
              textOutput("tickers"),
              DTOutput("submissions_table")
          )
      ),
      tabPanel("Explore Company Concept",
          sidebarPanel(
              textInput("concept_name", label = "Concept"),
          ),
          mainPanel(
              textOutput("concept_tag"),
              textOutput("concept_taxonomy"),
              textOutput("concept_description"),
              DTOutput("concept_table")
          )
      )
  )
)

server <- function(input, output, session) {
    get_facts <- reactive({
        req(input$ticker)
        cik <- getCik(input$ticker)
        req(cik)
        facts <- fromJSON(facts.url(cik))
        facts$facts$`us-gaap`
    })

    get_facts_names <- reactive({
        names(get_facts())
    })

    get_concept <- reactive({
        req(input$ticker,
            input$concept_name %in% get_facts_names())
        cik <- getCik(input$ticker)
        req(cik)
        req(input$concept_name)
        concepts <- fromJSON(concept.url(cik, input$concept_name))
        concepts
    })
    
    observe({
        updateSelectInput(session, "select_fact",
                          choices = get_facts_names())
    })

    output$fact_label <- renderText({
        req(input$select_fact)
        fact <- input$select_fact
        company.facts <- get_facts()
        company.facts[[fact]]$label
    })

    output$fact_description <- renderText({
        req(input$select_fact)
        fact <- input$select_fact
        company.facts <- get_facts()
        company.facts[[fact]]$description
    })

    output$fact_table <- renderDT({
        req(input$select_fact)
        fact <- input$select_fact
        company.facts <- get_facts()
        tbl <- company.facts[[fact]]$units$USD
        tbl <- tbl[!duplicated(tbl[c("end", "val")]),]
        # Annual
        if (input$form_type == 1){
            #tbl <- tbl[!is.na(tbl$frame) & tbl$form == "10-K",]
            tbl <- tbl[tbl$form == "10-K",]
        # Quarterly
        } else if (input$form_type == 2){
            tbl <- tbl[!is.na(tbl$frame) & tbl$form == "10-Q",]
        # Debug
        } else {
            #tbl <- tbl[!is.na(tbl$frame),]
            tbl
        }
        datatable(tbl[order(as.Date(tbl$end), decreasing=T),],
                  options = list(pageLength = 10),
                  filter = 'top',
                  selection = 'single')
    })

    output$company_name <- renderText({
        req(input$ticker)
        getSubmissions(input$ticker)$name
    })

    output$tickers <- renderText({
        req(input$ticker)
        getSubmissions(input$ticker)$tickers
    })

    output$submissions_table <- renderDT({
        req(input$ticker)
        tbl <- getSubmissions(input$ticker)$filings
        datatable(tbl,
                  options = list(pageLength = 10),
                  filter = 'top',
                  selection = 'single')
    })

    output$concept_tag <- renderText({
        req(input$ticker, input$concept_name)
        get_concept()$tag
    })

    output$concept_taxonomy <- renderText({
        req(input$ticker, input$concept_name)
        get_concept()$taxonomy
    })

    output$concept_description <- renderText({
        req(input$ticker, input$concept_name)
        get_concept()$description
    })

    output$concept_table <- renderDT({
        req(input$ticker,
            input$concept_name)
        tbl <- get_concept()$units$USD
        tbl <- tbl[order(as.Date(tbl$end), decreasing=T),]
        datatable(tbl,
                  options = list(pageLength = 10),
                  filter = 'top',
                  selection = 'single')
    })
}

shinyApp(ui, server, options = list(port = 8080))

