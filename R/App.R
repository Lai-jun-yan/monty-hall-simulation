library(shiny)

ui <- fluidPage(
  
  titlePanel("🚪 Monty Hall - Step 1 選門"),
  
  h3("請選一扇門，然後按確定"),
  
  fluidRow(
    column(4, actionButton("door1", "🚪 Door 1", width = "100%")),
    column(4, actionButton("door2", "🚪 Door 2", width = "100%")),
    column(4, actionButton("door3", "🚪 Door 3", width = "100%"))
  ),
  
  br(),
  actionButton("confirm", "✔ 確定選擇"),
  actionButton("reset", "🔄 重置"),
  
  br(), br(),
  h3(textOutput("status")),
  
  uiOutput("decision_ui"),
  h3(textOutput("final"))
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    temp_choice = NULL,
    choice = NULL,
    open = NULL,
    final_open = NULL,
    win = NULL,        # ⭐ 新增
    locked = FALSE,
    stage = "choose"
  )
  
  # =========================
  # 🎯 選門
  # =========================
  observeEvent(input$door1, { if (!rv$locked) rv$temp_choice <- 1 })
  observeEvent(input$door2, { if (!rv$locked) rv$temp_choice <- 2 })
  observeEvent(input$door3, { if (!rv$locked) rv$temp_choice <- 3 })
  
  # =========================
  # 🎯 confirm → 主持人開門
  # =========================
  observeEvent(input$confirm, {
    
    req(rv$temp_choice)
    
    rv$choice <- rv$temp_choice
    rv$locked <- TRUE
    rv$stage <- "reveal"
    
    doors <- 1:3
    candidates <- setdiff(doors, rv$choice)
    
    rv$open <- sample(candidates, 1)
  })
  
  # =========================
  # 🎯 UI
  # =========================
  output$decision_ui <- renderUI({
    
    if (rv$stage == "reveal") {
      tagList(
        h4(paste("主持人打開 Door", rv$open)),
        h4("要換門嗎？"),
        actionButton("switch", "🔁 換門"),
        actionButton("stay", "🙅 不換")
      )
    }
  })
  
  # =========================
  # 🔁 換門（2/3 機率贏）
  # =========================
  observeEvent(input$switch, {
    
    rv$stage <- "final"
    
    z <- rbinom(1, 1, 2/3)
    
    other <- setdiff(1:3, c(rv$choice, rv$open))
    
    rv$final_open <- other
    
    rv$win <- (z == 1)
  })
  
  # =========================
  # 🙅 不換門（1/3 機率贏）
  # =========================
  observeEvent(input$stay, {
    
    rv$stage <- "final"
    
    z <- rbinom(1, 1, 1/3)
    
    rv$final_open <- rv$choice
    
    rv$win <- (z == 1)
  })
  
  # =========================
  # 🔄 reset
  # =========================
  observeEvent(input$reset, {
    
    rv$temp_choice <- NULL
    rv$choice <- NULL
    rv$open <- NULL
    rv$final_open <- NULL
    rv$win <- NULL
    rv$locked <- FALSE
    rv$stage <- "choose"
  })
  
  # =========================
  # 📌 status
  # =========================
  output$status <- renderText({
    
    if (is.null(rv$temp_choice)) {
      "尚未選擇任何門"
      
    } else if (!rv$locked) {
      paste("你目前選擇 Door", rv$temp_choice)
      
    } else {
      paste("已選 Door", rv$choice,
            "｜主持人開 Door", rv$open)
    }
  })
  
  # =========================
  # 🎯 final
  # =========================
  output$final <- renderText({
    
    if (rv$stage != "final") return(NULL)
    
    if (rv$win) {
      paste("🎉 你贏了！Door", rv$final_open,"是跑車")
    } else {
      paste("🐐 你輸了！Door", rv$final_open,"是羊")
    }
  })
}

shinyApp(ui, server)