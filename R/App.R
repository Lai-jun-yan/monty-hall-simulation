library(shiny)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
        background: url('background.png') no-repeat center center fixed;
        background-size: contain;
        background-repeat: no-repeat;
        background-position: center;
      }

      /* 半透明遮罩 */
      body::before {
        content: '';
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0,0,0,0.35);
        z-index: -1;
      }

      /* 卡片 UI */
      .card {
        background-color: rgba(255,255,255,0.9);
        padding: 20px;
        border-radius: 15px;
        max-width: 850px;
        margin: auto;
        margin-top: 150px;
      }

      .action-button {
        font-size: 18px;
        margin-bottom: 10px;
      }
    "))
  ),
  
  uiOutput("page_ui")
)


server <- function(input, output, session) {
  
  rv <- reactiveValues(
    page = "home",
    temp_choice = NULL,
    choice = NULL,
    open = NULL,
    final_open = NULL,
    win = NULL,
    locked = FALSE,
    stage = "choose"
  )
  
  # =========================
  # 🏠 首頁 → 開始遊戲
  # =========================
  observeEvent(input$start, {
    rv$page <- "game"
  })
  
  # =========================
  # 🚪 選門
  # =========================
  observeEvent(input$door1, { if (!rv$locked) rv$temp_choice <- 1 })
  observeEvent(input$door2, { if (!rv$locked) rv$temp_choice <- 2 })
  observeEvent(input$door3, { if (!rv$locked) rv$temp_choice <- 3 })
  
  # =========================
  # ✔ 確定 → 主持人開門
  # =========================
  observeEvent(input$confirm, {
    
    req(rv$temp_choice)
    
    rv$choice <- rv$temp_choice
    rv$locked <- TRUE
    rv$stage <- "reveal"
    
    candidates <- setdiff(1:3, rv$choice)
    rv$open <- sample(candidates, 1)
  })
  
  # =========================
  # 🎨 UI（首頁 + 遊戲頁）
  # =========================
  output$page_ui <- renderUI({
    
    # ================= HOME =================
    if (rv$page == "home") {
      
      div(
        class = "card",
        style = "text-align:center; margin-top:150px;",
        
        h1("🎮 Monty Hall Game"),
        br(),
        h4("統計計算課堂報告的示範"),
        h4("看起來超沒用的，請大家見諒"),
        br(),
        actionButton("start", "🚪 開始遊戲", width = "200px")
      )
      
    } else {
      
      # ================= GAME =================
      div(
        class = "card",
        
        h2("🚪 Monty Hall"),
        
        wellPanel(
          h4("📌 遊戲說明"),
          tags$ul(
            tags$li("3 扇門：1 🚗 + 2 🐐"),
            tags$li("你先選門"),
            tags$li("主持人會開一扇🐐門"),
            tags$li("你可以換或不換")
          )
        ),
        
        h3("請選一扇門"),
        
        fluidRow(
          column(4, actionButton("door1", "🚪 Door 1", width="100%")),
          column(4, actionButton("door2", "🚪 Door 2", width="100%")),
          column(4, actionButton("door3", "🚪 Door 3", width="100%"))
        ),
        
        br(),
        actionButton("confirm", "✔ 確定選擇"),
        actionButton("reset", "🔄 重置"),
        
        br(), br(),
        h4(textOutput("status")),
        
        uiOutput("decision_ui"),
        
        h3(textOutput("final"))
      )
    }
  })
  
  # =========================
  # 🎯 狀態顯示
  # =========================
  output$status <- renderText({
    
    if (is.null(rv$temp_choice)) {
      "尚未選擇任何門"
      
    } else if (!rv$locked) {
      paste("你選擇 Door", rv$temp_choice)
      
    } else {
      paste("已選 Door", rv$choice,
            "｜主持人開 Door", rv$open)
    }
  })
  
  # =========================
  # 🔁 換 / 不換 UI
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
  # 🔁 換門
  # =========================
  observeEvent(input$switch, {
    
    rv$stage <- "final"
    
    z <- rbinom(1, 1, 2/3)
    
    other <- setdiff(1:3, c(rv$choice, rv$open))
    
    rv$final_open <- other
    rv$win <- (z == 1)
  })
  
  # =========================
  # 🙅 不換門
  # =========================
  observeEvent(input$stay, {
    
    rv$stage <- "final"
    
    z <- rbinom(1, 1, 1/3)
    
    rv$final_open <- rv$choice
    rv$win <- (z == 1)
  })
  
  # =========================
  # 🎯 結果
  # =========================
  output$final <- renderText({
    
    if (rv$stage != "final") return(NULL)
    
    if (rv$win) {
      paste("🎉 你贏了！Door", rv$final_open, "是🚗")
    } else {
      paste("🐐 你輸了！Door", rv$final_open, "是🐐")
    }
  })
  
  # =========================
  # 🔄 reset（回首頁）
  # =========================
  observeEvent(input$reset, {
    
    rv$temp_choice <- NULL
    rv$choice <- NULL
    rv$open <- NULL
    rv$final_open <- NULL
    rv$win <- NULL
    rv$locked <- FALSE
    rv$stage <- "choose"
    
    rv$page <- "home"
  })
}

shinyApp(ui, server)