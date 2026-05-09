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
        background-color: black;
      }

      /* 半透明遮罩 */
      body::before {
        content: '';
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0,0,0,0.05);
        z-index: -1;
      }

      /* 卡片 UI */
      .card {
        background-color: rgba(255,255,255,0.92);
        padding: 25px;
        border-radius: 15px;
        max-width: 900px;
        margin: auto;
        margin-top: 100px;
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
  # 🧭 頁面切換
  # =========================
  observeEvent(input$start, rv$page <- "game")
  observeEvent(input$about_btn, rv$page <- "about")
  observeEvent(input$back_home, rv$page <- "home")
  
  # =========================
  # 🚪 選門
  # =========================
  observeEvent(input$door1, { if (!rv$locked) rv$temp_choice <- 1 })
  observeEvent(input$door2, { if (!rv$locked) rv$temp_choice <- 2 })
  observeEvent(input$door3, { if (!rv$locked) rv$temp_choice <- 3 })
  
  # =========================
  # ✔ 確定 → 開門
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
  # 🎨 UI（首頁 / 遊戲 / 作者）
  # =========================
  output$page_ui <- renderUI({
    
    # ================= HOME =================
    if (rv$page == "home") {
      
      div(
        class = "card",
        style = "text-align:center;",
        
        h1("🎮 Monty Hall Game"),
        h4("經典機率直覺挑戰"),
        
        br(),
        
        actionButton("start", "🚪 開始遊戲", width = "200px"),
        br(),
        actionButton("about_btn", "👨‍💻 作者介紹", width = "200px")
      )
      
      # ================= GAME =================
    } else if (rv$page == "game") {
      
      div(
        class = "card",
        
        h2("🚪 Monty Hall"),
        
        wellPanel(
          h4("📌 遊戲說明"),
          tags$ul(
            tags$li("3 扇門：1 🚗 + 2 🐐"),
            tags$li("你先選門"),
            tags$li("主持人會打開一扇🐐門"),
            tags$li("你可以選擇換或不換")
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
        
        h3(textOutput("final")),
        
        br(),
        actionButton("back_home", "🏠 回首頁")
      )
      
      # ================= ABOUT =================
    } else if (rv$page == "about") {
      
      div(
        class = "card",
        
        h1("👨‍💻 作者介紹"),
        
        br(),
        
        h3("賴俊延"),
        
        p("這是一個使用 R Shiny 製作的 Monty Hall 機率互動遊戲。"),
        
        br(),
        
        h4("🔧 使用技術"),
        tags$ul(
          tags$li("R / Shiny"),
          tags$li("CSS"),
          tags$li("GitHub"),
          tags$li("shinyapps.io")
        ),
        
        br(),
        
        br(),
        
        h3("林采宣"),
        
        p("這是一個使用 R Shiny 製作的 Monty Hall 機率互動遊戲。"),
        
        br(),
        
        h4("🔧 使用技術"),
        tags$ul(
          tags$li("R / Shiny"),
          tags$li("CSS"),
          tags$li("GitHub"),
          tags$li("shinyapps.io")
        ),
        
        actionButton("back_home", "🏠 回首頁")
      )
    }
  })
  
  # =========================
  # 📊 狀態
  # =========================
  output$status <- renderText({
    if (is.null(rv$temp_choice)) {
      "尚未選擇任何門"
    } else if (!rv$locked) {
      paste("你選擇 Door", rv$temp_choice)
    } else {
      paste("已選 Door", rv$choice, "｜主持人開 Door", rv$open)
    }
  })
  
  # =========================
  # 🔁 決策 UI
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
    rv$final_open <- setdiff(1:3, c(rv$choice, rv$open))
    rv$win <- (rbinom(1, 1, 2/3) == 1)
  })
  
  # =========================
  # 🙅 不換
  # =========================
  observeEvent(input$stay, {
    rv$stage <- "final"
    rv$final_open <- rv$choice
    rv$win <- (rbinom(1, 1, 1/3) == 1)
  })
  
  # =========================
  # 🏠 reset
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
}

shinyApp(ui, server)