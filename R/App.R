library(shiny)

ui <- fluidPage(
  
  titlePanel("🚪 三門問題 Monty Hall 模擬"),
  
  sidebarLayout(
    sidebarPanel(
      h4("操作說明"),
      p("1️⃣ 選一個門"),
      p("2️⃣ 主持人會開一扇羊門"),
      p("3️⃣ 決定要不要換門"),
      p("4️⃣ 看結果"),
      
      actionButton("reset", "🔄 重新開始")
    ),
    
    mainPanel(
      
      h3("請選一個門"),
      
      fluidRow(
        column(4, actionButton("door1", "🚪 Door 1", width = "100%")),
        column(4, actionButton("door2", "🚪 Door 2", width = "100%")),
        column(4, actionButton("door3", "🚪 Door 3", width = "100%"))
      ),
      
      br(),
      h3(textOutput("status")),
      
      uiOutput("host_ui"),
      uiOutput("result_ui")
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    car = sample(1:3, 1),
    choice = NULL,
    open = NULL,
    stage = "init"
  )
  
  # 重置遊戲
  observeEvent(input$reset, {
    rv$car <- sample(1:3, 1)
    rv$choice <- NULL
    rv$open <- NULL
    rv$stage <- "init"
  })
  
  # 玩家選門
  observeEvent(input$door1, { rv$choice <- 1; rv$stage <- "picked" })
  observeEvent(input$door2, { rv$choice <- 2; rv$stage <- "picked" })
  observeEvent(input$door3, { rv$choice <- 3; rv$stage <- "picked" })
  
  # 主持人開門
  observeEvent(rv$stage == "picked", {
    
    req(rv$choice)
    
    available <- setdiff(1:3, c(rv$choice, rv$car))
    rv$open <- sample(available, 1)
    
    rv$stage <- "reveal"
  })
  
  # 換門
  observeEvent(input$stay, {
    rv$stage <- "final"
  })
  
  observeEvent(input$switch, {
    
    rv$choice <- setdiff(1:3, c(rv$choice, rv$open))
    rv$choice <- rv$choice[1]
    
    rv$stage <- "final"
  })
  
  # 狀態文字
  output$status <- renderText({
    
    if (rv$stage == "init") {
      "請選一個門 🚪"
    } else if (rv$stage == "picked") {
      paste("你選了門", rv$choice, "，主持人正在開門...")
    } else if (rv$stage == "reveal") {
      paste("主持人打開門", rv$open, "是 🐐")
    } else {
      "結果已揭曉"
    }
  })
  
  # 是否要換門 UI
  output$host_ui <- renderUI({
    
    if (rv$stage == "reveal") {
      tagList(
        h4(paste("主持人打開門", rv$open, "是 🐐")),
        h4("你要換門嗎？"),
        
        actionButton("switch", "🔁 換門"),
        actionButton("stay", "🙅 不換")
      )
    }
  })
  
  # 結果
  output$result_ui <- renderUI({
    
    if (rv$stage == "final") {
      
      if (rv$choice == rv$car) {
        h2("🎉 恭喜你得到 🚗 跑車！")
      } else {
        h2(paste("🐐 你選到山羊（跑車在", rv$car, "號門）"))
      }
    }
  })
}

shinyApp(ui, server)