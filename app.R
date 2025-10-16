# 依赖
library(shiny)
library(dplyr)
library(scales)
library(tibble)

# 固定的运费区间
freight_table <- tibble::tribble(
  ~low, ~high, ~price,
   30,    40,   26000,
   40,    50,   26000,
   50,    60,   27000,
   60,    70,   27000,
   70,    80,   27500,
   80,    90,   28000
)

# 计算函数
calc_total_cost <- function(weight, purchase_cost_wan) {
  row <- freight_table %>% 
    dplyr::filter(purchase_cost_wan >= low, purchase_cost_wan < high)

  if (nrow(row) == 0 && purchase_cost_wan < min(freight_table$low)) {
    row <- freight_table[1, ]
  }
  if (nrow(row) == 0 && purchase_cost_wan >= max(freight_table$high)) {
    row <- freight_table[nrow(freight_table), ]
  }

  freight_per_ton <- row$price[1]
  purchase_cost_yuan <- purchase_cost_wan * 10000

  # 正常运费
  freight_total <- freight_per_ton * weight
  total_cost    <- purchase_cost_yuan + freight_total
  cost_per_ton  <- total_cost / weight

  # 运费+1200
  freight_per_ton_plus1200 <- freight_per_ton + 1200
  freight_total_plus1200   <- freight_per_ton_plus1200 * weight
  total_cost_plus1200      <- purchase_cost_yuan + freight_total_plus1200
  cost_per_ton_plus1200    <- total_cost_plus1200 / weight

  list(
    weight = weight,
    purchase_cost_wan = purchase_cost_wan,
    freight_per_ton = freight_per_ton,
    freight_total = freight_total,
    total_cost = total_cost,
    cost_per_ton = cost_per_ton,
    freight_per_ton_plus1200 = freight_per_ton_plus1200,
    freight_total_plus1200 = freight_total_plus1200,
    total_cost_plus1200 = total_cost_plus1200,
    cost_per_ton_plus1200 = cost_per_ton_plus1200
  )
}

# UI
ui <- fluidPage(
  titlePanel("整柜总成本计算（按采购价区间套运费）"),
  fluidRow(
    column(
      width = 4,
      wellPanel(
        numericInput("weight", "重量（吨）", value = 28, min = 0, step = 0.01),
        numericInput("purchase", "采购价（万元）", value = 48.384, min = 0, step = 0.001),
        helpText("说明：采购价单位为“万元人民币”；运费单位是“元/吨”。"),
        actionButton("go", "计算", class = "btn-primary")
      )
    ),
    column(
      width = 8,
      h4("结果"),
      uiOutput("summary"),
      tags$hr(),
      h4("明细"),
      tableOutput("detail")
    )
  )
)

# Server
server <- function(input, output, session) {
  result <- eventReactive(input$go, {
    req(input$weight, input$purchase)
    validate(
      need(input$weight > 0, "重量必须大于 0"),
      need(input$purchase >= 0, "采购价不能为负数")
    )
    calc_total_cost(weight = input$weight, purchase_cost_wan = input$purchase)
  })

  output$summary <- renderUI({
    req(result()); r <- result()
    tagList(
      div(
        style = "font-size:16px; line-height:1.8;",
        HTML(sprintf("每吨运费：<b>%s 元/吨</b>", comma(r$freight_per_ton))), tags$br(),
        HTML(sprintf("运费总额：<b>%s 元</b>", comma(r$freight_total))), tags$br(),
        HTML(sprintf("总价（采购价+运费）：<b>%s 元</b>", comma(r$total_cost))), tags$br(),
        HTML(sprintf("单吨总成本：<b>%s 元/吨</b>", comma(round(r$cost_per_ton, 2)))),

        tags$hr(),
        HTML(sprintf("每吨运费+1200 后：<b>%s 元/吨</b>", comma(r$freight_per_ton_plus1200))), tags$br(),
        HTML(sprintf("运费总额（+1200）：<b>%s 元</b>", comma(r$freight_total_plus1200))), tags$br(),
        HTML(sprintf("总价（+1200）：<b>%s 元</b>", comma(r$total_cost_plus1200))), tags$br(),
        HTML(sprintf("单吨总成本（+1200）：<b>%s 元/吨</b>", comma(round(r$cost_per_ton_plus1200, 2))))
      )
    )
  })

  output$detail <- renderTable({
    req(result()); r <- result()
    data.frame(
      指标 = c("重量（吨）", "采购价（万元）", 
               "每吨运费（元/吨）", "运费总额（元）", "总价（元）", "单吨总成本（元/吨）",
               "每吨运费+1200（元/吨）", "运费总额（+1200）（元）", "总价（+1200）（元）", "单吨总成本（+1200）（元/吨）"),
      数值 = c(
        sprintf("%.2f", r$weight),
        sprintf("%.2f", r$purchase_cost_wan),
        comma(r$freight_per_ton),
        comma(r$freight_total),
        comma(r$total_cost),
        comma(round(r$cost_per_ton, 2)),
        comma(r$freight_per_ton_plus1200),
        comma(r$freight_total_plus1200),
        comma(r$total_cost_plus1200),
        comma(round(r$cost_per_ton_plus1200, 2))
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, digits = 2)
}

shinyApp(ui, server)
