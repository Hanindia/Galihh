library(shiny)

# Membuat data frame dengan data yang diberikan
data <- data.frame(
  Bulan = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des"),
  Pengunjung = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  Transaksi = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  Barang_Per_Transaksi = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  Rating = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  Iklan = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  Penjualan = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Melakukan regresi linear berganda
model <- lm(Penjualan ~ Pengunjung + Transaksi + Barang_Per_Transaksi + Rating + Iklan, data = data)
summary(model)

# Mendefinisikan UI
ui <- fluidPage(
  titlePanel("Dashboard Prediksi Penjualan"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pengunjung", "Masukkan jumlah pengunjung:", value = 200000),
      numericInput("transaksi", "Masukkan jumlah transaksi:", value = 10000),
      numericInput("barang", "Masukkan rata-rata barang per transaksi:", value = 5),
      sliderInput("rating", "Rating kepuasan pelanggan:", min = 1, max = 10, value = 8.5),
      numericInput("iklan", "Masukkan jumlah iklan online:", value = 30000),
      actionButton("predictButton", "Prediksi Penjualan")
    ),
    mainPanel(
      h4("Penjualan yang Diprediksi:"),
      textOutput("predictedSales"),
      plotOutput("histogram"),
      plotOutput("scatterplot")
    )
  )
)

# Mendefinisikan logika server
server <- function(input, output) {
  observeEvent(input$predictButton, {
    new_data <- data.frame(
      Pengunjung = input$pengunjung,
      Transaksi = input$transaksi,
      Barang_Per_Transaksi = input$barang,
      Rating = input$rating,
      Iklan = input$iklan
    )
    
    prediksi <- predict(model, newdata = new_data)
    output$predictedSales <- renderText({
      paste0("$", round(prediksi, 2))
    })
  })
  
  output$histogram <- renderPlot({
    hist(data$Penjualan, main = "Histogram Penjualan", xlab = "Penjualan", col = "pink", border = "grey")
  })
  
  output$scatterplot <- renderPlot({
    plot(data$Pengunjung, data$Penjualan, xlab = "Jumlah Pengunjung", ylab = "Penjualan", col = "blue")
  })
}

# Menjalankan aplikasi
shinyApp(ui = ui, server = server)




model_interaction <- lm(Penjualan ~ Pengunjung + Transaksi + Barang_Per_Transaksi + Rating + Iklan + Pengunjung * Iklan, data = data)
summary(model_interaction)
