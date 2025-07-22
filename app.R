#----------------------------------------------------------------#
#                 KODE APLIKASI SHINY LENGKAP                    #
#----------------------------------------------------------------#

# 1. PASTIKAN SEMUA LIBRARY INI SUDAH TERINSTALL
# install.packages(c("shiny", "shinydashboard", "readxl", "DT", "officer", "flextable", "ggplot2", "dplyr", "sf", "leaflet", "car", "nortest", "EnvStats", "lmtest", "cluster", "factoextra", "fpc"))

library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(sf)
library(leaflet) 
library(car)
library(nortest)
library(EnvStats) 
library(lmtest) # Ditambahkan untuk uji asumsi regresi
library(cluster) # Untuk analisis clustering
library(factoextra) # Untuk visualisasi clustering
library(fpc) # Untuk validasi clustering

#================================================================#
#                           UI (USER INTERFACE)                  #
#================================================================#
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Dashboard Sosial"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("database")),
      menuItem("Uji Asumsi", tabName = "uji_asumsi", icon = icon("flask")),
      menuItem("Uji Beda Rata-rata", tabName = "uji_rata", icon = icon("balance-scale")),
      menuItem("Uji Varians", tabName = "uji_varians", icon = icon("chart-area")),
      menuItem("Uji Proporsi", tabName = "uji_proporsi", icon = icon("percentage")),
      menuItem("ANOVA (>2 Kelompok)", tabName = "anova", icon = icon("braille")),
      # --- MENU BARU DITAMBAHKAN DI SINI --- #
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart")),
      menuItem("Analisis Clustering K-Means", tabName = "clustering", icon = icon("project-diagram")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi_data", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .selectize-dropdown-content .option {
          white-space: normal !important;
          word-wrap: break-word;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "Selamat Datang di Dashboard Analisis Sosial",
                  width = 12, solidHeader = TRUE, status = "primary",
                  h4("Tentang Dashboard Ini"),
                  p("Dashboard ini dirancang untuk menganalisis dan memvisualisasikan data kondisi sosial per kabupaten/kota."),
                  h4("Metadata"),
                  p("Data yang digunakan: 'Kondisi_Sosial_Per_KabKota_Excel.xlsx' dan 'Peta_Kabupaten_Kota.geojson'. Pastikan kedua file ini berada di folder yang sama dengan aplikasi."),
                  p("Dibuat menggunakan R dan Shiny."),
                  h4("Cara Penggunaan"),
                  p("Gunakan menu di samping untuk bernavigasi. 'Manajemen Data' untuk kategorisasi, 'Uji Asumsi' untuk tes statistik, dan 'Eksplorasi Data' untuk visualisasi.")
                )
              )
      ),
      
      tabItem(tabName = "manajemen_data",
              fluidRow(
                box(
                  title = "Pengaturan Kategorisasi",
                  width = 4,
                  solidHeader = TRUE,
                  status = "primary",
                  uiOutput("variabel_selector"),
                  selectInput("jumlah_kategori", "Pilih Jumlah Kategori:",
                              choices = c(2, 3, 5), selected = 3),
                  hr(),
                  h4("Unduh Hasil"),
                  downloadButton("download_data", "Unduh Data (CSV)"),
                  downloadButton("download_interpretasi", "Unduh Interpretasi (TXT)")
                ),
                box(
                  title = "Hasil Kategorisasi Data",
                  width = 8,
                  solidHeader = TRUE,
                  status = "primary",
                  DTOutput("tabel_data"),
                  hr(),
                  h4("Interpretasi"),
                  verbatimTextOutput("interpretasi_output")
                )
              )
      ),
      
      tabItem(tabName = "uji_asumsi",
              fluidRow(
                box(
                  title = "Uji Normalitas",
                  width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                  fluidRow(
                    column(width = 4,
                           h4("Pengaturan"),
                           uiOutput("norm_var_selector"),
                           radioButtons("norm_test_method", "Pilih Metode Uji:",
                                        choices = c("Shapiro-Wilk" = "shapiro",
                                                    "Kolmogorov-Smirnov (Lilliefors)" = "ks"),
                                        selected = "shapiro")
                    ),
                    column(width = 8,
                           h4(textOutput("norm_test_title")),
                           verbatimTextOutput("norm_test_result"),
                           hr(),
                           h5("Interpretasi:", style = "font-weight:bold;"),
                           textOutput("norm_test_interpretation")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Uji Homogenitas Varian (Levene's Test)",
                  width = 12, solidHeader = TRUE, status = "success", collapsible = TRUE,
                  fluidRow(
                    column(width = 4,
                           h4("Pengaturan"),
                           p("Uji ini memerlukan grup yang dibuat di tab 'Manajemen Data'."),
                           uiOutput("homog_var_selector"),
                           p(strong("Variabel Grup:"), "'Kategori' dari tab Manajemen Data.")
                    ),
                    column(width = 8,
                           h4("Hasil"),
                           verbatimTextOutput("homog_test_result"),
                           hr(),
                           h5("Interpretasi:", style = "font-weight:bold;"),
                           textOutput("homog_test_interpretation")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Unduh Hasil", width = 12,
                  downloadButton("download_asumsi", "Unduh Semua Hasil Uji (Word)")
                )
              )
      ),
      
      tabItem(tabName = "uji_rata",
              fluidRow(
                box(title = "Uji-t Satu Sampel", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                    fluidRow(
                      column(width = 4,
                             h4("Pengaturan"),
                             uiOutput("ttest1_var_selector"),
                             numericInput("ttest1_mu", "Nilai Hipotesis Rata-rata (μ):", 0)
                      ),
                      column(width = 8,
                             h4("Hasil Uji"),
                             verbatimTextOutput("ttest1_result"),
                             hr(),
                             h5("Interpretasi:", style = "font-weight:bold;"),
                             textOutput("ttest1_interpretation")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Uji-t Dua Sampel Independen", width = 12, solidHeader = TRUE, status = "success", collapsible = TRUE,
                    fluidRow(
                      column(width = 4,
                             h4("Pengaturan"),
                             p("Uji ini memerlukan grup (2 kategori) yang dibuat di tab 'Manajemen Data'."),
                             uiOutput("ttest2_var_selector"),
                             p(strong("Variabel Grup:"), "'Kategori' (2 Level) dari tab Manajemen Data.")
                      ),
                      column(width = 8,
                             h4("Hasil Uji"),
                             verbatimTextOutput("ttest2_result"),
                             hr(),
                             h5("Interpretasi:", style = "font-weight:bold;"),
                             textOutput("ttest2_interpretation")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Unduh Hasil", width = 12,
                    downloadButton("download_uji_rata", "Unduh Hasil Uji Rata-rata (Word)")
                )
              )
      ),
      
      tabItem(tabName = "uji_varians",
              fluidRow(
                box(title = "Uji Varians Satu Sampel (Chi-Square Test)", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                    fluidRow(
                      column(width = 4,
                             h4("Pengaturan"),
                             uiOutput("var1_var_selector"),
                             numericInput("var1_sigma_sq", "Nilai Hipotesis Varians (σ²):", 1, min = 0)
                      ),
                      column(width = 8,
                             h4("Hasil Uji"),
                             uiOutput("var1_test_result_table"),
                             hr(),
                             h5("Interpretasi:", style = "font-weight:bold;"),
                             textOutput("var1_test_interpretation")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Uji Varians Dua Sampel (F-Test)", width = 12, solidHeader = TRUE, status = "success", collapsible = TRUE,
                    fluidRow(
                      column(width = 4,
                             h4("Pengaturan"),
                             p("Uji ini memerlukan grup (2 kategori) yang dibuat di tab 'Manajemen Data'."),
                             uiOutput("var2_var_selector"),
                             p(strong("Variabel Grup:"), "'Kategori' (2 Level) dari tab Manajemen Data.")
                      ),
                      column(width = 8,
                             h4("Hasil Uji"),
                             uiOutput("var2_test_result_table"),
                             hr(),
                             h5("Interpretasi:", style = "font-weight:bold;"),
                             textOutput("var2_test_interpretation")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Unduh Hasil", width = 12,
                    downloadButton("download_uji_varians", "Unduh Hasil Uji Varians (Word)")
                )
              )
      ),
      
      tabItem(tabName = "uji_proporsi",
              fluidRow(
                box(
                  title = "Langkah 1: Persiapan Variabel Proporsi", width = 12, solidHeader = TRUE, status = "info",
                  p("Uji proporsi memerlukan variabel biner (Dua kategori, misal: 'Tinggi'/'Rendah'). Buat variabel tersebut di sini."),
                  column(width = 4,
                         uiOutput("prop_var_selector"),
                         selectInput("prop_threshold_method", "Metode Penentuan Batas:",
                                     choices = c("Median", "Rata-rata (Mean)", "Nilai Kustom")),
                         uiOutput("prop_custom_threshold_ui")
                  ),
                  column(width = 4,
                         textInput("prop_label_sukses", "Label untuk Kategori 'Sukses' (di atas batas):", "Tinggi"),
                         textInput("prop_label_gagal", "Label untuk Kategori 'Gagal' (di bawah batas):", "Rendah"),
                         actionButton("prop_generate_button", "Buat Variabel Biner", icon = icon("cogs"))
                  ),
                  column(width = 4,
                         h4("Status"),
                         verbatimTextOutput("prop_status_output")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Uji Proporsi Satu Sampel", width = 6, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                  p("Menguji apakah proporsi kategori 'sukses' pada data sama dengan nilai hipotesis."),
                  hr(),
                  numericInput("prop1_p_hipotesis", "Nilai Proporsi Hipotesis (antara 0 dan 1):", 0.5, min = 0, max = 1, step = 0.01),
                  h4("Hasil Uji"),
                  uiOutput("prop1_test_result_table"),
                  hr(),
                  h5("Interpretasi:", style = "font-weight:bold;"),
                  textOutput("prop1_test_interpretation")
                ),
                box(
                  title = "Uji Proporsi Dua Sampel", width = 6, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                  p("Membandingkan proporsi kategori 'sukses' antara dua kelompok. Kelompok dibuat di tab 'Manajemen Data'."),
                  hr(),
                  uiOutput("prop2_group_selectors_ui"),
                  h4("Hasil Uji"),
                  uiOutput("prop2_test_result_table"),
                  hr(),
                  h5("Interpretasi:", style = "font-weight:bold;"),
                  textOutput("prop2_test_interpretation")
                )
              ),
              fluidRow(
                box(title = "Unduh Hasil", width = 12,
                    downloadButton("download_uji_proporsi", "Unduh Hasil Uji Proporsi (Word)")
                )
              )
      ),
      
      tabItem(tabName = "anova",
              fluidRow(
                box(
                  title = "ANOVA Satu Arah (One-Way ANOVA)", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                  fluidRow(
                    column(width = 4,
                           h4("Pengaturan"),
                           p("Uji ini membandingkan rata-rata variabel numerik berdasarkan kelompok yang dibuat di tab 'Manajemen Data'."),
                           uiOutput("anova1_var_selector"),
                           p(strong("Variabel Grup:"), "'Kategori' dari tab Manajemen Data.")
                    ),
                    column(width = 8,
                           h4("Hasil Tabel ANOVA"),
                           uiOutput("anova1_result_table"),
                           hr(),
                           h4("Uji Lanjutan (Post-Hoc Tukey HSD)"),
                           p("Tabel ini menunjukkan perbandingan antar-pasang kelompok. Jika p-value < 0.05, maka rata-rata kedua kelompok tersebut berbeda signifikan."),
                           uiOutput("anova1_posthoc_table"),
                           hr(),
                           h5("Interpretasi Keseluruhan:", style = "font-weight:bold;"),
                           textOutput("anova1_interpretation")
                    )
                  )
                )
              ),
              fluidRow(
                box(title = "Unduh Hasil", width = 12,
                    downloadButton("download_anova", "Unduh Hasil ANOVA (Word)")
                )
              )
      ),
      
      # --- KONTEN HALAMAN BARU DITAMBAHKAN DI SINI --- #
      tabItem(tabName = "clustering",
              fluidRow(
                box(
                  title = "Pengaturan Analisis Clustering K-Means", width = 4, solidHeader = TRUE, status = "primary",
                  h4("Pilih Variabel untuk Clustering"),
                  uiOutput("clustering_var_selector"),
                  helpText("Pilih variabel numerik yang akan digunakan dalam analisis clustering."),
                  
                  hr(),
                  h4("Pengaturan Clustering"),
                  numericInput("num_clusters", "Jumlah Cluster (k):", value = 3, min = 2, max = 10),
                  checkboxInput("use_distance_matrix", "Gunakan Matriks Penimbang Jarak", value = TRUE),
                  helpText("Jika dicentang, akan menggunakan matriks penimbang jarak dari file Excel."),
                  
                  hr(),
                  actionButton("run_clustering", "Jalankan Analisis Clustering", icon = icon("play-circle"), class = "btn-primary"),
                  
                  hr(),
                  h4("Unduh Hasil"),
                  downloadButton("download_clustering", "Unduh Hasil Clustering (Word)")
                ),
                box(
                  title = "Hasil Clustering", width = 8, solidHeader = TRUE, status = "primary",
                  tabsetPanel(
                    tabPanel("Ringkasan Hasil",
                             h4("Informasi Cluster"),
                             DTOutput("cluster_summary_table"),
                             hr(),
                             h4("Statistik Cluster"),
                             verbatimTextOutput("cluster_stats")
                    ),
                    tabPanel("Daftar Wilayah per Cluster",
                             h4("Daftar Nama Wilayah Berdasarkan Cluster"),
                             helpText("Tabel ini menunjukkan nama wilayah (kabupaten/kota) dan cluster yang mereka masuki."),
                             DTOutput("region_cluster_table"),
                             hr(),
                             h4("Ringkasan Jumlah Wilayah per Cluster"),
                             DTOutput("cluster_count_table")
                    ),
                    tabPanel("Validasi Cluster",
                             h4("Metrik Validasi Clustering"),
                             DTOutput("cluster_validation_table"),
                             hr(),
                             h4("Interpretasi"),
                             verbatimTextOutput("cluster_interpretation")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Visualisasi Clustering", width = 6, solidHeader = TRUE, status = "info",
                  plotOutput("cluster_plot", height = "400px")
                ),
                box(
                  title = "Elbow Method untuk Menentukan k Optimal", width = 6, solidHeader = TRUE, status = "info",
                  plotOutput("elbow_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Peta Clustering (Interaktif)", width = 12, solidHeader = TRUE, status = "success",
                  leafletOutput("cluster_map", height = "500px")
                )
              )
      ),
      
      tabItem(tabName = "regresi",
              fluidRow(
                box(
                  title = "Pengaturan Regresi Linear Berganda", width = 4, solidHeader = TRUE, status = "primary",
                  uiOutput("reg_var_dependen_selector"),
                  uiOutput("reg_var_independen_selector"),
                  
                  # --- BARIS INSTRUKSI DITAMBAHKAN DI SINI --- #
                  helpText("Tahan tombol Ctrl (Windows) atau Cmd (Mac) untuk memilih beberapa variabel."),
                  
                  actionButton("run_regression", "Jalankan Analisis Regresi", icon = icon("play-circle")),
                  hr(),
                  h4("Unduh Hasil"),
                  downloadButton("download_regresi", "Unduh Hasil Regresi (Word)")
                ),
                box(
                  title = "Hasil Model Regresi Linear Berganda", width = 8, solidHeader = TRUE, status = "primary",
                  verbatimTextOutput("reg_model_summary")
                )
              ),
              fluidRow(
                box(
                  title = "Uji Asumsi Klasik", width = 12, solidHeader = TRUE, status = "info", collapsible = TRUE, collapsed = FALSE,
                  tabsetPanel(
                    tabPanel("1. Normalitas Residual", 
                             fluidRow(
                               column(width = 4,
                                      h4("Pengaturan Uji Normalitas"),
                                      radioButtons("reg_norm_test_method", "Pilih Metode Uji:",
                                                   choices = c("Shapiro-Wilk" = "shapiro", "Kolmogorov-Smirnov (Lilliefors)" = "ks"),
                                                   selected = "shapiro")
                               ),
                               column(width = 8,
                                      h4(textOutput("reg_norm_test_title")),
                                      verbatimTextOutput("reg_norm_test_result"),
                                      h5("Interpretasi:", style = "font-weight:bold;"),
                                      textOutput("reg_norm_test_interpretation")
                               )
                             )
                    ),
                    tabPanel("2. Autokorelasi (Durbin-Watson)",
                             h4("Hasil Uji Durbin-Watson"),
                             verbatimTextOutput("autokorelasi_test_result"),
                             h5("Interpretasi:", style = "font-weight:bold;"),
                             textOutput("autokorelasi_test_interpretation")
                    ),
                    tabPanel("3. Multikolinearitas (VIF)",
                             h4("Hasil Uji Variance Inflation Factor (VIF)"),
                             p("Nilai VIF > 10 (beberapa ahli menyebut > 5) mengindikasikan adanya multikolinearitas."),
                             DTOutput("multikolinearitas_test_result"),
                             h5("Interpretasi:", style = "font-weight:bold;"),
                             textOutput("multikolinearitas_test_interpretation")
                    ),
                    tabPanel("4. Homoskedastisitas (Breusch-Pagan)",
                             h4("Hasil Uji Breusch-Pagan"),
                             verbatimTextOutput("homoskedastisitas_test_result"),
                             h5("Interpretasi:", style = "font-weight:bold;"),
                             textOutput("homoskedastisitas_test_interpretation")
                    )
                  )
                )
              )
      ),
      
      tabItem(tabName = "eksplorasi_data",
              fluidRow(
                box(
                  title = "Pengaturan Eksplorasi", width = 4, status = "info", solidHeader = TRUE,
                  selectInput("variabel_eksplorasi", "Pilih Variabel:", choices = NULL),
                  radioButtons("pilihan_peringkat", "Tampilkan Peringkat:",
                               choices = c("10 Tertinggi" = "top", "10 Terendah" = "bottom"),
                               selected = "top"),
                  hr(),
                  downloadButton("unduh_laporan", "Unduh Laporan (Word)"),
                  downloadButton("unduh_gabungan", "Unduh Semua Halaman")
                ),
                box(
                  title = "Statistik Deskriptif", width = 8, status = "info", solidHeader = TRUE,
                  tableOutput("summary_table"),
                  verbatimTextOutput("interpretasi_stat")
                )
              ),
              fluidRow(
                box(
                  title = "Diagram Batang Peringkat Kabupaten/Kota", width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("peringkat_plot", height = "450px")
                )
              ),
              fluidRow(
                box(
                  title = "Peta Interaktif (OSM - Memerlukan Internet)",
                  width = 12, status = "info", solidHeader = TRUE,
                  leafletOutput("peta_interaktif", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Peta Statis (Offline)",
                  width = 12, status = "success", solidHeader = TRUE,
                  plotOutput("peta_statis", height = "600px")
                )
              )
      )
    )
  )
)

#================================================================#
#                           SERVER (LOGIC)                       #
#================================================================#
server <- function(input, output, session) {
  
  nama_kolom_kabupaten <- "Nama_Kab_Kota" 
  nama_kolom_kode <- "Kode_Kab_Kota"
  
  jumlah_vars <- c(
    "Jumlah_Penduduk_Dibawah_Lima_Tahun", "Jumlah_Penduduk_Perempuan",
    "Jumlah_Penduduk_Diatas_Enam_Puluh_Lima_Tahun",
    "Jumlah_Penduduk_Usia_Lima_Belas_Tahun_Ke_Atas_Berpendidikan_Rendah",
    "Jumlah_Penduduk_Miskin", "Jumlah_Penduduk_Tidak_Bisa_Baca_Tulis", "Jumlah_Penduduk"
  )
  
  data_sosial <- reactive({
    req(file.exists("Kondisi_Sosial_Per_KabKota_Excel.xlsx"))
    read_excel("Kondisi_Sosial_Per_KabKota_Excel.xlsx")
  })
  
  geojson_data <- reactive({
    req(file.exists("Peta_Kabupaten_Kota.geojson"))
    sf::st_read("Peta_Kabupaten_Kota.geojson")
  })
  
  # Memuat matriks penimbang jarak
  distance_matrix <- reactive({
    req(file.exists("Matriks_Penimbang_Jarak.xlsx"))
    tryCatch({
      read_excel("Matriks_Penimbang_Jarak.xlsx")
    }, error = function(e) {
      NULL
    })
  })
  
  #--- LOGIKA UNTUK MANAJEMEN DATA ---#
  output$variabel_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("variabel", "Pilih Variabel untuk Dikategorikan:",
                choices = kolom_numerik, selected = kolom_numerik[1])
  })
  
  kategori_info <- reactive({
    req(input$variabel, input$jumlah_kategori)
    df <- data_sosial()
    validate(
      need(nama_kolom_kabupaten %in% names(df), paste("Eror: Kolom '", nama_kolom_kabupaten, "' tidak ditemukan.")),
      need(nama_kolom_kode %in% names(df), paste("Eror: Kolom '", nama_kolom_kode, "' tidak ditemukan."))
    )
    variabel_dipilih <- input$variabel
    jumlah_kategori <- as.numeric(input$jumlah_kategori)
    labels <- switch(input$jumlah_kategori,
                     "2" = c("Rendah", "Tinggi"),
                     "3" = c("Rendah", "Sedang", "Tinggi"),
                     "5" = c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi"))
    data_vector <- df[[variabel_dipilih]]
    min_val <- min(data_vector, na.rm = TRUE)
    max_val <- max(data_vector, na.rm = TRUE)
    breaks <- seq(min_val, max_val, length.out = jumlah_kategori + 1)
    kategori_values <- cut(data_vector, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
    df$Kategori <- kategori_values
    list(data = df,
         breaks = breaks, 
         labels = labels)
  })
  
  output$tabel_data <- renderDT({
    req(input$variabel)
    data_to_show <- kategori_info()$data %>% select(all_of(nama_kolom_kode), all_of(nama_kolom_kabupaten), all_of(input$variabel), Kategori)
    dt <- datatable(data_to_show, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE), selection = 'none')
    if (input$variabel %in% jumlah_vars) {
      dt <- dt %>% formatRound(columns = input$variabel, digits = 0, mark = "")
    } else {
      dt <- dt %>% formatRound(columns = input$variabel, digits = 2, mark = "", dec.mark = ".")
    }
    dt
  })
  
  output$interpretasi_output <- renderText({
    info <- kategori_info()
    rentang_teks <- ""
    for (i in 1:length(info$labels)) {
      penutup <- ifelse(i == length(info$labels), "]", ")")
      batas_bawah <- formatC(info$breaks[i], format = "f", digits = 2, decimal.mark = ".")
      batas_atas <- formatC(info$breaks[i+1], format = "f", digits = 2, decimal.mark = ".")
      rentang_teks <- paste0(rentang_teks, sprintf("   - %-15s: [%s - %s%s\n", info$labels[i], batas_bawah, batas_atas, penutup))
    }
    paste0("Interpretasi:\n\nVariabel '", input$variabel, "' dikategorikan menjadi ",
           length(info$labels), " kelompok.\nMetode: Equal Interval.\n\nRentang Nilai:\n", rentang_teks)
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("hasil_kategorisasi_", input$variabel, ".csv"),
    content = function(file) {
      write.csv(kategori_info()$data, file, row.names = FALSE)
    }
  )
  
  output$download_interpretasi <- downloadHandler(
    filename = function() paste0("interpretasi_", input$variabel, ".txt"),
    content = function(file) {
      writeLines(output$interpretasi_output(), file)
    }
  )
  
  #--- LOGIKA UNTUK UJI ASUMSI ---#
  output$norm_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("norm_var", "Pilih Variabel Numerik:", choices = kolom_numerik)
  })
  
  output$homog_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("homog_var", "Pilih Variabel Numerik:", choices = kolom_numerik)
  })
  
  norm_test_output <- reactive({
    req(input$norm_var, input$norm_test_method)
    data <- data_sosial()[[input$norm_var]]
    if (input$norm_test_method == "shapiro") {
      if (length(data) >= 3 && length(data) <= 5000) {
        shapiro.test(data)
      } else {
        list(method = "Shapiro-Wilk", 
             error = "Data harus berisi antara 3 s/d 5000 amatan.")
      }
    } else {
      lillie.test(data)
    }
  })
  
  output$norm_test_title <- renderText({
    req(input$norm_test_method)
    if (input$norm_test_method == "shapiro") "Hasil Uji Normalitas (Shapiro-Wilk)" else "Hasil Uji Normalitas (Kolmogorov-Smirnov - Lilliefors)"
  })
  
  output$norm_test_result <- renderPrint({
    test_result <- norm_test_output(); req(test_result)
    if (!is.null(test_result$error)) cat(test_result$error) else test_result
  })
  
  output$norm_test_interpretation <- renderText({
    test_result <- norm_test_output(); req(test_result)
    if (!is.null(test_result$error)) return("")
    p_value <- test_result$p.value
    if (p_value > 0.05) paste0("Kesimpulan: P-value (", round(p_value, 4), ") > 0.05, maka data berdistribusi normal.") else paste0("Kesimpulan: P-value (", round(p_value, 4), ") <= 0.05, maka data tidak berdistribusi normal.")
  })
  
  homog_test_output <- reactive({
    req(input$homog_var, input$variabel, kategori_info())
    validate(need(input$homog_var == input$variabel, message = "Variabel yang diuji harus sama dengan variabel yang dikategorikan di 'Manajemen Data'."))
    data_kategori <- kategori_info()$data
    if (length(unique(data_kategori$Kategori)) > 1) {
      leveneTest(as.formula(paste("`", input$homog_var, "` ~ Kategori", sep="")), data = data_kategori)
    }
  })
  
  output$homog_test_result <- renderPrint({
    data_kategori <- tryCatch(kategori_info()$data, error = function(e) NULL)
    if(is.null(data_kategori) || length(unique(data_kategori$Kategori)) <= 1) return("Pastikan ada > 1 grup di 'Manajemen Data'.")
    req(homog_test_output())
  })
  
  output$homog_test_interpretation <- renderText({
    test_result <- homog_test_output(); req(test_result)
    p_value <- test_result$`Pr(>F)`[1]
    if (is.na(p_value)) return("Tidak dapat mengambil P-value.")
    if (p_value > 0.05) paste0("Kesimpulan: P-value (", round(p_value, 4), ") > 0.05, maka varian antar kelompok homogen.") else paste0("Kesimpulan: P-value (", round(p_value, 4), ") <= 0.05, maka varian antar kelompok tidak homogen.")
  })
  
  output$download_asumsi <- downloadHandler(
    filename = function() paste0("hasil_uji_asumsi_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- read_docx() %>% body_add_par("Hasil Uji Asumsi", style = "heading 1")
      doc %>% body_add_par(if(input$norm_test_method == "shapiro") "Uji Normalitas (Shapiro-Wilk)" else "Uji Normalitas (Kolmogorov-Smirnov)", style = "heading 2")
      doc %>% body_add_par(paste("Variabel:", input$norm_var))
      doc %>% body_add_par(paste(capture.output(norm_test_output()), collapse="\n"))
      doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$norm_test_interpretation())
      doc %>% body_add_par("Uji Homogenitas Varian (Levene's Test)", style = "heading 2")
      doc %>% body_add_par(paste("Variabel:", input$homog_var, "| Grup:", input$variabel))
      homog_output <- tryCatch(homog_test_output(), error = function(e) e)
      if (!inherits(homog_output, "error")) {
        df_homog <- as.data.frame(homog_output) %>% tibble::rownames_to_column("Sumber")
        doc %>% body_add_flextable(flextable(df_homog) %>% autofit())
        doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$homog_test_interpretation())
      } else {
        doc %>% body_add_par(paste("Error:", as.character(homog_output)))
      }
      print(doc, target = file)
    }
  )
  
  #--- LOGIKA UNTUK UJI BEDA RATA-RATA ---#
  output$ttest1_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("ttest1_var", "Pilih Variabel Numerik:", choices = kolom_numerik)
  })
  
  ttest1_output <- reactive({
    req(input$ttest1_var)
    t.test(data_sosial()[[input$ttest1_var]], mu = input$ttest1_mu)
  })
  
  output$ttest1_result <- renderPrint({ ttest1_output() })
  
  output$ttest1_interpretation <- renderText({
    p_value <- ttest1_output()$p.value
    if (p_value > 0.05) paste0("Kesimpulan: P-value (", round(p_value, 4), ") > 0.05. Rata-rata sampel tidak berbeda signifikan dari nilai hipotesis (", input$ttest1_mu, ").") else paste0("Kesimpulan: P-value (", round(p_value, 4), ") <= 0.05. Rata-rata sampel berbeda signifikan dari nilai hipotesis (", input$ttest1_mu, ").")
  })
  
  output$ttest2_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("ttest2_var", "Pilih Variabel Numerik:", choices = kolom_numerik)
  })
  
  ttest2_output <- reactive({
    req(input$ttest2_var, kategori_info())
    data_kat <- kategori_info()$data
    validate(need(length(levels(data_kat$Kategori)) == 2, "Uji ini memerlukan tepat 2 kategori."))
    t.test(as.formula(paste("`", input$ttest2_var, "` ~ Kategori", sep="")), data = data_kat, var.equal = TRUE)
  })
  
  output$ttest2_result <- renderPrint({ ttest2_output() })
  
  output$ttest2_interpretation <- renderText({
    p_value <- ttest2_output()$p.value
    if (p_value > 0.05) "Kesimpulan: Tidak ada perbedaan rata-rata yang signifikan antara kedua kelompok." else "Kesimpulan: Terdapat perbedaan rata-rata yang signifikan antara kedua kelompok."
  })
  
  output$download_uji_rata <- downloadHandler(
    filename = function() paste0("hasil_uji_rata-rata_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- read_docx() %>% body_add_par("Hasil Uji Beda Rata-rata", style = "heading 1")
      doc %>% body_add_par("Uji-t Satu Sampel", style = "heading 2")
      doc %>% body_add_par(paste("Variabel:", input$ttest1_var, "| Nilai Hipotesis:", input$ttest1_mu))
      doc %>% body_add_par(paste(capture.output(ttest1_output()), collapse="\n"))
      doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$ttest1_interpretation())
      doc %>% body_add_par("Uji-t Dua Sampel Independen", style = "heading 2")
      doc %>% body_add_par(paste("Variabel:", input$ttest2_var, "| Grup:", input$variabel))
      ttest2_safe <- tryCatch(ttest2_output(), error=function(e)e)
      if(!inherits(ttest2_safe, "error")){
        doc %>% body_add_par(paste(capture.output(ttest2_safe), collapse="\n"))
        doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$ttest2_interpretation())
      } else {
        doc %>% body_add_par("Gagal: Uji ini memerlukan tepat 2 kategori.")
      }
      print(doc, target = file)
    }
  )
  
  #--- LOGIKA UNTUK UJI VARIANS ---#
  output$var1_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("var1_var", "Pilih Variabel Numerik:", choices = kolom_numerik)
  })
  
  var1_test_output <- reactive({
    req(input$var1_var, input$var1_sigma_sq)
    validate(need(input$var1_sigma_sq > 0, "Nilai hipotesis varians harus > 0."))
    varTest(data_sosial()[[input$var1_var]], sigma.squared = input$var1_sigma_sq)
  })
  
  output$var1_test_result_table <- renderUI({
    test_result <- var1_test_output()
    df_res <- data.frame(
      Statistik = c("Statistik Chi-Square", "Derajat Bebas (df)", "P-value", "Varians Sampel"),
      Nilai = c(test_result$statistic, 
                test_result$parameters, 
                test_result$p.value,
                test_result$estimate)
    )
    ft <- flextable(df_res) %>% 
      colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% 
      autofit() %>% 
      theme_box()
    htmltools_value(ft)
  })
  
  output$var1_test_interpretation <- renderText({
    p_value <- var1_test_output()$p.value
    if (p_value > 0.05) paste0("Kesimpulan: Varians sampel tidak berbeda signifikan dari nilai hipotesis (", input$var1_sigma_sq, ").") else paste0("Kesimpulan: Varians sampel berbeda signifikan dari nilai hipotesis (", input$var1_sigma_sq, ").")
  })
  
  output$var2_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("var2_var", "Pilih Variabel Numerik:", choices = kolom_numerik)
  })
  
  var2_test_output <- reactive({
    req(input$var2_var, kategori_info())
    data_kat <- kategori_info()$data
    validate(need(length(levels(data_kat$Kategori)) == 2, "Uji ini memerlukan tepat 2 kategori. Silakan atur di 'Manajemen Data'."))
    var.test(as.formula(paste("`", input$var2_var, "` ~ Kategori", sep="")), data = data_kat)
  })
  
  output$var2_test_result_table <- renderUI({
    test_result <- var2_test_output()
    df_res <- data.frame(
      Statistik = c("Statistik F", "Derajat Bebas Numerator", "Derajat Bebas Denominator", "P-value", "Rasio Varians"),
      Nilai = c(test_result$statistic, 
                test_result$parameter[1], 
                test_result$parameter[2], 
                test_result$p.value,
                test_result$estimate)
    )
    ft <- flextable(df_res) %>% 
      colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% 
      autofit() %>% 
      theme_box()
    htmltools_value(ft)
  })
  
  output$var2_test_interpretation <- renderText({
    p_value <- var2_test_output()$p.value
    if (p_value > 0.05) "Kesimpulan: Tidak ada perbedaan varians yang signifikan antara kedua kelompok (varian homogen)." else "Kesimpulan: Terdapat perbedaan varians yang signifikan antara kedua kelompok (varian tidak homogen)."
  })
  
  output$download_uji_varians <- downloadHandler(
    filename = function() paste0("hasil_uji_varians_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- read_docx() %>% body_add_par("Hasil Uji Varians", style = "heading 1")
      
      doc %>% body_add_par("Uji Varians Satu Sampel (Chi-Square Test)", style = "heading 2")
      doc %>% body_add_par(paste("Variabel:", input$var1_var, "| Nilai Hipotesis Varians:", input$var1_sigma_sq))
      test_result1 <- var1_test_output()
      df_res1 <- data.frame(
        Statistik = c("Statistik Chi-Square", "Derajat Bebas (df)", "P-value", "Varians Sampel"),
        Nilai = c(test_result1$statistic, test_result1$parameters, test_result1$p.value, test_result1$estimate)
      )
      ft1 <- flextable(df_res1) %>% colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% autofit() %>% theme_box()
      doc %>% body_add_flextable(ft1)
      doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$var1_test_interpretation())
      
      doc %>% body_add_par("Uji Varians Dua Sampel (F-Test)", style = "heading 2")
      doc %>% body_add_par(paste("Variabel:", input$var2_var, "| Grup:", input$variabel))
      var2_safe <- tryCatch(var2_test_output(), error = function(e) e)
      if (!inherits(var2_safe, "error")) {
        test_result2 <- var2_safe
        df_res2 <- data.frame(
          Statistik = c("Statistik F", "Derajat Bebas Numerator", "Derajat Bebas Denominator", "P-value", "Rasio Varians"),
          Nilai = c(test_result2$statistic, test_result2$parameter[1], test_result2$parameter[2], test_result2$p.value, test_result2$estimate)
        )
        ft2 <- flextable(df_res2) %>% colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% autofit() %>% theme_box()
        doc %>% body_add_flextable(ft2)
        doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$var2_test_interpretation())
      } else {
        doc %>% body_add_par("Gagal: Uji ini memerlukan tepat 2 kategori yang diatur di tab 'Manajemen Data'.")
      }
      print(doc, target = file)
    }
  )
  
  #--- LOGIKA UNTUK UJI PROPORSI ---#
  prop_data <- reactiveVal(NULL)
  
  output$prop_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("prop_var_biner", "Pilih Variabel Numerik untuk Dibuat Biner:", choices = kolom_numerik)
  })
  
  output$prop_custom_threshold_ui <- renderUI({
    if (input$prop_threshold_method == "Nilai Kustom") {
      numericInput("prop_custom_threshold", "Masukkan Nilai Batas Kustom:", value = 0)
    }
  })
  
  observeEvent(input$prop_generate_button, {
    req(input$prop_var_biner, input$prop_threshold_method, input$prop_label_sukses, input$prop_label_gagal)
    
    df <- data_sosial()
    var_to_bin <- df[[input$prop_var_biner]]
    
    threshold <- switch(input$prop_threshold_method,
                        "Median" = median(var_to_bin, na.rm = TRUE),
                        "Rata-rata (Mean)" = mean(var_to_bin, na.rm = TRUE),
                        "Nilai Kustom" = {
                          req(input$prop_custom_threshold)
                          input$prop_custom_threshold
                        })
    
    sukses_label <- input$prop_label_sukses
    gagal_label <- input$prop_label_gagal
    
    df$prop_variable_biner <- factor(ifelse(var_to_bin > threshold, sukses_label, gagal_label), levels = c(sukses_label, gagal_label))
    
    prop_data(df)
    
    output$prop_status_output <- renderText({
      paste("Variabel biner 'prop_variable_biner' berhasil dibuat.\n",
            "Batas (threshold):", round(threshold, 2), "\n",
            "Jumlah '", sukses_label, "': ", sum(df$prop_variable_biner == sukses_label), "\n",
            "Jumlah '", gagal_label, "': ", sum(df$prop_variable_biner == gagal_label))
    })
  })
  
  prop1_test_output <- reactive({
    req(prop_data(), input$prop1_p_hipotesis)
    df <- prop_data()
    
    jumlah_sukses <- sum(df$prop_variable_biner == input$prop_label_sukses, na.rm = TRUE)
    jumlah_total <- nrow(df)
    
    prop.test(x = jumlah_sukses, n = jumlah_total, p = input$prop1_p_hipotesis)
  })
  
  output$prop1_test_result_table <- renderUI({
    validate(need(!is.null(prop_data()), "Silakan buat variabel biner terlebih dahulu di Langkah 1."))
    test_result <- prop1_test_output()
    df_res <- data.frame(
      Statistik = c("Statistik Chi-Square", "Derajat Bebas (df)", "P-value", "Proporsi Sampel"),
      Nilai = c(test_result$statistic, test_result$parameter, test_result$p.value, test_result$estimate)
    )
    ft <- flextable(df_res) %>% colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% autofit() %>% theme_box()
    htmltools_value(ft)
  })
  
  output$prop1_test_interpretation <- renderText({
    validate(need(!is.null(prop_data()), ""))
    p_value <- prop1_test_output()$p.value
    if (p_value > 0.05) {
      paste0("Kesimpulan: Proporsi sampel tidak berbeda signifikan dari proporsi hipotesis (", input$prop1_p_hipotesis, ").")
    } else {
      paste0("Kesimpulan: Proporsi sampel berbeda signifikan dari proporsi hipotesis (", input$prop1_p_hipotesis, ").")
    }
  })
  
  output$prop2_group_selectors_ui <- renderUI({
    req(kategori_info())
    choices <- levels(kategori_info()$data$Kategori)
    validate(need(length(choices) >= 2, "Perlu minimal 2 kelompok di 'Manajemen Data' untuk perbandingan."))
    fluidRow(
      column(6, selectInput("prop2_group1", "Pilih Kelompok 1:", choices = choices, selected = choices[1])),
      column(6, selectInput("prop2_group2", "Pilih Kelompok 2:", choices = choices, selected = choices[2]))
    )
  })
  
  prop2_test_output <- reactive({
    req(prop_data(), kategori_info(), input$prop2_group1, input$prop2_group2)
    validate(need(input$prop2_group1 != input$prop2_group2, "Kelompok 1 dan Kelompok 2 harus berbeda."))
    
    df_prop <- prop_data()
    df_kat <- kategori_info()$data
    
    df_merged <- df_prop %>%
      select(all_of(nama_kolom_kode), prop_variable_biner) %>%
      left_join(df_kat %>% select(all_of(nama_kolom_kode), Kategori), by = nama_kolom_kode)
    
    group1_data <- df_merged %>% filter(Kategori == input$prop2_group1)
    group2_data <- df_merged %>% filter(Kategori == input$prop2_group2)
    
    x1 <- sum(group1_data$prop_variable_biner == input$prop_label_sukses, na.rm = TRUE)
    n1 <- nrow(group1_data)
    
    x2 <- sum(group2_data$prop_variable_biner == input$prop_label_sukses, na.rm = TRUE)
    n2 <- nrow(group2_data)
    
    validate(need(n1 > 0 && n2 > 0, "Salah satu kelompok yang dipilih tidak memiliki data."))
    
    prop.test(x = c(x1, x2), n = c(n1, n2))
  })
  
  output$prop2_test_result_table <- renderUI({
    validate(need(!is.null(prop_data()), "Silakan buat variabel biner terlebih dahulu di Langkah 1."))
    test_result <- prop2_test_output()
    df_res <- data.frame(
      Statistik = c("Statistik Chi-Square", "Derajat Bebas (df)", "P-value", "Proporsi Kelompok 1", "Proporsi Kelompok 2"),
      Nilai = c(test_result$statistic, test_result$parameter, test_result$p.value, test_result$estimate[1], test_result$estimate[2])
    )
    ft <- flextable(df_res) %>% colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% autofit() %>% theme_box()
    htmltools_value(ft)
  })
  
  output$prop2_test_interpretation <- renderText({
    validate(need(!is.null(prop_data()), ""))
    p_value <- prop2_test_output()$p.value
    if (p_value > 0.05) {
      "Kesimpulan: Tidak ada perbedaan proporsi yang signifikan antara kedua kelompok."
    } else {
      "Kesimpulan: Terdapat perbedaan proporsi yang signifikan antara kedua kelompok."
    }
  })
  
  output$download_uji_proporsi <- downloadHandler(
    filename = function() paste0("hasil_uji_proporsi_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- read_docx() %>% body_add_par("Hasil Uji Proporsi", style = "heading 1")
      
      prop1_safe <- tryCatch(prop1_test_output(), error = function(e) e)
      if (!inherits(prop1_safe, "error")) {
        doc %>% body_add_par("Uji Proporsi Satu Sampel", style = "heading 2")
        test_result1 <- prop1_safe
        df_res1 <- data.frame(
          Statistik = c("Statistik Chi-Square", "Derajat Bebas (df)", "P-value", "Proporsi Sampel"),
          Nilai = c(test_result1$statistic, test_result1$parameter, test_result1$p.value, test_result1$estimate)
        )
        ft1 <- flextable(df_res1) %>% colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% autofit() %>% theme_box()
        doc %>% body_add_flextable(ft1)
        doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$prop1_test_interpretation())
      }
      
      prop2_safe <- tryCatch(prop2_test_output(), error = function(e) e)
      if (!inherits(prop2_safe, "error")) {
        doc %>% body_add_par("Uji Proporsi Dua Sampel", style = "heading 2")
        test_result2 <- prop2_safe
        df_res2 <- data.frame(
          Statistik = c("Statistik Chi-Square", "Derajat Bebas (df)", "P-value", "Proporsi Kelompok 1", "Proporsi Kelompok 2"),
          Nilai = c(test_result2$statistic, test_result2$parameter, test_result2$p.value, test_result2$estimate[1], test_result2$estimate[2])
        )
        ft2 <- flextable(df_res2) %>% colformat_double(j = "Nilai", big.mark = "", digits = 4) %>% autofit() %>% theme_box()
        doc %>% body_add_flextable(ft2)
        doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$prop2_test_interpretation())
      }
      
      print(doc, target = file)
    }
  )
  
  #--- LOGIKA UNTUK ANOVA ---#
  
  output$anova1_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("anova1_var", "Pilih Variabel Numerik (Dependen):", choices = kolom_numerik)
  })
  
  anova1_model <- reactive({
    req(input$anova1_var, kategori_info())
    df <- kategori_info()$data
    validate(need(length(levels(df$Kategori)) >= 2, "ANOVA memerlukan minimal 2 kelompok dari 'Manajemen Data'."))
    
    formula <- as.formula(paste("`", input$anova1_var, "` ~ Kategori", sep=""))
    aov(formula, data = df)
  })
  
  output$anova1_result_table <- renderUI({
    model_summary <- summary(anova1_model())
    df_res <- as.data.frame(model_summary[[1]])
    df_res <- tibble::rownames_to_column(df_res, "Sumber Variasi")
    names(df_res)[names(df_res) == "Pr(>F)"] <- "P-value"
    
    ft <- flextable(df_res) %>% 
      colformat_double(j = c("Df", "Sum Sq", "Mean Sq", "F value", "P-value"), big.mark = "", digits = 4) %>%
      autofit() %>%
      theme_box()
    htmltools_value(ft)
  })
  
  anova1_posthoc_model <- reactive({
    req(anova1_model())
    TukeyHSD(anova1_model())
  })
  
  output$anova1_posthoc_table <- renderUI({
    res_posthoc <- as.data.frame(anova1_posthoc_model()$Kategori)
    res_posthoc <- tibble::rownames_to_column(res_posthoc, "Perbandingan Kelompok")
    names(res_posthoc)[names(res_posthoc) == "p adj"] <- "P-value Adjusted"
    
    ft <- flextable(res_posthoc) %>%
      colformat_double(j = c("diff", "lwr", "upr", "P-value Adjusted"), big.mark = "", digits = 4) %>%
      autofit() %>%
      theme_box() %>%
      color(j = "P-value Adjusted", color = function(x) ifelse(x < 0.05, "red", "black"))
    
    htmltools_value(ft)
  })
  
  output$anova1_interpretation <- renderText({
    p_value <- summary(anova1_model())[[1]][["Pr(>F)"]][1]
    if (p_value < 0.05) {
      "Hasil ANOVA signifikan (p < 0.05), menunjukkan bahwa setidaknya ada satu kelompok yang rata-ratanya berbeda secara signifikan dari yang lain. Lihat tabel Post-Hoc untuk melihat pasangan kelompok mana yang berbeda."
    } else {
      "Hasil ANOVA tidak signifikan (p >= 0.05), menunjukkan bahwa tidak ada perbedaan rata-rata yang signifikan antar kelompok."
    }
  })
  
  output$download_anova <- downloadHandler(
    filename = function() paste0("hasil_anova_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- read_docx() %>% body_add_par("Hasil Analisis Varians (ANOVA)", style = "heading 1")
      
      doc %>% body_add_par("ANOVA Satu Arah", style = "heading 2")
      anova1_safe <- tryCatch({
        df_res <- as.data.frame(summary(anova1_model())[[1]])
        df_res <- tibble::rownames_to_column(df_res, "Sumber Variasi")
        names(df_res)[names(df_res) == "Pr(>F)"] <- "P-value"
        ft <- flextable(df_res) %>% colformat_double(j = 2:6, big.mark = "", digits = 4) %>% autofit() %>% theme_box()
        doc %>% body_add_flextable(ft)
        doc %>% body_add_par("Interpretasi:", style="heading 3") %>% body_add_par(output$anova1_interpretation())
        
        doc %>% body_add_par("Uji Lanjutan Tukey HSD", style="heading 3")
        res_posthoc <- as.data.frame(anova1_posthoc_model()$Kategori)
        res_posthoc <- tibble::rownames_to_column(res_posthoc, "Perbandingan")
        names(res_posthoc)[names(res_posthoc) == "p adj"] <- "P-value Adjusted"
        ft_posthoc <- flextable(res_posthoc) %>% colformat_double(j = 2:5, big.mark = "", digits = 4) %>% autofit() %>% theme_box()
        doc %>% body_add_flextable(ft_posthoc)
      }, error = function(e) {
        doc %>% body_add_par(paste("Gagal menjalankan ANOVA Satu Arah:", e$message))
      })
      
      print(doc, target = file)
    }
  )
  
  # --- LOGIKA BARU UNTUK ANALISIS CLUSTERING K-MEANS --- #
  
  # Selector untuk variabel clustering
  output$clustering_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("clustering_vars", "Pilih Variabel untuk Clustering:",
                choices = kolom_numerik, multiple = TRUE, 
                selected = kolom_numerik[1:min(3, length(kolom_numerik))])
  })
  
  # Reactive untuk menyimpan hasil clustering
  clustering_results <- eventReactive(input$run_clustering, {
    req(input$clustering_vars, input$num_clusters)
    
    df <- data_sosial()
    validate(
      need(length(input$clustering_vars) >= 2, "Pilih setidaknya 2 variabel untuk clustering."),
      need(input$num_clusters >= 2 && input$num_clusters <= 10, "Jumlah cluster harus antara 2-10.")
    )
    
    # Persiapkan data untuk clustering
    clustering_data <- df %>%
      select(all_of(c(nama_kolom_kode, nama_kolom_kabupaten, input$clustering_vars))) %>%
      filter(complete.cases(.))
    
    # Standardisasi data
    data_for_clustering <- scale(clustering_data[, input$clustering_vars, drop = FALSE])
    rownames(data_for_clustering) <- clustering_data[[nama_kolom_kode]]
    
    # Terapkan matriks penimbang jarak jika dipilih
    if (input$use_distance_matrix && !is.null(distance_matrix())) {
      dist_matrix <- distance_matrix()
      # Coba cocokkan dengan kode kabupaten
      if (nama_kolom_kode %in% names(dist_matrix)) {
        common_codes <- intersect(clustering_data[[nama_kolom_kode]], dist_matrix[[nama_kolom_kode]])
        if (length(common_codes) > 0) {
          # Filter data berdasarkan kode yang ada di matriks jarak
          clustering_data <- clustering_data[clustering_data[[nama_kolom_kode]] %in% common_codes, ]
          data_for_clustering <- scale(clustering_data[, input$clustering_vars, drop = FALSE])
          rownames(data_for_clustering) <- clustering_data[[nama_kolom_kode]]
        }
      }
    }
    
    # Jalankan K-means clustering
    set.seed(123) # Untuk reproducibility
    kmeans_result <- kmeans(data_for_clustering, centers = input$num_clusters, nstart = 25)
    
    # Tambahkan hasil cluster ke data
    clustering_data$Cluster <- as.factor(kmeans_result$cluster)
    
    # Hitung metrik validasi
    silhouette_score <- mean(silhouette(kmeans_result$cluster, dist(data_for_clustering))[, 3])
    within_ss <- kmeans_result$tot.withinss
    between_ss <- kmeans_result$betweenss
    total_ss <- kmeans_result$totss
    
    # Hitung elbow method untuk k optimal
    wss <- sapply(2:8, function(k) {
      kmeans(data_for_clustering, centers = k, nstart = 10)$tot.withinss
    })
    
    list(
      data = clustering_data,
      scaled_data = data_for_clustering,
      kmeans_result = kmeans_result,
      silhouette_score = silhouette_score,
      within_ss = within_ss,
      between_ss = between_ss,
      total_ss = total_ss,
      wss_values = wss,
      variables_used = input$clustering_vars
    )
  })
  
  # Tabel ringkasan cluster
  output$cluster_summary_table <- renderDT({
    results <- clustering_results()
    req(results)
    
    summary_data <- results$data %>%
      group_by(Cluster) %>%
      summarise(
        Jumlah_Observasi = n(),
        .groups = "drop"
      )
    
    # Tambahkan statistik deskriptif untuk setiap variabel
    for (var in results$variables_used) {
      var_stats <- results$data %>%
        group_by(Cluster) %>%
        summarise(
          mean_val = round(mean(.data[[var]], na.rm = TRUE), 2),
          .groups = "drop"
        )
      summary_data[[paste0("Rata_rata_", gsub("_", "_", var))]] <- var_stats$mean_val
    }
    
    datatable(summary_data, 
              options = list(pageLength = 10, scrollX = TRUE), 
              rownames = FALSE)
  })
  
  # Statistik cluster
  output$cluster_stats <- renderPrint({
    results <- clustering_results()
    req(results)
    
    cat("=== STATISTIK CLUSTERING ===\n\n")
    cat("Jumlah Cluster:", input$num_clusters, "\n")
    cat("Jumlah Observasi:", nrow(results$data), "\n")
    cat("Variabel yang Digunakan:", paste(results$variables_used, collapse = ", "), "\n\n")
    
    cat("=== METRIK KUALITAS CLUSTERING ===\n")
    cat("Silhouette Score:", round(results$silhouette_score, 4), "\n")
    cat("Total Within Sum of Squares:", round(results$within_ss, 2), "\n")
    cat("Between Sum of Squares:", round(results$between_ss, 2), "\n")
    cat("Total Sum of Squares:", round(results$total_ss, 2), "\n")
    cat("Proporsi Varians Dijelaskan:", round(results$between_ss / results$total_ss * 100, 2), "%\n\n")
    
    cat("=== PUSAT CLUSTER (STANDARDIZED) ===\n")
    print(round(results$kmeans_result$centers, 3))
  })
  
  # Tabel daftar wilayah per cluster
  output$region_cluster_table <- renderDT({
    results <- clustering_results()
    req(results)
    
    # Buat tabel dengan nama wilayah dan cluster
    region_table <- results$data %>%
      select(all_of(c(nama_kolom_kode, nama_kolom_kabupaten, "Cluster"))) %>%
      arrange(Cluster, !!sym(nama_kolom_kabupaten)) %>%
      rename(
        "Kode" = !!sym(nama_kolom_kode),
        "Nama Wilayah" = !!sym(nama_kolom_kabupaten),
        "Cluster" = "Cluster"
      )
    
    datatable(region_table, 
              options = list(
                pageLength = 15, 
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = c(0, 2))
                )
              ), 
              rownames = FALSE,
              filter = 'top') %>%
      formatStyle('Cluster',
                  backgroundColor = styleEqual(
                    unique(region_table$Cluster),
                    rainbow(length(unique(region_table$Cluster)), alpha = 0.3)
                  ))
  })
  
  # Tabel ringkasan jumlah wilayah per cluster
  output$cluster_count_table <- renderDT({
    results <- clustering_results()
    req(results)
    
    count_table <- results$data %>%
      group_by(Cluster) %>%
      summarise(
        Jumlah_Wilayah = n(),
        Contoh_Wilayah = paste(head(!!sym(nama_kolom_kabupaten), 3), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(Cluster)
    
    datatable(count_table, 
              options = list(
                dom = 't',
                columnDefs = list(
                  list(className = 'dt-center', targets = c(0, 1))
                )
              ), 
              rownames = FALSE) %>%
      formatStyle('Cluster',
                  backgroundColor = styleEqual(
                    count_table$Cluster,
                    rainbow(nrow(count_table), alpha = 0.3)
                  ))
  })
  
  # Tabel validasi cluster
  output$cluster_validation_table <- renderDT({
    results <- clustering_results()
    req(results)
    
    validation_data <- data.frame(
      Metrik = c("Silhouette Score", "Within Sum of Squares", "Between Sum of Squares", 
                 "Total Sum of Squares", "Proporsi Varians Dijelaskan (%)"),
      Nilai = c(
        round(results$silhouette_score, 4),
        round(results$within_ss, 2),
        round(results$between_ss, 2),
        round(results$total_ss, 2),
        round(results$between_ss / results$total_ss * 100, 2)
      )
    )
    
    datatable(validation_data, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Interpretasi clustering
  output$cluster_interpretation <- renderText({
    results <- clustering_results()
    req(results)
    
    sil_score <- results$silhouette_score
    var_explained <- results$between_ss / results$total_ss * 100
    
    sil_interpretation <- if (sil_score > 0.7) {
      "sangat baik"
    } else if (sil_score > 0.5) {
      "baik"
    } else if (sil_score > 0.25) {
      "cukup"
    } else {
      "kurang baik"
    }
    
    paste0(
      "INTERPRETASI HASIL CLUSTERING:\n\n",
      "1. Kualitas Clustering: Silhouette score sebesar ", round(sil_score, 3), 
      " menunjukkan bahwa kualitas clustering ", sil_interpretation, ".\n\n",
      "2. Varians yang Dijelaskan: Model clustering menjelaskan ", 
      round(var_explained, 1), "% dari total varians dalam data.\n\n",
      "3. Rekomendasi: ",
      if (sil_score > 0.5) {
        "Hasil clustering dapat diandalkan untuk analisis lebih lanjut."
      } else {
        "Pertimbangkan untuk mengubah jumlah cluster atau variabel yang digunakan."
      }
    )
  })
  
  # Plot clustering
  output$cluster_plot <- renderPlot({
    results <- clustering_results()
    req(results)
    
    # Gunakan PCA untuk visualisasi jika lebih dari 2 variabel
    if (length(results$variables_used) > 2) {
      pca_result <- prcomp(results$scaled_data, scale. = FALSE)
      plot_data <- data.frame(
        PC1 = pca_result$x[, 1],
        PC2 = pca_result$x[, 2],
        Cluster = results$data$Cluster,
        Kabupaten = results$data[[nama_kolom_kabupaten]]
      )
      
      ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_text(aes(label = substr(Kabupaten, 1, 10)), size = 2.5, vjust = -1) +
        labs(title = "Hasil Clustering K-Means (PCA Plot)",
             x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
             y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)")) +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else {
      # Plot 2D jika hanya 2 variabel
      plot_data <- data.frame(
        X = results$scaled_data[, 1],
        Y = results$scaled_data[, 2],
        Cluster = results$data$Cluster,
        Kabupaten = results$data[[nama_kolom_kabupaten]]
      )
      
      ggplot(plot_data, aes(x = X, y = Y, color = Cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_text(aes(label = substr(Kabupaten, 1, 10)), size = 2.5, vjust = -1) +
        labs(title = "Hasil Clustering K-Means",
             x = results$variables_used[1],
             y = results$variables_used[2]) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  # Elbow plot
  output$elbow_plot <- renderPlot({
    results <- clustering_results()
    req(results)
    
    elbow_data <- data.frame(
      k = 2:8,
      wss = results$wss_values
    )
    
    ggplot(elbow_data, aes(x = k, y = wss)) +
      geom_line(size = 1, color = "blue") +
      geom_point(size = 3, color = "red") +
      geom_vline(xintercept = input$num_clusters, linetype = "dashed", color = "green") +
      labs(title = "Elbow Method untuk Menentukan Jumlah Cluster Optimal",
           x = "Jumlah Cluster (k)",
           y = "Within Sum of Squares") +
      theme_minimal() +
      scale_x_continuous(breaks = 2:8)
  })
  
  # Peta clustering
  output$cluster_map <- renderLeaflet({
    results <- clustering_results()
    req(results, geojson_data())
    
    # Gabungkan data clustering dengan geojson
    map_data <- geojson_data()
    cluster_data <- results$data %>%
      select(all_of(c(nama_kolom_kode, nama_kolom_kabupaten)), Cluster)
    
    # Merge dengan geojson berdasarkan kode kabupaten
    map_data <- map_data %>%
      left_join(cluster_data, by = setNames(nama_kolom_kode, nama_kolom_kode))
    
    # Filter hanya data yang memiliki cluster
    map_data_filtered <- map_data %>%
      filter(!is.na(Cluster))
    
    # Buat palet warna untuk cluster
    cluster_colors <- rainbow(input$num_clusters)
    pal <- colorFactor(cluster_colors, domain = map_data_filtered$Cluster)
    
    # Buat popup content
    popup_content <- paste0(
      "<strong>", map_data_filtered[[nama_kolom_kabupaten]], "</strong><br/>",
      "Kode: ", map_data_filtered[[nama_kolom_kode]], "<br/>",
      "Cluster: ", map_data_filtered$Cluster
    )
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(
        data = map_data_filtered,
        fillColor = ~pal(Cluster),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = popup_content,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = map_data_filtered$Cluster,
        opacity = 0.8,
        title = "Cluster",
        position = "bottomright"
      )
  })
  
  # Download handler untuk clustering
  output$download_clustering <- downloadHandler(
    filename = function() paste0("hasil_clustering_", Sys.Date(), ".docx"),
    content = function(file) {
      results <- clustering_results()
      req(results)
      
      doc <- read_docx() %>%
        body_add_par("Hasil Analisis Clustering K-Means", style = "heading 1")
      
      # Informasi umum
      doc %>% body_add_par("Informasi Umum", style = "heading 2")
      doc %>% body_add_par(paste("Jumlah Cluster:", input$num_clusters))
      doc %>% body_add_par(paste("Jumlah Observasi:", nrow(results$data)))
      doc %>% body_add_par(paste("Variabel yang Digunakan:", paste(results$variables_used, collapse = ", ")))
      doc %>% body_add_par(paste("Menggunakan Matriks Penimbang Jarak:", ifelse(input$use_distance_matrix, "Ya", "Tidak")))
      
      # Metrik validasi
      doc %>% body_add_par("Metrik Validasi Clustering", style = "heading 2")
      validation_data <- data.frame(
        Metrik = c("Silhouette Score", "Within Sum of Squares", "Between Sum of Squares", 
                   "Total Sum of Squares", "Proporsi Varians Dijelaskan (%)"),
        Nilai = c(
          round(results$silhouette_score, 4),
          round(results$within_ss, 2),
          round(results$between_ss, 2),
          round(results$total_ss, 2),
          round(results$between_ss / results$total_ss * 100, 2)
        )
      )
      doc %>% body_add_flextable(flextable(validation_data) %>% autofit())
      
      # Pusat cluster
      doc %>% body_add_par("Pusat Cluster (Standardized)", style = "heading 2")
      centers_df <- as.data.frame(results$kmeans_result$centers)
      centers_df <- tibble::rownames_to_column(centers_df, "Cluster")
      doc %>% body_add_flextable(flextable(centers_df) %>% autofit())
      
      # Daftar wilayah per cluster
      doc %>% body_add_par("Daftar Wilayah per Cluster", style = "heading 2")
      region_table <- results$data %>%
        select(all_of(c(nama_kolom_kode, nama_kolom_kabupaten, "Cluster"))) %>%
        arrange(Cluster, !!sym(nama_kolom_kabupaten)) %>%
        rename(
          "Kode" = !!sym(nama_kolom_kode),
          "Nama Wilayah" = !!sym(nama_kolom_kabupaten),
          "Cluster" = "Cluster"
        )
      doc %>% body_add_flextable(flextable(region_table) %>% 
                                   autofit() %>%
                                   theme_box() %>%
                                   color(~ Cluster == 1, ~ Cluster, color = "red") %>%
                                   color(~ Cluster == 2, ~ Cluster, color = "blue") %>%
                                   color(~ Cluster == 3, ~ Cluster, color = "green"))
      
      # Ringkasan jumlah wilayah per cluster
      doc %>% body_add_par("Ringkasan Jumlah Wilayah per Cluster", style = "heading 2")
      count_table <- results$data %>%
        group_by(Cluster) %>%
        summarise(
          Jumlah_Wilayah = n(),
          Contoh_Wilayah = paste(head(!!sym(nama_kolom_kabupaten), 3), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(Cluster)
      doc %>% body_add_flextable(flextable(count_table) %>% autofit() %>% theme_box())
      
      # Interpretasi
      doc %>% body_add_par("Interpretasi", style = "heading 2")
      doc %>% body_add_par(output$cluster_interpretation())
      
      print(doc, target = file)
    }
  )
  
  # --- LOGIKA BARU UNTUK REGRESI LINEAR BERGANDA --- #
  
  kolom_numerik_reg <- reactive({
    df <- data_sosial()
    kolom <- names(df)[sapply(df, is.numeric)]
    setdiff(kolom, nama_kolom_kode)
  })
  
  output$reg_var_dependen_selector <- renderUI({
    selectInput("reg_var_dependen", "Pilih Variabel Dependen (Y):", choices = kolom_numerik_reg())
  })
  
  output$reg_var_independen_selector <- renderUI({
    req(input$reg_var_dependen)
    pilihan_independen <- setdiff(kolom_numerik_reg(), input$reg_var_dependen)
    selectInput("reg_var_independen", "Pilih Variabel Independen (X):",
                choices = pilihan_independen, multiple = TRUE)
  })
  
  reg_model_reactive <- eventReactive(input$run_regression, {
    req(input$reg_var_dependen, input$reg_var_independen)
    validate(
      need(length(input$reg_var_independen) >= 1, "Silakan pilih setidaknya satu variabel independen.")
    )
    
    data <- data_sosial()
    var_y <- input$reg_var_dependen
    vars_x <- input$reg_var_independen
    
    formula_reg <- as.formula(paste0("`", var_y, "` ~ ", paste0("`", vars_x, "`", collapse = " + ")))
    
    model <- lm(formula_reg, data = data)
    return(model)
  })
  
  output$reg_model_summary <- renderPrint({
    model <- reg_model_reactive()
    req(model)
    summary(model)
  })
  
  # Uji Asumsi Klasik
  
  # 1. Normalitas Residual
  reg_norm_test_output <- reactive({
    req(reg_model_reactive(), input$reg_norm_test_method)
    model <- reg_model_reactive()
    res <- residuals(model)
    
    if (input$reg_norm_test_method == "shapiro") {
      if (length(res) >= 3 && length(res) <= 5000) {
        shapiro.test(res)
      } else {
        list(method = "Shapiro-Wilk", error = "Jumlah residual harus antara 3 s/d 5000.")
      }
    } else {
      lillie.test(res)
    }
  })
  
  output$reg_norm_test_title <- renderText({
    req(input$reg_norm_test_method)
    if (input$reg_norm_test_method == "shapiro") "Hasil Uji Normalitas Residual (Shapiro-Wilk)" else "Hasil Uji Normalitas Residual (Kolmogorov-Smirnov - Lilliefors)"
  })
  
  output$reg_norm_test_result <- renderPrint({
    test_result <- reg_norm_test_output()
    req(test_result)
    if (!is.null(test_result$error)) cat(test_result$error) else test_result
  })
  
  output$reg_norm_test_interpretation <- renderText({
    test_result <- reg_norm_test_output()
    req(test_result)
    if (!is.null(test_result$error)) return("")
    p_value <- test_result$p.value
    if (p_value > 0.05) paste0("Kesimpulan: P-value (", round(p_value, 4), ") > 0.05, maka residual berdistribusi normal.") else paste0("Kesimpulan: P-value (", round(p_value, 4), ") <= 0.05, maka residual tidak berdistribusi normal.")
  })
  
  # 2. Autokorelasi
  autokorelasi_test <- reactive({
    req(reg_model_reactive())
    dwtest(reg_model_reactive())
  })
  
  output$autokorelasi_test_result <- renderPrint({
    autokorelasi_test()
  })
  
  output$autokorelasi_test_interpretation <- renderText({
    test <- autokorelasi_test()
    req(test)
    dw_stat <- test$statistic
    p_value <- test$p.value
    
    kesimpulan <- if (p_value < 0.05) {
      if (dw_stat < 2) "terindikasi adanya autokorelasi positif."
      else "terindikasi adanya autokorelasi negatif."
    } else {
      "tidak ada bukti kuat adanya autokorelasi (asumsi terpenuhi)."
    }
    paste0("Kesimpulan: P-value (", round(p_value, 4), ") dan statistik DW (", round(dw_stat, 2), ") menunjukkan bahwa ", kesimpulan)
  })
  
  # 3. Multikolinearitas
  multikolinearitas_test <- reactive({
    req(reg_model_reactive())
    model <- reg_model_reactive()
    # VIF memerlukan setidaknya 2 prediktor
    if (length(coef(model)) > 2) {
      vif_values <- vif(model)
      data.frame(Variabel = names(vif_values), VIF = vif_values)
    } else {
      NULL
    }
  })
  
  output$multikolinearitas_test_result <- renderDT({
    vif_df <- multikolinearitas_test()
    if (is.null(vif_df)) {
      datatable(data.frame(Pesan = "Uji VIF memerlukan setidaknya 2 variabel independen."), options = list(dom = 't'), rownames=FALSE)
    } else {
      datatable(vif_df, options = list(dom = 't'), rownames=FALSE) %>% formatRound('VIF', 2)
    }
  })
  
  output$multikolinearitas_test_interpretation <- renderText({
    vif_df <- multikolinearitas_test()
    req(vif_df)
    if (any(vif_df$VIF > 10)) {
      "Kesimpulan: Ditemukan setidaknya satu variabel dengan VIF > 10, mengindikasikan adanya masalah multikolinearitas yang serius."
    } else {
      "Kesimpulan: Semua variabel memiliki nilai VIF di bawah 10, menunjukkan tidak ada masalah multikolinearitas yang serius (asumsi terpenuhi)."
    }
  })
  
  # 4. Homoskedastisitas
  homoskedastisitas_test <- reactive({
    req(reg_model_reactive())
    bptest(reg_model_reactive())
  })
  
  output$homoskedastisitas_test_result <- renderPrint({
    homoskedastisitas_test()
  })
  
  output$homoskedastisitas_test_interpretation <- renderText({
    test <- homoskedastisitas_test()
    req(test)
    p_value <- test$p.value
    if (p_value > 0.05) {
      paste0("Kesimpulan: P-value (", round(p_value, 4), ") > 0.05, maka tidak ada bukti adanya heteroskedastisitas (varian residual homogen/asumsi terpenuhi).")
    } else {
      paste0("Kesimpulan: P-value (", round(p_value, 4), ") <= 0.05, maka terindikasi adanya masalah heteroskedastisitas (varian residual tidak homogen).")
    }
  })
  
  # Download Handler untuk Regresi
  output$download_regresi <- downloadHandler(
    filename = function() paste0("hasil_regresi_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- read_docx() %>% 
        body_add_par("Hasil Analisis Regresi Linear Berganda", style = "heading 1")
      
      # Hasil Model
      doc %>% body_add_par("Ringkasan Model Regresi", style = "heading 2")
      model <- reg_model_reactive()
      if (!is.null(model)) {
        doc %>% body_add_par(paste(capture.output(summary(model)), collapse = "\n"))
      } else {
        doc %>% body_add_par("Model belum dijalankan atau gagal.")
      }
      
      # Uji Asumsi
      doc %>% body_add_par("Hasil Uji Asumsi Klasik", style = "heading 1")
      
      # Normalitas
      doc %>% body_add_par("1. Uji Normalitas Residual", style = "heading 2")
      doc %>% body_add_par(paste(capture.output(reg_norm_test_output()), collapse = "\n"))
      doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$reg_norm_test_interpretation())
      
      # Autokorelasi
      doc %>% body_add_par("2. Uji Autokorelasi (Durbin-Watson)", style = "heading 2")
      doc %>% body_add_par(paste(capture.output(autokorelasi_test()), collapse = "\n"))
      doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$autokorelasi_test_interpretation())
      
      # Multikolinearitas
      doc %>% body_add_par("3. Uji Multikolinearitas (VIF)", style = "heading 2")
      vif_df <- multikolinearitas_test()
      if (!is.null(vif_df)) {
        doc %>% body_add_flextable(flextable(vif_df) %>% autofit())
        doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$multikolinearitas_test_interpretation())
      } else {
        doc %>% body_add_par("Tidak dapat melakukan uji VIF (memerlukan min. 2 variabel independen).")
      }
      
      # Homoskedastisitas
      doc %>% body_add_par("4. Uji Homoskedastisitas (Breusch-Pagan)", style = "heading 2")
      doc %>% body_add_par(paste(capture.output(homoskedastisitas_test()), collapse = "\n"))
      doc %>% body_add_par("Interpretasi:", style = "heading 3") %>% body_add_par(output$homoskedastisitas_test_interpretation())
      
      print(doc, target = file)
    }
  )
  
  
  #--- LOGIKA UNTUK EKSPLORASI DATA ---#
  observe({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    updateSelectInput(session, "variabel_eksplorasi", choices = kolom_numerik)
  })
  
  output$summary_table <- renderTable({
    req(input$variabel_eksplorasi)
    data <- data_sosial()[[input$variabel_eksplorasi]]
    modus <- as.numeric(names(sort(table(data), decreasing = TRUE)[1]))
    stats <- data.frame(
      Statistik = c("Mean", "Median", "Modus", "Minimum", "Maksimum", "Range", "Standar Deviasi"),
      Nilai = round(c(mean(data, na.rm = TRUE), median(data, na.rm = TRUE), modus, min(data, na.rm = TRUE), max(data, na.rm = TRUE), diff(range(data, na.rm = TRUE)), sd(data, na.rm = TRUE)), 2)
    )
    t(stats)
  }, rownames = TRUE, colnames = FALSE)
  
  output$interpretasi_stat <- renderText({
    req(input$variabel_eksplorasi)
    data <- data_sosial()[[input$variabel_eksplorasi]]
    mean_val <- mean(data, na.rm = TRUE); median_val <- median(data, na.rm = TRUE); sd_val <- sd(data, na.rm = TRUE)
    simpul <- ifelse(abs(mean_val - median_val) < sd_val * 0.1, "distribusi simetris", "distribusi tidak simetris")
    paste0("Rata-rata: ", round(mean_val, 2), ", Median: ", round(median_val, 2), ", SD: ", round(sd_val, 2), " → Data memiliki ", simpul, ".")
  })
  
  output$peringkat_plot <- renderPlot({
    req(input$variabel_eksplorasi, input$pilihan_peringkat)
    df <- data_sosial()
    var_eksplorasi <- input$variabel_eksplorasi
    plot_df <- df %>%
      select(all_of(c(nama_kolom_kode, nama_kolom_kabupaten, var_eksplorasi))) %>%
      filter(!is.na(.data[[var_eksplorasi]])) %>%
      mutate(label_unik = paste(.data[[nama_kolom_kode]], .data[[nama_kolom_kabupaten]]))
    if (input$pilihan_peringkat == "top") {
      plot_df <- plot_df %>% arrange(desc(.data[[var_eksplorasi]])) %>% head(10)
      plot_title <- paste("10 Kabupaten/Kota dengan", gsub("_", " ", var_eksplorasi), "Tertinggi")
    } else {
      plot_df <- plot_df %>% arrange(.data[[var_eksplorasi]]) %>% head(10)
      plot_title <- paste("10 Kabupaten/Kota dengan", gsub("_", " ", var_eksplorasi), "Terendah")
    }
    plot_df$label_unik <- factor(plot_df$label_unik, levels = rev(plot_df$label_unik))
    plot_df$label_text <- if (var_eksplorasi %in% jumlah_vars) format(plot_df[[var_eksplorasi]], big.mark = "", scientific = FALSE, trim = TRUE) else as.character(round(plot_df[[var_eksplorasi]], 2))
    max_val <- max(plot_df[[var_eksplorasi]], na.rm = TRUE)
    ggplot(plot_df, aes(x = .data[[var_eksplorasi]], y = label_unik)) +
      geom_col(fill = "steelblue", width = 0.7) +
      geom_text(aes(label = label_text), hjust = 0, nudge_x = max_val * 0.01, size = 3.5) +
      labs(title = plot_title, x = gsub("_", " ", var_eksplorasi), y = "Kode & Nama Kabupaten/Kota") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank()) +
      coord_cartesian(xlim = c(0, max_val * 1.25))
  }, res = 96)
  
  output$peta_interaktif <- renderLeaflet({
    req(input$variabel_eksplorasi, geojson_data())
    map_data <- geojson_data()
    var_eksplorasi <- input$variabel_eksplorasi
    validate(need(var_eksplorasi %in% names(map_data), "Variabel tidak ditemukan di file GeoJSON."))
    map_data_filtered <- map_data %>% filter(!is.na(.data[[var_eksplorasi]]))
    highlight_data <- if (input$pilihan_peringkat == "top") map_data_filtered %>% arrange(desc(.data[[var_eksplorasi]])) %>% head(10) else map_data_filtered %>% arrange(.data[[var_eksplorasi]]) %>% head(10)
    pal <- colorNumeric(palette = "YlOrRd", domain = highlight_data[[var_eksplorasi]])
    popup_content <- paste0("<strong>", highlight_data[[nama_kolom_kabupaten]], "</strong><br/>", gsub("_", " ", var_eksplorasi), ": ", format(highlight_data[[var_eksplorasi]], big.mark = "", decimal.mark = ",", scientific = FALSE))
    leaflet() %>%
      addTiles() %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(data = map_data, fillColor = "grey", weight = 0.5, color = "white", fillOpacity = 0.5) %>%
      addPolygons(data = highlight_data, fillColor = ~pal(highlight_data[[var_eksplorasi]]), weight = 2, color = "black", fillOpacity = 0.9, popup = popup_content) %>%
      addLegend(pal = pal, values = highlight_data[[var_eksplorasi]], opacity = 0.8, title = gsub("_", " ", var_eksplorasi), position = "bottomright")
  })
  
  output$peta_statis <- renderPlot({
    req(input$variabel_eksplorasi, geojson_data())
    map_data <- geojson_data()
    var_eksplorasi <- input$variabel_eksplorasi
    validate(need(var_eksplorasi %in% names(map_data), "Variabel tidak ditemukan di file GeoJSON."))
    map_data_filtered <- map_data %>% filter(!is.na(.data[[var_eksplorasi]]))
    highlight_data <- if (input$pilihan_peringkat == "top") map_data_filtered %>% arrange(desc(.data[[var_eksplorasi]])) %>% head(10) else map_data_filtered %>% arrange(.data[[var_eksplorasi]]) %>% head(10)
    plot_title <- paste("Peta 10 Kabupaten/Kota", gsub("_", " ", var_eksplorasi), if(input$pilihan_peringkat == "top") "Tertinggi" else "Terendah")
    ggplot() +
      geom_sf(data = map_data, fill = "grey85", color = "white", size = 0.2) +
      geom_sf(data = highlight_data, aes(fill = .data[[var_eksplorasi]]), color = "black", size = 0.4) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1, name = gsub("_", " ", var_eksplorasi)) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), legend.position = "bottom", legend.key.width = unit(1.5, "cm"))
  }, res = 96)
  
  output$unduh_laporan <- downloadHandler(
    filename = function() paste0("eksplorasi_", input$variabel_eksplorasi, ".docx"),
    content = function(file) {
      req(input$variabel_eksplorasi)
      data <- data_sosial()[[input$variabel_eksplorasi]]
      modus <- as.numeric(names(sort(table(data), decreasing = TRUE)[1]))
      stats_df <- data.frame(
        Statistik = c("Mean", "Median", "Modus", "Min", "Max", "Range", "SD"),
        Nilai = as.character(round(c(mean(data, na.rm = TRUE), median(data, na.rm = TRUE), modus, min(data, na.rm = TRUE), max(data, na.rm = TRUE), diff(range(data, na.rm = TRUE)), sd(data, na.rm = TRUE)), 2))
      )
      doc <- read_docx() %>% body_add_par(paste("Laporan Eksplorasi Variabel:", input$variabel_eksplorasi), style = "heading 1")
      doc %>% body_add_flextable(flextable(stats_df))
      doc %>% body_add_par("Interpretasi:", style = "heading 2") %>% body_add_par(output$interpretasi_stat())
      print(doc, target = file)
    }
  )
  
  output$unduh_gabungan <- downloadHandler(
    filename = function() "gabungan_eksplorasi.pdf",
    content = function(file) {
      writeLines("Fitur unduh gabungan akan segera dikembangkan.", file)
    }
  )
}

#================================================================#
#                         MENJALANKAN APLIKASI                   #
#================================================================#
shinyApp(ui, server)