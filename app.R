#----------------------------------------------------------------#
#                 KODE APLIKASI SHINY LENGKAP                    #
#----------------------------------------------------------------#

# 1. PASTIKAN SEMUA LIBRARY INI SUDAH TERINSTALL
# install.packages(c("shiny", "shinydashboard", "readxl", "DT", "officer", "flextable", "ggplot2", "dplyr", "sf", "leaflet", "car", "nortest", "EnvStats", "lmtest", "cluster", "factoextra", "corrplot", "pheatmap"))

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
library(cluster) # Untuk clustering analysis
library(factoextra) # Untuk visualisasi clustering
library(corrplot) # Untuk visualisasi korelasi
library(pheatmap) # Untuk heatmap

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
      menuItem("Analisis Clustering", tabName = "clustering", icon = icon("sitemap")),
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
      
      # --- TAB ANALISIS CLUSTERING --- #
      tabItem(tabName = "clustering",
              fluidRow(
                box(
                  title = "Pengaturan Analisis Clustering", width = 4, solidHeader = TRUE, status = "primary",
                  h4("Persiapan Data"),
                  uiOutput("cluster_var_selector"),
                  helpText("Pilih variabel numerik untuk analisis clustering. Tahan Ctrl/Cmd untuk memilih beberapa variabel."),
                  
                  hr(),
                  h4("Matriks Penimbang Jarak"),
                  selectInput("distance_method", "Metode Perhitungan Jarak:",
                              choices = c("Euclidean" = "euclidean",
                                        "Manhattan" = "manhattan", 
                                        "Maximum" = "maximum",
                                        "Canberra" = "canberra",
                                        "Minkowski" = "minkowski"),
                              selected = "euclidean"),
                  
                  conditionalPanel(
                    condition = "input.distance_method == 'minkowski'",
                    numericInput("minkowski_p", "Parameter p untuk Minkowski:", value = 2, min = 1, step = 0.5)
                  ),
                  
                  hr(),
                  h4("Metode Clustering"),
                  selectInput("cluster_method", "Pilih Metode Clustering:",
                              choices = c("K-Means" = "kmeans",
                                        "Hierarchical (Ward)" = "ward",
                                        "Hierarchical (Complete)" = "complete",
                                        "Hierarchical (Average)" = "average"),
                              selected = "kmeans"),
                  
                  conditionalPanel(
                    condition = "input.cluster_method == 'kmeans'",
                    numericInput("n_clusters_kmeans", "Jumlah Cluster (K):", value = 3, min = 2, max = 10, step = 1)
                  ),
                  
                  conditionalPanel(
                    condition = "input.cluster_method != 'kmeans'",
                    numericInput("n_clusters_hier", "Jumlah Cluster:", value = 3, min = 2, max = 10, step = 1)
                  ),
                  
                  hr(),
                  actionButton("run_clustering", "Jalankan Analisis Clustering", icon = icon("play-circle"), class = "btn-primary"),
                  
                  hr(),
                  h4("Unduh Hasil"),
                  downloadButton("download_clustering", "Unduh Hasil Clustering (Word)", class = "btn-success")
                ),
                
                box(
                  title = "Ringkasan Hasil Clustering", width = 8, solidHeader = TRUE, status = "primary",
                  verbatimTextOutput("cluster_summary"),
                  hr(),
                  h4("Tabel Hasil Clustering"),
                  DTOutput("cluster_results_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Matriks Jarak", width = 6, solidHeader = TRUE, status = "info", collapsible = TRUE,
                  p("Matriks jarak menunjukkan tingkat kemiripan antar observasi."),
                  plotOutput("distance_matrix_plot", height = "400px")
                ),
                
                box(
                  title = "Visualisasi Clustering", width = 6, solidHeader = TRUE, status = "info", collapsible = TRUE,
                  plotOutput("cluster_visualization", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Dendrogram (Untuk Hierarchical Clustering)", width = 6, solidHeader = TRUE, status = "success", collapsible = TRUE,
                  plotOutput("dendrogram_plot", height = "500px")
                ),
                
                box(
                  title = "Silhouette Analysis", width = 6, solidHeader = TRUE, status = "success", collapsible = TRUE,
                  p("Silhouette analysis mengevaluasi kualitas clustering. Nilai mendekati 1 menunjukkan clustering yang baik."),
                  plotOutput("silhouette_plot", height = "400px"),
                  verbatimTextOutput("silhouette_summary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Statistik Deskriptif per Cluster", width = 12, solidHeader = TRUE, status = "warning", collapsible = TRUE,
                  DTOutput("cluster_descriptive_stats")
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
  
  # --- LOGIKA UNTUK ANALISIS CLUSTERING --- #
  
  # Selector untuk variabel clustering
  output$cluster_var_selector <- renderUI({
    df <- data_sosial()
    kolom_numerik <- names(df)[sapply(df, is.numeric)]
    kolom_numerik <- setdiff(kolom_numerik, nama_kolom_kode)
    selectInput("cluster_variables", "Pilih Variabel untuk Clustering:",
                choices = kolom_numerik, multiple = TRUE, 
                selected = kolom_numerik[1:min(3, length(kolom_numerik))])
  })
  
  # Reactive untuk data clustering yang sudah diproses
  cluster_data_processed <- reactive({
    req(input$cluster_variables)
    df <- data_sosial()
    
    # Ambil data yang dipilih
    cluster_data <- df[, c(nama_kolom_kabupaten, nama_kolom_kode, input$cluster_variables), drop = FALSE]
    
    # Hapus baris dengan missing values
    cluster_data_clean <- cluster_data[complete.cases(cluster_data), ]
    
    # Standardisasi data numerik (z-score normalization)
    numeric_cols <- input$cluster_variables
    cluster_data_clean[numeric_cols] <- scale(cluster_data_clean[numeric_cols])
    
    return(cluster_data_clean)
  })
  
  # Reactive untuk matriks jarak
  distance_matrix_reactive <- reactive({
    req(input$distance_method, cluster_data_processed())
    
    cluster_data <- cluster_data_processed()
    numeric_data <- cluster_data[, input$cluster_variables, drop = FALSE]
    
    # Hitung matriks jarak
    if (input$distance_method == "minkowski") {
      req(input$minkowski_p)
      dist_matrix <- dist(numeric_data, method = "minkowski", p = input$minkowski_p)
    } else {
      dist_matrix <- dist(numeric_data, method = input$distance_method)
    }
    
    return(dist_matrix)
  })
  
  # Reactive untuk hasil clustering
  clustering_results <- eventReactive(input$run_clustering, {
    req(input$cluster_method, cluster_data_processed(), distance_matrix_reactive())
    
    cluster_data <- cluster_data_processed()
    dist_matrix <- distance_matrix_reactive()
    numeric_data <- cluster_data[, input$cluster_variables, drop = FALSE]
    
    if (input$cluster_method == "kmeans") {
      req(input$n_clusters_kmeans)
      set.seed(123) # Untuk reproducibility
      kmeans_result <- kmeans(numeric_data, centers = input$n_clusters_kmeans, nstart = 25)
      cluster_assignments <- kmeans_result$cluster
      method_info <- list(method = "K-Means", centers = kmeans_result$centers, 
                         withinss = kmeans_result$withinss, betweenss = kmeans_result$betweenss,
                         totss = kmeans_result$totss)
    } else {
      # Hierarchical clustering
      if (input$cluster_method == "ward") {
        hc_result <- hclust(dist_matrix, method = "ward.D2")
      } else {
        hc_result <- hclust(dist_matrix, method = input$cluster_method)
      }
      
      req(input$n_clusters_hier)
      cluster_assignments <- cutree(hc_result, k = input$n_clusters_hier)
      method_info <- list(method = paste("Hierarchical", input$cluster_method), 
                         hclust_obj = hc_result)
    }
    
    # Gabungkan hasil dengan data asli
    result_data <- cluster_data
    result_data$Cluster <- as.factor(cluster_assignments)
    
    return(list(data = result_data, method_info = method_info, 
               distance_matrix = dist_matrix, numeric_data = numeric_data))
  })
  
  # Output ringkasan clustering
  output$cluster_summary <- renderPrint({
    results <- clustering_results()
    req(results)
    
    cat("=== RINGKASAN ANALISIS CLUSTERING ===\n\n")
    cat("Metode:", results$method_info$method, "\n")
    cat("Metode Jarak:", input$distance_method, "\n")
    if (input$distance_method == "minkowski") {
      cat("Parameter p:", input$minkowski_p, "\n")
    }
    cat("Jumlah Observasi:", nrow(results$data), "\n")
    cat("Jumlah Variabel:", length(input$cluster_variables), "\n")
    cat("Variabel yang Digunakan:", paste(input$cluster_variables, collapse = ", "), "\n")
    cat("Jumlah Cluster:", length(unique(results$data$Cluster)), "\n\n")
    
    # Distribusi cluster
    cat("Distribusi per Cluster:\n")
    print(table(results$data$Cluster))
    
    # Informasi tambahan untuk K-Means
    if (results$method_info$method == "K-Means") {
      cat("\nWithin Sum of Squares per Cluster:\n")
      print(results$method_info$withinss)
      cat("\nBetween Sum of Squares:", results$method_info$betweenss, "\n")
      cat("Total Sum of Squares:", results$method_info$totss, "\n")
      cat("Ratio BSS/TSS:", round(results$method_info$betweenss/results$method_info$totss, 4), "\n")
    }
  })
  
  # Tabel hasil clustering
  output$cluster_results_table <- renderDT({
    results <- clustering_results()
    req(results)
    
    display_data <- results$data %>%
      select(all_of(nama_kolom_kode), all_of(nama_kolom_kabupaten), Cluster, everything()) %>%
      select(-all_of(input$cluster_variables)) # Hapus variabel yang sudah di-standardisasi
    
    # Tambahkan variabel asli (tidak di-standardisasi)
    original_data <- data_sosial()[, input$cluster_variables, drop = FALSE]
    display_data <- cbind(display_data, original_data)
    
    datatable(display_data, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = input$cluster_variables, digits = 2)
  })
  
  # Plot matriks jarak
  output$distance_matrix_plot <- renderPlot({
    req(distance_matrix_reactive())
    
    dist_matrix <- as.matrix(distance_matrix_reactive())
    
    # Buat heatmap matriks jarak
    pheatmap(dist_matrix, 
             color = colorRampPalette(c("blue", "white", "red"))(50),
             main = paste("Matriks Jarak (", input$distance_method, ")", sep = ""),
             show_rownames = FALSE, show_colnames = FALSE,
             fontsize = 8)
  })
  
  # Visualisasi clustering
  output$cluster_visualization <- renderPlot({
    results <- clustering_results()
    req(results)
    
    if (length(input$cluster_variables) >= 2) {
      # PCA untuk visualisasi jika lebih dari 2 variabel
      if (length(input$cluster_variables) > 2) {
        pca_result <- prcomp(results$numeric_data, scale. = FALSE) # Data sudah di-standardisasi
        plot_data <- data.frame(
          PC1 = pca_result$x[,1],
          PC2 = pca_result$x[,2],
          Cluster = results$data$Cluster
        )
        
        ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
          geom_point(size = 3, alpha = 0.7) +
          stat_ellipse(type = "confidence", level = 0.95) +
          labs(title = "Visualisasi Clustering (PCA)",
               x = paste("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)", sep=""),
               y = paste("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)", sep="")) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else {
        # Scatter plot untuk 2 variabel
        plot_data <- data.frame(
          Var1 = results$numeric_data[,1],
          Var2 = results$numeric_data[,2],
          Cluster = results$data$Cluster
        )
        
        ggplot(plot_data, aes(x = Var1, y = Var2, color = Cluster)) +
          geom_point(size = 3, alpha = 0.7) +
          stat_ellipse(type = "confidence", level = 0.95) +
          labs(title = "Visualisasi Clustering",
               x = input$cluster_variables[1],
               y = input$cluster_variables[2]) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      }
    } else {
      # Plot untuk 1 variabel (histogram)
      plot_data <- data.frame(
        Value = results$numeric_data[,1],
        Cluster = results$data$Cluster
      )
      
      ggplot(plot_data, aes(x = Value, fill = Cluster)) +
        geom_histogram(alpha = 0.7, bins = 20) +
        facet_wrap(~Cluster, ncol = 1) +
        labs(title = "Distribusi Clustering",
             x = input$cluster_variables[1]) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  # Dendrogram untuk hierarchical clustering
  output$dendrogram_plot <- renderPlot({
    results <- clustering_results()
    req(results)
    
    if (results$method_info$method != "K-Means") {
      hc_obj <- results$method_info$hclust_obj
      plot(hc_obj, main = paste("Dendrogram (", results$method_info$method, ")", sep = ""),
           xlab = "Observasi", ylab = "Jarak", cex = 0.8)
      
      # Tambahkan garis untuk cut
      n_clusters <- if (input$cluster_method == "kmeans") input$n_clusters_kmeans else input$n_clusters_hier
      abline(h = sort(hc_obj$height, decreasing = TRUE)[n_clusters-1], col = "red", lty = 2, lwd = 2)
      text(x = 1, y = sort(hc_obj$height, decreasing = TRUE)[n_clusters-1], 
           labels = paste("Cut untuk", n_clusters, "cluster"), pos = 3, col = "red")
    } else {
      plot.new()
      text(0.5, 0.5, "Dendrogram hanya tersedia untuk Hierarchical Clustering", 
           cex = 1.2, col = "gray")
    }
  })
  
  # Silhouette analysis
  output$silhouette_plot <- renderPlot({
    results <- clustering_results()
    req(results)
    
    sil <- silhouette(as.numeric(results$data$Cluster), results$distance_matrix)
    fviz_silhouette(sil) +
      theme_minimal() +
      labs(title = "Silhouette Analysis") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$silhouette_summary <- renderPrint({
    results <- clustering_results()
    req(results)
    
    sil <- silhouette(as.numeric(results$data$Cluster), results$distance_matrix)
    cat("=== RINGKASAN SILHOUETTE ANALYSIS ===\n\n")
    cat("Rata-rata Silhouette Width:", round(mean(sil[,3]), 4), "\n\n")
    cat("Interpretasi:\n")
    avg_sil <- mean(sil[,3])
    if (avg_sil > 0.7) {
      cat("- Clustering sangat baik (> 0.7)\n")
    } else if (avg_sil > 0.5) {
      cat("- Clustering cukup baik (0.5 - 0.7)\n")
    } else if (avg_sil > 0.25) {
      cat("- Clustering lemah (0.25 - 0.5)\n")
    } else {
      cat("- Clustering tidak baik (< 0.25)\n")
    }
    
    cat("\nSilhouette Width per Cluster:\n")
    sil_summary <- aggregate(sil[,3], by = list(Cluster = sil[,1]), FUN = mean)
    names(sil_summary) <- c("Cluster", "Avg_Silhouette_Width")
    print(sil_summary)
  })
  
  # Statistik deskriptif per cluster
  output$cluster_descriptive_stats <- renderDT({
    results <- clustering_results()
    req(results)
    
    # Gabungkan dengan data asli
    original_data <- data_sosial()
    cluster_with_original <- merge(results$data[, c(nama_kolom_kode, "Cluster")], 
                                  original_data, by = nama_kolom_kode)
    
    # Hitung statistik deskriptif per cluster
    stats_list <- list()
    for (cluster_id in sort(unique(cluster_with_original$Cluster))) {
      cluster_data <- cluster_with_original[cluster_with_original$Cluster == cluster_id, 
                                          input$cluster_variables, drop = FALSE]
      
      stats <- data.frame(
        Cluster = cluster_id,
        Variabel = input$cluster_variables,
        N = nrow(cluster_data),
        Mean = round(sapply(cluster_data, mean, na.rm = TRUE), 2),
        Median = round(sapply(cluster_data, median, na.rm = TRUE), 2),
        SD = round(sapply(cluster_data, sd, na.rm = TRUE), 2),
        Min = round(sapply(cluster_data, min, na.rm = TRUE), 2),
        Max = round(sapply(cluster_data, max, na.rm = TRUE), 2)
      )
      
      stats_list[[as.character(cluster_id)]] <- stats
    }
    
    all_stats <- do.call(rbind, stats_list)
    rownames(all_stats) <- NULL
    
    datatable(all_stats, 
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = c("Mean", "Median", "SD", "Min", "Max"), digits = 2)
  })
  
  # Download handler untuk clustering
  output$download_clustering <- downloadHandler(
    filename = function() paste0("hasil_clustering_", Sys.Date(), ".docx"),
    content = function(file) {
      results <- clustering_results()
      req(results)
      
      doc <- read_docx() %>% 
        body_add_par("Hasil Analisis Clustering dengan Matriks Penimbang Jarak", style = "heading 1")
      
      # Informasi umum
      doc %>% body_add_par("Informasi Umum", style = "heading 2")
      doc %>% body_add_par(paste("Metode Clustering:", results$method_info$method))
      doc %>% body_add_par(paste("Metode Perhitungan Jarak:", input$distance_method))
      if (input$distance_method == "minkowski") {
        doc %>% body_add_par(paste("Parameter p (Minkowski):", input$minkowski_p))
      }
      doc %>% body_add_par(paste("Jumlah Observasi:", nrow(results$data)))
      doc %>% body_add_par(paste("Jumlah Cluster:", length(unique(results$data$Cluster))))
      doc %>% body_add_par(paste("Variabel yang Digunakan:", paste(input$cluster_variables, collapse = ", ")))
      
      # Distribusi cluster
      doc %>% body_add_par("Distribusi per Cluster", style = "heading 2")
      cluster_dist <- as.data.frame(table(results$data$Cluster))
      names(cluster_dist) <- c("Cluster", "Jumlah_Observasi")
      doc %>% body_add_flextable(flextable(cluster_dist) %>% autofit())
      
      # Hasil clustering
      doc %>% body_add_par("Tabel Hasil Clustering", style = "heading 2")
      display_data <- results$data %>%
        select(all_of(nama_kolom_kode), all_of(nama_kolom_kabupaten), Cluster)
      doc %>% body_add_flextable(flextable(display_data) %>% autofit())
      
      # Silhouette analysis
      sil <- silhouette(as.numeric(results$data$Cluster), results$distance_matrix)
      doc %>% body_add_par("Evaluasi Kualitas Clustering (Silhouette Analysis)", style = "heading 2")
      doc %>% body_add_par(paste("Rata-rata Silhouette Width:", round(mean(sil[,3]), 4)))
      
      avg_sil <- mean(sil[,3])
      interpretation <- if (avg_sil > 0.7) {
        "Clustering sangat baik (> 0.7)"
      } else if (avg_sil > 0.5) {
        "Clustering cukup baik (0.5 - 0.7)"
      } else if (avg_sil > 0.25) {
        "Clustering lemah (0.25 - 0.5)"
      } else {
        "Clustering tidak baik (< 0.25)"
      }
      doc %>% body_add_par(paste("Interpretasi:", interpretation))
      
      # Statistik deskriptif
      doc %>% body_add_par("Statistik Deskriptif per Cluster", style = "heading 2")
      original_data <- data_sosial()
      cluster_with_original <- merge(results$data[, c(nama_kolom_kode, "Cluster")], 
                                    original_data, by = nama_kolom_kode)
      
      for (cluster_id in sort(unique(cluster_with_original$Cluster))) {
        doc %>% body_add_par(paste("Cluster", cluster_id), style = "heading 3")
        cluster_data <- cluster_with_original[cluster_with_original$Cluster == cluster_id, 
                                            input$cluster_variables, drop = FALSE]
        
        stats_table <- data.frame(
          Variabel = input$cluster_variables,
          N = nrow(cluster_data),
          Mean = round(sapply(cluster_data, mean, na.rm = TRUE), 2),
          Median = round(sapply(cluster_data, median, na.rm = TRUE), 2),
          SD = round(sapply(cluster_data, sd, na.rm = TRUE), 2)
        )
        
        doc %>% body_add_flextable(flextable(stats_table) %>% autofit())
      }
      
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