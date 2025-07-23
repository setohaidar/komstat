# Fitur Download pada Dashboard R Shiny - Analisis Kondisi Sosial

## 📥 Deskripsi Fitur

Telah ditambahkan fitur download komprehensif pada semua menu dashboard R Shiny untuk analisis kondisi sosial. Pengguna dapat mengunduh:
- **Interpretasi** dari uji statistik (format .txt)
- **Gambar/Plot** (format .png/.html)
- **Tabel data** (format .csv/.xlsx)

## 🎯 Menu yang Telah Ditambahkan Fitur Download

### 1. **Manajemen Data**
**Tombol Download:**
- `Download Tabel CSV` - Data dengan kategorisasi dalam format CSV
- `Download Tabel Excel` - Data dengan kategorisasi dalam format Excel
- `Download Interpretasi` - Penjelasan metode kategorisasi dan rentang nilai

**File yang Dihasilkan:**
```
data_kategorisasi_{variabel}_{tanggal}.csv
data_kategorisasi_{variabel}_{tanggal}.xlsx
interpretasi_kategorisasi_{variabel}_{tanggal}.txt
```

### 2. **Eksplorasi Data**
**Tombol Download:**
- `Download Grafik` - Diagram batang peringkat (PNG)
- `Download Data CSV` - Data eksplorasi dalam format CSV
- `Download Interpretasi` - Interpretasi statistik deskriptif
- `Download Statistik` - Tabel statistik deskriptif (CSV)
- `Download Peta Statis (PNG)` - Peta offline
- `Download Peta Interaktif (HTML)` - Peta interaktif

**File yang Dihasilkan:**
```
grafik_peringkat_{variabel}_{tanggal}.png
data_eksplorasi_{variabel}_{tanggal}.csv
interpretasi_eksplorasi_{variabel}_{tanggal}.txt
statistik_deskriptif_{variabel}_{tanggal}.csv
peta_statis_{variabel}_{tanggal}.png
peta_interaktif_{variabel}_{tanggal}.html
```

### 3. **Uji Asumsi**
**Tombol Download:**
- `Download Hasil Uji` - Hasil uji normalitas/homogenitas
- `Download Interpretasi` - Interpretasi hasil uji

**File yang Dihasilkan:**
```
hasil_uji_normalitas_{variabel}_{tanggal}.txt
interpretasi_normalitas_{variabel}_{tanggal}.txt
hasil_uji_homogenitas_{variabel}_{tanggal}.txt
interpretasi_homogenitas_{variabel}_{tanggal}.txt
```

### 4. **Uji Beda Rata-rata**
**Tombol Download:**
- `Download Hasil Uji` - Hasil uji t-test
- `Download Interpretasi` - Interpretasi hasil uji

**File yang Dihasilkan:**
```
hasil_uji_t_satu_sampel_{variabel}_{tanggal}.txt
interpretasi_uji_t_satu_sampel_{variabel}_{tanggal}.txt
hasil_uji_t_dua_sampel_{variabel}_{tanggal}.txt
interpretasi_uji_t_dua_sampel_{variabel}_{tanggal}.txt
```

### 5. **Uji Varians**
**Tombol Download:**
- `Download Hasil Uji` - Tabel hasil uji varians (CSV)
- `Download Interpretasi` - Interpretasi hasil uji

**File yang Dihasilkan:**
```
hasil_uji_varians_satu_sampel_{variabel}_{tanggal}.csv
interpretasi_uji_varians_satu_sampel_{variabel}_{tanggal}.txt
hasil_uji_varians_dua_sampel_{variabel}_{tanggal}.csv
interpretasi_uji_varians_dua_sampel_{variabel}_{tanggal}.txt
```

### 6. **Uji Proporsi**
**Tombol Download:**
- `Download Hasil Uji` - Tabel hasil uji proporsi (CSV)
- `Download Interpretasi` - Interpretasi hasil uji

**File yang Dihasilkan:**
```
hasil_uji_proporsi_satu_sampel_{tanggal}.csv
interpretasi_uji_proporsi_satu_sampel_{tanggal}.txt
hasil_uji_proporsi_dua_sampel_{tanggal}.csv
interpretasi_uji_proporsi_dua_sampel_{tanggal}.txt
```

### 7. **ANOVA (>2 Kelompok)**
**Tombol Download:**
- `Download Tabel ANOVA` - Tabel ANOVA (CSV)
- `Download Post-Hoc` - Hasil uji Tukey HSD (CSV)
- `Download Interpretasi` - Interpretasi hasil ANOVA

**File yang Dihasilkan:**
```
hasil_anova_{variabel}_{tanggal}.csv
posthoc_tukey_{variabel}_{tanggal}.csv
interpretasi_anova_{variabel}_{tanggal}.txt
```

### 8. **Regresi Linear Berganda**
**Tombol Download:**
- `Download Summary Model` - Ringkasan model regresi
- `Download Data Regresi` - Data yang digunakan untuk regresi (CSV)
- `Download Uji Asumsi` - Kompilasi semua uji asumsi klasik

**File yang Dihasilkan:**
```
summary_regresi_{variabel_dependen}_{tanggal}.txt
data_regresi_{variabel_dependen}_{tanggal}.csv
uji_asumsi_regresi_{variabel_dependen}_{tanggal}.txt
```

### 9. **Analisis Clustering K-Means**
**Tombol Download:**
- `Download Data Cluster` - Data dengan hasil clustering (CSV)
- `Download Ringkasan` - Ringkasan statistik per cluster (CSV)
- `Download Interpretasi` - Interpretasi hasil clustering
- `Download Plot Cluster` - Visualisasi clustering (PNG)
- `Download Elbow Plot` - Plot elbow method (PNG)

**File yang Dihasilkan:**
```
data_clustering_k{jumlah_cluster}_{tanggal}.csv
ringkasan_clustering_k{jumlah_cluster}_{tanggal}.csv
interpretasi_clustering_k{jumlah_cluster}_{tanggal}.txt
plot_clustering_k{jumlah_cluster}_{tanggal}.png
elbow_plot_clustering_{tanggal}.png
```

## 📋 Fitur Utama

### ✅ **Interpretasi Lengkap**
- Setiap file interpretasi menyertakan tanggal, variabel yang digunakan, dan kesimpulan yang mudah dipahami
- Format yang konsisten dan terstruktur
- Bahasa Indonesia yang jelas dan profesional

### ✅ **Format File yang Beragam**
- **CSV** - Untuk data tabular yang mudah dibuka di Excel/spreadsheet
- **TXT** - Untuk interpretasi dan hasil uji statistik
- **PNG** - Untuk gambar dengan kualitas tinggi (300 DPI)
- **HTML** - Untuk peta interaktif
- **XLSX** - Untuk file Excel native

### ✅ **Penamaan File yang Sistematis**
- Semua file menggunakan format: `jenis_output_{variabel}_{tanggal}.ekstensi`
- Mudah diorganisir dan diidentifikasi
- Mencegah konflik penamaan

### ✅ **Kualitas Ekspor yang Tinggi**
- Gambar diekspor dengan resolusi 300 DPI
- Peta interaktif dalam format HTML yang self-contained
- Data dalam format yang kompatibel dengan software analisis lain

## 🔧 Dependencies Tambahan

Untuk mendukung fitur download, telah ditambahkan library:
```r
library(openxlsx)     # Untuk export Excel yang lebih baik
library(htmlwidgets)  # Untuk export widget HTML
library(webshot)      # Untuk export plot leaflet (opsional)
```

## 💡 Cara Penggunaan

1. **Pilih variabel** dan atur parameter analisis di setiap menu
2. **Jalankan analisis** dengan mengklik tombol yang tersedia
3. **Tunggu hasil muncul** di dashboard
4. **Klik tombol download** yang diinginkan untuk mengunduh file
5. **File akan tersimpan** di folder download browser Anda

## 🎨 Desain UI

- Tombol download menggunakan **color coding**:
  - 🔵 **Biru (btn-primary)** - Data/Hasil utama
  - 🟢 **Hijau (btn-success)** - Data tambahan/Excel
  - 🟡 **Kuning (btn-warning)** - Uji asumsi/validasi
  - 🔵 **Info (btn-info)** - Interpretasi/Plot

## 📝 Catatan Implementasi

- Semua download handler menggunakan **reactive expressions** yang sudah ada
- **Error handling** untuk kasus data tidak tersedia
- **Validasi input** sebelum download
- **Format tanggal** menggunakan `Sys.Date()` untuk konsistensi

Fitur download ini mempermudah pengguna untuk:
- 📊 **Menyimpan hasil analisis** untuk laporan
- 📈 **Menggunakan gambar** dalam presentasi
- 📋 **Mengolah data lebih lanjut** di software lain
- 📖 **Dokumentasi lengkap** dari setiap analisis yang dilakukan