#Instruksi 2
#Mengimport data set
library(readr)
nakes <- read_csv("https://raw.githubusercontent.com/dwi-agustian/ahs_unpad/refs/heads/main/master_nakes.csv")

#identifikasi variabel yang tersedia dari dataset
library(dplyr)
names(nakes)
str(nakes)
glimpse(nakes)
glimpse(nakes$jenis_sdmk)
summary(nakes)
table(nakes$jenis_sdmk)

#recode ke variabel jenis_SDMK ke yang lebih sederhana
nakes$katsdmk <- dplyr::case_when(
  grepl("\\bDokter\\b", nakes$jenis_sdmk, ignore.case = TRUE) & 
    !grepl("Spesialis|Subspesialis", nakes$jenis_sdmk, ignore.case = TRUE) ~ "Dokter Umum",
  grepl("Spesialis", nakes$jenis_sdmk, ignore.case = TRUE) & 
    !grepl("Subspesialis", nakes$jenis_sdmk, ignore.case = TRUE) ~ "Dokter Spesialis",
  grepl("Subspesialis|Dokter Sub Spesialis Lainnya", nakes$jenis_sdmk, ignore.case = TRUE) ~ "Dokter Subspesialis",
  grepl("Dokter Gigi Spesialis", nakes$jenis_sdmk, ignore.case = TRUE) ~ "Dokter Spesialis",
  TRUE ~ "Non-Dokter"
)
table(nakes$katsdmk)

#Membuat data set baru berisi dokter saja dan yang berpraktek di jawa barat
nakes_jabar <- nakes %>%
  filter(
    katsdmk %in% c("Dokter Umum", "Dokter Spesialis", "Dokter Subspesialis") & 
      nama_prov == "JAWA BARAT"
  )

t.a_nonjabar <- nakes %>%
  filter(
    katsdmk %in% c("Dokter Umum", "Dokter Spesialis", "Dokter Subspesialis") &
      nama_prov != "JAWA BARAT"
  )

#identifikasi adanya tempat izin praktek dokter yang lebih dari 1
n_distinct(nakes_jabar$NIK)
namanakes_jabar %>%
  count(NIK) %>%        #karena setiap 1 NIK punya izin praktik baru, akan didata dengan NIK sama
  filter(n>1)

print(nakes_jabar %>%
        count(NIK) %>%
        filter(n>1),
      n=8062)

#tabel agregat hitung jumlah dokter (berdasarkan NIK) per kota kabupaten
t.a_nakesjabar <- nakes_jabar %>%
  + group_by(nama_kab) %>%
  + summarise(nakes_jabar = n_distinct(NIK))

#atau
t.a_nakesjabar <- aggregate(
  NIK ~ nama_kab, data = nakes_jabar, 
  FUN = function(x) length(unique(x)))

#mengexport data
write.csv(nakes,file = "data nakes.csv")
write.csv(nakes_jabar, file = "data nakes jabar.csv")
write.csv(t.a_nakesjabar, file = "Tabel aggregate Nakes Jabar.csv")
