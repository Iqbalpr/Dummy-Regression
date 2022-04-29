# Install package terlebih dahulu
library(dplyr) # Digunakan dalam fungsi "sample_n"
library(normtest) # Digunakan dalam uji asumsi Normalitas
library(nortest) # Digunakan dalam uji asumsi Normalitas
library(olsrr) # Digunakan dalam uji asumsi Multikolinearitas
library(lmtest) # Digunakan dalam uji asumsi Homoskedastisitas dan Autokorelasi

# import data yang akan digunakan
salaries <- read.csv("C:/Users/User/Downloads/Salaries.csv")
View(salaries) # Untuk menampilkan data yang digunakan

# Kita hapus kolom yang tidak digunakan (id, discipline, yrs.since.phd)
salaries2 <- salaries[-c(1,3,4)]
View(salaries2)

# Gunakan 200 random data sebagai contoh dalam melakukan pengujian
set.seed(24) # agar hasil data acak yang digunakan tidak berbeda selama pengujian
data = sample_n(salaries2,200)
str(data) # Untuk menapilkan tipe data yang akan digunakan
View(data)

# Mengecek dan menghapus outlier
boxplot(data$salary)
nilai_outlier_2 = boxplot.stats(data$salary)$out
nilai_outlier_2

data2 = data[-c(69, 153), ]
View(data2)

#------------------------------------------------------
# EKPLORASI DATA
#------------------------------------------------------

# Kita ubah data yang masih character, menjadi numerik
# d = dummy
d_prof = ifelse(data2$rank == "Prof",1,0)
d_Assocprof = ifelse(data2$rank == "AssocProf",1,0)
d_Asstprof = ifelse(data2$rank == "AsstProf",1,0)
d_Male = ifelse(data2$sex == "Male",1,0)
d_Female = ifelse(data2$sex == "Female",1,0)
# Membuat dataframe baru yang akan digunakan
data3 = data.frame(data2$yrs.service, data2$salary, d_prof, d_Assocprof, d_Asstprof, d_Male, d_Female)
View(data3)
str(data3)

## (Male = 1 & Prof = 0)
model_Maleprof0 = lm(data3$data2.salary ~ data3$d_Assocprof + data3$d_Asstprof + data3$data2.yrs.service + data3$d_Male)
summary(model_Maleprof0)

#(Asstprof = 1)
# Salaries =  (127476.7 - 51650.2) - 321.9 yrs.service + 5906.5 d_Male
#(Assocprof = 1)
# Salaries = (127476.7 - -32594.2) - 321.9 yrs.service + 5906.5 d_Male
#(Prof = 1)
# Salaries = 127476.7 - 321.9 yrs.service + 5906.5 d_Male

## (Female = 1 & Prof = 0)
model_Femaleprof0 = lm(data3$data2.salary ~ data3$d_Assocprof + data3$d_Asstprof + data3$data2.yrs.service + data3$d_Female)
summary(model_Femaleprof0)

#(Asstprof = 1)
# Salaries = (135282.0 - 53373.2) - 381.2 yrs.service - 6161.1 d_Female
#(Assocprof = 1)
# Salaries = (135282.0 - 34020.7) - 381.2 yrs.service - 6161.1 d_Female
#(Prof = 1)
# Salaries = 135282.0 - 381.2 yrs.service - 6161.1 d_Female

#------------------------------------------------------
# UJI ASUMSI
#------------------------------------------------------

## Uji Normalitas (lilliefors)
# Uji lilliefors
model_residual2 = resid(model_Maleprof0)
lillie.test(model_residual2)
hist(model_residual2)
qqnorm(model_residual2)
# Karena nilai p-value > 0.05, menandakan bahwa data berdistribusi normal

## Uji Multikolinearitas
ols_vif_tol(model_Maleprof0)
plot(data3)
cor(data3)
# Karena nilai VIF < 10 dan korelasi antar variabel dibawah 0.8, menandakan bahwa data bebas multikolinearitas

## Uji Autokorelasi
dwtest(model_Maleprof0)
# Karena nilai p-value > 0.05, menandakan bahwa data model bebas dari autokorelas

## Uji Homoskedastisitas
bptest(model_Maleprof0)
# Karena nilai p-value = 0.0001468 dan dibawah 0.05, maka model mengalami heteroskedastisitas
# Pengujian pelanggaran Homoskedastisitas
# Melaukan transformasi data
salary_baru = 1/(data3$data2.salary)^2
data_baru = data.frame(data3, salary_baru)
new_model = lm(data_baru$salary_baru ~ data_baru$d_Assocprof + data_baru$d_Asstprof + data_baru$data2.yrs.service + data_baru$d_Male)
bptest(new_model)
summary(new_model)

#Penangan kedua
ncvTest(new_model)
