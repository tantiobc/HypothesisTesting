library(readxl)
alat = read_excel("C:/praktikum/tes_akhir.xlsx", sheet = "nomor2")

alat_A = alat$A
alat_B = alat$B
S1 = sd(alat_A)		    #standar deviasi sampel x1 
S2 = sd(alat_B)		    #standar deviasi sampel x2
n1 = length(alat_A)	  #banyak observasi x1
n2 = length(alat_B)	  #banyak observasi x2
alpha = 0.01	        #taraf signifikansi

# Boxplot data Alat A dan Alat B
boxplot(alat, horizontal=T, main="Uji Kandungan CO Alat A dan B")

#Cara Manual (Bandingkan F hitung dan F tabel)
F = S1^2/S2^2				                      #F hitung
F.lower = qf(alpha, n1-1, n2-1)			      #chi tabel eka arah
F.upper = qf(1-alpha, n1-1, n2-1) 		    #chi tabel eka arah
F.half.alpha = qf(1-alpha/2, n1-1, n2-1)	#chi tabel dwi arah
F.twosided = c(-F.half.alpha, F.half.alpha)

#P-value (bandingkan p-value dengan alpha)
pval.lower = pf(F, n1-1, n2-1)					            #eka arah
pval.upper = pf(F, n1-1, n2-1, lower.tail = FALSE)	#eka arah
pval.twosided = 2*pf(F, n1-1, n2-1)				          #dwi arah

#Cara Otomatis
var.test(alat_A, alat_B, ratio = 1, alternative = c("two.sided", "less", "greater"), conf.level = 0.99)