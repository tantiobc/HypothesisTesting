library(readxl)
station = read_excel("C:/praktikum/prak.xlsx", sheet = "contoh3")

station1 = station$station1
station2 = na.omit(station$station2)
S1 = sd(station1)		  #standar deviasi sampel x1 
S2 = sd(station2)		  #standar deviasi sampel x2
n1 = length(station1)	#banyak observasi x1
n2 = length(station2)	#banyak observasi x2
alpha = 0.05	        #taraf signifikansi

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
var.test(station1, station2, ratio = 1, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)