library(readxl)
ban = read_excel("C:/praktikum/prak.xlsx", sheet = "contoh2")

radial = ban$radial
belted = ban$belted
d = radial-belted
dbar = mean(d)
mu0 = 0         #nilai hipotesis
sd = sd(d)		  #standar deviasi sampel 
n = length(d)		#banyak observasi
alpha = 0.05		#taraf signifikansi

#Cara Manual
t = (dbar)/(sd/sqrt(n))	              #t hitung
t.lower = qt(alpha,df = n-1)		      #t tabel eka arah
t.upper = qt(1-alpha,df=n-1)				  #t tabel eka arah
t.half.alpha = qt(1-alpha/2,df=n-1)		#t tabel dwi arah
t.twosided = c(-t.half.alpha, t.half.alpha)

#p-value (bandingkan p-value dengan alpha)
pval.lower = pt(t, df=n-1)						          #eka arah
pval.upper = pt(t, df=n-1, lower.tail = FALSE)	#eka arah
pval.twosided = 2*pt(t, df=n-1)					        #dwi arah

#Cara otomatis
t.test(radial, belted, mu=mu0, paired=TRUE, alternative = c("two.sided","less","greater"), conf.level=0.95)