library(readxl)
film = read_excel("C:/praktikum/prak.xlsx", sheet = "contoh1")

film1 = na.omit(film$company1)
film2 = film$company2
xbar1 = mean(film1)	           #mean sampel company1
xbar2 = mean(film2)	           #mean sampel company2
S1 = sd(film1)		             #standar deviasi sampel company1
S2 = sd(film2)		             #standar deviasi sampel company2
n1 = length(film1)             #banyak observasi company1
n2 = length(film2)             #banyak observasi company2
alpha = 0.05		               #taraf 
mu0 = 10                       #nilai hipotesis

#Cara Manual
df = ((S1^2/n1) + (S2^2/n2))^2/(((1/(n1-1))*(S1^2/n1)^2)+((1/(n2-1))*(S2^2/n2)^2))
xbar = xbar2 - xbar1
t = (xbar-mu0)/(sqrt((S2^2/n2)+(S1^2/n1))) 	#t hitung
t.lower = qt(alpha, df)			                #t tabel eka arah
t.upper = qt(1-alpha, df)			              #t tabel eka arah
t.half.alpha = qt(1-alpha/2, df)	          #t tabel dwi arah
t.twosided = c(-t.half.alpha, t.half.alpha)

#p-value (bandingkan p-value dengan alpha)
pval.lower = pt(t, df=n1+n2-1)						          #eka arah
pval.upper = pt(t, df=n1+n2-1, lower.tail = FALSE)	#eka arah
pval.twosided = 2*pt(t, df=n1+n2-1)					        #dwi arah

#Cara otomatis
t.test(film1, film2, mu=mu0, paired = FALSE, var.equal=FALSE, alternative=c("two.sided","less","greater"),conf.level=0.95)