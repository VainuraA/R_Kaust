#ronorm annab suvalised arvud mingi keskmise väärtuse ja standard hälbe kaudu,
#esimesena mitu arvu tahad, siis keskmine vöörtus ja siis standardhälve

rnorm(1, 170, 10)
rnorm(5, 170, 10)

pikkused = rnorm(20, 170, 10)

taburett = rnorm(20, 40, 10)

kokku = pikkused + taburett

length(pikkused[pikkused>180])
length(taburett[taburett>50])
length(kokku[kokku>230])