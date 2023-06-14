teste.escore = function(dados, escore, comp.mult, correcao.p)
{
  
  method = "Estatística Qs - Teste Escore"
  data.name = deparse(substitute(dados))
  fb  = apply(dados, 1, function(x) (sum(x*escore))/sum(x))
  fb  = round(fb,3)
  esp = colSums(dados)/sum(dados)
  mua = sum(escore*esp)
  va  = sum((escore-mua)^2*esp)
  sf  = sum(sapply(1:nrow(dados), function(i) sum(dados[i,])*((fb[i]-mua)^2)))
  Qs  = ((sum(dados)-1)*sf)/(sum(dados)*va)
  Qs  = round(Qs,3)
  gl  = nrow(dados)-1
  p   = 1-pchisq(Qs,gl)
  p   = round(p,5)
  names(Qs) = "Qs"
  names(gl) = "df"
  global = structure(list(statistic=Qs, parameter=gl, p.value=p,
                          method=method, data.name=data.name),
                     class="htest")
  # Comparações múltiplas
  comp = function(dados,escore,i,j)
  {
    fb  = apply(dados, 1, function(x) (sum(x*escore))/sum(x))
    fb  = round(fb,3)
    dados2 = dados[c(i,j),]
    group = rownames(dados2)
    esp = colSums(dados2)/sum(dados2)
    mua = sum(escore*esp)
    va  = sum((escore-mua)^2*esp)
    vbf = ((sum(dados2) - sum(dados2[1,]))/(sum(dados2[1,])*(sum(dados2)-1)))*va
    Qs  = ((fb[i]-mua)^2)/vbf
    Qs  = round(Qs,3)
    gl  = nrow(dados2)-1
    p   = 1-pchisq(Qs,gl)
    p2  = p.adjust(p, method = correcao.p,n = choose(nrow(dados),2))
    p   = round(p,5)
    p2  = round(p2,5)
    mult = cbind(paste(group[1], "-", group[2]), Qs, gl, p, p2)
    colnames(mult) = c("Grupo", "Qs", "gl", "p.value", "correct.p")
    return(mult)
  } 
  if(comp.mult){
    ij = (combn(x = 1:nrow(dados), m = 2))
    mult = t(mapply(function(x,y) comp(dados,escore,x,y), ij[1,], ij[2,]))
    mult = as.data.frame(mult)
    mult[,2:5] = apply(mult[,2:5],2,function(x) as.numeric(as.character(x)))
    colnames(mult) = c("Grupo", "Qs", "gl", "p.value", "correct.p")
    return(structure(list(Global = global, Comp.multiplas = mult, Fbar = fb)))
  } else {
    return(structure(list(Global = global)))
  }
} 


library(knitr)

resulmo <- function (data) 
{
  library(knitr)
  options(scipen = 999)
  options(digits = 4)
  options(knitr.kable.NA = '')
  nam = names(as.data.frame(data))
  x = as.data.frame(data)
  d1 = sapply(x, list)
  d2 = sapply(d1, na.omit)
  h = ifelse(is.list(d2), 1, 2)
  ww = list(f1 = function(d2) {
    return(d2)
  }, f2 = function(d2) {
    return(as.data.frame(d2))
  })
  d5 = ww[[h]]
  d5 = d5(d2)
  qww = ifelse(ncol(x) == 1, 2, 1)
  qw = list(n1 = sapply(d5, length), n2 = nrow(d2))
  n = qw[[qww]]
  f1 = function(x) {
    r = sum((x - mean(x, na.rm = TRUE))^2, na.rm = T)
    return(r)
  }
  s1 = sapply(x, f1)
  mm2 = matrix(s1, ncol = 1)
  mm2 = cbind(mm2, n)
  m2 = mm2[, 1]/mm2[, 2]
  f2 = function(x) {
    r = sum((x - mean(x, na.rm = TRUE))^3, na.rm = T)
    return(r)
  }
  s2 = sapply(x, f2)
  mm3 = matrix(s2, ncol = 1)
  mm3 = cbind(mm3, n)
  m3 = mm3[, 1]/mm3[, 2]
  f3 = function(x) {
    r = sum((x - mean(x, na.rm = TRUE))^4, na.rm = T)
    return(r)
  }
  s3 = sapply(x, f3)
  mm4 = matrix(s3, ncol = 1)
  mm4 = cbind(mm4, n)
  m4 = mm4[, 1]/mm4[, 2]
  med = function(x) {
    m = mean(x, na.rm = TRUE)
    return(m)
  }
  maxi = function(x) {
    maxi = max(x, na.rm = TRUE)
    r = return(maxi)
  }
  mini = function(x) {
    mini = min(x, na.rm = TRUE)
    r = return(mini)
  }
  medi = function(x) {
    medi = median(x, na.rm = TRUE)
    return(medi)
  }
  quant1 = function(x) {
    quant1 = quantile(x, na.rm = TRUE, prob = c(0.25))
    return(quant1)
  }
  quant3 = function(x) {
    quant3 = quantile(x, na.rm = TRUE, prob = c(0.75))
    return(quant3)
  }
  na = function(x){
    na = length(x[is.na(x)])
    return(na)
  }
  Na = sapply(x, na)
  p = sapply(x, shapiro.test)
  m = sapply(x, med)
  max = sapply(x, maxi)
  min = sapply(x, mini)
  medi = sapply(x, medi)
  quantil1 = sapply(x, quant1)
  quantil3 = sapply(x, quant3)
  a = max - min
  vari = as.numeric(mm2[, 1])/as.numeric(mm2[, 2] - 1)
  ste = vari^0.5
  mse = ste/(n^0.5)
  cv = ste * 100/m
  as = m3/(m2^(3/2))
  cur = m4/m2^2
  inf = 10000 - (Na + n)
  N   = inf + Na + n
  p = as.numeric(p[2, ])
  # r = cbind(Na, n, min, quantil1, m, medi, quantil3, max, a, vari, ste, mse, cv, as, cur, p)
  r = cbind(Na, n, min, quantil1, m, medi, quantil3, max, a, vari, ste, cur, p)
  r = round(r, 4)
  r = as.data.frame(r)
  # colnames(r) = c("Na's", "n", "Minimo", "1°Quantil","Média", "Mediana", "3°Quantil", "Máximo",
  #                 "Amplitude total", "Variância", "Desvio padrão", "Erro padrão",
  #                 "Coef. de variação (%)","Assimetria", "Curtose", "P-value (Shapiro-Wilk)")
  colnames(r) = c("Na's", "n", "Minimo", "1°Quantil","Média", "Mediana", "3°Quantil", "Máximo",
                  "Ampl. total", "Variância", "Dp",
                  "Coef. variação", "Shapiro")
  row.names(r) = " "
  return(kable(r,align = "c", format = "html"))
}


