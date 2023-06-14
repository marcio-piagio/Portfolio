resumo <- function (data) 
{
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
  # r = rbind(Na, n, min, quantil1, m, medi, quantil3, max, a, vari, ste, mse, cv, as, cur, p)
  # r = cbind(Na, n, min, quantil1, m, medi, quantil3, max, a, vari, ste, cur, p)
  r = rbind(min, quantil1, m, medi, quantil3, max, a, vari, ste, mse, cv, as, cur, p)
  r = round(r, 4)
  r = as.data.frame(r)
  # row.names(r) = c("Na's", "n", "Minimo", "1°Quantil","Média", "Mediana", "3°Quantil", "Máximo",
  #                 "Amplitude total", "Variância", "Desvio padrão", "Erro padrão",
  #                 "Coef. de variação (%)","Assimetria", "Curtose", "P-value (Shapiro-Wilk)")
  
  row.names(r) = c("Minimo", "1°Quantil","Média", "Mediana", "3°Quantil", "Máximo",
                  "Amplitude total", "Variância", "Desvio padrão", "Erro padrão",
                  "Coef. de variação (%)","Assimetria", "Curtose", "P-value (Shapiro-Wilk)")
  
  # colnames(r) = c("Na's", "n", "Minimo", "1°Quantil","Média", "Mediana", "3°Quantil", "Máximo",
  #                 "Ampl. total", "Variância", "Dp",
  #                 "Coef. variação", "Shapiro")
  # row.names(r) = " "
  return(r)
}

