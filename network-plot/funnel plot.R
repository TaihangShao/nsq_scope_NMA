#OS
ord=seq(1,10,1)

funnel(a, method.bias = "Egger",order = ord,
       col = "black",
       legend = FALSE,
       digits.pval = 2)
#PFS
ord=seq(1,11,1)

funnel(a, method.bias = "Egger",order = ord,
       col = "black",
       legend = FALSE,
       digits.pval = 2)
