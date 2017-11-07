output = matrix(nrow=45,ncol=10)
colnames(output) = seq(0,9)/5
rownames(output) = seq(6,50)

for(diff in 1:10){
  for(samples in 6:50){
    p_vals = c()
    for(k in 1:30){
      p_vals[k] = as.numeric(t.test(rnorm(samples,0,1),rnorm(samples,diff/5,1))$p.value)
    }
    output[samples-5,diff] = mean(p_vals)
  }
}

z=output
x=1:nrow(z)
y=1:ncol(z)
persp(x,y,z,theta = 135, phi = 30, col = "green3", scale = T,xlab="samples",ylab="effect size",zlab="mean p")
