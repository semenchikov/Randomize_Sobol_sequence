RS <- function (n, s, u, a, b)	{

library(gsl) 
library("matlab")

q<-qrng_alloc(type="sobol",s)
rs<-qrng_get(q,n)



for (i in 1:s)  	{
		
			
			for (j in 1:n)  	{
			
					sum<-0
					for (k in 1:u)  	{	
						
								rs[j,i]<-2*rs[j,i]
	   							sum<-sum+(rs[j,i]%/%1)*(2^(-k))
								rs[j,i]<-rs[j,i]-(rs[j,i]%/%1)	   

	  		       		 		}
					rs[j,i]<-sum
					p=runif(1)
					rs[j,i]<-rs[j,i] + (2^(-u))*p
					rs[j,i]<-a+(b-a)*rs[j,i]
					}
			}
return (rs)
}

