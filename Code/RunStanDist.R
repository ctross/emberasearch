########################################################## Extract data for Stan    
model_dat=list(
DistanceVec=Distance[Distance!=999999], 
Nvec=length(Distance[Distance!=999999]))

################################################################# Fit Normal
model_code_N0 <- '
data{
int Nvec;
vector<lower=0>[Nvec] DistanceVec;
}

parameters {
real A;
real<lower=0> B; 
}

model{
A ~ normal(0,5);
B ~ cauchy(0,1); 
 
for(n in 1:Nvec)
DistanceVec[n] ~ normal(A,B)T[0,]; 
}

generated quantities{
vector[Nvec] log_lik;

for(n in 1:Nvec)
    log_lik[n] = normal_lpdf( DistanceVec[n] | A, B ) - normal_lccdf(0 | A, B);
}

'

 mN0 <- stan( model_code=model_code_N0, data=model_dat,refresh=100,chains=1,seed=1234)
 print(mN0, pars=c("A","B"))

############################################################### Fit Truncated N
model_code_N0t <- '
data{
int Nvec;
vector[Nvec] DistanceVec;
}

parameters {
real A;
real<lower=0> B; 

real<lower=max(DistanceVec)> U;
}

model{
A ~ normal(0,5);
B ~ cauchy(0,1);  
U ~ normal(20,5)T[max(DistanceVec),];

for(n in 1:Nvec)
DistanceVec[n] ~ normal(A,B)T[0,U]; 
}

generated quantities{
vector[Nvec] log_lik;

for(n in 1:Nvec)
    log_lik[n] = normal_lpdf( DistanceVec[n] | A, B )  -log_diff_exp(normal_lcdf(U | A, B),normal_lcdf(0 | A, B));
    
}
                        
'

 mN0t <- stan( model_code=model_code_N0t, data=model_dat,refresh=100,chains=1,seed=1234)
 print(mN0t, pars=c("A","B","U"))

################################################################# Fit Log Normal
model_code_LN <- '
data{
int Nvec;
vector[Nvec] DistanceVec;
}

parameters {
real A;
real<lower=0> B; 
}

model{
A ~ normal(0,5);
B ~ cauchy(0,1);  

DistanceVec ~ lognormal(A,B); 
}

generated quantities{
vector[Nvec] log_lik;

for(n in 1:Nvec)
    log_lik[n] = lognormal_lpdf( DistanceVec[n] | A, B );
}

'

 mLN <- stan( model_code=model_code_LN, data=model_dat,refresh=100,chains=1,seed=1234)
 print(mLN, pars=c("A","B"))

############################################################### Fit Truncated LN
model_code_LNt <- '
data{
int Nvec;
vector[Nvec] DistanceVec;
}

parameters {
real A;
real<lower=0> B; 

real<lower=max(DistanceVec)> U;
}

model{
A ~ normal(0,5);
B ~ cauchy(0,1);  
U ~ normal(20,5)T[max(DistanceVec),];

for(n in 1:Nvec)
DistanceVec[n] ~ lognormal(A,B)T[,U]; 
}

generated quantities{
vector[Nvec] log_lik;

for(n in 1:Nvec)
    log_lik[n] = lognormal_lpdf( DistanceVec[n] | A, B ) - lognormal_lcdf(U | A, B);
    
}

'

 mLNt <- stan( model_code=model_code_LNt, data=model_dat,refresh=100,chains=1,seed=1234)
 print(mLNt, pars=c("A","B","U"))
  
##################################################################### Fit Pareto
model_code_P <- '
data{
int Nvec;
vector[Nvec] DistanceVec;
}

parameters {
real<lower=0,upper=min(DistanceVec)> A;
real<lower=0> B; 
}

model{
A ~ uniform(0,min(DistanceVec));
B ~ cauchy(0,1);  

DistanceVec ~ pareto(A,B); 
}

generated quantities{
vector[Nvec] log_lik;

for(n in 1:Nvec)
    log_lik[n] = pareto_lpdf( DistanceVec[n] | A, B );
}

'

 mP <- stan( model_code=model_code_P, data=model_dat,refresh=100,chains=1,seed=1234)
 print(mP, pars=c("A","B"))
 
########################################################### Fit Truncated Pareto
model_code_Pt <- '
data{
int Nvec;
vector[Nvec] DistanceVec;
}

parameters {
real<lower=0,upper=min(DistanceVec)> A;  
real<lower=0> B; 
real<lower=max(DistanceVec)> U;
}

model{
A ~ uniform(0,min(DistanceVec));
B ~ cauchy(0,1);  
U ~ normal(20,5)T[max(DistanceVec),];

for(n in 1:Nvec)
DistanceVec[n] ~ pareto(A,B)T[,U]; 
}

generated quantities{
vector[Nvec] log_lik;

for(n in 1:Nvec)
    log_lik[n] = pareto_lpdf( DistanceVec[n] | A, B ) - pareto_lcdf(U | A, B);
}

'

 mPt <- stan( model_code=model_code_Pt, data=model_dat,refresh=100,chains=1,seed=1234)
 print(mPt, pars=c("A","B","U"))
 

   
waicComp <- compare(mP,mPt,mLN,mLNt,mN0,mN0t) 

X <- xtable(round(waicComp@output[,c(1,3,4)],1))
  writeLines(print(X),"Table2.txt")
 
 
 
 
 
 
