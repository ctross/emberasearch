############################################################ Behavior after hits 
model_dat=list(
Distance=Distance, 
AngDiff=ScaleBeta(AngDiff),
Yield=Yield,
Recovery=Recovery, 
N=N,
Lags=Lags,
MaxTrip=max(Trip),
MaxTicks=MaxTicks)
  
 model_code_3 <- '
functions{
######################################### Quick Gaussian Process Cholesky Factor
matrix Quick_GP_L(int SIZE, real COR, real DECAY, real SCALE){
 matrix[SIZE,SIZE] Rho;                       # Cholesky factor to be made
 real RealSIZE;                               # Used to normalize distance
 real G;                                      # Intermediate storage

 RealSIZE = SIZE;                             # Convert Int to Real

 for(i in 1:(SIZE-1)){
 for(j in (i+1):SIZE){
  G = ((j-i) * (j-i))/(RealSIZE*RealSIZE);    # Calculate normalized sq distance
  Rho[i,j] = COR * exp(-DECAY * G);                 # Estimate correlations
  Rho[j,i] = Rho[i,j];                        # Fill other triangle
                      }}

 for (i in 1:SIZE){
  Rho[i,i] = 1;                               # Fill diagonal
                  }

  Rho = SCALE*cholesky_decompose(Rho);        # Decompose Rho

return Rho;
}
}

data{
int MaxTrip;
int MaxTicks;
int N[MaxTrip];
int Lags;

real Distance[MaxTicks,MaxTrip];
real AngDiff[MaxTicks,MaxTrip];
int Yield[MaxTicks,MaxTrip];
int Recovery[MaxTicks,MaxTrip];
}

parameters {
real AlphaDist;
vector[2] MuBetaDist;
real<lower=0> SDDist; 

real AlphaAngle;
vector[2] MuBetaAngle;
real<lower=0> DAngle; 

vector<lower=0, upper=1>[4] Rho;
vector<lower=0>[4] Decay;
vector<lower=0>[4] Scale;

vector[Lags] Delta[2,2];
}

transformed parameters{
vector[Lags] BetaDist[2];
vector[Lags] BetaAngle[2];

BetaDist[1] = MuBetaDist[1] + Delta[1,1];
BetaAngle[1] = MuBetaAngle[1] + Delta[1,2];

BetaDist[2] = MuBetaDist[2] + Delta[2,1];
BetaAngle[2] = MuBetaAngle[2] + Delta[2,2];
}


model{
Rho ~ beta(12,2);
Decay ~ normal(0,5);
Scale ~ normal(0,5);

MuBetaDist ~ normal(0,2.5);
MuBetaAngle ~ normal(0,2.5);

Delta[1,1] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[1], Decay[1], Scale[1]));
Delta[1,2] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[2], Decay[2], Scale[2]));

Delta[2,1] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[3], Decay[3], Scale[3]));
Delta[2,2] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[4], Decay[4], Scale[4]));

AlphaDist ~ normal(0,5);
SDDist ~ cauchy(0,1);  

AlphaAngle ~ normal(0,5);
DAngle ~ normal(1,0.5); 

for(j in 1:MaxTrip){
{

 vector[N[j]-Lags] PredAngle;
 vector[N[j]-Lags] PredDist;
 vector[N[j]-Lags] Dist;
 vector[N[j]-Lags] AngleDifference;
  
 for(i in (Lags+1):N[j]){
  PredDist[i-Lags] = AlphaDist; 
     for(k in 1:Lags){
    PredDist[i-Lags] = PredDist[i-Lags] + BetaDist[1,k]*Yield[i-k,j] + BetaDist[2,k]*Recovery[i-k,j];  
        }} 
        
  for(i in (Lags+1):N[j]){
  PredAngle[i-Lags] = AlphaAngle; 
     for(k in 1:Lags){
    PredAngle[i-Lags] = PredAngle[i-Lags] + BetaAngle[1,k]*Yield[i-k,j] + BetaAngle[2,k]*Recovery[i-k,j];  
        }}       
              
   for(i in (Lags+1):N[j]){            
Dist[i-Lags] = Distance[i,j];              
AngleDifference[i-Lags] = AngDiff[i,j];
              }
Dist ~ lognormal(PredDist,SDDist);
AngleDifference ~ beta(inv_logit(PredAngle)*DAngle, (1-inv_logit(PredAngle))*DAngle);
          }}    

}

'

m3 <- stan( model_code=model_code_3, data=model_dat,refresh=refresh,chains=nchains,seed=seed,control=list(adapt_delta=ad, max_treedepth=mtd),iter=iter,warmup=warmup)



