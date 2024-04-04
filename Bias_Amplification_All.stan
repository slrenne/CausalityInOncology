data{
  int number_of_points;
  array [number_of_points] real x;
  array [number_of_points] real y;
  array [number_of_points] real z;
}

parameters {
  real alpha;
  real beta;
  real gamma;
  real sigma;
}

model{
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  gamma ~ normal (0,1);
  sigma ~ normal(0,1);
  
  for (i in 1:number_of_points){
     y[i] ~ normal(alpha+beta*x[i]+gamma*z[i],sigma);
  }
}
  


