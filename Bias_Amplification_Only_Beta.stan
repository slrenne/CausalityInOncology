data{
  int number_of_points;
  array [number_of_points] real x;
  array [number_of_points] real y;
}

parameters {
  real alpha;
  real beta;
  real sigma;
}

model{
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  
  for (i in 1:number_of_points){
     y[i] ~ normal(alpha+beta*x[i],sigma);
  }
}
