data{
  int number_of_points;
  array [number_of_points] real ETOH;
  array [number_of_points] real BLP;
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
     BLP[i] ~ normal(alpha+beta*ETOH[i],sigma);
  }
}
  


