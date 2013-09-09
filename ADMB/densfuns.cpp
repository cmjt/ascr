#include <admodel.h>

// Helper prototypes:
dvariable bessi0 (dvariable x);

// Normal distribution functions:

dvariable log_dnorm (double x, const prevariable& mu, const prevariable& sigma)
{
  const double pi = 3.141592653589793238463;
  return -0.5*log(2*pi) - log(sigma) - square(x - mu)/(2*square(sigma));
}

dvariable log_dpois (double x, const prevariable& mu)
{
  return log_density_poisson(x, mu);
}

// dvariable log_dvm (double x, dvector mu, const dvariable& kappa)
// {
//   const double pi = 3.14159265359;
//   return kappa*cos(x - mu) - log(2*pi*bessi0(kappa));
// }

//Helpers:
dvariable bessi0 (dvariable x)
{
  dvariable ax,ans;
  dvariable y;
    if ((ax = fabs(x)) < 3.75){
    y = x/3.75;
    y *= y;
    ans = 1.0 + y*(3.5156229 + y*(3.0899424 + y*(1.2067492 + y*(0.2659732 + y*(0.360768e-1 + y*0.45813e-2)))));
  } else {
    y = 3.75/ax;
    ans = (exp(ax)/sqrt(ax))*(0.39894228 + y*(0.1328592e-1 + y*(0.225319e-2 + y*(-0.157565e-2 + y*(0.916281e-2 + y*(-0.2057706e-1 + y*(0.2635537e-1 + y*(-0.1647633e-1 + y*0.392377e-2))))))));
  }
  return ans;
}
