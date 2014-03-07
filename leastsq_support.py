# -*- coding: utf-8 -*-
"""
leastsq_support - short-cuts for scipy.optimize.leastsq
"""
from scipy.optimize import leastsq

def gaussian(x, p):
  """
  Gaussian function

  @param x : variable
  @param p : parameters [height, center, sigma]
  """
  return p[0]*(1/sqrt(2*pi*(p[2]**2)))*exp(-(x-p[1])**2/(2*p[2]**2))


def gaussian_error_function(p, x, y):
  """
  Difference between model and actual data

  @param p : parameters [height, center, sigma]
  @type  p : list

  @param x : variable
  @type  x : numpy array

  @param y : measured value
  @type  y : numpy array

  @return: numpy array
  """
  return (gaussian(x,p) - y)

def width_half_max(st_dev):
  """
  Gaussian full width at half maximum from standard deviation

  @param st_dev : standard deviation
  @type  st_dev : float

  @return: float
  """
  return 2*sqrt(2*log(2))*st_dev

def st_dev(width_half_max):
  """
  Standard deviation from Gaussian full width at half maxiuam

  @param width_half_max : Gaussian full width at half maximum
  @type  width_half_max : float

  @return: float
  """
  return width_half_max/2/sqrt(2*log(2))

def fit_gaussian(error_function, initial_guess, x, y):
  """
  Invoke least squares fit to a Gaussian function

  @param error_function : function to compute difference between model and data
  @type  error_function : function instance

  @param initial_guess : parameters for an initial guess for the parameters
  @type  initial_guess : list of float

  @param x : independent variable
  @type  x : numpy array of float

  @param y : data
  @type  y : numpy array of float

  @return: (list of parameters, list of formal errors)
  """
  response = leastsq(error_function,
                     x0 = initial_guess,
                     args = (x,y),
                     full_output = True)
  pars, covar, info, msg, err_code = response
  if diag:
    print "Best fit parameter values:",pars
    print "Normalized covariance matrix:\n",covar
  std_dev = sqrt(mean(info['fvec']**2))
  err = [sqrt(covar[0,0])*std_dev,
         sqrt(covar[1,1])*std_dev,
         sqrt(covar[2,2])*std_dev]
  if diag:
    print "Fitted height =",pars[0],"+/-",err[0]
    print "Fitted offset =",pars[1],"+/-",err[1]
    print "Fitted st.dev.=",pars[2],"+/-",err[2]
    print "Number of iterations:",info['nfev']
    print "Residual standard deviation:",std_dev
  return pars,err

