# -*- coding: utf-8 -*-
"""
Non-linear least-squares fit to various functions.

The functions are identified by an integer ID and the number of parameters.
The functions currently coded are::
    1- Gaussians   A(1),A(4),... amplitudes
                   A(2),A(5),... offsets from X=0
                   A(3),A(6),... halfwidths
       Multiple Gaussians are added: Guas1 + Gaus2 + Gaus3 +...
    2- Polynomials A(1) + A(2)*X + A(3)*X*X + .......
    3- Cosines     A(1)          amplitude
                   A(2),A(4),... phases
                   A(3),A(5),... wavelengths
       Multiple cosines are multiplied: Cos1 * Cos2 * Cos3 *...
    4- Hyperfine structure: deuterated ammonia 1(1,1,+)-1(0,1,-);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
    5- Hyperfine structure: deuterated ammonia 1(1,1,+)-1(0,1,-);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
    6- Symmetric rotor multiplet: methyl acetylene J=6-5
          A(1) - column density
          A(2) - peak antenna temperature
          A(3) - LSR velocity
          A(4) - line width
    7- Symmetric rotor multiplet: methyl acetylene J=5-4
          Parameters as for (6) above.
    8- Hyperfine structure: deuterated ammonia 2(1,2,-)-2(0,2,+);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
    9- Hyperfine structure: deuterated ammonia 2(1,2,-)-2(0,2,+);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
   10- Hyperfine structure: ammonia 1(1);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
   11- Hyperfine structure: ammonia 1(1);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
   12- Hyperfine structure: ammonia 2(2);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
   13- Hyperfine structure: ammonia 2(2);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
   14- Pulse arrival time (seconds vs MHz).  A(1) is arrival time of
       undispersed pulse. A(2) is the dispersion measure:
       FNCTN = A(1) + 4150.*A(2)/X**2

This depends on sub-module calfit:
>>> import calfit as F
>>> help(NL.F)
Help on module Math.nonlinear.calfit in Math.nonlinear::

 NAME
    Math.nonlinear.calfit

 FILE
    /usr/local/lib/python2.6/dist-packages/Math/nonlinear/calfit.so

 DESCRIPTION
    This module 'calfit' is auto-generated with f2py (version:2).
    Functions:
      dispn2,minus = clft(x,y,weight,npts,a,sigmaa,flamda,ifnc,nterms)
      fnctn = fnctn(x,a,ifnc,nterms)
      deriv = fdrv(x,a,ifnc,nterms)
      det = mtnv(array,norder,nsize=shape(array,0))
      terms = terms(ifnc)
    COMMON blocks:
      /lus/ lu

>>> print(NL.F.clft.__doc__)
clft - Function signature:
  dispn2,minus = clft(x,y,weight,a,sigmaa,flamda,ifnc,[npts,nterms])
Required arguments:
  x : input rank-1 array('d') with bounds (npts)
  y : input rank-1 array('d') with bounds (npts)
  weight : input rank-1 array('d') with bounds (npts)
  a : in/output rank-1 array('d') with bounds (nterms)
  sigmaa : in/output rank-1 array('d') with bounds (nterms)
  flamda : input float
  ifnc : input int
Optional arguments:
  npts := len(x) input int
  nterms := len(a) input int
Return objects:
  dispn2 : float
  minus : int
"""
import calfit as F
import numpy as NP

diag = False

def nonlin_fit(data,pars,epsilon,function_ID):
  """
  Non-linear fit to an arbitrary function

  Function codes are::
    1- Gaussians   A(1),A(4),... amplitudes
                   A(2),A(5),... offsets from X=0
                   A(3),A(6),... halfwidths
       Multiple Gaussians are added: Guas1 + Gaus2 + Gaus3 +...
    2- Polynomials A(1) + A(2)*X + A(3)*X*X + .......
    3- Cosines     A(1)          amplitude
                   A(2),A(4),... phases
                   A(3),A(5),... wavelengths
       Multiple cosines are multiplied: Cos1 * Cos2 * Cos3 *...
    4- Hyperfine structure: deuterated ammonia 1(1,1,+)-1(0,1,-);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
    5- Hyperfine structure: deuterated ammonia 1(1,1,+)-1(0,1,-);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
    6- Symmetric rotor multiplet: methyl acetylene J=6-5
          A(1) - column density
          A(2) - peak antenna temperature
          A(3) - LSR velocity
          A(4) - line width
    7- Symmetric rotor multiplet: methyl acetylene J=5-4
          Parameters as for (6) above.
    8- Hyperfine structure: deuterated ammonia 2(1,2,-)-2(0,2,+);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
    9- Hyperfine structure: deuterated ammonia 2(1,2,-)-2(0,2,+);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
   10- Hyperfine structure: ammonia 1(1);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
   11- Hyperfine structure: ammonia 1(1);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
   12- Hyperfine structure: ammonia 2(2);
       low optical depth case. The parameters A(1) - A(3) are defined
       as for a single Gaussian.
   13- Hyperfine structure: ammonia 2(2);
       optical depth treated as a parameter. A(1) - A(3) are the
       Gaussian profile parameters. A(4) is the optical depth.
   14- Pulse arrival time (seconds vs MHz).  A(1) is arrival time of
       undispersed pulse. A(2) is the dispersion measure.

  @param data : x-values, y-values, weights
  @type  data : tuple of three equal length 1D numpy arrays

  @param pars : initial guess for the parameters
  @type  pars : 2D numpy array, parameters and their standard deviations

  @param epsilon : Acceptance criterion for acceptable fit. Suggest: 1e-5
  @type  epsilon : float

  @param function_ID : ID of function to fit
  @type  function_ID : int
  """
  x = data[:,0]
  y = data[:,1]
  w = data[:,2]
  n_data = data.shape[0]
  a       = asfortranarray(pars[:,0])
  sigma_a = asfortranarray(pars[:,1])
  n_pars = pars.shape[0]
  if epsilon == 0.:
    epsilon = 1e-5
  if diag:
    print "Initial parameters:",a
    print "Parameter sigmas:",sigma_a
    print "Step size parameter =",epsilon
    print "Function ID =",function_ID
  dispn2, status = F.clft(x ,y, w, a, sigma_a, epsilon, function_ID)
  if diag:
    print "New parameters:",a
    print "New sigmas:",sigma_a
  return a, sigma_a, dispn2, status

if __name__ == "__main__":
  from pylab import *
  x = NP.linspace(1400, 1900, num=100)
  pars = NP.array([[.5,  .1],
                   [50., .1]])
  a = pars[:,0]
  y1 = a[0] + 4150*a[1]/x**2
  y2 = y1 + 0.03*randn(len(x))

  figure(1)
  plot(y1,x,label="Model")
  plot(y2,x,'.',label="with noise")
  xlabel("Delay (s)")
  ylabel("Frequency (MHz)")
  title("Dispersion for DM ="+str(a[1]))

  data = NP.ones((len(x),3))
  data[:,0] = x
  data[:,1] = y2
  data[:,2] = 0.95

  a, sigma_a, sigma_sqd, status = nonlin_fit(data,pars,1e-5,14)
  if status == 1:
    print "Bad initial guess"
  elif status == -2:
    print "Too many parameters"
  elif status == -1:
    print "Not enough data points"
  else:
    print "t0 =",a[0],"+/-",sigma_a[0]
    print "DM =",a[1],"+/-",sigma_a[1]
    print "sigma^2 of fit is",sigma_sqd

  y3 = a[0] + 4150*a[1]/x**2
  plot(y3,x,'--',label="Fit")
  legend()
  show()