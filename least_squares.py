# -*- coding: utf-8 -*-
"""
leastsq_support - short-cuts for scipy.optimize.leastsq
"""
import logging
import numpy as np

from math import factorial, log, pi, sqrt
from scipy.optimize import leastsq

logger = logging.getLogger(__name__)

def gaussian(x, p):
  """
  Gaussian function

  @param x : variable
  @param p : parameters [height, center, sigma]
  """
  return p[0] * (1/np.sqrt(2*pi*(p[2]**2))) * np.exp(-(x-p[1])**2/(2*p[2]**2))

def simple_gaussian(x, p):
  """
  un-normalized gaussian for fitting
  """
  stdev = st_dev(p[2])
  return p[0] * np.exp(-(x-p[1])**2/(stdev)**2)
  

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
  return (simple_gaussian(x,p) - y)

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
  logger.debug("fit_gaussian: initial guess: %s", initial_guess)
  logger.debug("fit_gaussian: x = %s", x)
  logger.debug("fit_gaussian: y = %s", y)
  response = leastsq(error_function,
                     x0 = initial_guess,
                     args = (x,y),
                     full_output = True)
  pars, covar, info, msg, err_code = response
  logger.debug("fit_gaussian: best fit parameter values: %s",pars)
  logger.debug("fit_gaussian: normalized covariance matrix: %s",covar)
  logger.debug("fit_gaussian: final function value: %s\n", info['fvec'])
  #std_dev = sqrt(mean(info['fvec']**2))
  std_dev = sqrt((info['fvec']**2).mean())
  err = [sqrt(covar[0,0])*std_dev,
         sqrt(covar[1,1])*std_dev,
         sqrt(covar[2,2])*std_dev]
  logger.debug("fit_gaussian: fitted height = %f+/-%f",pars[0],err[0])
  logger.debug("fit_gaussian: fitted offset = %f+/-%f",pars[1],err[1])
  logger.debug("fit_gaussian: fitted st.dev.= %f+/-%f",pars[2],err[2])
  logger.debug("fit_gaussian: number of iterations: %s",info['nfev'])
  logger.debug("fit_gaussian: residual standard deviation: %f",std_dev)
  return pars,err

def savitzky_golay(y, window_size, order, deriv=0, rate=1):
    r"""Smooth (and optionally differentiate) data with a Savitzky-Golay filter.
    The Savitzky-Golay filter removes high frequency noise from data.
    It has the advantage of preserving the original shape and
    features of the signal better than other types of filtering
    approaches, such as moving averages techniques.
    
    Parameters
    ----------
    y : array_like, shape (N,)
        the values of the time history of the signal.
    window_size : int
        the length of the window. Must be an odd integer number.
    order : int
        the order of the polynomial used in the filtering.
        Must be less then `window_size` - 1.
    deriv: int
        the order of the derivative to compute (default = 0 means only smoothing)
        
    Returns
    -------
    ys : ndarray, shape (N)
        the smoothed signal (or it's n-th derivative).
        
    Notes
    -----
    The Savitzky-Golay is a type of low-pass filter, particularly
    suited for smoothing noisy data. The main idea behind this
    approach is to make for each point a least-square fit with a
    polynomial of high order over a odd-sized window centered at
    the point.
    
    Examples
    --------
    t = np.linspace(-4, 4, 500)
    y = np.exp( -t**2 ) + np.random.normal(0, 0.05, t.shape)
    ysg = savitzky_golay(y, window_size=31, order=4)
    import matplotlib.pyplot as plt
    plt.plot(t, y, label='Noisy signal')
    plt.plot(t, np.exp(-t**2), 'k', lw=1.5, label='Original signal')
    plt.plot(t, ysg, 'r', label='Filtered signal')
    plt.legend()
    plt.show()
    
    References
    ----------
    .. [1] A. Savitzky, M. J. E. Golay, Smoothing and Differentiation of
       Data by Simplified Least Squares Procedures. Analytical
       Chemistry, 1964, 36 (8), pp 1627-1639.
    .. [2] Numerical Recipes 3rd Edition: The Art of Scientific Computing
       W.H. Press, S.A. Teukolsky, W.T. Vetterling, B.P. Flannery
       Cambridge University Press ISBN-13: 9780521880688
    .. [3] http://scipy.github.io/old-wiki/pages/Cookbook/SavitzkyGolay
    """
    try:
        window_size = np.abs(np.int(window_size))
        order = np.abs(np.int(order))
    except ValueError as msg:
        raise ValueError("window_size and order have to be of type int")
    if window_size % 2 != 1 or window_size < 1:
        raise TypeError("window_size size must be a positive odd number")
    if window_size < order + 2:
        raise TypeError("window_size is too small for the polynomials order")
    order_range = list(range(order+1))
    half_window = (window_size -1) // 2
    # precompute coefficients
    b = np.mat([[k**i for i in order_range] for k in range(-half_window, half_window+1)])
    m = np.linalg.pinv(b).A[deriv] * rate**deriv * factorial(deriv)
    # pad the signal at the extremes with
    # values taken from the signal itself
    firstvals = y[0] - np.abs( y[1:half_window+1][::-1] - y[0] )
    lastvals = y[-1] + np.abs(y[-half_window-1:-1][::-1] - y[-1])
    y = np.concatenate((firstvals, y, lastvals))
    return np.convolve( m[::-1], y, mode='valid')

def savitzky_golay_piecewise(xvals, data, kernel=11, order =4):
    """
    In cyclic voltammetry, voltage (being the abcissa) changes like a triangle
    wave. And in the signal there are cusps at the turning points (at switching
    potentials) which should never be smoothed. In this case, Savitzky-Golay
    smoothing should be done piecewise, ie. separately on pieces monotonic in x.
    """
    turnpoint=0
    last=len(xvals)
    if xvals[1]>xvals[0] : #x is increasing?
        for i in range(1,last) : #yes
            if xvals[i]<xvals[i-1] : #search where x starts to fall
                turnpoint=i
                break
    else: #no, x is decreasing
        for i in range(1,last) : #search where it starts to rise
            if xvals[i]>xvals[i-1] :
                turnpoint=i
                break
    if turnpoint==0 : #no change in direction of x
        return savitzky_golay(data, kernel, order)
    else:
        #smooth the first piece
        firstpart=savitzky_golay(data[0:turnpoint],kernel,order)
        #recursively smooth the rest
        rest=savitzky_golay_piecewise(xvals[turnpoint:], data[turnpoint:], kernel, order)
