# -*- coding: utf-8 -*-
"""
Local mathematical functions in addition to the Python 'math' module.

The smoothing functions are adapted from:
http://www.swharden.com/blog/2008-11-17-linear-data-smoothing-in-python/
"""

from math import *
from Math.statistics import *
from Math import Bin
import numpy

def solid_angle(angle):
  """
  Solid angle for a circular pattern

  @param angle : diameter of circular pattern in angle units (e.g. deg)
  @type  angle : float

  @return: float, in square units (e.g. sq. deg)
  """
  return (pi/4)*angle**2
  
def gaussian(x,mean,stdev):
  """
  Un-normalized gaussian

  @param x : independent variable
  @type  x : float

  @param mean : value of x at the peak
  @type  mean : float

  @param stdev : standard deviation
  @type  stdev : float

  @return: float
  """
  return exp(-0.5*pow((float(x)-mean)/stdev,2))

def normalized_gaussian(x,mean,stdev):
  """
  gaussian function whose area under the curve is unity.

  @param x : independent variable
  @type  x : float

  @param mean : value of x at the peak
  @type  mean : float

  @param stdev : standard deviation
  @type  stdev : float

  @return: float
  """
  return gaussian(x,mean,stdev)/sqrt(2*pi)/stdev

def scaled_gaussian(x,mean,fwhm):
    """returns a normalized gauusian's value at x, given the mean and
    full width at half maximam"""
    stdev = fwhm/2./sqrt(2*log(2))
    return normalized_gaussian(x,mean,stdev)

def decimal_to_binary(integer,nbits=8,grouped=0):
    """Converts integer to binary string of length nbits, sign bit and
    then m.s.b. on the left.  Negative numbers are twos-complements, i.e.,
    bitwise complement + 1."""
    # Just remember that minus sign and ignore it
    if integer < 0:
        negative = True
        integer = abs(integer+1)
    else:
        negative = False
    # build up the strin
    result = ''
    # part of number left to process
    remaining_integer = integer
    while (remaining_integer > 0) & (nbits > 0):
        lsb = remaining_integer % 2
        if negative:
            lsb = 1-lsb
        result = ''.join((str(lsb),result))
        remaining_integer = remaining_integer >> 1
        nbits -= 1
    while nbits > 0:
        if negative:
            result = ''.join(('1',result))
        else:
            result = ''.join(('0',result))
        nbits -= 1
    if grouped:
      temp = result
      result = ""
      for bit in range(len(temp)):
        if bit and (bit % grouped) == 0:
          result += ' '
        result += temp[bit]
    return result

def binary_to_decimal(string):
    """Converts a binary string, sign bit then msb on left, to an integer.
    Negative numbers are assumed to be twos-complements, i.e., bitwise
    complement + 1."""
    length = len(string)
    # negative part doesn't work
    if string[0] == '1':
        negative = True
    else:
        negative = False
    magnitude = 0
    for i in range(length-1,0,-1):
        # print 'index=',i
        if string[i] == '1':
            magnitude = magnitude + pow(2,length-i-1)
        # print i,string[i],magnitude
    if negative:
        magnitude = -pow(2,length-1)+magnitude
    return int(magnitude)

def power_of_2_ge(number):
  """This finds the integer greater than 'number' which is a power of two.
  It could be useful for zero-filling a data array to be processed with
  a fast Fourier transform."""
  exponent = math.log(number,2)
  if exponent != int(exponent):
    int_exp = int(exponent+1)
    return int(math.pow(2,int_exp))
  else:
    return number


def smoothList(datalist, degree=10):
  """
  Smooth a list of numbers by averaging 'degree' samples.

  This reduces the length of the data set by (degree-1) samples.

  @param datalist : data set
  @type  datalist : list of float

  @param degree : width of smoothing rectangle in number of samples
  @type  degree : int

  @return: list of smoothed data
  """
  # Initialize the output to zeros.
  smoothed = [0]*(len(datalist)-degree+1)
  # smooth the data
  for i in range(len(smoothed)):
    smoothed[i] = sum(datalist[i:i+degree])/float(degree)
  return smoothed

def smoothListTriangle(datalist, degree=5):
  """
  Smooth a list of numbers by convolving with a triangle.

  The returned data set is shorter by 2*degree-1 samples

  @param datalist : data set
  @type  datalist : list of float

  @param degree : width of triangle at half maximum in number of samples
  @type  degree : int

  @return: list of smoothed data
  """
  # initialize the smoothed result
  window = degree*2-1
  smoothed = [0.0]*(len(datalist)-window)
  # create an array of weights
  weight = []
  for x in range(1,2*degree):
    weight.append(degree-abs(degree-x))
  w=numpy.array(weight)
  # smooth the data
  for i in range(len(smoothed)):
    smoothed[i] = sum(numpy.array(datalist[i:i+window])*w)/float(sum(w))
  return smoothed

def smoothListGaussian(datalist, degree=5):
  """
  Gaussian data smoothing function.

  The data returned are 2*degree-1 fewer.

  @param datalist : data set
  @type  datalist : list of float

  @param degree : Gaussian halfwidth at half maximum in number of samples
  @type  degree : int

  @return: list of smoothed data
  """
  #initialize the smoothed data
  window = degree*2-1
  smoothed = [0.0]*(len(datalist)-window)
  # create the weights
  weight = numpy.array([1.0]*window)
  weightGauss = []
  for i in range(window):
    i = i-degree+1
    frac = i/float(window)
    gauss = 1/(numpy.exp((4*(frac))**2))
    weightGauss.append(gauss)
  weight = numpy.array(weightGauss)*weight
  # smooth the data
  for i in range(len(smoothed)):
    smoothed[i] = sum(numpy.array(datalist[i:i+window])*weight)/sum(weight)
  return smoothed
