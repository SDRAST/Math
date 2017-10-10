import logging

import math as m
import numpy as np

logger = logging.getLogger(__name__)

def gaussian(x, mean, stdev):
  """
  Un-normalized gaussian

  @param x : independent variable
  @type  x : float

  @param mean : location of the center of the gaussian
  @type  mean : float

  @param stdev : standard deviation
  @type  stdev : float

  @return: float
  """
  if type(x) == float or type(x) == int:
    return m.exp(-0.5*m.pow((float(x)-mean)/stdev,2))
  elif type(x) == np.ndarray:
    if type(mean) == list:
      mean = np.array(mean)
    return np.exp(-0.5*np.power((x.astype(float)-mean)/stdev,2))
  else:
    raise TypeError('type %s is invalid' % type(x))

def multigaussian(x, pos, std, *args):
  """
  Returns the value of the sum of multiple Gaussians.
  
  Note that if x does not have points within the range of any of the Gaussians
  then zeros are returned.
  
  @param x : independent variable
  @type  x : float
  
  @param pos : position of one Gaussian in x
  @type  pos : list of float
  
  @param std : standard deviation for all the Gaussians
  @type  std : float
  
  @param peaks : tuples with ampl and relative pos of the individual Gaussians
  @type  peaks : list of tuples of float
  """
  #logger.debug("multigaussian: extra arguments: %s",args)
  amplitude = 0
  numargs = len(args)
  #logger.debug("multigaussian: number of arguments: %d",numargs)
  for index in range(numargs):
    #logger.debug("multigaussian: argument %d: %s",index,args[index])
    amp = args[index][0]
    relpos = args[index][1]
    position = pos+relpos
    #logger.debug("multigaussian: position: %f",position)
    amplitude += amp*gaussian(x, position, std)
  return np.array(amplitude)

def MMgauss(x, pos, std, *args):
  """
  A form of 'multigauss' that curfit can use.
  
  This is a sum of Gaussians in which the position 'pos', width (std) and 
  amplitudes (*args) are allowed to vary. The widths of all the Gaussians
  are equal to 'std'. The relative positions of all the Gaussians are passed
  through a global 'other_pars'.
  
  The arbitrary number of amplitudes of the Gaussians are passed in '*args'.
  The same number of positions for the Gaussians are defined as global to the
  module, e.g.::
    multigauss.other_pars = -3,-2,-1,0,1,2,3,4
  
  
  @param x : independent variable
  @type  x : nparray of float
  
  @param pos : position of the reference peak
  @type  pos : float
  
  @param std : st-dev same for all Gaussians
  @type  std : float
  
  @param args : amplitudes of the Gaussians
  @type  args : list or tuple of float
  """
  global other_pars
  #logger.debug("MMgauss: 'other_pars' is %s", other_pars)
  amps = args
  logger.debug("MMgauss: amplitudes: %s", amps)
  relpos = other_pars
  if len(amps) != len(relpos):
    logger.error("MMgauss: len(amps)=%d, len(relpos)=%d",len(amps),len(relpos))
    raise ValueError("MMgauss: amps and relpos must have the same number")
  npeaks = len(amps)
  #logger.debug("MMgauss %d peaks",npeaks)
  #logger.debug("MMgauss: amps=%s, pos=%s", amps, relpos)
  newargs = tuple(zip(amps,relpos))
  #logger.debug("MMgauss: new arguments: %s", newargs)
  values = multigaussian(x, pos, std, *newargs)
  return values

