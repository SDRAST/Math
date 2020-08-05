"""
Find clusters by vector quantization

Reference
=========
https://docs.scipy.org/doc/scipy/reference/cluster.vq.html
"""
import logging
import numpy as NP

import scipy.cluster.vq as VQ

logger = logging.getLogger()

def find_clusters(data, n_clusters=2):
  """
  find clusters in 2D data
  
  Args:
    data: numpy array of n 2D coordinates (observations)
  
  Notes
  =====
  ``whiten`` normalizes a group of observations on a per feature basis.
  Each feature is divided by its standard deviation across all observations 
  to give it unit variance.
  """
  
  whitened = VQ.whiten(data)
  ratio = 1
  while ratio >= 1:
    centers, distortion = VQ.kmeans(whitened, n_clusters)
    ratio = distortion/n_clusters
    logger.debug("cluster_order: distortion= %f", distortion)
    logger.debug("cluster_order: n_clusters= %s", n_clusters)
    ratio = distortion*n_clusters
    if n_clusters == len(data)-1:
      break
    n_clusters += 1
  factors = data/whitened
  logger.debug("cluster_order: factors:\n %s", factors)
  center_x = centers[:,0]*factors[0,0]
  center_y = centers[:,1]*factors[0,1]
  center_coords = NP.array([center_x, center_y])
  logger.debug("cluster_order: centers:\n%s", center_coords)
  return center_coords
