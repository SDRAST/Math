# -*- coding: utf-8 -*-
"""
Allan variance

not yet tested since bug fixes - tbhk 2007-10-12
"""
import math
import logging

module_logger = logging.getLogger(__name__)

def group_variance(data, n_data, n_grps):
   """
   Computes the variance of n_data points of data[], averaged
   in groups of n_grps and returns the variance for these groups
   """
   num_avgd = n_data/n_grps
   avg = []
   # Find the average each group
   for grp in range(n_grps):
      average = 0
      for sample in range(num_avgd):
         average += data[int(grp*num_avgd + sample)]
      avg.append(average/num_avgd)
   # Compute the variance for the groups
   sigma2 = 0
   for grp in range(n_grps):
      sigma2 += math.pow((avg[grp]-avg[grp-1]),2)/n_grps/2
   return sigma2

def allan_variance(data):
   """
   Computes Allan variance
   
   Definition of variance::
   
                               2
            sum (sample - mean)
     var =  --------------------
               n_samples
             
   Computes the variances obtained when data[] is grouped in ever larger
   groups, increasing by factors of two from groups of size two.  Returns
   two lists, the first giving the group sizes and the seconds the variances.
   Only 2^N, where N = int(log(ndata,2)), data are used.
   
   A set of Gaussian noise should produce ever smaller variations by sqrt(2)
   for the first group to the last.
   """
   ndata = len(data)
   exponent = int(math.log(ndata,2))
   max_data = math.pow(2,exponent)
   group_size = 2
   sizes = []
   vars = []
   while group_size < max_data/2:
      n_grps = int(max_data/group_size)
      var = group_variance(data, ndata, n_grps)
      sizes.append(group_size)
      vars.append(var)
      group_size *= 2
   return sizes,vars

