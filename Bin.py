# -*- coding: utf-8 -*-
"""
Binary operations

Example::
 In [1]: import Math.Bin as B

 In [2]: B.setbit(0,12)
 Out[2]: 4096

 In [3]: "%x" % B.setbit(0,12)
 Out[3]: '1000'
"""

def setbit(word,bit):
  """
  set bit in word to 1
  """
  bitvalue = 2**bit
  return word | bitvalue

def clrbit(word,bit):
  """
  Clear bit in word (set to 0)
  """
  bitvalue = 2**bit
  return word ^ bitvalue

def getbit(word,bit):
  """
  get the value of bit in word
  """
  bitvalue = 2**bit
  if word & bitvalue:
    return 1
  else:
    return 0

def bitcnt(val):
  '''
  Counts the number of set bits in the binary value.
  '''
  ret_val=0
  shift_val=val
  while shift_val>= 1:
    if shift_val & 1:
      ret_val +=1
    shift_val = shift_val >> 1
  return ret_val
