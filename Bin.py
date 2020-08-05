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
import logging
import re

logger = logging.getLogger(__name__)

def setbit(word,bit):
  """
  set bit in word to 1
  """
  bitvalue = 2**bit
  #logger.debug("bit value = %d", bitvalue)
  new = word | bitvalue
  #logger.debug("new word = %d", new)
  return new

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

def field_mask(bits, leftshift=0):
  """
  Set a bit mask
  
  Example::
    In [2]: bin(field_mask("5:0"))
    Out[2]: '0b111111'
    In [4]: bin(field_mask("5:0", leftshift=6))
    Out[4]: '0b111111000000'
    In [5]: bin(field_mask("111111", leftshift=6))
    Out[5]: '0b111111000000'
    In [6]: field_mask("111111", leftshift=6)
    Out[6]: 4032

  @param bits : mask bit pattern, like '111111' or '0b111111' or range '5:0'
  @type  bits : str
  
  @param leftshift : number of bits to move to left (to m.s.b.)
  @type  leftshift : int
  
  @return: int
  """
  if type(bits) != str:
    raise ValueError ("first argument must be a binary or bit range string")
  if re.search(":",bits):
    parts = bits.split(":")
    left = int(parts[0])
    right = int(parts[-1])
    mask_value = 0
    for bit in range(left,right-1,-1):
      mask_value += 2**bit
  else:
    try:
      mask_value = int(bits, 2)
    except ValueError as details:
      logger.error("%s is not a binary string", bits)
      raise ValueError(details)
  if leftshift:
    return mask_value << leftshift
  else:
    return mask_value

def get_field(word, mask=255):
  """
  Extract a value from a range of bits (bit field)
  
  Test::
    In [5]: get_field(int("101010010101",2), mask="11:6")
    DEBUG:Math.Bin:get_field: word:      0b101010010101
    DEBUG:Math.Bin:get_field: mask:      0b111111000000
    DEBUG:Math.Bin:get_field: extracted: 0b101010000000
    DEBUG:Math.Bin:get_field: shifted:   0b101010
    Out[5]: 42
    
  @param value : word to be masked
  @type  value : int
  
  @param mask : bitmask; default 255 ('0b11111111'), l.s.byte
  @type  mask : int
  
  @return: int
  """
  #logger.debug("get_field: word: %s (%d)", bin(word), word)
  if type(mask) != int:
    #logger.debug("get_field: given mask: %s", mask)
    mask = field_mask(mask)
  else:
    #logger.debug("get_field: given mask: %d", mask)
    pass
  #logger.debug("get_field: using mask: %s", bin(mask))
  extracted = word & mask
  #logger.debug("get_field: extracted: %s", bin(extracted))
  maskshift = 0
  while getbit(mask, maskshift) == 0:
    maskshift += 1
  extracted = extracted >> maskshift
  #logger.debug("get_field: shifted: %s", bin(extracted))
  return extracted

def set_field(word, value, mask=255):
  """
  Set a value into a bit field
  
  Test::
    In [5]: bin(set_field(int("101010101010",2), int("010101",2), mask="11:6"))
    DEBUG:Math.Bin:set_field: word:         0b101010101010
    DEBUG:Math.Bin:set_field: value:               0b10101
    DEBUG:Math.Bin:set_field: mask:         0b111111000000
    DEBUG:Math.Bin:set_field: masked word:        0b101010
    DEBUG:Math.Bin:set_field: shifted value: 0b10101000000
    Out[5]:                                 '0b10101101010'

  @param word : word to be modified
  @type  word : int
  
  @param value : value to be inserted
  @type  value : int

  @param mask : bitmask; default 255 ('0b11111111'), l.s.byte
  @type  mask : int
  
  @return: int
  """
  # clear the masked field
  #logger.debug("set_field: word:  %s (%d)", bin(word), word)
  #logger.debug("set_field: value: %s (%d)", bin(value), value)
  #logger.debug("set_field: mask:  %s (%d)", bin(mask), mask)
  if type(mask) != int:
    mask = field_mask(mask)
  new_word = word & ~mask
  #logger.debug("set_field: masked word: %s (%d)", bin(new_word), new_word)
  leftshift = 0
  while getbit(mask,leftshift) == 0:
    leftshift += 1
    #logger.debug("set_field: shifting by %d", leftshift)
  value = value << leftshift
  #logger.debug("set_field: shifted value: %s", bin(value))
  return new_word + value

# from https://www.falatic.com/index.php/108/python-and-bitwise-rotation

rol = lambda val, r_bits, max_bits: \
    (val << r_bits%max_bits) & (2**max_bits-1) | \
    ((val & (2**max_bits-1)) >> (max_bits-(r_bits%max_bits)))
rol.__doc__ = """Rotate left: 0b1001 --> 0b0011"""

ror = lambda val, r_bits, max_bits: \
    ((val & (2**max_bits-1)) >> r_bits%max_bits) | \
    (val << (max_bits-(r_bits%max_bits)) & (2**max_bits-1))
ror.__doc__ = """Rotate right: 0b1001 --> 0b1100"""

def reverse(x, n):
    """
    Reverse the pattern
    
    from http://stackoverflow.com/questions/12681945/reversing-bits-of-python-integer
    
    @param x : bit pattern
    @type  x : int
    
    @param n : number of bits in the pattern
    @type  n : int
    """
    result = 0
    for i in range(n):
        if (x >> i) & 1: result |= 1 << (n - 1 - i)
    return result

def bin2sint(bin):
    """
    binary to signed integer
    
    A digitizer only tests for > or < some level. This makes a 1s-complement
    notation preferred.  The -0 value is avoided. >0 is represented as 00000000
    and <0 by 11111111.  So then::

      In [11]: bin2sint('00000000') 
      Out[11]: 1
      In [12]: bin2sint('11111111')
      Out[12]: -1

      In [13]: bin2sint('00000001')
      Out[13]: 3
      In [14]: bin2sint('11111110') 
      Out[14]: -3

      In [15]: bin2sint('00000011')
      Out[15]: 7
      In [16]: bin2sint('11111100')
      Out[16]: -7
      
      In [17]: bin2sint('10000000') 
      Out[17]: -255
      In [18]: bin2sint('01111111')
      Out[18]: 255
    """
    if bin[0] == '1':
        # Negative
        integer = -(2**(len(bin)-1) - int(bin[1:], 2))
    else:
        integer = int(bin, 2)

    integer = int(2*integer + 1)
    return integer

