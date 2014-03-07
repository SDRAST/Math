# -*- coding: utf-8 -*-
"""
Classes for properties of geometrical objects.

This isn't exactly difficult math but it illustrates object-oriented
programming pretty well.

For simple calculations it isn't necessary to create instances of the objects.
For example::

 In [1]: from Math.geometry import *

 In [2]: Sphere(6000).area
 Out[2]: 452389342.11693019

 In [3]: Sphere(6000).horizon(0.01)
 Out[3]: 10.95443745708116

Sometimes, an instance is useful though::

 In [4]: s = Sphere(6000)
 
 In [5]: r = s.horizon(300)
 
 In [6]: visible_fraction = Circle(r).area/s.area
 
 In [7]: visible_fraction
 Out[7]: 0.023242630385487552
"""
import math

class Circular(object):
  """
  Superclass for circular objects, like Circle() and Sphere()
  """
  def __init__(self,radius):
    """
    Create an instance and define radius, circumference and horizon
    """
    self.radius = radius
    self.circumference = self._circumference()

  def _circumference(self):
    return 2*math.pi*self.radius

  def tangent_distance(self, height):
    """
    Distance to the tangent point

    @param height : radial outward distance from circle, in same units as radius
    """
    assert height >= 0, "Height cannot be negative"
    cos_theta = float(self.radius)/(self.radius + height)
    theta = math.acos(cos_theta)
    return self.radius*math.sin(theta)
  
class Circle(Circular):
  """
  Circle, a subclass of Circular()
  """
  def __init__(self,radius):
    """
    Create an instance and define the area
    """
    super(Circle,self).__init__(radius)
    self.area = self._area()

  def _area(self):
    return math.pi*self.radius**2

class Sphere(Circular):
  """
  Sphere, a subclass of Circular()
  """
  def __init__(self,radius):
    """
    Create an instance and define area and volume
    """
    super(Sphere,self).__init__(radius)
    self.area = self._area()
    self.volume = self._volume()

  def _area(self):
    return 4*math.pi*self.radius**2

  def _volume(self):
    return 4*math.pi*self.radius**3/3