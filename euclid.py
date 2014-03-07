# -*- coding: utf-8 -*-
"""
euclid - 2D and 3D maths module

Overview
========

This module provides 2D and 3D elements and transformations oriented towards
graphical applications.  The transformations are affine transformations.
Thus 2D elements are transformed with 3D matrices and 3D elements with 4D
matrices.

A discussion about the theoretical rigor of pyeuclid is founs in
https://github.com/ezag/pyeuclid/issues/1

A discussion of the mathematics of perspective may be found at
http://www.math.utah.edu/~treiberg/Perspect/Perspect.htm

Superclasses::

  Geometry     - Superclass defines methods to be implemented by sub-classes
  
2D Geometry
-----------
Classes::

  Point2       - point, represented by an X and a Y coordinate
  Vector2      - vector, presented by (X,Y) coordinates relative to (0,0)
  Line2        - infinite line, represented by a Point2 and a Vector2
  Ray2         - line bouned by one point
  LineSegment2 - line bounded by two points
  Circle       - circle represent by a Point2 center and a float radius

3D Geometry
-----------
This uses a graphics-oriented coordinate system  The angle names are best
understood by having X pointing to the observer's right, Y pointing up, and
Z pointing towards the observer.

Classes::

  Point3       - Point in 3D space
  Vector3      - 3D vector in XYZ Euclidean space
  Line3        - Line in 3D space
  Ray3         - Semi-infinite line in 3D space
  LineSegment3 - Line segment in 3D space
  Matrix3      - 3x3 matrix for working with 2D affine transformations
  Sphere       - Sphere in 3D space
  Plane        - A 3D plane class

4D Geometry
-----------

Classes::

  Matrix4      - 4x4 matrix for working with 3D affine transformations.
  Quaternion   - w is the real part, (x, y, z) are the imaginary parts

Acknowledgements
================
Copyright (c) 2006 Alex Holkner
Alex.Holkner@mail.google.com

Original documentation is at
 http://http://partiallydisassembled.net/euclid/index.html
"""
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by the
# Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.
# 
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
# for more details.
# 
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA


__docformat__ = 'restructuredtext'
__version__ = '$Id: euclid.py 37 2011-08-21 22:24:05Z elfnor@gmail.com $'
__revision__ = '$Revision: 37 $'

import math
import operator
import types
import logging

module_logger = logging.getLogger(__name__)

# Some magic here.  If _use_slots is True, the classes will derive from
# object and will define a __slots__ class variable.  If _use_slots is
# False, classes will be old-style and will not define __slots__.
#
# _use_slots = True:   Memory efficient, probably faster in future versions
#                      of Python, "better".
# _use_slots = False:  Ordinary classes, much faster than slots in current
#                      versions of Python (2.4 and 2.5).
_use_slots = True

_enable_swizzle_set = False # Recommended setting is False.

if _enable_swizzle_set:
    # Requires class to derive from object.
    _use_slots = True


class _EuclidMetaclass(type):
    """
    Implement _use_slots magic.
    """
    def __new__(cls, name, bases, dct):
        """
        """
        if '__slots__' in dct:
            dct['__getstate__'] = cls._create_getstate(dct['__slots__'])
            dct['__setstate__'] = cls._create_setstate(dct['__slots__'])
        if _use_slots:
            return type.__new__(cls, name, bases + (object,), dct)
        else:
            if '__slots__' in dct:
                del dct['__slots__']
            return types.ClassType.__new__(types.ClassType, name, bases, dct)

    @classmethod
    def _create_getstate(cls, slots):
        """
        """
        def __getstate__(self):
            d = {}
            for slot in slots:
                d[slot] = getattr(self, slot)
            return d
        return __getstate__

    @classmethod
    def _create_setstate(cls, slots):
        """
        """
        def __setstate__(self, state):
            for name, value in state.items():
                setattr(self, name, value)
        return __setstate__

__metaclass__ = _EuclidMetaclass

################################ Vectors ###########################

class Vector2:
    """
    2D vector in the xy-plane.

    Attributes::
    
      x - projection on the X-axis
      y - projection on the Y-axis

    Public Methods
    ==============

    
    Examples
    ========
    In [1]: from Math.euclid import *
    
    Creation::
    
     In [2]: from Math.euclid import *
     In [3]: v = Vector2(1,1)
     In [4]: w = Vector2(.5,1)

    Element Access::
    
     In [5]: v[0]
     Out[5]: 1
     In [6]: w[:]
     Out[6]: (0.5, 1)
     In [7]: w.x, w.y, w.xy, w.yx
     Out[7]: (0.5, 1, (0.5, 1), (1, 0.5))

    Unary Operations::
    
     In [8]: v.magnitude()
     Out[8]: 1.4142135623730951
     In [9]: vn = v.normalized()
     In [10]: vn
     Out[10]: Vector2(0.71, 0.71)
     In [11]: v.cross()
     Out[11]: Vector2(1.00, -1.00)

    Binary Operations::
    
     In [12]: v+w
     Out[12]: Vector2(1.50, 2.00)
     In [13]: v-w
     Out[13]: Vector2(0.50, 0.00)
     In [14]: 2*v
     Out[14]: Vector2(2.00, 2.00)
     In [15]: w/2
     Out[15]: Vector2(0.25, 0.00)
     In [16]: v.dot(w)
     Out[16]: 1.5
     In [17]: w.reflect(vn)
     Out[17]: Vector2(-1.00, -0.50)
     In [18]: v.angle(w)
     Out[18]: 0.3217505543966423     # radians
    """
    __slots__ = ['x', 'y']
    __hash__ = None

    def __init__(self, x=0, y=0):
        """
        Creates an instance of Vector2 class
        """
        self.x = x
        self.y = y

    def __copy__(self):
        """
        Returns a copy of itself
        """
        return self.__class__(self.x, self.y)

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Vector2(%.2f, %.2f)' % (self.x, self.y)

    def __eq__(self, other):
        """
        Test whether this instance is equal to another instance

        @param other : the other element in a binary operation
        @type  other : Vector2 or sequence of length 2
        """
        if isinstance(other, Vector2):
            return self.x == other.x and \
                   self.y == other.y
        else:
            assert hasattr(other, '__len__') and len(other) == 2
            return self.x == other[0] and \
                   self.y == other[1]

    def __ne__(self, other):
        """
        Test whether this instance is not equal to another instance

        @param other : the other element in a binary operation
        @type  other : Vector2 or sequence of length 2
        """
        return not self.__eq__(other)

    def __nonzero__(self):
        """
        Test if this instance (self) is non-zero
        """
        return self.x != 0 or self.y != 0

    def __len__(self):
        """
        Report length of this instance
        """
        return 2

    def __getitem__(self, key):
        """
        Returns x,y when self is a dictionary
        """
        return (self.x, self.y)[key]

    def __setitem__(self, key, value):
        """
        Sets x, l when self is a dictionary
        """
        l = [self.x, self.y]
        l[key] = value
        self.x, self.y = l

    def __iter__(self):
        """
        Returns an iterator object with a next() method.
        """
        return iter((self.x, self.y))

    def __getattr__(self, name):
        """
        Invoked on self.name where name is an attribute of self
        """
        try:
            return tuple([(self.x, self.y)['xy'.index(c)] \
                          for c in name])
        except ValueError:
            raise AttributeError, name

    if _enable_swizzle_set:
        def __setattr__(self, name, value):
            """
            Allows components of Vector2 and Vector3 to be set via swizzling
            
            e.g.  v.xyz = (1, 2, 3).  This is much, much slower than the more
            verbose v.x = 1; v.y = 2; v.z = 3,  and slows down ordinary element
            setting as well.
            
            This has detrimental performance on ordinary setattr as well
            if enabled
            """
            if len(name) == 1:
                object.__setattr__(self, name, value)
            else:
                try:
                    l = [self.x, self.y]
                    for c, v in map(None, name, value):
                        l['xy'.index(c)] = v
                    self.x, self.y = l
                except ValueError:
                    raise AttributeError, name

    def __add__(self, other):
        """
        Invoked for "+"; adds other to self.
        
        If other is a Vector2, a Vector2 is returned.
        If other is a Point2, a Point2 is returned.
        If other is a sequence of length 2, treat it like a vector.

        @param other : the other element in a binary operation
        @type  other : Vector2 or sequence of length 2
        """
        if isinstance(other, Vector2):
            # Vector + Vector -> Vector
            # Vector + Point -> Point
            # Point + Point -> Vector
            if self.__class__ is other.__class__:
                _class = Vector2
            else:
                _class = Point2
            return _class(self.x + other.x,
                          self.y + other.y)
        else:
            assert hasattr(other, '__len__') and len(other) == 2
            return Vector2(self.x + other[0],
                           self.y + other[1])
    __radd__ = __add__

    def __iadd__(self, other):
        """
        Invoked for "+="; increment self with other.

        @param other : the other element in a binary operation
        @type  other : Vector2 or sequence of length 2
        """
        if isinstance(other, Vector2):
            self.x += other.x
            self.y += other.y
        else:
            self.x += other[0]
            self.y += other[1]
        return self

    def __sub__(self, other):
        """
        Invoked for '-'; subtract other from self.

        If other is a Vector2, a Vector2 is returned.
        If other is a Point2, a Point2 is returned.
        If other is a sequence of length 2, treat it like a vector.

        @param other : the other element in a binary operation
        @type  other : Vector2 or sequence of length 2
        """
        if isinstance(other, Vector2):
            # Vector - Vector -> Vector
            # Vector - Point -> Point
            # Point - Point -> Vector
            if self.__class__ is other.__class__:
                _class = Vector2
            else:
                _class = Point2
            return _class(self.x - other.x,
                          self.y - other.y)
        else:
            assert hasattr(other, '__len__') and len(other) == 2
            return Vector2(self.x - other[0],
                           self.y - other[1])

    def __rsub__(self, other):
        """
        Invoked for "-"; subtract self from other.

        If other is a Vector2, a Vector2 is returned.
        If other is a Point2, a Point2 is returned.
        If other is a sequence of length 2, treat it like a vector.

        @param other : the other element in a binary operation
        @type  other : Vector2 or sequence of length 2
        """
        if isinstance(other, Vector2):
            return Vector2(other.x - self.x,
                           other.y - self.y)
        else:
            assert hasattr(other, '__len__') and len(other) == 2
            return Vector2(other.x - self[0],
                           other.y - self[1])

    def __mul__(self, other):
        """
        Invoked for "*" scalar multiplication of self by other

        @param other : other element in a binary operation
        @type  other : int, long or float

        @return: Vector2
        """
        assert type(other) in (int, long, float)
        return Vector2(self.x * other,
                       self.y * other)

    __rmul__ = __mul__

    def __imul__(self, other):
        """
        Invoked for '\*='; scalar multiplication of self

        @param other : other element in a binary operation
        @type  other : int, long or float

        @return: self
        """
        assert type(other) in (int, long, float)
        self.x *= other
        self.y *= other
        return self

    def __div__(self, other):
        """
        Invoked for "/"; divide self by a scalar
        """
        assert type(other) in (int, long, float)
        return Vector2(operator.div(self.x, other),
                       operator.div(self.y, other))


    def __rdiv__(self, other):
        """
        Invoked for "/"; divide other by self
        
        Returns a Vector2 with elements of other divided by each element of self.
        """
        assert type(other) in (int, long, float)
        return Vector2(operator.div(other, self.x),
                       operator.div(other, self.y))

    def __floordiv__(self, other):
        """
        Invoked for "//"; truncated division by other
        """
        assert type(other) in (int, long, float)
        return Vector2(operator.floordiv(self.x, other),
                       operator.floordiv(self.y, other))


    def __rfloordiv__(self, other):
        """
        Invoked for "//"; truncated division of other by self.
        """
        assert type(other) in (int, long, float)
        return Vector2(operator.floordiv(other, self.x),
                       operator.floordiv(other, self.y))

    def __truediv__(self, other):
        """
        Division of self by other with remainder, even for integers
        """
        assert type(other) in (int, long, float)
        return Vector2(operator.truediv(self.x, other),
                       operator.truediv(self.y, other))


    def __rtruediv__(self, other):
        """
        Division of other by self with remainder, even for integers
        """
        assert type(other) in (int, long, float)
        return Vector2(operator.truediv(other, self.x),
                       operator.truediv(other, self.y))
    
    def __neg__(self):
        """
        Return negative of self
        """
        return Vector2(-self.x,
                        -self.y)

    __pos__ = __copy__
    
    def __abs__(self):
        """
        Returns magnitude (absolute value) of Vector2 'self'
        """
        return math.sqrt(self.x ** 2 + \
                         self.y ** 2)

    magnitude = __abs__

    def magnitude_squared(self):
        """
        Returns square of Vector2 'self'
        """
        return self.x ** 2 + \
               self.y ** 2

    def normalize(self):
        """
        Converts self into a unit length vector
        """
        d = self.magnitude()
        if d:
            self.x /= d
            self.y /= d
        return self

    def normalized(self):
        """
        Returns a copy of self normalized to unit length.
        """
        d = self.magnitude()
        if d:
            return Vector2(self.x / d, 
                           self.y / d)
        return self.copy()

    def dot(self, other):
        """
        Dot (scalar) product of self with another Vector2 instance
        """
        assert isinstance(other, Vector2)
        return self.x * other.x + \
               self.y * other.y

    def cross(self):
        """
        Returns a Vector rotated CW by pi/2 w.r.t. self.
        """
        return Vector2(self.y, -self.x)

    def reflect(self, normal):
        """
        Return the vector reflected about the given normal

        In two dimensions, normal is the normal (perpendicular) to a line::
        
          In [3]: Vector2(1., 2.).reflect(Vector2(1, 0))
          Out[3]: Vector2(-1.00, 2.00)
        """
        assert isinstance(normal, Vector2)
        d = 2 * (self.x * normal.x + self.y * normal.y)
        return Vector2(self.x - d * normal.x,
                       self.y - d * normal.y)

    def angle(self, other):
        """
        Return the angle to the vector other
        """
        return math.acos(self.dot(other) / (self.magnitude()*other.magnitude()))

    def project(self, other):
        """
        Return one vector projected on the vector other
        """
        n = other.normalized()
        return self.dot(n)*n

class Vector3:
    """
    3D vector in XYZ Euclidean space

    Public attributes are the coordinates x,y,z
    """
    __slots__ = ['x', 'y', 'z']
    __hash__ = None

    def __init__(self, x=0, y=0, z=0):
        """
        Create a Vector3 instance
        """
        self.x = x
        self.y = y
        self.z = z

    def __copy__(self):
        """
        Returns a copy of itself
        """
        return self.__class__(self.x, self.y, self.z)

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Vector3(%.2f, %.2f, %.2f)' % (self.x,
                                              self.y,
                                              self.z)

    def __eq__(self, other):
        """
        Test whether this instance is equal to another instance

        @param other : the other element in a binary operation
        @type  other : Vector3 or sequence of length 3
        """
        if isinstance(other, Vector3):
            return self.x == other.x and \
                   self.y == other.y and \
                   self.z == other.z
        else:
            assert hasattr(other, '__len__') and len(other) == 3
            return self.x == other[0] and \
                   self.y == other[1] and \
                   self.z == other[2]

    def __ne__(self, other):
        """
        Test whether this instance is not equal to another instance

        @param other : the other element in a binary operation
        @type  other : Vector3 or sequence of length 3
        """
        return not self.__eq__(other)

    def __nonzero__(self):
        """
        Test if this instance (self) is non-zero
        """
        module_logger.debug("self is %s",type(self))
        module_logger.debug("components: %s, %s, %s", self.x, self.y, self.z)
        module_logger.debug("non-zero: %s, %s, %s",
                            self.x != 0 or self.y != 0 or self.z != 0)
        return self.x != 0 or self.y != 0 or self.z != 0

    def __len__(self):
        """
        Report length of this instance
        """
        return 3

    def __getitem__(self, key):
        """
        Returns x,y,z when self is a dictionary
        """
        return (self.x, self.y, self.z)[key]

    def __setitem__(self, key, value):
        """
        Sets x,y,z when self is a dictionary
        """
        l = [self.x, self.y, self.z]
        l[key] = value
        self.x, self.y, self.z = l

    def __iter__(self):
        """
        Returns an iterator object with a next() method.
        """
        return iter((self.x, self.y, self.z))

    def __getattr__(self, name):
        """
        Invoked on self.name where name is an attribute of self
        """
        try:
            return tuple([(self.x, self.y, self.z)['xyz'.index(c)] \
                          for c in name])
        except ValueError:
            raise AttributeError, name

    if _enable_swizzle_set:
        def __setattr__(self, name, value):
            """
            Allows components of Vector2 and Vector3 to be set via swizzling

            e.g.  v.xyz = (1, 2, 3).  This is much, much slower than the more
            verbose v.x = 1; v.y = 2; v.z = 3,  and slows down ordinary element
            setting as well.

            This has detrimental performance on ordinary setattr as well
            if enabled
            """
            if len(name) == 1:
                object.__setattr__(self, name, value)
            else:
                try:
                    l = [self.x, self.y, self.z]
                    for c, v in map(None, name, value):
                        l['xyz'.index(c)] = v
                    self.x, self.y, self.z = l
                except ValueError:
                    raise AttributeError, name


    def __add__(self, other):
        """
        Adds other to self.

        If other is a Vector3, a Vector3 is returned.
        If other is not a Vector3, a Point3 is returned.
        If other is a sequence of length 3, treat it like a vector.

        @param other : the other element in a binary operation
        @type  other : Vector3 or sequence of length 3
        """
        if isinstance(other, Vector3):
            # Vector + Vector -> Vector
            # Vector + Point -> Point
            # Point + Point -> Vector
            if self.__class__ is other.__class__:
                _class = Vector3
            else:
                _class = Point3
            return _class(self.x + other.x,
                          self.y + other.y,
                          self.z + other.z)
        else:
            assert hasattr(other, '__len__') and len(other) == 3
            return Vector3(self.x + other[0],
                           self.y + other[1],
                           self.z + other[2])
    __radd__ = __add__

    def __iadd__(self, other):
        """
        Increment self with other.

        @param other : the other element in a binary operation
        @type  other : Vector3 or sequence of length 3
        """
        if isinstance(other, Vector3):
            self.x += other.x
            self.y += other.y
            self.z += other.z
        else:
            self.x += other[0]
            self.y += other[1]
            self.z += other[2]
        return self

    def __sub__(self, other):
        """
        Subtract other from self.

        If other is a Vector3, a Vector3 is returned.
        If other is not a Vector3, a Point3 is returned.
        If other is a sequence of length 3, treat it like a vector.

        @param other : the other element in a binary operation
        @type  other : Vector3 or sequence of length 3
        """
        if isinstance(other, Vector3):
            # Vector - Vector -> Vector
            # Vector - Point -> Point
            # Point - Point -> Vector
            if self.__class__ is other.__class__:
                _class = Vector3
            else:
                _class = Point3
            return Vector3(self.x - other.x,
                           self.y - other.y,
                           self.z - other.z)
        else:
            assert hasattr(other, '__len__') and len(other) == 3
            return Vector3(self.x - other[0],
                           self.y - other[1],
                           self.z - other[2])

   
    def __rsub__(self, other):
        """
        Subtract self from other.

        If other is a Vector3, a Vector3 is returned.
        If other is a sequence of length 3, treat it like a vector.

        @param other : the other element in a binary operation
        @type  other : Vector3 or sequence of length 3
        """
        if isinstance(other, Vector3):
            return Vector3(other.x - self.x,
                           other.y - self.y,
                           other.z - self.z)
        else:
            assert hasattr(other, '__len__') and len(other) == 3
            return Vector3(other.x - self[0],
                           other.y - self[1],
                           other.z - self[2])

    def __mul__(self, other):
        """
        Invoked for "*" as the element multiplication of self by other

        This is an element by element multiplication, which is different from
        the dot and cross products, self.dot(other) and self.cross(other),

        Example::
        
          In [6]: v = Vector3(1, 0, 0)
          In [7]: p = Point3(0, 0, 0.5)
          In [8]: v*p
          Out[8]: Point3(0.00, 0.00, 0.00)
          In [9]: v.dot(p)
          Out[9]: 0.0
          In [10]: v.cross(p)
          Out[10]: Vector3(0.00, -0.50, 0.00)

        @param other : other element in a binary operation
        @type  other : int, long or float

        @return: Vector3
        """
        if isinstance(other, Vector3):
            # TODO component-wise mul/div in-place and on Vector2; docs.
            if self.__class__ is Point3 or other.__class__ is Point3:
                _class = Point3
            else:
                _class = Vector3
            return _class(self.x * other.x,
                          self.y * other.y,
                          self.z * other.z)
        else: 
            assert type(other) in (int, long, float)
            return Vector3(self.x * other,
                           self.y * other,
                           self.z * other)

    __rmul__ = __mul__

    def __imul__(self, other):
        """
        Invoked for '\*='; scalar multiplication of self

        @param other : other element in a binary operation
        @type  other : int, long or float

        @return: self
        """
        assert type(other) in (int, long, float)
        self.x *= other
        self.y *= other
        self.z *= other
        return self

    def __div__(self, other):
        """
        Invoked for "/"; divide self by a scalar
        """
        assert type(other) in (int, long, float)
        return Vector3(operator.div(self.x, other),
                       operator.div(self.y, other),
                       operator.div(self.z, other))


    def __rdiv__(self, other):
        """
        Invoked for "/"; divide other by self

        Returns a Vector3 with elements of other divided by each element of self.
        """
        assert type(other) in (int, long, float)
        return Vector3(operator.div(other, self.x),
                       operator.div(other, self.y),
                       operator.div(other, self.z))

    def __floordiv__(self, other):
        """
        Invoked for "//"; truncated division by other
        """
        assert type(other) in (int, long, float)
        return Vector3(operator.floordiv(self.x, other),
                       operator.floordiv(self.y, other),
                       operator.floordiv(self.z, other))


    def __rfloordiv__(self, other):
        """
        Invoked for "//"; truncated division of other by self.
        """
        assert type(other) in (int, long, float)
        return Vector3(operator.floordiv(other, self.x),
                       operator.floordiv(other, self.y),
                       operator.floordiv(other, self.z))

    def __truediv__(self, other):
        """
        Division of self by other with remainder, even for integers
        """
        assert type(other) in (int, long, float)
        return Vector3(operator.truediv(self.x, other),
                       operator.truediv(self.y, other),
                       operator.truediv(self.z, other))


    def __rtruediv__(self, other):
        """
        Division of other by self with remainder, even for integers
        """
        assert type(other) in (int, long, float)
        return Vector3(operator.truediv(other, self.x),
                       operator.truediv(other, self.y),
                       operator.truediv(other, self.z))
    
    def __neg__(self):
        """
        Return negative of self
        """
        return Vector3(-self.x,
                        -self.y,
                        -self.z)

    __pos__ = __copy__
    
    def __abs__(self):
        """
        Returns magnitude (absolute value) of Vector2 'self'
        """
        return math.sqrt(self.x ** 2 + \
                         self.y ** 2 + \
                         self.z ** 2)

    magnitude = __abs__

    def magnitude_squared(self):
        """
        Returns square of Vector3 'self'
        """
        return self.x ** 2 + \
               self.y ** 2 + \
               self.z ** 2

    def normalize(self):
        """
        Converts self into a unit length vector
        """
        d = self.magnitude()
        if d:
            self.x /= d
            self.y /= d
            self.z /= d
        return self

    def normalized(self):
        """
        Returns a copy of self normalized to unit length.
        """
        d = self.magnitude()
        if d:
            return Vector3(self.x / d, 
                           self.y / d, 
                           self.z / d)
        return self.copy()

    def dot(self, other):
        """
        Dot (scalar) product of self with another Vector3 instance
        """
        assert isinstance(other, Vector3)
        return self.x * other.x + \
               self.y * other.y + \
               self.z * other.z

    def cross(self, other):
        """
        Returns cross-product of self with other
        """
        assert isinstance(other, Vector3)
        return Vector3(self.y * other.z - self.z * other.y,
                       -self.x * other.z + self.z * other.x,
                       self.x * other.y - self.y * other.x)

    def reflect(self, normal):
        """
        Return the vector reflected about the given normal

        Assumes 'normal' is normalized

        In three dimensions, normal is the normal (perpendicular) to a plane
        """
        assert isinstance(normal, Vector3)
        d = 2 * (self.x * normal.x + self.y * normal.y + self.z * normal.z)
        return Vector3(self.x - d * normal.x,
                       self.y - d * normal.y,
                       self.z - d * normal.z)

    def rotate_around(self, axis, theta):
        """
        Return the vector rotated around axis through angle theta.

        Right hand rule applies.
        Adapted from equations published by Glenn Murray.
        http://inside.mines.edu/~gmurray/ArbitraryAxisRotation/ArbitraryAxisRotation.html
        """

        x, y, z = self.x, self.y,self.z
        u, v, w = axis.x, axis.y, axis.z

        # Extracted common factors for simplicity and efficiency
        r2 = u**2 + v**2 + w**2
        r = math.sqrt(r2)
        ct = math.cos(theta)
        st = math.sin(theta) / r
        dt = (u*x + v*y + w*z) * (1 - ct) / r2
        return Vector3((u * dt + x * ct + (-w * y + v * z) * st),
                       (v * dt + y * ct + ( w * x - u * z) * st),
                       (w * dt + z * ct + (-v * x + u * y) * st))

    def angle(self, other):
        """
        Return the angle to the vector other
        """
        return math.acos(self.dot(other) / (self.magnitude()*other.magnitude()))

    def project(self, other):
        """
        Return one vector projected on the vector other
        """
        n = other.normalized()
        return self.dot(n)*n

######################### Matrixes and Quaternions #################################

class Matrix3:
    """
    Class for a 3x3 matrix for working with 2D affine transformations.

    A Matrix3 can be multiplied with a Vector2 or any of the 2D geometry
    objects (Point2, Line2, Circle, etc).  So a Matrix3 is used to transform
    a Vector2.  Multiplying a Matrix3 by a Vector2 returns a Vector2,

    Example::
    
      >>> m1 = Matrix3.new_rotate(math.pi / 2)
      >>> m1 * Vector2(1.0, 1.0)
      Vector2(-1.00, 1.00)

    Public attributes::
    
     a b c
     e f g
     i j k
     
    Note there is no 'd' or 'h'.

    Operations::
    
      >>> m1 = Matrix3.new_translate(5.0, 6.0)
      >>> m2 = Matrix3.new_scale(1.0, 2.0)
      >>> m1 * m2
      Matrix3([    1.00     0.00     5.00
                   0.00     2.00     6.00
                   0.00     0.00     1.00])
      >>> m2 * m1
      Matrix3([    1.00     0.00     5.00
                   0.00     2.00    12.00
                   0.00     0.00     1.00])

    """
    __slots__ = list('abcefgijk')

    def __init__(self):
        """
        Create an instance of an identity matrix

        Example::
        
         >>> Matrix3()
         Matrix3([    1.00     0.00     0.00
                      0.00     1.00     0.00
                      0.00     0.00     1.00])
        """
        self.identity()

    def __copy__(self):
        """
        Returns a copy of itself
        """
        M = Matrix3()
        M.a = self.a
        M.b = self.b
        M.c = self.c
        M.e = self.e 
        M.f = self.f
        M.g = self.g
        M.i = self.i
        M.j = self.j
        M.k = self.k
        return M

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return ('Matrix3([% 8.2f % 8.2f % 8.2f\n'  \
                '         % 8.2f % 8.2f % 8.2f\n'  \
                '         % 8.2f % 8.2f % 8.2f])') \
                % (self.a, self.b, self.c,
                   self.e, self.f, self.g,
                   self.i, self.j, self.k)

    def __getitem__(self, key):
        """
        Returns a, b, ... k when self is a dictionary
        """
        return [self.a, self.e, self.i,
                self.b, self.f, self.j,
                self.c, self.g, self.k][key]

    def __setitem__(self, key, value):
        """
        Sets a, b, ... k when self is a dictionary
        """
        L = self[:]
        L[key] = value
        (self.a, self.e, self.i,
         self.b, self.f, self.j,
         self.c, self.g, self.k) = L

    def __mul__(self, other):
        """
        Invoked for '*' scalar multiplication of self by other

        @param other : other element in a binary operation
        @type  other : int, long or float

        @return: Matrix3
        """
        if isinstance(other, Matrix3):
            # Caching repeatedly accessed attributes in local variables
            # apparently increases performance by 20%.  Attrib: Will McGugan.
            Aa = self.a
            Ab = self.b
            Ac = self.c
            Ae = self.e
            Af = self.f
            Ag = self.g
            Ai = self.i
            Aj = self.j
            Ak = self.k
            Ba = other.a
            Bb = other.b
            Bc = other.c
            Be = other.e
            Bf = other.f
            Bg = other.g
            Bi = other.i
            Bj = other.j
            Bk = other.k
            C = Matrix3()
            C.a = Aa * Ba + Ab * Be + Ac * Bi
            C.b = Aa * Bb + Ab * Bf + Ac * Bj
            C.c = Aa * Bc + Ab * Bg + Ac * Bk
            C.e = Ae * Ba + Af * Be + Ag * Bi
            C.f = Ae * Bb + Af * Bf + Ag * Bj
            C.g = Ae * Bc + Af * Bg + Ag * Bk
            C.i = Ai * Ba + Aj * Be + Ak * Bi
            C.j = Ai * Bb + Aj * Bf + Ak * Bj
            C.k = Ai * Bc + Aj * Bg + Ak * Bk
            return C
        elif isinstance(other, Point2):
            A = self
            B = other
            P = Point2(0, 0)
            P.x = A.a * B.x + A.b * B.y + A.c
            P.y = A.e * B.x + A.f * B.y + A.g
            return P
        elif isinstance(other, Vector2):
            A = self
            B = other
            V = Vector2(0, 0)
            V.x = A.a * B.x + A.b * B.y 
            V.y = A.e * B.x + A.f * B.y 
            return V
        else:
            other = other.copy()
            other._apply_transform(self)
            return other

    def __imul__(self, other):
        """
        Invoked for '\*='; scalar multiplication of self

        @param other : other element in a binary operation
        @type  other : Matrix3

        @return: self
        """
        assert isinstance(other, Matrix3)
        # Cache attributes in local vars (see Matrix3.__mul__).
        Aa = self.a
        Ab = self.b
        Ac = self.c
        Ae = self.e
        Af = self.f
        Ag = self.g
        Ai = self.i
        Aj = self.j
        Ak = self.k
        Ba = other.a
        Bb = other.b
        Bc = other.c
        Be = other.e
        Bf = other.f
        Bg = other.g
        Bi = other.i
        Bj = other.j
        Bk = other.k
        self.a = Aa * Ba + Ab * Be + Ac * Bi
        self.b = Aa * Bb + Ab * Bf + Ac * Bj
        self.c = Aa * Bc + Ab * Bg + Ac * Bk
        self.e = Ae * Ba + Af * Be + Ag * Bi
        self.f = Ae * Bb + Af * Bf + Ag * Bj
        self.g = Ae * Bc + Af * Bg + Ag * Bk
        self.i = Ai * Ba + Aj * Be + Ak * Bi
        self.j = Ai * Bb + Aj * Bf + Ak * Bj
        self.k = Ai * Bc + Aj * Bg + Ak * Bk
        return self

    def identity(self):
        """
        Convert self to an identity matrix::
        
         1 0 0
         0 1 0
         0 0 1
        """
        self.a = self.f = self.k = 1.
        self.b = self.c = self.e = self.g = self.i = self.j = 0
        return self

    def scale(self, x, y):
        """
        Applies a scaling to this Matrix3 instance.

        Note that this method operates in-place (modifies the original
        matrix), and also returns itself as a result. This allows you to
        chain transforms together directly.
        
        Example::
        
          >>> m1 = Matrix3.new_translate(5.0, 6.0)
          >>> m1.scale(1.0, 2.0)
          Matrix3([    1.00     0.00     5.00
                       0.00     2.00     6.00
                       0.00     0.00     1.00])
        """
        self *= Matrix3.new_scale(x, y)
        return self

    def translate(self, x, y):
        """
        Example::
        
          >>> Matrix3().translate(1.0, 2.0).rotate(math.pi / 2).scale(4.0, 4.0)
          Matrix3([    0.00    -4.00     1.00
                       4.00     0.00     2.00
                       0.00     0.00     1.00])
        """
        self *= Matrix3.new_translate(x, y)
        return self 

    def rotate(self, angle):
        """
        Example::
        
          >>> Matrix3().translate(1.0, 2.0).rotate(math.pi / 2).scale(4.0, 4.0)
          Matrix3([    0.00    -4.00     1.00
                       4.00     0.00     2.00
                       0.00     0.00     1.00])
        """
        self *= Matrix3.new_rotate(angle)
        return self

    # Static constructors
    
    def new_identity(cls):
        """
        Creates a new instance of an identity matrix.

        Equivalent to Matrix3

        @param cls : like 'self' but for the class, not the instance
        """
        self = cls()
        return self
    new_identity = classmethod(new_identity)

    def new_scale(cls, x, y):
        """
        Creates a new scaling matrix

        @param cls : like 'self' but for the class, not the instance

        @param x : scaling for X axis
        @type  x : int or float

        @param y : scaling for Y axis
        @type  y : int or float

        @return: Matrix3 instance
        """
        self = cls()
        self.a = x
        self.f = y
        return self
    new_scale = classmethod(new_scale)

    def new_translate(cls, x, y):
        """
        Creates a new translation matrix

        @param cls : like 'self' but for the class, not the instance

        @param x : translation along X axis
        @type  x : int or float

        @param y : translation along Y axis
        @type  y : int or float

        @return: Matrix3 instance
        """
        self = cls()
        self.c = x
        self.g = y
        return self
    new_translate = classmethod(new_translate)

    def new_rotate(cls, angle):
        """
        Creates a new rotation matrix

        Example::
        
          In [8]: m = Matrix3.new_rotate(pi/4)
          In [9]: v = Vector2(0,1)
          In [10]: m*v
          Out[10]: Vector2(-0.71, 0.71)

        @param cls : like 'self' but for the class, not the instance

        @param angle : CCW rotation in the XY plane in radians
        @type  angle : float

        @param y : translation along Y axis
        @type  y : int or float

        @return: Matrix3 instance
        """
        self = cls()
        s = math.sin(angle)
        c = math.cos(angle)
        self.a = self.f = c
        self.b = -s
        self.e = s
        return self
    new_rotate = classmethod(new_rotate)

    def determinant(self):
        """
        Determinant of self.

        @return: float
        """
        return (self.a*self.f*self.k 
                + self.b*self.g*self.i 
                + self.c*self.e*self.j 
                - self.a*self.g*self.j 
                - self.b*self.e*self.k 
                - self.c*self.f*self.i)

    def inverse(self):
        """
        Inverts self.

        Example::
        
          In [8]: m = Matrix3.new_rotate(pi/4)
          In [9]: m
          Out[9]:
          Matrix3([    0.71    -0.71     0.00
                       0.71     0.71     0.00
                       0.00     0.00     1.00])
          In [10]: m.inverse()
          Out[10]:
          Matrix3([    0.71     0.71    -0.00
                      -0.71     0.71     0.00
                       0.00    -0.00     1.00])
        """
        tmp = Matrix3()
        d = self.determinant()

        if abs(d) < 0.001:
            # No inverse, return identity
            return tmp
        else:
            d = 1.0 / d

            tmp.a = d * (self.f*self.k - self.g*self.j)
            tmp.b = d * (self.c*self.j - self.b*self.k)
            tmp.c = d * (self.b*self.g - self.c*self.f)
            tmp.e = d * (self.g*self.i - self.e*self.k)
            tmp.f = d * (self.a*self.k - self.c*self.i)
            tmp.g = d * (self.c*self.e - self.a*self.g)
            tmp.i = d * (self.e*self.j - self.f*self.i)
            tmp.j = d * (self.b*self.i - self.a*self.j)
            tmp.k = d * (self.a*self.f - self.b*self.e)

            return tmp


class Matrix4:
    """
    Class for a 4x4 matrix for working with 3D affine transformations.

    A Matrix4 can be multiplied with a Vector3 or any of the 3D geometry
    objects (Point3, Line3, Sphere, etc).

    Examples::
    
      In [4]: pl = Plane(Point3(0,0,1),Vector3(0,0,1))
      In [5]: pl
      Out[5]: Plane(<0.00, 0.00, 1.00>.p = 1.00)
      In [6]: m = Matrix4().new_rotatex(pi/4)
      In [7]: m*pl
      Out[7]: Plane(<0.00, -0.71, 0.71>.p = 1.00)
      In [8]: v = Vector3(0,0,1)
      In [9]: m*v
      Out[9]: Vector3(0.00, -0.71, 0.71)       # returns a Vector3
      In [10]: m.transform(v)
      Out[10]: Point3(0.00, -0.71, 0.71)       # returns a Point3
      In [11]: p = Point3(0,1,0)
      In [12]: m*p
      Out[12]: Point3(0.00, 0.71, 0.71)
      In [13]: m.transform(p)
      Out[13]: Point3(0.00, 0.71, 0.71)

    Public attributes::
    
      a b c d
      e f g h
      i j k l
      m n o p
    """
    __slots__ = list('abcdefghijklmnop')

    def __init__(self):
        """
        Example::
        
          >>> Matrix4()
          Matrix4([    1.00     0.00     0.00     0.00
                       0.00     1.00     0.00     0.00
                       0.00     0.00     1.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        self.identity()

    def __copy__(self):
        """
        Returns a copy of itself
        """
        M = Matrix4()
        M.a = self.a
        M.b = self.b
        M.c = self.c
        M.d = self.d
        M.e = self.e 
        M.f = self.f
        M.g = self.g
        M.h = self.h
        M.i = self.i
        M.j = self.j
        M.k = self.k
        M.l = self.l
        M.m = self.m
        M.n = self.n
        M.o = self.o
        M.p = self.p
        return M

    copy = __copy__


    def __repr__(self):
        """
        Text representing this instance
        """
        return ('Matrix4([% 8.2f % 8.2f % 8.2f % 8.2f\n'  \
                '         % 8.2f % 8.2f % 8.2f % 8.2f\n'  \
                '         % 8.2f % 8.2f % 8.2f % 8.2f\n'  \
                '         % 8.2f % 8.2f % 8.2f % 8.2f])') \
                % (self.a, self.b, self.c, self.d,
                   self.e, self.f, self.g, self.h,
                   self.i, self.j, self.k, self.l,
                   self.m, self.n, self.o, self.p)

    def __getitem__(self, key):
        """
        Returns a, b, ... p when self is a dictionary
        """
        return [self.a, self.e, self.i, self.m,
                self.b, self.f, self.j, self.n,
                self.c, self.g, self.k, self.o,
                self.d, self.h, self.l, self.p][key]

    def __setitem__(self, key, value):
        """
        Sets a, b, ... p when self is a dictionary
        """
        L = self[:]
        L[key] = value
        (self.a, self.e, self.i, self.m,
         self.b, self.f, self.j, self.n,
         self.c, self.g, self.k, self.o,
         self.d, self.h, self.l, self.p) = L

    def __mul__(self, other):
        """
        Invoked for "*" scalar multiplication of self by other

        @param other : other element in a binary operation
        @type  other : Matrix3, Point3, Vector3, int, long, float

        @return: Matrix4
        """
        if isinstance(other, Matrix4):
            # Cache attributes in local vars (see Matrix3.__mul__).
            Aa = self.a
            Ab = self.b
            Ac = self.c
            Ad = self.d
            Ae = self.e
            Af = self.f
            Ag = self.g
            Ah = self.h
            Ai = self.i
            Aj = self.j
            Ak = self.k
            Al = self.l
            Am = self.m
            An = self.n
            Ao = self.o
            Ap = self.p
            Ba = other.a
            Bb = other.b
            Bc = other.c
            Bd = other.d
            Be = other.e
            Bf = other.f
            Bg = other.g
            Bh = other.h
            Bi = other.i
            Bj = other.j
            Bk = other.k
            Bl = other.l
            Bm = other.m
            Bn = other.n
            Bo = other.o
            Bp = other.p
            C = Matrix4()
            C.a = Aa * Ba + Ab * Be + Ac * Bi + Ad * Bm
            C.b = Aa * Bb + Ab * Bf + Ac * Bj + Ad * Bn
            C.c = Aa * Bc + Ab * Bg + Ac * Bk + Ad * Bo
            C.d = Aa * Bd + Ab * Bh + Ac * Bl + Ad * Bp
            C.e = Ae * Ba + Af * Be + Ag * Bi + Ah * Bm
            C.f = Ae * Bb + Af * Bf + Ag * Bj + Ah * Bn
            C.g = Ae * Bc + Af * Bg + Ag * Bk + Ah * Bo
            C.h = Ae * Bd + Af * Bh + Ag * Bl + Ah * Bp
            C.i = Ai * Ba + Aj * Be + Ak * Bi + Al * Bm
            C.j = Ai * Bb + Aj * Bf + Ak * Bj + Al * Bn
            C.k = Ai * Bc + Aj * Bg + Ak * Bk + Al * Bo
            C.l = Ai * Bd + Aj * Bh + Ak * Bl + Al * Bp
            C.m = Am * Ba + An * Be + Ao * Bi + Ap * Bm
            C.n = Am * Bb + An * Bf + Ao * Bj + Ap * Bn
            C.o = Am * Bc + An * Bg + Ao * Bk + Ap * Bo
            C.p = Am * Bd + An * Bh + Ao * Bl + Ap * Bp
            return C
        elif isinstance(other, Point3):
            A = self
            B = other
            P = Point3(0, 0, 0)
            P.x = A.a * B.x + A.b * B.y + A.c * B.z + A.d
            P.y = A.e * B.x + A.f * B.y + A.g * B.z + A.h
            P.z = A.i * B.x + A.j * B.y + A.k * B.z + A.l
            return P
        elif isinstance(other, Vector3):
            A = self
            B = other
            V = Vector3(0, 0, 0)
            V.x = A.a * B.x + A.b * B.y + A.c * B.z
            V.y = A.e * B.x + A.f * B.y + A.g * B.z
            V.z = A.i * B.x + A.j * B.y + A.k * B.z
            return V
        else:
            other = other.copy()
            other._apply_transform(self)
            return other

    def __imul__(self, other):
        """
        Invoked for '\*='; scalar multiplication of self

        @param other : other element in a binary operation
        @type  other : Matrix4

        @return: self
        """
        assert isinstance(other, Matrix4)
        # Cache attributes in local vars (see Matrix3.__mul__).
        Aa = self.a
        Ab = self.b
        Ac = self.c
        Ad = self.d
        Ae = self.e
        Af = self.f
        Ag = self.g
        Ah = self.h
        Ai = self.i
        Aj = self.j
        Ak = self.k
        Al = self.l
        Am = self.m
        An = self.n
        Ao = self.o
        Ap = self.p
        Ba = other.a
        Bb = other.b
        Bc = other.c
        Bd = other.d
        Be = other.e
        Bf = other.f
        Bg = other.g
        Bh = other.h
        Bi = other.i
        Bj = other.j
        Bk = other.k
        Bl = other.l
        Bm = other.m
        Bn = other.n
        Bo = other.o
        Bp = other.p
        self.a = Aa * Ba + Ab * Be + Ac * Bi + Ad * Bm
        self.b = Aa * Bb + Ab * Bf + Ac * Bj + Ad * Bn
        self.c = Aa * Bc + Ab * Bg + Ac * Bk + Ad * Bo
        self.d = Aa * Bd + Ab * Bh + Ac * Bl + Ad * Bp
        self.e = Ae * Ba + Af * Be + Ag * Bi + Ah * Bm
        self.f = Ae * Bb + Af * Bf + Ag * Bj + Ah * Bn
        self.g = Ae * Bc + Af * Bg + Ag * Bk + Ah * Bo
        self.h = Ae * Bd + Af * Bh + Ag * Bl + Ah * Bp
        self.i = Ai * Ba + Aj * Be + Ak * Bi + Al * Bm
        self.j = Ai * Bb + Aj * Bf + Ak * Bj + Al * Bn
        self.k = Ai * Bc + Aj * Bg + Ak * Bk + Al * Bo
        self.l = Ai * Bd + Aj * Bh + Ak * Bl + Al * Bp
        self.m = Am * Ba + An * Be + Ao * Bi + Ap * Bm
        self.n = Am * Bb + An * Bf + Ao * Bj + Ap * Bn
        self.o = Am * Bc + An * Bg + Ao * Bk + Ap * Bo
        self.p = Am * Bd + An * Bh + Ao * Bl + Ap * Bp
        return self

    def transform(self, other):
        """
        Apply self to the Point3 or Vector3 'other'

        @return: Point3 instance
        """
        A = self
        B = other
        P = Point3(0, 0, 0) # arguments not needed here
        P.x = A.a * B.x + A.b * B.y + A.c * B.z + A.d
        P.y = A.e * B.x + A.f * B.y + A.g * B.z + A.h
        P.z = A.i * B.x + A.j * B.y + A.k * B.z + A.l
        w =   A.m * B.x + A.n * B.y + A.o * B.z + A.p
        if w != 0:
            P.x /= w
            P.y /= w
            P.z /= w
        return P

    def identity(self):
        """
        Convert self to an identity matrix::
        
         1 0 0 0
         0 1 0 0
         0 0 1 0
         0 0 0 1
        """
        self.a = self.f = self.k = self.p = 1.
        self.b = self.c = self.d = self.e = self.g = self.h = \
        self.i = self.j = self.l = self.m = self.n = self.o = 0
        return self

    def scale(self, x, y, z):
        """
        Applies a scaling to this Matrix4 instance.

        Note that this method operates in-place (modifies the original
        matrix), and also returns itself as a result. This allows you to
        chain transforms together directly.

        Example::
        
          In [7]: m = Matrix4()
          In [8]: m.scale(2,3,4)
          Out[8]:
          Matrix4([    2.00     0.00     0.00     0.00
                       0.00     3.00     0.00     0.00
                       0.00     0.00     4.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        self *= Matrix4.new_scale(x, y, z)
        return self

    def translate(self, x, y, z):
        """
        Translates self by x, y, and z.

        Using the result of the Matrix4 scale method example::
        
          In [9]: m.translate(.5,.6,.7)
          Out[9]:
          Matrix4([    2.00     0.00     0.00     1.00
                       0.00     3.00     0.00     1.80
                       0.00     0.00     4.00     2.80
                       0.00     0.00     0.00     1.00])
        """
        self *= Matrix4.new_translate(x, y, z)
        return self 

    def rotatex(self, angle):
        """
        Rotate self CCW by 'angle' radians about the X axis

        Example::
        
          In [13]: p = Plane(Vector3(0,0,1),2.)
          In [15]: r1 = Matrix4().rotatex(pi/2)
          In [16]: r1*p
          Out[16]: Plane(<0.00, -1.00, 0.00>.p = 2.00)
        """
        self *= Matrix4.new_rotatex(angle)
        return self

    def rotatey(self, angle):
        """
        Rotate self CCW by 'angle' radians about the Y axis

        Example::
        
          In [17]: p
          Out[17]: Plane(<0.00, 0.00, 1.00>.p = 2.00)
          In [18]: r2 = Matrix4().rotatey(pi/2)
          In [19]: r2*p
          Out[19]: Plane(<1.00, 0.00, 0.00>.p = 2.00)
        """
        self *= Matrix4.new_rotatey(angle)
        return self

    def rotatez(self, angle):
        """
        Rotate self CCW by 'angle' radians about the Z axis

        Example::
        
          In [17]: p
          Out[17]: Plane(<0.00, 0.00, 1.00>.p = 2.00)
          In [20]: r3 = Matrix4().rotatez(pi/2)
          In [21]: r3*p
          Out[21]: Plane(<0.00, 0.00, 1.00>.p = 2.00)
        """
        self *= Matrix4.new_rotatez(angle)
        return self

    def rotate_axis(self, angle, axis):
        """
        Rotate self CCW by 'angle' radians about Vector3 'axis'

        'axis' does not have to be normalized.

        Example::
        
          In [22]: p
          Out[22]: Plane(<0.00, 0.00, 1.00>.p = 2.00)
          In [27]: r4 = Matrix4().rotate_axis(pi/2,Vector3(0,1,0))
          In [28]: r4*p
          Out[28]: Plane(<1.00, 0.00, 0.00>.p = 2.00)
        """
        self *= Matrix4.new_rotate_axis(angle, axis)
        return self

    def rotate_euler(self, heading, attitude, bank):
        """
        Rotate self with the given Euler rotation.

        heading  is a rotation around the Y axis,
        attitude is a rotation around the X axis and
        bank     is a rotation around the Z axis.
        All rotations are performed simultaneously, so this method avoids
        "gimbal lock" and is the usual method for implemented 3D rotations
        in a game.
        """
        self *= Matrix4.new_rotate_euler(heading, attitude, bank)
        return self

    def rotate_triple_axis(self, x, y, z):
        """
        Transform self with the tensor (outer) product of vector (x,y,z)

        
        """
        self *= Matrix4.new_rotate_triple_axis(x, y, z)
        return self

    def transpose(self):
        """
        Transposes self
        
        a b c d        a e i m
        e f g h   ==>  b f j n
        i j k l        c g k o
        m n o p        d h l p
        """
        (self.a, self.e, self.i, self.m,
         self.b, self.f, self.j, self.n,
         self.c, self.g, self.k, self.o,
         self.d, self.h, self.l, self.p) = \
        (self.a, self.b, self.c, self.d,
         self.e, self.f, self.g, self.h,
         self.i, self.j, self.k, self.l,
         self.m, self.n, self.o, self.p)

    def transposed(self):
        """
        Returns a transposed copy of self
        """
        M = self.copy()
        M.transpose()
        return M

    # Static constructors
    
    def new(cls, *values):
        """
        Create a new instance with specified elements
        """
        M = cls()
        M[:] = values
        return M
    new = classmethod(new)

    def new_identity(cls):
        """
        Creates a new instance of a Matrix4 identity matrix.

        Equivalent to Matrix4()
        Example::
        
          >>> m = Matrix4.new_identity()
          >>> m
          Matrix4([    1.00     0.00     0.00     0.00
                       0.00     1.00     0.00     0.00
                       0.00     0.00     1.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        self = cls()
        return self
    new_identity = classmethod(new_identity)

    def new_scale(cls, x, y, z):
        """
        Creates a new general scaling matrix along the x, y, and z axes.

        The three arguments indicate the desired scale factors along each of
        the three axes.
        
        Example::
        
          >>> m = Matrix4.new_scale(2.0, 3.0, 4.0)
          >>> m
          Matrix4([    2.00     0.00     0.00     0.00
                       0.00     3.00     0.00     0.00
                       0.00     0.00     4.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        self = cls()
        self.a = x
        self.f = y
        self.k = z
        return self
    new_scale = classmethod(new_scale)

    def new_translate(cls, x, y, z):
        """
        Creates a new translation matrix.
        
        Example::
        
          >>> m = Matrix4.new_translate(3.0, 4.0, 5.0)
          >>> m
          Matrix4([    1.00     0.00     0.00     3.00
                       0.00     1.00     0.00     4.00
                       0.00     0.00     1.00     5.00
                       0.00     0.00     0.00     1.00])
        """
        self = cls()
        self.d = x
        self.h = y
        self.l = z
        return self
    new_translate = classmethod(new_translate)

    def new_rotatex(cls, angle):
        """
        Create a new Matrix4 for a rotation around the X axis.

        angle is specified in radians.
        
        Example::
        
          >>> m = Matrix4.new_rotatex(math.pi / 2)
          >>> m
          Matrix4([    1.00     0.00     0.00     0.00
                       0.00     0.00    -1.00     0.00
                       0.00     1.00     0.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        self = cls()
        s = math.sin(angle)
        c = math.cos(angle)
        self.f = self.k = c
        self.g = -s
        self.j = s
        return self
    new_rotatex = classmethod(new_rotatex)

    def new_rotatey(cls, angle):
        """
        Create a new Matrix4 for a rotation around the Y axis.

        angle is specified in radians.

        Example::
        
          In [12]: Matrix4.new_rotatey(math.pi / 2)
          Out[12]:
          Matrix4([    0.00     0.00     1.00     0.00
                       0.00     1.00     0.00     0.00
                      -1.00     0.00     0.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        self = cls()
        s = math.sin(angle)
        c = math.cos(angle)
        self.a = self.k = c
        self.c = s
        self.i = -s
        return self    
    new_rotatey = classmethod(new_rotatey)
    
    def new_rotatez(cls, angle):
        """
        Create a new Matrix4 for a rotation around the Z axis.

        angle is specified in radians.

        Example::
        
          In [13]: Matrix4.new_rotatez(math.pi / 2)
          Out[13]:
          Matrix4([    0.00    -1.00     0.00     0.00
                       1.00     0.00     0.00     0.00
                       0.00     0.00     1.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        self = cls()
        s = math.sin(angle)
        c = math.cos(angle)
        self.a = self.f = c
        self.b = -s
        self.e = s
        return self
    new_rotatez = classmethod(new_rotatez)

    def new_rotate_axis(cls, angle, axis):
        """
        Create a Matrix4 for a rotation around the given axis.

        angle is specified in radians, and axis must be an instance of Vector3.
        It is not necessary to normalize the axis.

        Example::
        
          >>> m = Matrix4.new_rotate_axis(math.pi / 2, Vector3(1.0, 0.0, 0.0))
          >>> m
          Matrix4([    1.00     0.00     0.00     0.00
                       0.00     0.00    -1.00     0.00
                       0.00     1.00     0.00     0.00
                       0.00     0.00     0.00     1.00])
        """
        assert(isinstance(axis, Vector3))
        vector = axis.normalized()
        x = vector.x
        y = vector.y
        z = vector.z

        self = cls()
        s = math.sin(angle)
        c = math.cos(angle)
        c1 = 1. - c
        
        # from the glRotate man page
        self.a = x * x * c1 + c
        self.b = x * y * c1 - z * s
        self.c = x * z * c1 + y * s
        self.e = y * x * c1 + z * s
        self.f = y * y * c1 + c
        self.g = y * z * c1 - x * s
        self.i = x * z * c1 - y * s
        self.j = y * z * c1 + x * s
        self.k = z * z * c1 + c
        return self
    new_rotate_axis = classmethod(new_rotate_axis)

    def new_rotate_euler(cls, heading, attitude, bank):
        """
        Create a Matrix4 for the given Euler rotation.

        heading is a rotation around the Y axis,
        attitude around the X axis and
        bank around the Z axis.
        All rotations are performed simultaneously, so this method avoids
        "gimbal lock" and is the usual method for implemented 3D rotations
        in a game.

        Example::
        
          >>> m = Matrix4.new_rotate_euler(math.pi / 2, math.pi / 2, 0.0)
          >>> m
          Matrix4([    0.00    -0.00     1.00     0.00
                       1.00     0.00    -0.00     0.00
                      -0.00     1.00     0.00     0.00
                       0.00     0.00     0.00     1.00])

        From http://www.euclideanspace.com/
        """
        ch = math.cos(heading)
        sh = math.sin(heading)
        ca = math.cos(attitude)
        sa = math.sin(attitude)
        cb = math.cos(bank)
        sb = math.sin(bank)

        self = cls()
        self.a = ch * ca
        self.b = sh * sb - ch * sa * cb
        self.c = ch * sa * sb + sh * cb
        self.e = sa
        self.f = ca * cb
        self.g = -ca * sb
        self.i = -sh * ca
        self.j = sh * sa * cb + ch * sb
        self.k = -sh * sa * sb + ch * cb
        return self
    new_rotate_euler = classmethod(new_rotate_euler)

    def new_rotate_triple_axis(cls, x, y, z):
      """
      Rotate into a new coordinate system defined by Vector3 instances x,y,z
      
      Examples::
      
        In [48]: p
        Out[48]: Plane(<0.00, 0.00, 1.00>.p = 2.00)
        In [49]: r5 = Matrix4().rotate_triple_axis(Vector3(1.,0.,0.),
                                                   Vector3(0.,1.,0.),
                                                   Vector3(0.,0.,-1.))
        In [50]: r5*p
        Out[50]: Plane(<0.00, 0.00, -1.00>.p = 2.00)
        In [51]: r5 = Matrix4().rotate_triple_axis(Vector3(1.,0.,0.),
                                                   Vector3(0.,0.,1.),
                                                   Vector3(0.,1.,0.))
        In [52]: r5*p
        Out[52]: Plane(<0.00, 1.00, 0.00>.p = 2.00)
      """
      m = cls()
      
      m.a, m.b, m.c = x.x, y.x, z.x
      m.e, m.f, m.g = x.y, y.y, z.y
      m.i, m.j, m.k = x.z, y.z, z.z
      
      return m
    new_rotate_triple_axis = classmethod(new_rotate_triple_axis)

    def new_look_at(cls, eye, at, up):
      """
      Create a viewer's coordinate system.

      z is from the viewer to the upject
      x is perpendicular to the up vector and z
      y is perpendicular to x and z
      """
      z = (eye - at).normalized()
      x = up.cross(z).normalized()
      y = z.cross(x)
      
      m = cls.new_rotate_triple_axis(x, y, z)
      m.d, m.h, m.l = eye.x, eye.y, eye.z
      return m
    new_look_at = classmethod(new_look_at)
    
    def new_perspective(cls, fov_y, aspect, near, far):
        """
        Create a Matrix4 for projection onto the 2D viewing plane.

        This method is equivalent to the OpenGL call gluPerspective.
        fov_y is the view angle in the Y direction, in radians.
        aspect is the aspect ration width / height of the viewing plane.
        near and far are the distance to the near and far clipping planes.
        They must be positive and non-zero.

        Example::
        
          >>> m = Matrix4.new_perspective(math.pi / 2, 1024.0 / 768, 1.0, 100.0)
          >>> m
          Matrix4([    0.75     0.00     0.00     0.00
                       0.00     1.00     0.00     0.00
                       0.00     0.00    -1.02    -2.02
                       0.00     0.00    -1.00     0.00])

        From the gluPerspective man page
        """
        f = 1 / math.tan(fov_y / 2)
        self = cls()
        assert near != 0.0 and near != far
        self.a = f / aspect
        self.f = f
        self.k = (far + near) / (near - far)
        self.l = 2 * far * near / (near - far)
        self.o = -1
        self.p = 0
        return self
    new_perspective = classmethod(new_perspective)

    def determinant(self):
        """
        determinant of self
        
        @return: float
        """
        return ((self.a * self.f - self.e * self.b)
              * (self.k * self.p - self.o * self.l)
              - (self.a * self.j - self.i * self.b)
              * (self.g * self.p - self.o * self.h)
              + (self.a * self.n - self.m * self.b)
              * (self.g * self.l - self.k * self.h)
              + (self.e * self.j - self.i * self.f)
              * (self.c * self.p - self.o * self.d)
              - (self.e * self.n - self.m * self.f)
              * (self.c * self.l - self.k * self.d)
              + (self.i * self.n - self.m * self.j)
              * (self.c * self.h - self.g * self.d))

    def inverse(self):
        """
        Inverts self.
        """
        tmp = Matrix4()
        d = self.determinant();

        if abs(d) < 0.001:
            # No inverse, return identity
            return tmp
        else:
            d = 1.0 / d;

            tmp.a = d * ( self.f * (self.k * self.p - self.o * self.l)
                        + self.j * (self.o * self.h - self.g * self.p)
                        + self.n * (self.g * self.l - self.k * self.h));
            tmp.e = d * ( self.g * (self.i * self.p - self.m * self.l)
                        + self.k * (self.m * self.h - self.e * self.p)
                        + self.o * (self.e * self.l - self.i * self.h));
            tmp.i = d * (self.h * (self.i * self.n - self.m * self.j)
                        + self.l * (self.m * self.f - self.e * self.n)
                        + self.p * (self.e * self.j - self.i * self.f));
            tmp.m = d * (self.e * (self.n * self.k - self.j * self.o)
                        + self.i * (self.f * self.o - self.n * self.g)
                        + self.m * (self.j * self.g - self.f * self.k));
            
            tmp.b = d * (self.j * (self.c * self.p - self.o * self.d)
                        + self.n * (self.k * self.d - self.c * self.l)
                        + self.b * (self.o * self.l - self.k * self.p));
            tmp.f = d * (self.k * (self.a * self.p - self.m * self.d)
                        + self.o * (self.i * self.d - self.a * self.l)
                        + self.c * (self.m * self.l - self.i * self.p));
            tmp.j = d * (self.l * (self.a * self.n - self.m * self.b)
                        + self.p * (self.i * self.b - self.a * self.j)
                        + self.d * (self.m * self.j - self.i * self.n));
            tmp.n = d * (self.i * (self.n * self.c - self.b * self.o)
                        + self.m * (self.b * self.k - self.j * self.c)
                        + self.a * (self.j * self.o - self.n * self.k));
            
            tmp.c = d * (self.n * (self.c * self.h - self.g * self.d)
                        + self.b * (self.g * self.p - self.o * self.h)
                        + self.f * (self.o * self.d - self.c * self.p));
            tmp.g = d * (self.o * (self.a * self.h - self.e * self.d)
                        + self.c * (self.e * self.p - self.m * self.h)
                        + self.g * (self.m * self.d - self.a * self.p));
            tmp.k = d * (self.p * (self.a * self.f - self.e * self.b)
                        + self.d * (self.e * self.n - self.m * self.f)
                        + self.h * (self.m * self.b - self.a * self.n));
            tmp.o = d * (self.m * (self.f * self.c - self.b * self.g)
                        + self.a * (self.n * self.g - self.f * self.o)
                        + self.e * (self.b * self.o - self.n * self.c));
            
            tmp.d = d * (self.b * (self.k * self.h - self.g * self.l)
                        + self.f * (self.c * self.l - self.k * self.d)
                        + self.j * (self.g * self.d - self.c * self.h));
            tmp.h = d * (self.c * (self.i * self.h - self.e * self.l)
                        + self.g * (self.a * self.l - self.i * self.d)
                        + self.k * (self.e * self.d - self.a * self.h));
            tmp.l = d * (self.d * (self.i * self.f - self.e * self.j)
                        + self.h * (self.a * self.j - self.i * self.b)
                        + self.l * (self.e * self.b - self.a * self.f));
            tmp.p = d * (self.a * (self.f * self.k - self.j * self.g)
                        + self.e * (self.j * self.c - self.b * self.k)
                        + self.i * (self.b * self.g - self.f * self.c));

        return tmp;


class Quaternion:
    """
    w is the real part, (x, y, z) are the imaginary parts
    
    All methods and naming conventions are based on
    http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions
    """

    __slots__ = ['w', 'x', 'y', 'z']

    def __init__(self, w=1, x=0, y=0, z=0):
        self.w = w
        self.x = x
        self.y = y
        self.z = z

    def __copy__(self):
        """
        Returns a copy of itself
        """
        Q = Quaternion()
        Q.w = self.w
        Q.x = self.x
        Q.y = self.y
        Q.z = self.z
        return Q

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Quaternion(real=%.2f, imag=<%.2f, %.2f, %.2f>)' % \
            (self.w, self.x, self.y, self.z)

    def __mul__(self, other):
        """
        Invoked for "*" scalar multiplication of self by other

        @param other : other element in a binary operation
        @type  other : Quaternion, Vector3, int, long or float

        @return: Quaternion
        """
        if isinstance(other, Quaternion):
            Ax = self.x
            Ay = self.y
            Az = self.z
            Aw = self.w
            Bx = other.x
            By = other.y
            Bz = other.z
            Bw = other.w
            Q = Quaternion()
            Q.x =  Ax * Bw + Ay * Bz - Az * By + Aw * Bx    
            Q.y = -Ax * Bz + Ay * Bw + Az * Bx + Aw * By
            Q.z =  Ax * By - Ay * Bx + Az * Bw + Aw * Bz
            Q.w = -Ax * Bx - Ay * By - Az * Bz + Aw * Bw
            return Q
        elif isinstance(other, Vector3):
            w = self.w
            x = self.x
            y = self.y
            z = self.z
            Vx = other.x
            Vy = other.y
            Vz = other.z
            ww = w * w
            w2 = w * 2
            wx2 = w2 * x
            wy2 = w2 * y
            wz2 = w2 * z
            xx = x * x
            x2 = x * 2
            xy2 = x2 * y
            xz2 = x2 * z
            yy = y * y
            yz2 = 2 * y * z
            zz = z * z
            return other.__class__(\
               ww * Vx + wy2 * Vz - wz2 * Vy + \
               xx * Vx + xy2 * Vy + xz2 * Vz - \
               zz * Vx - yy * Vx,
               xy2 * Vx + yy * Vy + yz2 * Vz + \
               wz2 * Vx - zz * Vy + ww * Vy - \
               wx2 * Vz - xx * Vy,
               xz2 * Vx + yz2 * Vy + \
               zz * Vz - wy2 * Vx - yy * Vz + \
               wx2 * Vy - xx * Vz + ww * Vz)
        else:
            other = other.copy()
            other._apply_transform(self)
            return other

    def __imul__(self, other):
        """
        Invoked for '\*='; scalar multiplication of self

        @param other : other element in a binary operation
        @type  other : Quaternion

        @return: self
        """
        assert isinstance(other, Quaternion)
        Ax = self.x
        Ay = self.y
        Az = self.z
        Aw = self.w
        Bx = other.x
        By = other.y
        Bz = other.z
        Bw = other.w
        self.x =  Ax * Bw + Ay * Bz - Az * By + Aw * Bx    
        self.y = -Ax * Bz + Ay * Bw + Az * Bx + Aw * By
        self.z =  Ax * By - Ay * Bx + Az * Bw + Aw * Bz
        self.w = -Ax * Bx - Ay * By - Az * Bz + Aw * Bw
        return self

    def __abs__(self):
        """
        Returns magnitude (absolute value) of Quaternion 'self'
        """
        return math.sqrt(self.w ** 2 + \
                         self.x ** 2 + \
                         self.y ** 2 + \
                         self.z ** 2)

    magnitude = __abs__

    def magnitude_squared(self):
        """
        Returns square of Vector3 'self'
        """
        return self.w ** 2 + \
               self.x ** 2 + \
               self.y ** 2 + \
               self.z ** 2 

    def identity(self):
        """
        Change self to an identity quaternion

        Example::
        
         In [54]: Quaternion()
         Out[54]: Quaternion(real=1.00, imag=<0.00, 0.00, 0.00>)
        """
        self.w = 1
        self.x = 0
        self.y = 0
        self.z = 0
        return self

    def rotate_axis(self, angle, axis):
        """
        Apply a rotation of 'angle' radians about 'axis' to self
        """
        self *= Quaternion.new_rotate_axis(angle, axis)
        return self

    def rotate_euler(self, heading, attitude, bank):
        """
        Apply an Euler rotation to self.

        heading  is a rotation around the Y axis,
        attitude is a rotation around the X axis and
        bank     is a rotation around the Z axis.
        All rotations are performed simultaneously, so this method avoids
        "gimbal lock" and is the usual method for implemented 3D rotations
        in a game.
        """
        self *= Quaternion.new_rotate_euler(heading, attitude, bank)
        return self

    def rotate_matrix(self, m):
        self *= Quaternion.new_rotate_matrix(m)
        return self

    def conjugated(self):
        """
        Conjugated version of self
        """
        Q = Quaternion()
        Q.w = self.w
        Q.x = -self.x
        Q.y = -self.y
        Q.z = -self.z
        return Q

    def normalize(self):
        """
        Normalize self
        """
        d = self.magnitude()
        if d != 0:
            self.w /= d
            self.x /= d
            self.y /= d
            self.z /= d
        return self

    def normalized(self):
        """
        Returns a copy of self normalized to unit length.
        """
        d = self.magnitude()
        if d != 0:
            Q = Quaternion()
            Q.w = self.w / d
            Q.x = self.x / d
            Q.y = self.y / d
            Q.z = self.z / d
            return Q
        else:
            return self.copy()

    def get_angle_axis(self):
        """
        """
        if self.w > 1:
            self = self.normalized()
        angle = 2 * math.acos(self.w)
        s = math.sqrt(1 - self.w ** 2)
        if s < 0.001:
            return angle, Vector3(1, 0, 0)
        else:
            return angle, Vector3(self.x / s, self.y / s, self.z / s)

    def get_euler(self):
        """
        Return the heading, attitude and bank of self.
        
        heading  is a rotation around the Y axis,
        attitude is a rotation around the X axis and
        bank     is a rotation around the Z axis.
        All rotations are performed simultaneously, so this method avoids
        "gimbal lock" and is the usual method for implemented 3D rotations
        in a game.
        """
        t = self.x * self.y + self.z * self.w
        if t > 0.4999:
            heading = 2 * math.atan2(self.x, self.w)
            attitude = math.pi / 2
            bank = 0
        elif t < -0.4999:
            heading = -2 * math.atan2(self.x, self.w)
            attitude = -math.pi / 2
            bank = 0
        else:
            sqx = self.x ** 2
            sqy = self.y ** 2
            sqz = self.z ** 2
            heading = math.atan2(2 * self.y * self.w - 2 * self.x * self.z,
                                 1 - 2 * sqy - 2 * sqz)
            attitude = math.asin(2 * t)
            bank = math.atan2(2 * self.x * self.w - 2 * self.y * self.z,
                              1 - 2 * sqx - 2 * sqz)
        return heading, attitude, bank

    def get_matrix(self):
        """
        """
        xx = self.x ** 2
        xy = self.x * self.y
        xz = self.x * self.z
        xw = self.x * self.w
        yy = self.y ** 2
        yz = self.y * self.z
        yw = self.y * self.w
        zz = self.z ** 2
        zw = self.z * self.w
        M = Matrix4()
        M.a = 1 - 2 * (yy + zz)
        M.b = 2 * (xy - zw)
        M.c = 2 * (xz + yw)
        M.e = 2 * (xy + zw)
        M.f = 1 - 2 * (xx + zz)
        M.g = 2 * (yz - xw)
        M.i = 2 * (xz - yw)
        M.j = 2 * (yz + xw)
        M.k = 1 - 2 * (xx + yy)
        return M

    # Static constructors
    
    def new_identity(cls):
        return cls()
    new_identity = classmethod(new_identity)

    def new_rotate_axis(cls, angle, axis):
        assert(isinstance(axis, Vector3))
        axis = axis.normalized()
        s = math.sin(angle / 2)
        Q = cls()
        Q.w = math.cos(angle / 2)
        Q.x = axis.x * s
        Q.y = axis.y * s
        Q.z = axis.z * s
        return Q
    new_rotate_axis = classmethod(new_rotate_axis)

    def new_rotate_euler(cls, heading, attitude, bank):
        """

        heading  is a rotation around the Y axis,
        attitude is a rotation around the X axis and
        bank     is a rotation around the Z axis.
        All rotations are performed simultaneously, so this method avoids
        "gimbal lock" and is the usual method for implemented 3D rotations
        in a game.
        """
        Q = cls()
        c1 = math.cos(heading / 2)
        s1 = math.sin(heading / 2)
        c2 = math.cos(attitude / 2)
        s2 = math.sin(attitude / 2)
        c3 = math.cos(bank / 2)
        s3 = math.sin(bank / 2)

        Q.w = c1 * c2 * c3 - s1 * s2 * s3
        Q.x = s1 * s2 * c3 + c1 * c2 * s3
        Q.y = s1 * c2 * c3 + c1 * s2 * s3
        Q.z = c1 * s2 * c3 - s1 * c2 * s3
        return Q
    new_rotate_euler = classmethod(new_rotate_euler)
    
    def new_rotate_matrix(cls, m):
      """
      """
      if m[0*4 + 0] + m[1*4 + 1] + m[2*4 + 2] > 0.00000001:
        t = m[0*4 + 0] + m[1*4 + 1] + m[2*4 + 2] + 1.0
        s = 0.5/math.sqrt(t)
        
        return cls(
          s*t,
          (m[1*4 + 2] - m[2*4 + 1])*s,
          (m[2*4 + 0] - m[0*4 + 2])*s,
          (m[0*4 + 1] - m[1*4 + 0])*s
          )
        
      elif m[0*4 + 0] > m[1*4 + 1] and m[0*4 + 0] > m[2*4 + 2]:
        t = m[0*4 + 0] - m[1*4 + 1] - m[2*4 + 2] + 1.0
        s = 0.5/math.sqrt(t)
        
        return cls(
          (m[1*4 + 2] - m[2*4 + 1])*s,
          s*t,
          (m[0*4 + 1] + m[1*4 + 0])*s,
          (m[2*4 + 0] + m[0*4 + 2])*s
          )
        
      elif m[1*4 + 1] > m[2*4 + 2]:
        t = -m[0*4 + 0] + m[1*4 + 1] - m[2*4 + 2] + 1.0
        s = 0.5/math.sqrt(t)
        
        return cls(
          (m[2*4 + 0] - m[0*4 + 2])*s,
          (m[0*4 + 1] + m[1*4 + 0])*s,
          s*t,
          (m[1*4 + 2] + m[2*4 + 1])*s
          )
        
      else:
        t = -m[0*4 + 0] - m[1*4 + 1] + m[2*4 + 2] + 1.0
        s = 0.5/math.sqrt(t)
        
        return cls(
          (m[0*4 + 1] - m[1*4 + 0])*s,
          (m[2*4 + 0] + m[0*4 + 2])*s,
          (m[1*4 + 2] + m[2*4 + 1])*s,
          s*t
          )
    new_rotate_matrix = classmethod(new_rotate_matrix)
    
    def new_interpolate(cls, q1, q2, t):
        assert isinstance(q1, Quaternion) and isinstance(q2, Quaternion)
        Q = cls()

        costheta = q1.w * q2.w + q1.x * q2.x + q1.y * q2.y + q1.z * q2.z
        if costheta < 0.:
            costheta = -costheta
            q1 = q1.conjugated()
        elif costheta > 1:
            costheta = 1

        theta = math.acos(costheta)
        if abs(theta) < 0.01:
            Q.w = q2.w
            Q.x = q2.x
            Q.y = q2.y
            Q.z = q2.z
            return Q

        sintheta = math.sqrt(1.0 - costheta * costheta)
        if abs(sintheta) < 0.01:
            Q.w = (q1.w + q2.w) * 0.5
            Q.x = (q1.x + q2.x) * 0.5
            Q.y = (q1.y + q2.y) * 0.5
            Q.z = (q1.z + q2.z) * 0.5
            return Q

        ratio1 = math.sin((1 - t) * theta) / sintheta
        ratio2 = math.sin(t * theta) / sintheta

        Q.w = q1.w * ratio1 + q2.w * ratio2
        Q.x = q1.x * ratio1 + q2.x * ratio2
        Q.y = q1.y * ratio1 + q2.y * ratio2
        Q.z = q1.z * ratio1 + q2.z * ratio2
        return Q
    new_interpolate = classmethod(new_interpolate)

############################ Class Geometry ############################

class Geometry:
    """
    Superclass to define methods to be implemented by sub-classes

    It provides exceptions for unimplemented methods.

    Much maths thanks to Paul Bourke, http://paulbourke.net/geometry/
    """
    def _connect_unimplemented(self, other):
        raise AttributeError, 'Cannot connect %s to %s' % \
            (self.__class__, other.__class__)

    def _intersect_unimplemented(self, other):
        raise AttributeError, 'Cannot intersect %s and %s' % \
            (self.__class__, other.__class__)

    _intersect_point2 = _intersect_unimplemented
    _intersect_line2 = _intersect_unimplemented
    _intersect_circle = _intersect_unimplemented
    _connect_point2 = _connect_unimplemented
    _connect_line2 = _connect_unimplemented
    _connect_circle = _connect_unimplemented

    _intersect_point3 = _intersect_unimplemented
    _intersect_line3 = _intersect_unimplemented
    _intersect_sphere = _intersect_unimplemented
    _intersect_plane = _intersect_unimplemented
    _connect_point3 = _connect_unimplemented
    _connect_line3 = _connect_unimplemented
    _connect_sphere = _connect_unimplemented
    _connect_plane = _connect_unimplemented

    def intersect(self, other):
        raise NotImplementedError

    def connect(self, other):
        raise NotImplementedError

    def distance(self, other):
        c = self.connect(other)
        if c:
            return c.length
        return 0.0

############################# 2D Classes and Methods #########################

class Point2(Vector2, Geometry):
    """
    A class for a point on an XY plane.

    This is not a user accessible class. It cannot be instantiated but it can
    be created by methods of other classes

    Public attributes::
    
      x - coordinate (float)
      y - coordinate (float)
    """
    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Point2(%.2f, %.2f)' % (self.x, self.y)

    def intersect(self, other):
        """
        """
        return other._intersect_point2(self)

    def _intersect_circle(self, other):
        """
        Returns True if this Point instance is inside Circle 'other'
        """
        return _intersect_point2_circle(self, other)

    def connect(self, other):
        """
        Returns a LineSegment2 instance connecting self with other
        """
        return other._connect_point2(self)

    def _connect_point2(self, other):
        """
        Returns a LineSegment2 instance connecting self with other
        """
        return LineSegment2(other, self)
    
    def _connect_line2(self, other):
        """
        """
        c = _connect_point2_line2(self, other)
        if c:
            return c._swap()

    def _connect_circle(self, other):
        """
        """
        c = _connect_point2_circle(self, other)
        if c:
            return c._swap()

class Line2(Geometry):
    """
    An infinite line in an XY plane

    Public attributes::
    
     p - first point of the line
     v - vector from p to the second point
    """
    __slots__ = ['p', 'v']

    def __init__(self, *args):
        """
        Creates an instance of Line2.

        If three arguments are given, then the first must be a Point2, the
        second a Vector2 and the third a float.  It will create a line from
        the point along the direction of the vector with the length of the
        float.

        If two arguments are given, then the first must be a Point2 and the
        second may be a Vector2 or a Point2.

        If one argument is given, then it must be a Line2 and a copy is made.
        """
        if len(args) == 3:
            assert isinstance(args[0], Point2) and \
                   isinstance(args[1], Vector2) and \
                   type(args[2]) == float
            self.p = args[0].copy()
            self.v = args[1] * args[2] / abs(args[1])
        elif len(args) == 2:
            if isinstance(args[0], Point2) and isinstance(args[1], Point2):
                self.p = args[0].copy()
                self.v = args[1] - args[0]
            elif isinstance(args[0], Point2) and isinstance(args[1], Vector2):
                self.p = args[0].copy()
                self.v = args[1].copy()
            else:
                raise AttributeError, '%r' % (args,)
        elif len(args) == 1:
            if isinstance(args[0], Line2):
                self.p = args[0].p.copy()
                self.v = args[0].v.copy()
            else:
                raise AttributeError, '%r' % (args,)
        else:
            raise AttributeError, '%r' % (args,)
        
        if not self.v:
            raise AttributeError, 'Line has zero-length vector'

    def __copy__(self):
        """
        Returns a copy of itself
        """
        return self.__class__(self.p, self.v)

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Line2(<%.2f, %.2f> + u<%.2f, %.2f>)' % \
            (self.p.x, self.p.y, self.v.x, self.v.y)

    p1 = property(lambda self: self.p)
    p2 = property(lambda self: Point2(self.p.x + self.v.x, 
                                      self.p.y + self.v.y))

    def _apply_transform(self, t):
        """
        Apply a transform 't' the line.

        This applies a scalar to both the first point coordinates and the line length

        @param t : scaling value
        @type  t : int or long or float
        """
        self.p = t * self.p
        self.v = t * self.v

    def _u_in(self, u):
        """
        Is point u in self?
        """
        return True

    def intersect(self, other):
        """
        Computes the point at which self intersects other

        @param other : line segment
        @type  other : Line2 instance

        @return: Point2 instance or None
        """
        return other._intersect_line2(self)

    def _intersect_line2(self, other):
        """
        Computes the point at which other intersects self

        @param other : line segment
        @type  other : Line2 instance

        @return: Point2 instance or None
        """
        return _intersect_line2_line2(self, other)

    def _intersect_circle(self, other):
        return _intersect_line2_circle(self, other)

    def connect(self, other):
        return other._connect_line2(self)

    def _connect_point2(self, other):
        return _connect_point2_line2(other, self)

    def _connect_line2(self, other):
        return _connect_line2_line2(other, self)

    def _connect_circle(self, other):
        return _connect_circle_line2(other, self)

class Ray2(Line2):
    """
    A semi-infinite line from a point

    Public attributes inherited from Line2::
    
     p  - first Point instance
     v  - Vector instance to second point
     p2 - second Point instance

    Example::
    
     In [16]: r = Ray2(p1,p2)
     In [17]: r
     Out[17]: Ray2(<1.00, 1.00> + u<1.00, 1.00>)
     In [18]: r.p
     Out[18]: Point2(1.00, 1.00)
     In [19]: r.v
     Out[19]: Vector2(1.00, 1.00)
     In [20]: r.p2
     Out[20]: Point2(2.00, 2.00)
    """
    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Ray2(<%.2f, %.2f> + u<%.2f, %.2f>)' % \
            (self.p.x, self.p.y, self.v.x, self.v.y)

    def _u_in(self, u):
        """
        True if u is on the ray

        @param u : a normalized length
        @type  u : float
        """
        return u >= 0.0

class LineSegment2(Line2):
    """
    Class for a line bounded by two points.

    Public attributes::
    
      p  - first point (Point2 instance)
      v  - vector to second point (Vector2 instance)
      p2 - second point (Point2 instance)

    Example::
    
     In [8]: p1 = Point2(1,1)
     In [9]: p2 = Point2(2,2)
     In [10]: l = LineSegment2(p1,p2)
     In [11]: l
     Out[11]: LineSegment2(<1.00, 1.00> to <2.00, 2.00>)
     In [12]: l.p
     Out[12]: Point2(1.00, 1.00)
     In [13]: l.v
     Out[13]: Vector2(1.00, 1.00)
     In [14]: l.p2
     Out[14]: Point2(2.00, 2.00)
    """
    def __repr__(self):
        """
        Text description of line segment
        """
        return 'LineSegment2(<%.2f, %.2f> to <%.2f, %.2f>)' % \
            (self.p.x, self.p.y, self.p.x + self.v.x, self.p.y + self.v.y)

    def _u_in(self, u):
        """
        True if u is less than the normalized line length (i.e. = 1)

        @param u : normalized length value
        @type  u : float

        @return: bool
        """
        return u >= 0.0 and u <= 1.0

    def __abs__(self):
        """
        Length of line segment
        """
        return abs(self.v)

    def magnitude_squared(self):
        """
        Square of line segment length
        """
        return self.v.magnitude_squared()

    def _swap(self):
        # used by connect methods to switch order of points
        self.p = self.p2
        self.v *= -1
        return self

    length = property(lambda self: abs(self.v))

class Circle(Geometry):
    """
    Circle class, assumed to be in a 2D space

    Public attributes::
    
      c - center (Point2 instance)
      r - radius (float)
    """
    __slots__ = ['c', 'r']

    def __init__(self, center, radius):
        """
        Create a Circle instance

        Note that a circle can be created from three points:
        http://paulbourke.net/geometry/circlesphere/

        @type center - Point2 instance
        @type radius - float
        """
        assert isinstance(center, Vector2) and type(radius) == float
        self.c = center.copy()
        self.r = radius

    def __copy__(self):
        """
        Returns a copy of itself
        """
        return self.__class__(self.c, self.r)

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Circle(<%.2f, %.2f>, radius=%.2f)' % \
            (self.c.x, self.c.y, self.r)

    def _apply_transform(self, t):
        """
        Moves the center of the circle by a scaling factor t
        """
        self.c = t * self.c

    def intersect(self, other):
        """
        True if this Circle instance intersects another
        """
        return other._intersect_circle(self)

    def _intersect_point2(self, other):
        return _intersect_point2_circle(other, self)

    def _intersect_line2(self, other):
        return _intersect_line2_circle(other, self)

    def connect(self, other):
        return other._connect_circle(self)

    def _connect_point2(self, other):
        return _connect_point2_circle(other, self)

    def _connect_line2(self, other):
        c = _connect_circle_line2(self, other)
        if c:
            return c._swap()

    def _connect_circle(self, other):
        return _connect_circle_circle(other, self)

# 2D Geometry

def _intersect_point2_circle(P, C):
    """
    Returns True if point P lies with circle C.

    @type P : Point2 instance
    @type C : Circle instance
    """
    return abs(P - C.c) <= C.r

def _intersect_line2_line2(A, B):
    """
    Intersection point of two lines in 2 dimensions

    Reference
    =========
    http://en.wikipedia.org/wiki/Line-line_intersection
    http://paulbourke.net/geometry/pointlineplane/

    @param A : one line
    @type  A : Line2 instance

    @param B : another line
    @type  B : line2 instance

    @return: Point2 instance or None
    """
    d = B.v.y * A.v.x - B.v.x * A.v.y
    if d == 0:
        # the lines are parallel
        return None

    dy = A.p.y - B.p.y
    dx = A.p.x - B.p.x
    ua = (B.v.x * dy - B.v.y * dx) / d
    if not A._u_in(ua):
        return None
    ub = (A.v.x * dy - A.v.y * dx) / d
    if not B._u_in(ub):
        return None

    return Point2(A.p.x + ua * A.v.x,
                  A.p.y + ua * A.v.y)

def _intersect_line2_circle(L, C):
    """
    Returns the point or points at which line L intercepts circle C

    @type L : Line2 instance
    @type C : Circle instance

    @return: one Point2 instance or a LineSegment2 instance or None
    """
    a = L.v.magnitude_squared()
    b = 2 * (L.v.x * (L.p.x - C.c.x) + \
             L.v.y * (L.p.y - C.c.y))
    c = C.c.magnitude_squared() + \
        L.p.magnitude_squared() - \
        2 * C.c.dot(L.p) - \
        C.r ** 2
    det = b ** 2 - 4 * a * c
    if det < 0:
        return None
    sq = math.sqrt(det)
    u1 = (-b + sq) / (2 * a)
    u2 = (-b - sq) / (2 * a)
    if not L._u_in(u1):
        u1 = max(min(u1, 1.0), 0.0)
    if not L._u_in(u2):
        u2 = max(min(u2, 1.0), 0.0)

    # Tangent
    if u1 == u2:
        return Point2(L.p.x + u1 * L.v.x,
                      L.p.y + u1 * L.v.y)

    return LineSegment2(Point2(L.p.x + u1 * L.v.x,
                               L.p.y + u1 * L.v.y),
                        Point2(L.p.x + u2 * L.v.x,
                               L.p.y + u2 * L.v.y))

def _connect_point2_line2(P, L):
    """
    Minimum distance between a point and a line
    
    http://paulbourke.net/geometry/pointlineplane/
    
    @type P : Point2
    @type L : Line2
    """
    d = L.v.magnitude_squared()
    assert d != 0
    u = ((P.x - L.p.x) * L.v.x + \
         (P.y - L.p.y) * L.v.y) / d
    if not L._u_in(u):
        u = max(min(u, 1.0), 0.0)
    return LineSegment2(P,
                        Point2(L.p.x + u * L.v.x,
                               L.p.y + u * L.v.y))

def _connect_point2_circle(P, C):
    """
    @type P : Point2
    @type C : Circle
    """
    v = P - C.c
    v.normalize()
    v *= C.r
    return LineSegment2(P, Point2(C.c.x + v.x, C.c.y + v.y))

def _connect_line2_line2(A, B):
    """
    """
    d = B.v.y * A.v.x - B.v.x * A.v.y
    if d == 0:
        # Parallel, connect an endpoint with a line
        if isinstance(B, Ray2) or isinstance(B, LineSegment2):
            p1, p2 = _connect_point2_line2(B.p, A)
            return p2, p1
        # No endpoint (or endpoint is on A), possibly choose arbitrary point
        # on line.
        return _connect_point2_line2(A.p, B)

    dy = A.p.y - B.p.y
    dx = A.p.x - B.p.x
    ua = (B.v.x * dy - B.v.y * dx) / d
    if not A._u_in(ua):
        ua = max(min(ua, 1.0), 0.0)
    ub = (A.v.x * dy - A.v.y * dx) / d
    if not B._u_in(ub):
        ub = max(min(ub, 1.0), 0.0)

    return LineSegment2(Point2(A.p.x + ua * A.v.x, A.p.y + ua * A.v.y),
                        Point2(B.p.x + ub * B.v.x, B.p.y + ub * B.v.y))

def _connect_circle_line2(C, L):
    d = L.v.magnitude_squared()
    assert d != 0
    u = ((C.c.x - L.p.x) * L.v.x + (C.c.y - L.p.y) * L.v.y) / d
    if not L._u_in(u):
        u = max(min(u, 1.0), 0.0)
    point = Point2(L.p.x + u * L.v.x, L.p.y + u * L.v.y)
    v = (point - C.c)
    v.normalize()
    v *= C.r
    return LineSegment2(Point2(C.c.x + v.x, C.c.y + v.y), point)

def _connect_circle_circle(A, B):
    """
    Intersection of two circles

    http://paulbourke.net/geometry/circlesphere/

    @type A : Circle instance
    @type B : Circle instance
    @return: LineSegment2 instance
    """
    v = B.c - A.c
    d = v.magnitude()
    if A.r >= B.r and d < A.r:
        #centre B inside A
        s1,s2 = +1, +1
    elif B.r > A.r and d < B.r:
        #centre A inside B
        s1,s2 = -1, -1
    elif d >= A.r and d >= B.r:
        s1,s2 = +1, -1
    v.normalize()
    return LineSegment2(Point2(A.c.x + s1 * v.x * A.r, A.c.y + s1 * v.y * A.r),
                        Point2(B.c.x + s2 * v.x * B.r, B.c.y + s2 * v.y * B.r))


# 3D Geometry
# -------------------------------------------------------------------------

def _connect_point3_line3(P, L):
    """
    Minimum Distance between a point and a line

    http://paulbourke.net/geometry/pointlineplane/

    @type P : Point3 instance
    @type L : Line3 instance

    @return: LineSegment3 instance
    """
    d = L.v.magnitude_squared()
    assert d != 0
    u = ((P.x - L.p.x) * L.v.x + \
         (P.y - L.p.y) * L.v.y + \
         (P.z - L.p.z) * L.v.z) / d
    if not L._u_in(u):
        u = max(min(u, 1.0), 0.0)
    return LineSegment3(P, Point3(L.p.x + u * L.v.x,
                                  L.p.y + u * L.v.y,
                                  L.p.z + u * L.v.z))

def _connect_point3_sphere(P, S):
    """
    """
    v = P - S.c
    v.normalize()
    v *= S.r
    return LineSegment3(P, Point3(S.c.x + v.x, S.c.y + v.y, S.c.z + v.z))

def _connect_point3_plane(p, plane):
    """
    Minimum distance between a point and a plane

    http://paulbourke.net/geometry/pointlineplane/

    @type p : Point3 instance
    @type plane : Plane instance

    @return: LineSegment3 instance
    """
    n = plane.n.normalized()
    d = p.dot(plane.n) - plane.k
    return LineSegment3(p, Point3(p.x - n.x * d, p.y - n.y * d, p.z - n.z * d))

def _connect_line3_line3(A, B):
    """
    The shortest line between two lines in 3D

    http://paulbourke.net/geometry/pointlineplane/
    
    @type A : Vector3 instance
    @type B : Vector3 instance

    @return: LineSegment3 instance
    """
    assert A.v and B.v
    p13 = A.p - B.p
    d1343 = p13.dot(B.v)
    d4321 = B.v.dot(A.v)
    d1321 = p13.dot(A.v)
    d4343 = B.v.magnitude_squared()
    denom = A.v.magnitude_squared() * d4343 - d4321 ** 2
    if denom == 0:
        # Parallel, connect an endpoint with a line
        if isinstance(B, Ray3) or isinstance(B, LineSegment3):
            return _connect_point3_line3(B.p, A)._swap()
        # No endpoint (or endpoint is on A), possibly choose arbitrary
        # point on line.
        return _connect_point3_line3(A.p, B)

    ua = (d1343 * d4321 - d1321 * d4343) / denom
    if not A._u_in(ua):
        ua = max(min(ua, 1.0), 0.0)
    ub = (d1343 + d4321 * ua) / d4343
    if not B._u_in(ub):
        ub = max(min(ub, 1.0), 0.0)
    return LineSegment3(Point3(A.p.x + ua * A.v.x,
                               A.p.y + ua * A.v.y,
                               A.p.z + ua * A.v.z),
                        Point3(B.p.x + ub * B.v.x,
                               B.p.y + ub * B.v.y,
                               B.p.z + ub * B.v.z))

def _connect_line3_plane(L, P):
    """
    Intersection of a plane and a line

    http://paulbourke.net/geometry/pointlineplane/

    @type L : Line3 instance
    @type P : Plane instance

    @return: LineSegment3 or None
    """
    d = P.n.dot(L.v)
    if not d:
        # Parallel, choose an endpoint
        return _connect_point3_plane(L.p, P)
    u = (P.k - P.n.dot(L.p)) / d
    if not L._u_in(u):
        # intersects out of range, choose nearest endpoint
        u = max(min(u, 1.0), 0.0)
        return _connect_point3_plane(Point3(L.p.x + u * L.v.x,
                                            L.p.y + u * L.v.y,
                                            L.p.z + u * L.v.z), P)
    # Intersection
    return None

def _connect_sphere_line3(S, L):
    """
    Shortest LineSegment3 perpendicular to L which connects L to surface of S

    Example::

     In [1]: c = Point3(0,0,0)
     In [2]: s = Sphere(c,1.)
     In [3]: e1 = Point3(0.5, 0,-1)
     In [4]: e2 = Point3(0.5, 0, 1)
     In [5]: l = Line3(e1,e2)
     In [6]: _connect_sphere_line3(s,l)
     Out[6]: LineSegment3(<1.00, 0.00, 0.00> to <0.50, 0.00, 0.00>)
     In [7]: e3 = Point3(1.5, 0, -1)
     In [8]: e4 = Point3(1.5, 0,  1)
     In [9]: l2 = Line3(e3,e4)
     In [10]: _connect_sphere_line3(s,l2)
     Out[10]: LineSegment3(<1.00, 0.00, 0.00> to <1.50, 0.00, 0.00>)
    """
    d = L.v.magnitude_squared()
    assert d != 0
    u = ((S.c.x - L.p.x) * L.v.x + \
         (S.c.y - L.p.y) * L.v.y + \
         (S.c.z - L.p.z) * L.v.z) / d
    if not L._u_in(u):
        u = max(min(u, 1.0), 0.0)
    point = Point3(L.p.x + u * L.v.x, L.p.y + u * L.v.y, L.p.z + u * L.v.z)
    v = (point - S.c)
    v.normalize()
    v *= S.r
    return LineSegment3(Point3(S.c.x + v.x, S.c.y + v.y, S.c.z + v.z), 
                        point)

def _connect_sphere_sphere(A, B):
    v = B.c - A.c
    d = v.magnitude()
    if A.r >= B.r and d < A.r:
        #centre B inside A
        s1,s2 = +1, +1
    elif B.r > A.r and d < B.r:
        #centre A inside B
        s1,s2 = -1, -1
    elif d >= A.r and d >= B.r:
        s1,s2 = +1, -1

    v.normalize()
    return LineSegment3(Point3(A.c.x + s1* v.x * A.r,
                               A.c.y + s1* v.y * A.r,
                               A.c.z + s1* v.z * A.r),
                        Point3(B.c.x + s2* v.x * B.r,
                               B.c.y + s2* v.y * B.r,
                               B.c.z + s2* v.z * B.r))

def _connect_sphere_plane(S, P):
    c = _connect_point3_plane(S.c, P)
    if not c:
        return None
    p2 = c.p2
    v = p2 - S.c
    v.normalize()
    v *= S.r
    return LineSegment3(Point3(S.c.x + v.x, S.c.y + v.y, S.c.z + v.z), 
                        p2)

def _connect_plane_plane(A, B):
    """
    A line segment joining two parallel planes
    Example::
    
      In [53]: A = Plane(Point3(0,1,0),Point3(1,0,0),Point3(1,1,0))
      In [54]: B = Plane(Point3(0,1,1),Point3(1,0,1),Point3(1,1,1))
      In [55]: A,B
      Out[55]: (Plane(<0.00, 0.00, 1.00>.p = 0.00),
                Plane(<0.00, 0.00, 1.00>.p = 1.00))
      In [56]: _connect_plane_plane(A,B)
      Out[56]: LineSegment3(<0.00, 0.00, 0.00> to <0.00, 0.00, 1.00>)

    @type A : Plane instance
    @type B : Plane instance
    @return: LineSegment3 instance or None (if planes intersect)
    """
    if A.n.cross(B.n):
        # Planes intersect
        return None
    else:
        # Planes are parallel, connect to arbitrary point
        return _connect_point3_plane(A._get_point(), B)

def _intersect_point3_sphere(P, S):
    """
    Returns True if the point is in or on the sphere

    @type P : Point3 instance
    @type S : Sphere instance

    @return: boolean
    """
    return abs(P - S.c) <= S.r
    
def _intersect_line3_sphere(L, S):
    """
    Intersection of a line and a sphere

    http://paulbourke.net/geometry/circlesphere/

    Example::
    
     In [5]: from Math.euclid import *
     In [6]: c = Vector3(1,1,1)
     In [7]: S = Sphere(c,1)
     In [8]: L = Line3(Point3(0,0,0),Point3(2,2,2))
     In [10]: from Math.euclid import _intersect_line3_sphere
     In [11]: _intersect_line3_sphere(L, S)
     Out[11]: LineSegment3(<1.58, 1.58, 1.58> to <0.42, 0.42, 0.42>)

    @type L : Line3 instance
    @type S : Sphere instance

    @return: LineSegment3 instance or None
    """
    a = L.v.magnitude_squared()
    b = 2 * (L.v.x * (L.p.x - S.c.x) + \
             L.v.y * (L.p.y - S.c.y) + \
             L.v.z * (L.p.z - S.c.z))
    c = S.c.magnitude_squared() + \
        L.p.magnitude_squared() - \
        2 * S.c.dot(L.p) - \
        S.r ** 2
    det = b ** 2 - 4 * a * c
    if det < 0:
        return None
    sq = math.sqrt(det)
    u1 = (-b + sq) / (2 * a)
    u2 = (-b - sq) / (2 * a)
    if not L._u_in(u1):
        u1 = max(min(u1, 1.0), 0.0)
    if not L._u_in(u2):
        u2 = max(min(u2, 1.0), 0.0)
    return LineSegment3(Point3(L.p.x + u1 * L.v.x,
                               L.p.y + u1 * L.v.y,
                               L.p.z + u1 * L.v.z),
                        Point3(L.p.x + u2 * L.v.x,
                               L.p.y + u2 * L.v.y,
                               L.p.z + u2 * L.v.z))

def _intersect_line3_plane(L, P):
    """
    Intersection of a plane by a line
    """
    d = P.n.dot(L.v)
    if not d:
        # Parallel
        return None
    u = (P.k - P.n.dot(L.p)) / d
    if not L._u_in(u):
        return None
    return Point3(L.p.x + u * L.v.x,
                  L.p.y + u * L.v.y,
                  L.p.z + u * L.v.z)

def _intersect_plane_plane(A, B):
    """
    The intersection of two planes

    http://paulbourke.net/geometry/pointlineplane/

    @type A : Plane instance
    @type B : Plane instance
    @return: Line3 instance or None
    """
    n1_m = A.n.magnitude_squared()
    n2_m = B.n.magnitude_squared()
    n1d2 = A.n.dot(B.n)
    det = n1_m * n2_m - n1d2 ** 2
    if det == 0:
        # Parallel
        return None
    c1 = (A.k * n2_m - B.k * n1d2) / det
    c2 = (B.k * n1_m - A.k * n1d2) / det
    return Line3(Point3(c1 * A.n.x + c2 * B.n.x,
                        c1 * A.n.y + c2 * B.n.y,
                        c1 * A.n.z + c2 * B.n.z), 
                 A.n.cross(B.n))

class Point3(Vector3, Geometry):
    """
    Point in 3D space.
    """
    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Point3(%.2f, %.2f, %.2f)' % (self.x, self.y, self.z)

    def intersect(self, other):
        """
        Points intersect if they have the same coordinates
        """
        return other._intersect_point3(self)

    def _intersect_sphere(self, other):
        """
        Returns True if self is in or on the sphere 'other'

        Example::

          In [1]: s
          Out[1]: Sphere(<0.00, 0.00, 0.00>, radius=1.00)
          In [2]: p = Point3(0,0,1)
          In [3]: p._intersect_sphere(s)
          Out[3]: True
          In [4]: p1 = Point3(0,0,1.1)
          In [5]: p1._intersect_sphere(s)
          Out[5]: False
          In [6]: p2 = Point3(0,0,0.9)
          In [7]: p2._intersect_sphere(s)
          Out[7]: True
        """
        return _intersect_point3_sphere(self, other)

    def connect(self, other):
        """
        A connection between two 3D points is a 3D line segment
        """
        return other._connect_point3(self)

    def _connect_point3(self, other):
        """
        Returns LineSegment3 if the self and other are not the same
        """
        if self != other:
            return LineSegment3(other, self)
        return None

    def _connect_line3(self, other):
        """
        Line from 3D point to a 3D line

        Example::
        
          In [20]: e1 = Point3(0.5, 0, -1)
          In [21]: e2 = Point3(0.5, 0,  1)
          In [22]: l = Line3(e1, e2)
          In [23]: p
          In [24]: p = Point3(0, 0, 0.5)
          In [25]: p._connect_line3(l)
          Out[25]: LineSegment3(<0.50, 0.00, 0.50> to <0.00, 0.00, 0.50>)

        @type other : Line3 instance

        @return: LineSegment3 instance
        """
        c = _connect_point3_line3(self, other)
        if c:
            return c._swap()
        
    def _connect_sphere(self, other):
        """
        Shortest LineSegment3 from self to surface of other.
        
        Example::
        
          In [27]: p
          Out[27]: Point3(0.00, 0.00, 0.50)
          In [28]: s
          Out[28]: Sphere(<0.00, 0.00, 0.00>, radius=1.00)
          In [29]: p._connect_sphere(s)
          Out[29]: LineSegment3(<0.00, 0.00, 1.00> to <0.00, 0.00, 0.50>)
        """
        c = _connect_point3_sphere(self, other)
        if c:
            return c._swap()

    def _connect_plane(self, other):
        """
        """
        c = _connect_point3_plane(self, other)
        if c:
            return c._swap()

class Line3:
    """
    Line in 3D space, represented as a Point3 plus a Vector3

    Public Attributes::

     p - Point3 for one point on the line
     v - Vector3 to the other point of the line
    """
    __slots__ = ['p', 'v']

    def __init__(self, *args):
        """
        Create Line3 instance

        A line can be defined::
        
         - a point, a vector and a length, or by
         - two points, or
         - a point and a vector, or by
         - another line.
        """
        if len(args) == 3:
            assert isinstance(args[0], Point3) and \
                   isinstance(args[1], Vector3) and \
                   type(args[2]) == float
            self.p = args[0].copy()
            self.v = args[1] * args[2] / abs(args[1])
        elif len(args) == 2:
            if isinstance(args[0], Point3) and isinstance(args[1], Point3):
                self.p = args[0].copy()
                self.v = args[1] - args[0]
            elif isinstance(args[0], Point3) and isinstance(args[1], Vector3):
                self.p = args[0].copy()
                self.v = args[1].copy()
            else:
                raise AttributeError, '%r' % (args,)
        elif len(args) == 1:
            if isinstance(args[0], Line3):
                self.p = args[0].p.copy()
                self.v = args[0].v.copy()
            else:
                raise AttributeError, '%r' % (args,)
        else:
            raise AttributeError, '%r' % (args,)
        
        # XXX This is annoying.
        #if not self.v:
        #    raise AttributeError, 'Line has zero-length vector'

    def __copy__(self):
        """
        Returns a copy of itself
        """
        return self.__class__(self.p, self.v)

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Line3(<%.2f, %.2f, %.2f> + u<%.2f, %.2f, %.2f>)' % \
            (self.p.x, self.p.y, self.p.z, self.v.x, self.v.y, self.v.z)

    p1 = property(lambda self: self.p)
    p2 = property(lambda self: Point3(self.p.x + self.v.x, 
                                      self.p.y + self.v.y,
                                      self.p.z + self.v.z))

    def _apply_transform(self, t):
        """
        Applies the transform 't' to the 3D line.

        The first point coordinates and the line vector are transgformed by t

        @param t : Matrix4 or float
        """
        self.p = t * self.p
        self.v = t * self.v

    def _u_in(self, u):
        return True

    def intersect(self, other):
        """
        Intersection of self with other.

        This is defined if other is a Plane or Sphere instance
        """
        return other._intersect_line3(self)

    def _intersect_sphere(self, other):
        """
        Returns a LineSegment3 instance for the intersection of self and other
        """
        return _intersect_line3_sphere(self, other)

    def _intersect_plane(self, other):
        """
        Intersection point of self with Plane 'other'
        """
        return _intersect_line3_plane(self, other)

    def connect(self, other):
        """
        The shortest LineSegment3 between Line3 'other' and self
        """
        return other._connect_line3(self)

    def _connect_point3(self, other):
        """
        LineSegment3 which is the minimum distance between a point and self
        """
        return _connect_point3_line3(other, self)

    def _connect_line3(self, other):
        """
        The shortest LineSegment3 between Line3 'other' and self
        """
        return _connect_line3_line3(other, self)

    def _connect_sphere(self, other):
        """
        Shortest LineSegment3 connecting self to surface of other
        """
        return _connect_sphere_line3(other, self)

    def _connect_plane(self, other):
        """
        Shortest LineSegment3 connecting self to surface of othe
        """
        c = _connect_line3_plane(self, other)
        if c:
            return c

class Ray3(Line3):
    """
    Semi-infinite line in 3D space
    """
    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Ray3(<%.2f, %.2f, %.2f> + u<%.2f, %.2f, %.2f>)' % \
            (self.p.x, self.p.y, self.p.z, self.v.x, self.v.y, self.v.z)

    def _u_in(self, u):
        return u >= 0.0

class LineSegment3(Line3):
    """
    Line segment in 3D space
    """
    def __repr__(self):
        """
        Text representing this instance
        """
        return 'LineSegment3(<%.2f, %.2f, %.2f> to <%.2f, %.2f, %.2f>)' % \
            (self.p.x, self.p.y, self.p.z,
             self.p.x + self.v.x, self.p.y + self.v.y, self.p.z + self.v.z)

    def _u_in(self, u):
        return u >= 0.0 and u <= 1.0

    def __abs__(self):
        return abs(self.v)

    def magnitude_squared(self):
        """
        Returns square of LineSegment3 'self'
        """
        return self.v.magnitude_squared()

    def _swap(self):
        # used by connect methods to switch order of points
        self.p = self.p2
        self.v *= -1
        return self

    length = property(lambda self: abs(self.v))

class Sphere:
    """
    Sphere in 3D space

    Public attributes::

      c - center (Vector3)
      r - radius (float)
    """
    __slots__ = ['c', 'r']

    def __init__(self, center, radius):
        """
        Creates an instance of Sphere

        Note that a sphere can be created from four Point3 instances:
        http://paulbourke.net/geometry/circlesphere/
        
        @type center : Vector3
        @type radius : float
        """
        assert isinstance(center, Vector3) and (type(radius) == float
                                             or type(radius) == int)
        self.c = center.copy()
        self.r = radius

    def __copy__(self):
        """
        Returns a copy of itself
        """
        return self.__class__(self.c, self.r)

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Sphere(<%.2f, %.2f, %.2f>, radius=%.2f)' % \
            (self.c.x, self.c.y, self.c.z, self.r)

    def _apply_transform(self, t):
        """
        Applies the transform to the center point
        """
        self.c = t * self.c

    def intersect(self, other):
        """
        """
        return other._intersect_sphere(self)

    def _intersect_point3(self, other):
        """
        Returns True if the point 'other' is in or on self
        """
        return _intersect_point3_sphere(other, self)

    def _intersect_line3(self, other):
        """
        Returns intersection of Line3 instance with self
        """
        return _intersect_line3_sphere(other, self)

    def connect(self, other):
        return other._connect_sphere(self)

    def _connect_point3(self, other):
        return _connect_point3_sphere(other, self)

    def _connect_line3(self, other):
        c = _connect_sphere_line3(self, other)
        if c:
            return c._swap()

    def _connect_sphere(self, other):
        return _connect_sphere_sphere(other, self)

    def _connect_plane(self, other):
        c = _connect_sphere_plane(self, other)
        if c:
            return c

class Plane:
    """
    A 3D plane class

    Public attributes::
    
     n - normalized Vector3 normal to the plane
     k - normal distance from the origin = n.p where p is a Point3 in the plane
    """
    __slots__ = ['n', 'k']

    def __init__(self, *args):
        """
        Equation of a plane
        
        The arguments may be::
        
         - a set of three Point3 instances, or
         - a Point3 in the plane and a Vector3 normal to it, or
         - a Vecto3 normal and a float for distance from the origin.

        Examples::
        
          In [14]: Plane(Point3(),Point3(1,0,0),Point3(0,1,0))
          Out[14]: Plane(<0.00, 0.00, 1.00>.p = 0.00)
          In [15]: Plane(Point3(0,0,1),Point3(1,0,1),Point3(0,1,1))
          Out[15]: Plane(<0.00, 0.00, 1.00>.p = 1.00)
          In [16]: Plane(Point3(0,0,0),Vector3(0,0,1))
          Out[16]: Plane(<0.00, 0.00, 1.00>.p = 0.00)
          In [17]: Plane(Point3(0,0,1),Vector3(0,0,1))
          Out[17]: Plane(<0.00, 0.00, 1.00>.p = 1.00)

        Reference
        =========
        http://paulbourke.net/geometry/pointlineplane/
        """
        if len(args) == 3:
            assert isinstance(args[0], Point3) and \
                   isinstance(args[1], Point3) and \
                   isinstance(args[2], Point3)
            self.n = (args[1] - args[0]).cross(args[2] - args[0])
            module_logger.debug("Instantiating Plane; normal: %s",self.n)
            module_logger.debug("normal is %s", type(self.n))
            self.n.normalize()
            self.k = self.n.dot(args[0])
        elif len(args) == 2:
            if isinstance(args[0], Point3) and isinstance(args[1], Vector3):
                self.n = args[1].normalized()
                self.k = self.n.dot(args[0])
            elif isinstance(args[0], Vector3) and type(args[1]) == float:
                self.n = args[0].normalized()
                self.k = args[1]
            else:
                raise AttributeError, '%r' % (args,)

        else:
            raise AttributeError, '%r' % (args,)
        module_logger.debug("normal is null:",(self.n == Vector3(0,0,0)))
        #if not self.n:  <<< doesn't work
        if self.n.__nonzero__() == False:
            raise AttributeError, 'Points on plane are colinear'

    def __copy__(self):
        """
        Returns a copy of itself
        """
        return self.__class__(self.n, self.k)

    copy = __copy__

    def __repr__(self):
        """
        Text representing this instance
        """
        return 'Plane(<%.2f, %.2f, %.2f>.p = %.2f)' % \
            (self.n.x, self.n.y, self.n.z, self.k)

    def _get_point(self):
        """
        Return an arbitrary point on the plane
        """
        if self.n.z:
            return Point3(0., 0., self.k / self.n.z)
        elif self.n.y:
            return Point3(0., self.k / self.n.y, 0.)
        else:
            return Point3(self.k / self.n.x, 0., 0.)

    def _apply_transform(self, t):
        """
        """
        p = t * self._get_point()
        self.n = t * self.n
        self.k = self.n.dot(p)

    def intersect(self, other):
        return other._intersect_plane(self)

    def _intersect_line3(self, other):
        """
        Returns point of intersection of Line3 with self
        """
        return _intersect_line3_plane(other, self)

    def _intersect_plane(self, other):
        """
        Intersection of self with other

        @return: Line3 instance
        """
        return _intersect_plane_plane(self, other)

    def connect(self, other):
        return other._connect_plane(self)

    def _connect_point3(self, other):
        return _connect_point3_plane(other, self)

    def _connect_line3(self, other):
        return _connect_line3_plane(other, self)

    def _connect_sphere(self, other):
        return _connect_sphere_plane(other, self)

    def _connect_plane(self, other):
        return _connect_plane_plane(other, self)

