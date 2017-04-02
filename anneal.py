# -*- coding: iso-8859-15 -*-
""" 
Traveling salesman problem solved using Simulated Annealing.

Kristjan Haule
email: haule@physics.rutgers.edu

The steps consis of::
 • Pick initial configuration X and an initial temperature T0. T0 should be
   much higher than the changes in function f to be minimized when typical
   Monte Carlo step is taken.
 • Loop through decreasing temperature Ti
   – Equilibrate at Ti using Metropolis with selected, allowed elementary 
     changes in the system. The system is equilibrated when the rate of change 
     of f averaged over some number of Monte Carlo steps is small.
   – Measure the thermal average of f. If f does not decrease at this
     temperature Ti compared to previous temperature Ti−1, exit the loop.
   – Decrease Ti to Ti+1. For example, reduces the effective temperature by
     10%. 
The major difficulty (art) in implementation of the algorithm is that there is
no obvious analogy for the temperature T with respect to a free parameter in 
the combinatorial problem. Furthermore, avoidance of entrainment in local 
minima (quenching) is dependent on the ”annealing schedule”, the choice of 
initial temperature, how many iterations are performed at each temperature, and
how much the temperature is decremented at each step as cooling proceeds.

Example: The Traveling Salesman problem

The seller visits N cities (i = 0...N − 1) with given positions Ri, returning 
finally to his city of origin. Each city is to be visited only once, and the 
route is to be made as short as possible. This problem belongs to a class known
as NP-complete problems, whose computation time for an exact solution increases
with N exponentially. The traveling salesman problem also belongs to a class of
minimization problems for which the objective function E has many local minima.
In practical cases, it is often enough to be able to choose from these a 
minimum which, even if not absolute, cannot be significantly improved upon.
The annealing method manages to achieve this, while limiting its calculations 
to scale as a small power of N.

As a problem in simulated annealing, the traveling salesman problem is handled
as follows:
 1. Configuration: The cities are numbered i = 0...N − 1 and each has coordinate
    Ri. A configuration is a permutation of the number 0...N − 1, interpreted as 
    the order in which the cities are visited.
 2. Rearrangements: An efficient set of moves are::
     a. A section of path is removed and then replaced with the same cities 
        running in the opposite order; or
     b. a section of path is removed and then replaced in between two cities on 
        another, randomly chosen, part of the path.
 3. Objective Function: In the simplest form of the problem, E is taken just as
    the total length of journey
   
The above two mentioned moves are hardest to implement. The figure 
travelling_salesman.pdf explains them in an example

http://www.physics.rutgers.edu/~haule/681/MC.pdf
"""
import logging
from scipy import exp, rand, zeros

logger = logging.getLogger(__name__)
   
def anneal(R, Distance, maxTsteps=100, Tstart=0.2, fCool=0.9, Preverse=0.5):
    """
    Approximate the shortest path for visiting all cities
    
    @param R: : an array of node coordinates
    @type  R : numpy array of floats
    
    @param Distance : function to compute distance
    @type  Distance : function
    
    @param maxTsteps : maximum number of temperature steps
    
    @param plotter : optional function for plotting nodes and edges
    @type  plotter : function
    
    @return: tuple of list, list of lists, float
    """
    def transpt(nodes, n):
      nct = len(nodes)
    
      newnodes=[]
      # Segment in the range n[0]...n[1]
      for j in range( (n[1]-n[0])%nct + 1):
        newnodes.append(nodes[ (j+n[0])%nct ])
      # is followed by segment n[5]...n[2]
      for j in range( (n[2]-n[5])%nct + 1):
        newnodes.append(nodes[ (j+n[5])%nct ])
      # is followed by segment n[3]...n[4]
      for j in range( (n[4]-n[3])%nct + 1):
        newnodes.append(nodes[ (j+n[3])%nct ])
      return newnodes
    
    def reverse(nodes, n):
      """
      Switches the positions of two nodes
      
      @param nodes : a list of node numbers
      @type  nodes : list of int
      
      @param n : a pair of nodes
      @type  n : list of lists of int
      """
      nct = len(nodes)
      nn = (1+ ((n[1]-n[0]) % nct))/2 # half the length of the segment to be reversed
      # the segment is reversed in the following way 
      # n[0]<->n[1], n[0]+1<->n[1]-1, n[0]+2<->n[1]-2,...
      # Start at the ends of the segment and swap pairs of cities, moving towards
      # the center.
      for j in range(nn):
         k = (n[0]+j) % nct
         l = (n[1]-j) % nct
         (nodes[k],nodes[l]) = (nodes[l],nodes[k])  # swap

    def TotalDistance(nodes, R):
      """
      Gets the total distance between nodes in a list
      
      @param nodes : a list of node numbers
      @type  nodes : list of int
      
      @param R : an array of node coordinates
      @type  R : numpy array of floats
      """
      dist=0
      for i in range(len(nodes)-1):
        dist += Distance(R[nodes[i]],R[nodes[i+1]])
      dist += Distance(R[nodes[-1]],R[nodes[0]])
      return dist

    # The index table -- the order the cities are visited.
    num_nodes = len(R)
    nodes = range(num_nodes)
    maxSteps = 100*num_nodes     # Number of steps at constant temperature
    maxAccepted = 10*num_nodes   # Number of accepted steps at constant temperature
    # Distance of the travel at the beginning
    dist = TotalDistance(nodes, R)

    # Stores points of a move
    n = zeros(6, dtype=int)
    nct = len(R) # number of cities
    
    T = Tstart # temperature
    
    for t in range(maxTsteps):  # Over temperature

        accepted = 0
        for i in range(maxSteps): # At each temperature, many Monte Carlo steps
            
            while True: # Will find two random cities sufficiently close by
                # Two cities n[0] and n[1] are choosen at random
                n[0] = int((nct)*rand())     # select one city at random
                n[1] = int((nct-1)*rand())   # select another city, but not the same
                if (n[1] >= n[0]): n[1] += 1   #
                if (n[1] < n[0]): (n[0],n[1]) = (n[1],n[0]) # swap, because it must be: n[0]<n[1]
                nn = (n[0]+nct -n[1]-1) % nct  # number of cities not on the segment n[0]..n[1]
                if nn>=3: break
        
            # We want to have one index before and one after the two cities
            # The order hence is [n2,n0,n1,n3]
            n[2] = (n[0]-1) % nct  # index before n0  -- see figure in the lecture notes
            n[3] = (n[1]+1) % nct  # index after n2   -- see figure in the lecture notes
            
            if Preverse > rand(): 
                # Here we reverse a segment
                # What would be the cost to reverse the path between nodes[n[0]]-nodes[n[1]]?
                de = Distance(R[nodes[n[2]]], R[nodes[n[1]]]) + \
                     Distance(R[nodes[n[3]]], R[nodes[n[0]]]) - \
                     Distance(R[nodes[n[2]]], R[nodes[n[0]]]) - \
                     Distance(R[nodes[n[3]]], R[nodes[n[1]]])
                
                if de<0 or exp(-de/T)>rand(): # Metropolis
                    accepted += 1
                    dist += de
                    reverse(nodes, n)
            else:
                # Here we transpose a segment
                nc = (n[1]+1+ int(rand()*(nn-1)))%nct  # Another point outside n[0],n[1] segment.
                n[4] = nc
                n[5] = (nc+1) % nct
        
                # Cost to transpose a segment
                de = -Distance(R[nodes[n[1]]],R[nodes[n[3]]]) - \
                      Distance(R[nodes[n[0]]],R[nodes[n[2]]]) - \
                      Distance(R[nodes[n[4]]],R[nodes[n[5]]])
                de += Distance(R[nodes[n[0]]],R[nodes[n[4]]]) + \
                      Distance(R[nodes[n[1]]],R[nodes[n[5]]]) + \
                      Distance(R[nodes[n[2]]],R[nodes[n[3]]])
                
                if de<0 or exp(-de/T)>rand(): # Metropolis
                    accepted += 1
                    dist += de
                    nodes = transpt(nodes, n)
                    
            if accepted > maxAccepted: break
            
        logger.debug("anneal: T=%10.5f, distance= %10.5f, accepted steps= %d",
                      T, dist, accepted)
        T *= fCool               # The system is cooled down
        if accepted == 0: break  # If the path does not want to change any more, we can stop
    logger.debug("anneal: new node order: %s", list(nodes)) 
    return list(nodes), R.take(list(nodes),axis=0), dist
    
    
    
