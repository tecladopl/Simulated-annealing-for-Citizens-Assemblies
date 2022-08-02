# Simulated annealing for Citizens' Assemblies
R script with simulated annealing(GenSA) implementation for randomized selection of Citizens’ Assemblies participants.

## Overview
![Schema](/assets/scene.gif "")

We are going to find the answer for the question - How to draw a group of people from some population in a random way, but make sure that group represents the population with given characteristics exactly or is the closest solution.
###Simulated annealing
Simulated annealing is a metaheuristic algorithm which means it is a conception of a way to solve a particular problem, not the solution itself. Owes its name to metalurgic technique which is similar to. 
Modus operandi overview: If we have to solve an algorithmic problem we need to introduce a mathematical evaluation function with one result. This result allows us to rate the drawn set of suggested solutions. 
We are drawing the first set from the whole population and defining it as the current best solution. When a drawn set of numbers is not satisfying, which can be measured with evaluation function output, another set is drawn, but it has to be close to the previous draw. If the new one is better(or “not very worse”) we are marking it as the new best current solution. We are repeating this until we achieve objectives or when the situation is not getting better for many continuous draws.
In our scenario we are drawing participants from the pool and verifying, if those matching criteria. If not, we are drawing them again and again. While we are getting closer to the right solution, each group drawn becomes more like the previous one.

## Details
### Simulated annealing
In our case scenario simulated annealing algorithm may be described as follows:
1. We set a parameter T called temperature.
2. We randomly select N individuals to form the panel P.
3. We calculate the value of the evaluation function F(P).
4. We draw a new panel composition P' in the neighborhood of P according to normal probability with mean at P and variance equal to T.
5. We calculate the value of the evaluation function F(P').
6. We calculate the value of A=e(F(P)-F(P'))/T. We draw a number x from the interval [0,1]. If it is less than A, we replace the composition of panel P with the composition of panel P'.
7. We reduce the temperature T=nT where n is a constant from the interval (0,1).
8. If the temperature is greater than zero we return to step 3, otherwise we return the composition of panel P.

### Evaluation function
To allow simulated annealing to measure accuracy of its selection we need to introduce an evaluation function depending on the case. For participant’s randomized selection let’s define F(P) function:
![Equation](/assets/equation.png "")

where Xk(P) is the amount of particular characteristic in current iteration, Xk is the number of that characteristic of ideal composition and zk is the parameter of particular characteristic, zk>0 and  zk=1, when every characteristic counts the same.
Let’s assume we need to find a staff for a panel with Xm of men, Xw of women, Xp people with primary degree, Xs people with secondary degree and Xh people with higher degree. Mark amounts of every composition with  Xm(P), Xw(P), Xp(P), Xs(P), Xh(P). Suppose also every part is equal. For that we define evaluation function as:
![Equation](/assets/equation2.png "")

For ideal composition F(P)=0.

### R language implementation
For Simulated annealing implementation The GenSA Package was chosen and the above evaluation function was written. In addition to the evaluation function and initial temperature, GenSA accepts as arguments: the initial values of the function, upstream and downstream constraints for their changes and configuration parameters. For panelists computations verification of possible duplicates is needed. 
Scripts may end in cases: 
* temperature reaches 0,
* ideal composition was found,
* maximum time or number or iterations was exceeded.


This is the reason why it is important to choose the right parameters.
## Bibliography

Generalized Simulated Annealing (The GenSA Package), Gubian S., Xiang Y., Suomela B., Hoeng J., CRAN Project, online: https://cran.r-project.org/web/packages/GenSA/GenSA.pdf

Generalized Simulated Annealing for Global Optimization: The GenSA Package, Gubian S., Xiang Y., Suomela B., Hoeng J., CRAN Project, online: https://journal.r-project.org/archive/2013/RJ-2013-002/RJ-2013-002.pdf

Optymalizacja. Symulowane wyżarzanie, dr hab. inż. Komosiński M., Instytut Informatyki Politechnika Poznańska, online: http://www.cs.put.poznan.pl/mkomosinski/lectures/optimization/SA.pdf

Simulated annealing, Wikipedia, online: https://en.wikipedia.org/wiki/Simulated_annealing

Symulowane wyżarzanie, Wikipedia, online: https://pl.wikipedia.org/wiki/Symulowane_wy%C5%BCarzanie