# pollards-rho-method

In this function we make use of Pollard's rho method to solve the discrete logarithm problem

$$
g^t = h \in \mathbb{F}_p^*
$$

Where $g$ is a primitive root modulo $p$. The idea is to find a collision between $g^i h^j$ and $g^k h^l$ for some known exponents
$i,j,k,l \in \mathbb{F}_p$

The function we use to create the discrete dynamical system is simple

$$
f(x) = \begin{cases}
gx, 0 \leq x < p/3 \\
x^2, p/3 \leq x < 2p/3 \\
hx, 2p/3 \leq x < p
\end{cases}
$$

## Usage

Simple enough to use:

```haskell
rho_method g h p x_0
```
