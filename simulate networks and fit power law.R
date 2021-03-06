# install.packages('igraph')
library(igraph)
size = 50
# tree graph with two children for each node
g = graph.tree(size, children = 2)
plot(g)

# star network
g = graph.star(size)
plot(g)

# full network
g = graph.full(size)
plot(g)

# ring network
g = graph.ring(size)
plot(g)

# connect the node with the vertices not farther than a given
limit
g = connect.neighborhood(graph.ring(size), 4)
plot(g)

# random network
g = erdos.renyi.game(size, 0.1)
plot(g)
# LIMITATOINS: To0 Random, does not follow power law.

# Using Watts-Strogatz Model

g2 <- watts.strogatz.game(1, 6, 5, 0.05)
g2
plot(g2)

# LIMITATOINS: Fixed no of nodes, does not follow power law.

# scale-free network -> a  network whose degree 
#distribution follows power law asymptotically
##based on barabasi Albert model
'''It incorporates two important general concepts:
  growth and preferential attachment. Both growth and preferential 
attachment exist widely in real networks.

Growth means that the number of nodes in the network increases over time.
Preferential attachment means that the more connected a node is,
the more likely it is to receive new links.'''

g = barabasi.game(size)
plot(g)

# another wat to generate scale-free graph
g1 <- sample_pa(50)
plot(g1)

# degree and degree distribution
d = degree(g, mode = "all")
hist(d, probability = T)

dd = degree.distribution(g, mode = "all", cumulative = FALSE)


# Plot degree distribution
# write a function to plot the degree distribution
plot_degree_distribution = function(graph) {
  # calculate degree
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative =
                             FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree
(log)", ylab = "Probability (log)",
       col = 1, main = "Degree Distribution")
}
plot_degree_distribution(g)


g.big.ba = barabasi.game(2000)
g.big.er = erdos.renyi.game(2000, 0.1)
plot_degree_distribution(g.big.ba)
plot_degree_distribution(g.big.er)





# plot and fit the power law distribution
# Form: Y = k/X^alpha,
# here: probability = k/degree^a
# taking log: log(probability) = log(k)-a*log(degree)
# if we do simple linear regression  of log(probability) ~ log(degree):
# then intercept = log(k)
# slope = -a
## hence, value of a = -slope
fit_power_law = function(graph) {
  # calculate degree
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative =
                             FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] *
                                    log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree
(log)", ylab = "Probability (log)",
       col = 1, main = "Degree Distribution")
  curve(power.law.fit, col = "red", add = T, n = length(d))
}

g.big.ba = barabasi.game(1000)
fit_power_law(g.big.ba)

g.big.ba = barabasi.game(10000)
fit_power_law(g.big.ba)
