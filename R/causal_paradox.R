# causal_paradox.R
# https://www.rpubs.com/osazuwa/
# https://www.rpubs.com/osazuwa/odsc_blitz

raw_data <- read.delim("https://raw.githubusercontent.com/altdeep/causal_ml_seminar/master/data/survey.txt", sep = " ", stringsAsFactors = T)
# Let's reshuffle the data to be safe.
n <- nrow(raw_data)
idx <- sample(1:n, size = n, replace = FALSE)
onethird <- floor(n/3)
eval_data <- raw_data[idx, ][1:onethird, ]
training_data <- raw_data[idx, ][(onethird+1):(2*onethird), ]
test_data <- raw_data[idx, ][(2*onethird + 1):n, ]

head(eval_data)

dag <- empty.graph(nodes = c("A","S","E","O","R","T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set
nodes(dag)

graphviz.plot(dag)

A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")

A.prob <- array(c(0.3,0.5,0.2), dim = 3, dimnames = list(A = A.lv))
S.prob <- array(c(0.6,0.4), dim = 2, dimnames = list(S = S.lv))
E.prob <- array(c(0.75,0.25,0.72,0.28,0.88,0.12,0.64,0.36,0.70,0.30,0.90,0.10), dim = c(2,3,2), dimnames = list(E = E.lv, A = A.lv, S = S.lv))
O.prob <- array(c(0.96,0.04,0.92,0.08), dim = c(2,2), dimnames = list(O = O.lv, E = E.lv))
R.prob <- array(c(0.25,0.75,0.2,0.8), dim = c(2,2), dimnames = list(R = R.lv, E = E.lv))
T.prob <- array(c(0.48,0.42,0.10,0.56,0.36,0.08,0.58,0.24,0.18,0.70,0.21,0.09), dim = c(3,2,2), dimnames = list(T = T.lv, O = O.lv, R = R.lv))
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)

# fit cpt table to network
bn <- custom.fit(dag, cpt)
#plot(bn)

# classic frequentist and maximum likelihood estimates. In bnlearn, we can compute them with t
# he bn.fit function. bn.fit complements the custom.fit function we used in the previous section; 
# the latter constructs a BN using a set of custom parameters specified by the user, while the 
# former estimates the same from the data.

bn.mle <- bn.fit(dag, data = raw_data, method = "mle")
bn.mle

# Note that we assume we know the structure of the DAG, so dag is an input of bn.fit function.
# As an alternative, we can also do Bayesian estimation of the parameters. This will provide 
# the maximum a posterior point values of the posterior. The Bayesian modeling depends on the 
# data type, in the discrete case, it makes use of the Dirichlet conjugate prior.To use Bayesian 
# estimation, set the method argument of bn.fit must be set to "bayes".

bn.bayes <- bn.fit(dag, data = raw_data, method = "bayes", iss = 10)



