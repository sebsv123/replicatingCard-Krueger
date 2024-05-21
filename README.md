# replicatingCard-Krueger
Short replication sheet code in R for Card and Krueger "Minimum Wages and Employment: A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania" (1994) for an Uni project (econometric class)
I tried my best having the exact same values as referred in Table 4, pg.780, noticing that, despite the difference between my replication and the real models is pretty small (regarding coefficients, sd...), 
is not an easy task emulating the model due the exact path the authors choosed is pretty detailed and is not clarified at all in the paper.
This replication got most of the non-easy details to be noticed in order to reach the exact same results, so this code is open for getting suggestions in order to modify and having a current valid R code able to screen this elegant work.
I also included an alternative model that takes in account some of the dependent variables are non-linear, while heteroskedacity was detected by White test in the original model and RESET test shows the current model is misspecified, so an alternative model is proposed considering these three factors, trying to solve them while not falling into overfitting.
