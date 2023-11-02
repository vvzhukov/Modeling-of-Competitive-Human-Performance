x = c(5, 9, 3, 2, 8, 6, 3, 5, 4, 5, 3, 5, 6, 6, 6, 3, 1, 4, 3, 3, 5, 4, 5, 2, 5, 5, 6, 5, 5, 5, 4, 3)
m_ln <- dislnorm$new(x)
bsLN <- bootstrap(m_ln, no_of_sims= 1000, seed=1)	
