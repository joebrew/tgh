library(dplyr)

# Number of parameters / coefficients to be estimated : 6
# alternative (1) + 
# route (1) + 
# effectiveness (1) +
# nausea (1) + 
# duration (1) + 
# costs (1)
# Best guess at parameter values
parameters <- c(1.2,
                -0.3,
                0.2,
                -1,
                -0.1,
                -0.01)

# Number of alternatives: 3  (vacc, mda, none)

# Number of choices : 16



df <- 
  expand.grid(alternative = 
                c('vaccine',
                  'mda'),
              effectiveness = 
                c(25, 50, 75, 100),
              nausea = 
                c('none', 'moderate', 'severe'),
              frequency = 
                c(1, 2, 3),
              costs = 
                c(0, 2, 5, 10),
              incidence = c(0, 30, 60))



# FROM http://www.erim.eur.nl/fileadmin/user_upload/R_code_generic.txt
# 
# # Create an attribute table (similar to table 2)
# attribute_table <-
#   data.frame(group = c(rep('alternatives', 4),
#                        rep('drug_administration', 4),
#                        rep('efectiveness', 4),
#                        rep('side_effects', 2),
#                        rep('treatment_repeat', 4),
#                        rep('cost', 4)),
#              name = c(
#                  c('constant', 'alt1', 'alt2', 'alt3'),
#                  rep('drug_administration', 4),
#                  rep('effectiveness', 4),
#                  rep('side_effects', 2),
#                  rep('treatment_repeat', 4),
#                  rep('cost', 4)),
#              description = 
#                c(
#                  # alternatives
#                  c('alternative-specific constant',
#                    'drug alternative 1',
#                    'drug alternative 2',
#                    'opt-out alternative'),
#                  # drug administration
#                  c('one injection',
#                    'two injections',
#                    'pills one day',
#                    'pills one week'),
#                  # effectiveness
#                  c(40, 60, 80, 100),
#                  # side effects
#                  c('nausea_no', 'nausea_yes'),
#                  # treatment repeat
#                  c(1, 2, 5, 10),
#                  # cost
#                  c(1, 5, 20, 50)
#                ),
#              parameter_label = 
#                c(
#                  # alternatives
#                  c('A', 'A', 'A', 'A'),
#                  # drug administration
#                  c('B0', 'B1', 'B2', 'B3'),
#                  # effectiveness
#                  c(rep('C', 4)),
#                  # side effects
#                  c(rep('D', 2)),
#                  # treatment repeat
#                  c(rep('E', 4)),
#                  # cost
#                  c(rep('F', 4))
#                ),
#              parameter_prior_value = 
#                c(
#                  # alternative
#                  c(rep(1.23, 4)),
#                  # drug administration
#                  c(0, -0.31, -0.21, -0.44),
#                  # effectiveness
#                  c(rep(0.028, 4)),
#                  # side effects
#                  c(rep(-1.10, 2)),
#                  # treatment repeat
#                  c(rep(-0.04, 4)),
#                  # cost
#                  c(rep(-0.0015, 4))
#                ),
#              dce_design_code = 
#                c(
#                  # alternative
#                  c(NA, 1, 1, 0),
#                  # drug administration
#                  c(NA, 1, 1, 1),
#                  # effectiveness
#                  c(5, 10, 25, 50),
#                  # side effects
#                  c(0, 1),
#                  # treatment repeat
#                  c(1, 2, 5, 10),
#                  # cost
#                  c(1, 5, 20, 50)
#                ),
#              type = c(
#                rep('alternative', 4),
#                rep('attribute', 18)
#              ))

# # Following steps from paper
# 
# test_alpha=0.05 #…fill in the significance level…
# z_one_minus_alpha<-qnorm(1-test_alpha)
# 
# 
# test_beta= 0.20 #…fill in the statistical power level…
# z_one_minus_beta<-qnorm(1-test_beta)
# 
# #…fill in the initial belief about the parameter values, each value separated by a comma…)
# 
# parameters <-c(1.23, 
#                -0.31,
#                -0.21,
#                -0.44,
#                0.028,
#                -1.1,
#                -0.04,
#                -0.0015) 

# 
# # …fill in the number of parameters to be estimated…
# ncoefficients <- 8
# # …fill in the number of alternatives per choice set…
# nalts <- 3
# # …fill in the number of choice sets…
# nchoices <- 16
# 
# 
# 
# # load the design information
# design<-as.matrix(df)
# 
# 
# #compute the information matrix
# # initialize a matrix of size ncoefficients by ncoefficients filled with zeros.
# info_mat=matrix(rep(0,ncoefficients*ncoefficients), ncoefficients, ncoefficients) 
# # compute exp(design matrix times initial parameter values) 
# exputilities=exp(design%*%parameters)
# # loop over all choice sets
# for (k_set in 1:nchoices) {
#   # select alternatives in the choice set
#   alternatives=((k_set-1)*nalts+1) : (k_set*nalts)
#   # obtain vector of choice shares within the choice set
#   p_set=exputilities[alternatives]/sum(exputilities[alternatives])
#   # also put these probabilities on the diagonal of a matrix that only contains zeros
#   p_diag=diag(p_set)
#   # compute middle term P-pp’
#   middle_term<-p_diag-p_set%o%p_set
#   # pre- and postmultiply with the Xs from the design matrix for the alternatives in this choice set
#   full_term<-t(design[alternatives,])%*%middle_term%*%design[alternatives,]
#   # Add contribution of this choice set to the information matrix
#   info_mat<-info_mat+full_term 
# } # end of loop over choice sets
# #get the inverse of the information matrix (i.e., gets the variance-covariance matrix)
# sigma_beta<-solve(info_mat,diag(ncoefficients)) 
# 
# 
# # Use the parameter values as effect size. Other values can be used here.
# effectsize<-parameters
# # formula for sample size calculation is n>[(z_(beta)+z_(1-alpha))*sqrt(Σγκ)/delta]^2 
# N<-((z_one_minus_beta + z_one_minus_alpha)*sqrt(diag(sigma_beta))/abs(effectsize))^2
# # Display results (required sample size for each coefficient)
# N
# 
