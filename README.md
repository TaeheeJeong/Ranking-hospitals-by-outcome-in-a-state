# Ranking-hospitals-by-outcome-in-a-state
rankhospital.R  takes three arguments: the 2-character abbreviated name of a
state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
The function reads the outcome-of-care-measures.csv le and returns a character vector with the name
of the hospital that has the ranking specied by the num argument. For example, the call
rankhospital("MD", "heart failure", 5)
would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
for heart failure. The num argument can take values "best", "worst", or an integer indicating the ranking
(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
state, then the function should return NA.
