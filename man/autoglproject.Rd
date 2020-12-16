\name{auto.glproject}
\alias{auto.glproject}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatic G League Projection (Data Frame)
%%  ~~function to do ... ~~
}
\description{
This function takes a data frame with the needed G League statistics of any number of players and classifies them as making an NBA rotation (Tier 1 Player) or not making an NBA rotation (Tier 2 Player). It also outputs the probability of players eventually breaking into a rotation (0 to 99.9 percent).
}
\usage{
auto.glproject(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data frame that contains the following statistics: Usage rate, field goal attempts, field goal percentage, three point attempts, three point percentage, free throw attempts, free throw percentage, rebounds, assists, steals, blocks, and fouls.
}
}
\details{
All statistics should be per 100 possessions where possible.

Percentages (Field Goal percentage, Free Throws, Threes) should be in decimal form.

Rates (Usage) should remain as whole numbers with decimals (Between 0 and 100).
}
\value{
Outputs a data frame with classified tiers and rotation probabilities.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jake Coyne, Freelance NBA & BYU Statistics
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
