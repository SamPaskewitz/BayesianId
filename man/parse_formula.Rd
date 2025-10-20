\name{parse_formula}
\alias{parse_formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parse a regression model formula into its component parts.
}
\description{
This is a function to split a model formula (including a complicated brms regression formula) into its component parts. It is not designed to handle the more exotic functionality of brms (nonlinear models, mixture models), just plain old mixed effects generalized linear regression.
}
\usage{
parse_formula(fmla)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmla}{
%%     ~~Describe \code{fmla} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (fmla)
{
    lhs = strsplit(as.character(fmla), "\\s*\\~\\s*")[[2]]
    rhs_terms = attr(terms(fmla), which = "term.labels")
    is_random = grepl("\\|", rhs_terms)
    if (any(is_random)) {
        fixed = rhs_terms[!is_random]
        random = paste0("(", rhs_terms[is_random], ")")
    }
    else {
        fixed = rhs_terms
        random = NULL
    }
    is_interaction = grepl("\\:", fixed)
    if (any(is_interaction)) {
        fixed_main = fixed[!is_interaction]
        fixed_interaction = fixed[is_interaction]
    }
    else {
        fixed_main = fixed
        fixed_interaction = NULL
    }
    return(list(lhs = lhs, fixed = fixed, random = random, fixed_main = fixed_main,
        fixed_interaction = fixed_interaction))
  }
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
